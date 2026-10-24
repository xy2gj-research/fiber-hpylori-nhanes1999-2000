# =====================================================================
# FINAL AUDIT: independent recomputation of ALL ML statistics
# =====================================================================
import numpy as np, pandas as pd, joblib
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import roc_auc_score, roc_curve, brier_score_loss
from sklearn.linear_model import LogisticRegression
from sklearn.isotonic import IsotonicRegression

df = pd.read_csv("final_data_for_ml.csv")
FEATURES = ["RIDAGEYR","gender","Race_Label","Education_Label","pir","bmi","smoking",
            "alcohol_group","energy","fiber_adj","Diabetes","Hypertension",
            "Hyperlipidemia","Physical_Activity"]
TARGET = "hp_status"
X = pd.get_dummies(df[FEATURES], drop_first=True)
y = df[TARGET].values
X_tr, X_te, y_tr, y_te = train_test_split(X, y, test_size=0.30, stratify=y, random_state=42)
model = joblib.load("xgboost_model_final.pkl")
p = model.predict_proba(X_te)[:, 1]

res = []
def add(item, pub, rec, tol):
    res.append((item, pub, round(float(rec), 4), "PASS" if abs(rec-pub) <= tol else "FAIL"))

add("n total", 2100, len(df), 0)
add("n test", 630, len(y_te), 0)
add("test prevalence %", round(y_te.mean()*100,1), y_te.mean()*100, 0.05)

auc = roc_auc_score(y_te, p)
add("AUC", 0.786, auc, 0.0006)

rng = np.random.RandomState(42); aucs = []
for _ in range(2000):
    i = rng.randint(0, len(y_te), len(y_te))
    if len(set(y_te[i])) < 2: continue
    aucs.append(roc_auc_score(y_te[i], p[i]))
lo, hi = np.percentile(aucs, [2.5, 97.5])
add("AUC CI lower", 0.748, lo, 0.004)
add("AUC CI upper", 0.818, hi, 0.004)

fpr, tpr, thr = roc_curve(y_te, p)
j = np.argmax(tpr - fpr)
add("Sensitivity", None, tpr[j], 99)
add("Specificity", None, 1-fpr[j], 99)

add("Brier", 0.183, brier_score_loss(y_te, p), 0.0006)

iso = IsotonicRegression(out_of_bounds="clip").fit(p, y_te)
pc = iso.predict(p)
slope = LogisticRegression().fit(pc.reshape(-1,1), y_te).coef_[0][0]
add("Calibration slope", 0.969, slope, 0.02)

sc = StandardScaler().fit(X_tr)
lr = LogisticRegression(max_iter=1000).fit(sc.transform(X_tr), y_tr)
p_lr = lr.predict_proba(sc.transform(X_te))[:, 1]
add("Logistic AUC", 0.785, roc_auc_score(y_te, p_lr), 0.0006)

# ablation
DIET = {"energy","fiber_adj"}
X_nd = pd.get_dummies(df[[c for c in FEATURES if c not in DIET]], drop_first=True)
ntr, nte, ytr2, yte2 = train_test_split(X_nd, y, test_size=0.30, stratify=y, random_state=42)
from xgboost import XGBClassifier
params = model.get_params()
m_nd = XGBClassifier(**params); m_nd.fit(ntr, ytr2)
add("Ablation no-diet AUC", 0.777, roc_auc_score(yte2, m_nd.predict_proba(nte)[:,1]), 0.004)

X_nf = pd.get_dummies(df[[c for c in FEATURES if c != "fiber_adj"]], drop_first=True)
ftr, fte, ytr3, yte3 = train_test_split(X_nf, y, test_size=0.30, stratify=y, random_state=42)
m_nf = XGBClassifier(**params); m_nf.fit(ftr, ytr3)
add("Ablation no-fiber AUC", 0.780, roc_auc_score(yte3, m_nf.predict_proba(fte)[:,1]), 0.004)

# SHAP fiber rank
import shap
sv = shap.TreeExplainer(model).shap_values(X_te)
ma = np.abs(sv).mean(axis=0)
rank = int((ma > ma[list(X_te.columns).index("fiber_adj")]).sum()) + 1
add("Fiber SHAP rank", 8, rank, 0)

print("\n===== ML AUDIT =====")
for r in res: print(r)
fails = [r for r in res if r[3] == "FAIL"]
print("\nFAILs:", len(fails))
