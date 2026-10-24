# Figure 3 v5.1: composite 2x2 (A ROC, B calibration, C SHAP importance, D SHAP beeswarm)
import pandas as pd
import numpy as np
import xgboost as xgb
import shap
from sklearn.model_selection import train_test_split
from sklearn.metrics import roc_curve, auc, brier_score_loss
from sklearn.linear_model import LogisticRegression
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

plt.rcParams["font.family"] = "Arial"
plt.rcParams["axes.linewidth"] = 0.8
OUT = r"E:/hp/研究内容/图表/第二版/"

df = pd.read_csv("final_data_for_ml.csv")
X = df.drop(columns=["hp_status"]); y = df["hp_status"]
categorical_cols = ["gender", "Race_Label", "Education_Label", "smoking",
                    "alcohol_group", "Diabetes", "Hypertension",
                    "Hyperlipidemia", "Physical_Activity"]
X_encoded = pd.get_dummies(X, columns=categorical_cols, drop_first=True)
X_train, X_test, y_train, y_test = train_test_split(
    X_encoded, y, test_size=0.3, random_state=42, stratify=y)

model = xgb.XGBClassifier(n_estimators=100, learning_rate=0.05, max_depth=4,
                          subsample=0.8, colsample_bytree=0.8,
                          objective="binary:logistic", random_state=42, eval_metric="logloss")
model.fit(X_train, y_train)
p = model.predict_proba(X_test)[:, 1]
fpr, tpr, _ = roc_curve(y_test, p)
roc_auc = auc(fpr, tpr)
rng = np.random.RandomState(42)
aucs = []
for _ in range(1000):
    idx = rng.randint(0, len(y_test), len(y_test))
    if len(np.unique(y_test.iloc[idx])) < 2: continue
    f, t, _ = roc_curve(y_test.iloc[idx], p[idx])
    aucs.append(auc(f, t))
lo, hi = np.percentile(aucs, 2.5), np.percentile(aucs, 97.5)
brier = brier_score_loss(y_test, p)
logit_p = np.log(p / (1 - p))
cal = LogisticRegression().fit(logit_p.reshape(-1, 1), y_test)
cal_slope, cal_int = cal.coef_[0][0], cal.intercept_[0]
lr = LogisticRegression(max_iter=2000).fit(X_train, y_train)
p_lr = lr.predict_proba(X_test)[:, 1]
fpr_lr, tpr_lr, _ = roc_curve(y_test, p_lr)
auc_lr = auc(fpr_lr, tpr_lr)
cal_df = pd.DataFrame({"p": p, "y": y_test.values})
cal_df["bin"] = pd.qcut(cal_df["p"], 10, duplicates="drop")
grp = cal_df.groupby("bin", observed=True).agg(mp=("p", "mean"), oy=("y", "mean"))

explainer = shap.TreeExplainer(model)
sv = explainer.shap_values(X_test)

def pretty(c):
    m = {
        "RIDAGEYR": "Age (years)", "pir": "Poverty income ratio", "bmi": "Body mass index",
        "energy": "Total energy intake", "fiber_adj": "Dietary fiber (energy-adj.)",
        "gender_Male": "Male sex", "smoking_1": "Smoking",
        "Race_Label_Mexican American": "Mexican American",
        "Race_Label_Non-Hispanic Black": "Non-Hispanic Black",
        "Race_Label_Non-Hispanic White": "Non-Hispanic White",
        "Race_Label_Other Hispanic": "Other Hispanic",
        "Race_Label_Other Race": "Other race",
        "Education_Label_High School Grad": "High school graduate",
        "Education_Label_> High School": "> High school education",
        "alcohol_group_Non-Drinker": "Alcohol: non-drinker",
        "Diabetes_Yes": "Diabetes", "Hypertension_Yes": "Hypertension",
        "Hyperlipidemia_Unknown": "Hyperlipidemia: unknown",
        "Hyperlipidemia_Yes": "Hyperlipidemia",
        "Physical_Activity_Inactive": "Physically inactive",
        "Physical_Activity_Unknown": "Physical activity: unknown",
    }
    return m.get(c, c)

X_test_p = X_test.copy()
X_test_p.columns = [pretty(c) for c in X_test.columns]
mean_abs = np.abs(sv).mean(axis=0)
order = np.argsort(mean_abs)[::-1]

fig = plt.figure(figsize=(7.2, 7.0))
gs = fig.add_gridspec(2, 2, hspace=0.42, wspace=0.58,
                      left=0.22, right=0.90, top=0.96, bottom=0.08)

# A: ROC
axA = fig.add_subplot(gs[0, 0])
axA.plot(fpr, tpr, color="#D55E00", lw=1.6,
         label=f"XGBoost (AUC = {roc_auc:.3f},\n95% CI: {lo:.3f}\u2013{hi:.3f})")
axA.plot(fpr_lr, tpr_lr, color="#009E73", lw=1.2, ls="-.",
         label=f"Logistic regression\n(AUC = {auc_lr:.3f})")
axA.plot([0, 1], [0, 1], ls="--", color="grey", lw=1.0, label="Chance (AUC = 0.500)")
axA.set_xlabel("False Positive Rate (1 \u2212 Specificity)", fontsize=7.5)
axA.set_ylabel("True Positive Rate (Sensitivity)", fontsize=7.5)
axA.set_title("A. Discrimination (ROC curves)", fontsize=9, loc="left", fontweight="bold")
axA.legend(loc="lower right", fontsize=6.2, frameon=False)
axA.set_xlim(0, 1); axA.set_ylim(0, 1.02)
axA.tick_params(labelsize=6.5)
for s in ("top", "right"): axA.spines[s].set_visible(False)

# B: calibration
axB = fig.add_subplot(gs[0, 1])
axB.plot([0, 1], [0, 1], ls="--", color="grey", lw=1.0, label="Perfect calibration")
axB.plot(grp["mp"], grp["oy"], "o-", color="#0072B2", lw=1.4, ms=4,
         label="XGBoost (deciles of predicted risk)")
for _, r in grp.iterrows():
    axB.plot([r["mp"], r["mp"]], [r["oy"], r["mp"]], color="#0072B2", lw=0.4, alpha=0.3)
axB.set_xlabel("Mean predicted probability of seropositivity", fontsize=7.5)
axB.set_ylabel("Observed proportion seropositive", fontsize=7.5)
axB.set_title("B. Calibration (held-out test set)", fontsize=9, loc="left", fontweight="bold")
axB.text(0.03, 0.97,
         f"Calibration slope = {cal_slope:.3f}\nCalibration intercept = {cal_int:.3f}\nBrier score = {brier:.3f}",
         transform=axB.transAxes, fontsize=6.8, va="top",
         bbox=dict(boxstyle="round,pad=0.3", fc="#F2F2F2", ec="grey", lw=0.5))
axB.legend(loc="lower right", fontsize=6.2, frameon=False)
axB.set_xlim(0, 1); axB.set_ylim(0, 1)
axB.tick_params(labelsize=6.5)
for s in ("top", "right"): axB.spines[s].set_visible(False)

NTOP = 12
# C: SHAP bar
axC = fig.add_subplot(gs[1, 0])
top = order[:NTOP][::-1]
axC.barh(range(len(top)), mean_abs[top], color="#1E88E5", height=0.7)
axC.set_yticks(range(len(top)))
axC.set_yticklabels([X_test_p.columns[i] for i in top], fontsize=6.3)
axC.set_xlabel("mean(|SHAP value|)", fontsize=7.5)
axC.set_title("C. SHAP feature importance", fontsize=9, loc="left", fontweight="bold")
axC.tick_params(axis="x", labelsize=6.5)
for s in ("top", "right"): axC.spines[s].set_visible(False)

# D: beeswarm
axD = fig.add_subplot(gs[1, 1])
sv_top = sv[:, order[:NTOP]]
Xd = X_test_p.iloc[:, order[:NTOP]].values.astype(float)
cmap = plt.cm.coolwarm
for j in range(NTOP):
    col = Xd[:, j]
    cmin, cmax = np.nanmin(col), np.nanmax(col)
    cn = (col - cmin) / (cmax - cmin + 1e-12)
    yv = np.full(Xd.shape[0], NTOP - 1 - j) + np.random.RandomState(j).normal(0, 0.12, Xd.shape[0])
    axD.scatter(sv_top[:, j], yv, c=cmap(cn), s=2.0, lw=0, alpha=0.85)
axD.set_yticks(range(NTOP))
axD.set_yticklabels([X_test_p.columns[i] for i in order[:NTOP]][::-1], fontsize=6.3)
axD.axvline(0, color="grey", lw=0.6)
axD.set_xlabel("SHAP value (impact on model output)", fontsize=7.5)
axD.set_title("D. SHAP summary plot", fontsize=9, loc="left", fontweight="bold")
axD.tick_params(axis="x", labelsize=6.5)
for s in ("top", "right"): axD.spines[s].set_visible(False)
sm = plt.cm.ScalarMappable(cmap="coolwarm", norm=plt.Normalize(0, 1))
cb = fig.colorbar(sm, ax=[axC, axD], fraction=0.03, pad=0.02, ticks=[0, 1])
cb.ax.set_yticklabels(["Low", "High"], fontsize=6.5)
cb.set_label("Feature value", fontsize=7)

fig.savefig(OUT + "Figure3_ML_Performance_SHAP_v5.tif", dpi=600, format="tiff",
            pil_kwargs={"compression": "tiff_lzw"})
fig.savefig(OUT + "Figure3_ML_Performance_SHAP_v5_300dpi.tif", dpi=300, format="tiff",
            pil_kwargs={"compression": "tiff_lzw"})
plt.close(fig)
print("Figure 3 composite saved")

from PIL import Image
for f, d in [("Figure3_ML_Performance_SHAP_v5.tif", 600), ("Figure3_ML_Performance_SHAP_v5_300dpi.tif", 300)]:
    im = Image.open(OUT + f)
    if im.mode == "RGBA":
        bg = Image.new("RGB", im.size, (255, 255, 255))
        bg.paste(im, mask=im.split()[3])
        bg.save(OUT + f, dpi=(d, d), compression="tiff_lzw")
    print(f, Image.open(OUT + f).size, Image.open(OUT + f).mode)
