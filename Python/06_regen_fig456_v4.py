# Regenerate Figure 4 (ROC+CI+calibration), Figure 5 (SHAP merged), Figure 6 (dependence)
# Output: 600 DPI, RGB, journal-sized
import pandas as pd
import numpy as np
import xgboost as xgb
from sklearn.model_selection import train_test_split
from sklearn.metrics import roc_curve, auc, brier_score_loss
from sklearn.linear_model import LogisticRegression
import shap
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
print(f"AUC = {roc_auc:.3f} (95% CI {lo:.3f}-{hi:.3f})")

# --- calibration & other performance metrics (reviewer #9) ---
brier = brier_score_loss(y_test, p)
logit_p = np.log(p / (1 - p))
cal = LogisticRegression().fit(logit_p.reshape(-1, 1), y_test)
cal_slope, cal_int = cal.coef_[0][0], cal.intercept_[0]
pred01 = (p >= 0.5).astype(int)
sens = ((pred01 == 1) & (y_test == 1)).sum() / (y_test == 1).sum()
spec = ((pred01 == 0) & (y_test == 0)).sum() / (y_test == 0).sum()
lr = LogisticRegression(max_iter=2000).fit(X_train, y_train)
p_lr = lr.predict_proba(X_test)[:, 1]
auc_lr = auc(*roc_curve(y_test, p_lr)[:2][::-1][:2]) if False else auc(roc_curve(y_test, p_lr)[0], roc_curve(y_test, p_lr)[1])
print(f"Brier = {brier:.3f} | calibration slope = {cal_slope:.3f} | intercept = {cal_int:.3f}")
print(f"Sens@0.5 = {sens:.3f} | Spec@0.5 = {spec:.3f} | Logistic AUC = {auc_lr:.3f}")

# ---------- Figure 4: ROC ----------
fig, ax = plt.subplots(figsize=(5.2, 5.0))
ax.plot(fpr, tpr, color="#D55E00", lw=2,
        label=f"XGBoost (AUC = {roc_auc:.3f}, 95% CI: {lo:.3f}\u2013{hi:.3f})")
ax.plot([0, 1], [0, 1], ls="--", color="#0072B2", lw=1.5, label="Chance (AUC = 0.500)")
ax.set_xlabel("False Positive Rate (1 \u2212 Specificity)", fontsize=11)
ax.set_ylabel("True Positive Rate (Sensitivity)", fontsize=11)
ax.set_title("ROC Curve of the Exploratory XGBoost Model\n(held-out test set, n = 630)", fontsize=11.5)
ax.legend(loc="lower right", fontsize=9.5, frameon=False)
ax.text(0.42, 0.22, f"Brier score = {brier:.3f}\nCalibration slope = {cal_slope:.3f}",
        fontsize=9, color="#333333")
ax.set_xlim(0, 1); ax.set_ylim(0, 1.02)
for s in ("top", "right"): ax.spines[s].set_visible(False)
fig.tight_layout()
fig.savefig(OUT + "Figure4_ROC_Curve_v4.tif", dpi=600, format="tiff", pil_kwargs={"compression": "tiff_lzw"})
plt.close(fig)
print("Figure4 saved")

# ---------- SHAP ----------
explainer = shap.TreeExplainer(model)
sv = explainer.shap_values(X_test)

def pretty(c):
    m = {
        "RIDAGEYR": "Age (years)", "pir": "Poverty income ratio", "bmi": "Body mass index",
        "energy": "Total energy intake", "fiber_adj": "Dietary fiber (energy-adjusted)",
        "gender_Male": "Sex: male", "smoking_1": "Smoking: yes",
        "Race_Label_Mexican American": "Race: Mexican American",
        "Race_Label_Non-Hispanic Black": "Race: Non-Hispanic Black",
        "Race_Label_Non-Hispanic White": "Race: Non-Hispanic White",
        "Race_Label_Other Hispanic": "Race: Other Hispanic",
        "Race_Label_Other Race": "Race: other race",
        "Education_Label_High School Grad": "Education: high school graduate",
        "Education_Label_> High School": "Education: more than high school",
        "alcohol_group_Non-Drinker": "Alcohol: non-drinker",
        "Diabetes_Yes": "Diabetes: yes", "Hypertension_Yes": "Hypertension: yes",
        "Hyperlipidemia_Unknown": "Hyperlipidemia: unknown",
        "Hyperlipidemia_Yes": "Hyperlipidemia: yes",
        "Physical_Activity_Inactive": "Physical activity: inactive",
        "Physical_Activity_Unknown": "Physical activity: unknown",
    }
    return m.get(c, c)

X_test_p = X_test.copy()
X_test_p.columns = [pretty(c) for c in X_test.columns]

mean_abs = np.abs(sv).mean(axis=0)
order = np.argsort(mean_abs)[::-1]

# ---------- Figure 5: bar + beeswarm, stacked vertically ----------
fig = plt.figure(figsize=(7.2, 6.6))
gs = fig.add_gridspec(2, 1, height_ratios=[1, 1], hspace=0.28,
                      left=0.30, right=0.90, top=0.96, bottom=0.07)

ax1 = fig.add_subplot(gs[0])
top = order[:15][::-1]
ax1.barh(range(len(top)), mean_abs[top], color="#1E88E5", height=0.7)
ax1.set_yticks(range(len(top)))
ax1.set_yticklabels([X_test_p.columns[i] for i in top], fontsize=8)
ax1.set_xlabel("mean(|SHAP value|) (average impact on model output)", fontsize=8.5)
ax1.set_title("A. SHAP feature importance (held-out test set, n = 630)",
              fontsize=10, loc="left", fontweight="bold")
ax1.tick_params(axis="x", labelsize=8)
for s in ("top", "right"): ax1.spines[s].set_visible(False)

ax2 = fig.add_subplot(gs[1])
sv_top = sv[:, order[:15]]
Xd = X_test_p.iloc[:, order[:15]].values.astype(float)
cmap = plt.cm.coolwarm
for j in range(15):
    col = Xd[:, j]
    cmin, cmax = np.nanmin(col), np.nanmax(col)
    cn = (col - cmin) / (cmax - cmin + 1e-12)
    yv = np.full(Xd.shape[0], 14 - j) + np.random.RandomState(j).normal(0, 0.12, Xd.shape[0])
    ax2.scatter(sv_top[:, j], yv, c=cmap(cn), s=3.0, lw=0, alpha=0.85)
ax2.set_yticks(range(15))
ax2.set_yticklabels([X_test_p.columns[i] for i in order[:15]][::-1], fontsize=8)
ax2.axvline(0, color="grey", lw=0.6)
ax2.set_xlabel("SHAP value (impact on model output)", fontsize=8.5)
ax2.set_title("B. SHAP summary plot", fontsize=10, loc="left", fontweight="bold")
ax2.tick_params(axis="x", labelsize=8)
for s in ("top", "right"): ax2.spines[s].set_visible(False)
sm = plt.cm.ScalarMappable(cmap="coolwarm", norm=plt.Normalize(0, 1))
cb = fig.colorbar(sm, ax=[ax1, ax2], fraction=0.03, pad=0.02, ticks=[0, 1])
cb.ax.set_yticklabels(["Low", "High"], fontsize=8)
cb.set_label("Feature value", fontsize=8.5)
fig.savefig(OUT + "Figure5_SHAP_Combined_v4.tif", dpi=600, format="tiff", pil_kwargs={"compression": "tiff_lzw"})
plt.close(fig)
print("Figure5 saved")

# ---------- Figure 6: dependence ----------
fcol = list(X_test.columns).index("fiber_adj")
rcol = list(X_test.columns).index("Race_Label_Non-Hispanic White")
fig, ax = plt.subplots(figsize=(5.4, 4.4))
sc = ax.scatter(X_test.iloc[:, fcol], sv[:, fcol], c=X_test.iloc[:, rcol],
                cmap="coolwarm", s=9, alpha=0.75, lw=0, vmin=0, vmax=1)
ax.axhline(0, color="#CC0000", ls="--", lw=1)
ax.set_xlabel("Energy-adjusted dietary fiber intake (g/day)", fontsize=10.5)
ax.set_ylabel("SHAP value for dietary fiber\n(impact on predicted H. pylori seropositivity)", fontsize=10)
ax.set_title("SHAP Dependence Plot for Dietary Fiber", fontsize=11.5)
cb = fig.colorbar(sc, ax=ax, ticks=[0, 1])
cb.ax.set_yticklabels(["No", "Yes"], fontsize=9)
cb.set_label("Non-Hispanic White", fontsize=9.5)
for s in ("top", "right"): ax.spines[s].set_visible(False)
fig.tight_layout()
fig.savefig(OUT + "Figure6_SHAP_Dependence_v4.tif", dpi=600, format="tiff", pil_kwargs={"compression": "tiff_lzw"})
plt.close(fig)
print("Figure6 saved")
