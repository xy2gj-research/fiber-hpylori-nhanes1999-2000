# Figure 4 v5: Panel A ROC (with 95% CI) + Panel B calibration curve
import pandas as pd
import numpy as np
import xgboost as xgb
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
print(f"AUC = {roc_auc:.3f} ({lo:.3f}-{hi:.3f}) | LR AUC = {auc_lr:.3f}")
print(f"Brier = {brier:.3f} | slope = {cal_slope:.3f} | intercept = {cal_int:.3f}")

# calibration by deciles
cal_df = pd.DataFrame({"p": p, "y": y_test.values})
cal_df["bin"] = pd.qcut(cal_df["p"], 10, duplicates="drop")
grp = cal_df.groupby("bin", observed=True).agg(mp=("p", "mean"), oy=("y", "mean"), n=("y", "size"))

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(7.2, 3.6))

# Panel A: ROC
ax1.plot(fpr, tpr, color="#D55E00", lw=1.8,
         label=f"XGBoost (AUC = {roc_auc:.3f},\n95% CI: {lo:.3f}\u2013{hi:.3f})")
ax1.plot(fpr_lr, tpr_lr, color="#009E73", lw=1.4, ls="-.",
         label=f"Logistic regression\n(AUC = {auc_lr:.3f})")
ax1.plot([0, 1], [0, 1], ls="--", color="grey", lw=1.2, label="Chance (AUC = 0.500)")
ax1.set_xlabel("False Positive Rate (1 \u2212 Specificity)", fontsize=8.5)
ax1.set_ylabel("True Positive Rate (Sensitivity)", fontsize=8.5)
ax1.set_title("A. Discrimination (ROC curves)", fontsize=9.5, loc="left", fontweight="bold")
ax1.legend(loc="lower right", fontsize=7.2, frameon=False)
ax1.set_xlim(0, 1); ax1.set_ylim(0, 1.02)
ax1.tick_params(labelsize=7.5)
for s in ("top", "right"): ax1.spines[s].set_visible(False)

# Panel B: calibration
ax2.plot([0, 1], [0, 1], ls="--", color="grey", lw=1.2, label="Perfect calibration")
ax2.plot(grp["mp"], grp["oy"], "o-", color="#0072B2", lw=1.6, ms=4.5,
         label="XGBoost (deciles of predicted risk)")
for _, r in grp.iterrows():
    ax2.plot([r["mp"], r["mp"]], [r["oy"], r["mp"]], color="#0072B2", lw=0.4, alpha=0.3)
ax2.set_xlabel("Mean predicted probability of seropositivity", fontsize=8.5)
ax2.set_ylabel("Observed proportion seropositive", fontsize=8.5)
ax2.set_title("B. Calibration (held-out test set)", fontsize=9.5, loc="left", fontweight="bold")
ax2.text(0.03, 0.97,
         f"Calibration slope = {cal_slope:.3f}\nCalibration intercept = {cal_int:.3f}\nBrier score = {brier:.3f}",
         transform=ax2.transAxes, fontsize=7.5, va="top",
         bbox=dict(boxstyle="round,pad=0.3", fc="#F2F2F2", ec="grey", lw=0.5))
ax2.legend(loc="lower right", fontsize=7.2, frameon=False)
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)
ax2.tick_params(labelsize=7.5)
for s in ("top", "right"): ax2.spines[s].set_visible(False)

fig.tight_layout()
fig.savefig(OUT + "Figure4_ROC_Calibration_v5.tif", dpi=600, format="tiff",
            pil_kwargs={"compression": "tiff_lzw"})
fig.savefig(OUT + "Figure4_ROC_Calibration_v5_300dpi.tif", dpi=300, format="tiff",
            pil_kwargs={"compression": "tiff_lzw"})
plt.close(fig)
print("Figure 4 v5 saved (600 + 300 dpi)")

# RGB conversion
from PIL import Image
for f in ["Figure4_ROC_Calibration_v5.tif", "Figure4_ROC_Calibration_v5_300dpi.tif"]:
    im = Image.open(OUT + f)
    if im.mode == "RGBA":
        bg = Image.new("RGB", im.size, (255, 255, 255))
        bg.paste(im, mask=im.split()[3])
        dpi = 600 if "300dpi" not in f else 300
        bg.save(OUT + f, dpi=(dpi, dpi), compression="tiff_lzw")
        print(f, "-> RGB")
