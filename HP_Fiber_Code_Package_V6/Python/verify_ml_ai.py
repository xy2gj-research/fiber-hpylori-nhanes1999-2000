# Verification: XGBoost split, AUC, SHAP ranking (read-only, no file writes)
import pandas as pd
import numpy as np
import xgboost as xgb
from sklearn.model_selection import train_test_split
from sklearn.metrics import roc_curve, auc
import shap

df = pd.read_csv("final_data_for_ml.csv")
print("ML sample n:", len(df), " prevalence:", round(df['hp_status'].mean(), 4))

X = df.drop(columns=['hp_status'])
y = df['hp_status']
categorical_cols = ['gender', 'Race_Label', 'Education_Label', 'smoking',
                    'alcohol_group', 'Diabetes', 'Hypertension',
                    'Hyperlipidemia', 'Physical_Activity']
X_encoded = pd.get_dummies(X, columns=categorical_cols, drop_first=True)
print("n raw features:", X.shape[1], " n encoded:", X_encoded.shape[1])

X_train, X_test, y_train, y_test = train_test_split(
    X_encoded, y, test_size=0.3, random_state=42, stratify=y)
print("train:", X_train.shape[0], " test:", X_test.shape[0])

xgb_model = xgb.XGBClassifier(
    n_estimators=100, learning_rate=0.05, max_depth=4,
    subsample=0.8, colsample_bytree=0.8, objective='binary:logistic',
    random_state=42, eval_metric='logloss')
xgb_model.fit(X_train, y_train)
y_pred_prob = xgb_model.predict_proba(X_test)[:, 1]
fpr, tpr, _ = roc_curve(y_test, y_pred_prob)
print("AUC:", round(auc(fpr, tpr), 4))

# bootstrap 95% CI for AUC
rng = np.random.RandomState(42)
aucs = []
for _ in range(1000):
    idx = rng.randint(0, len(y_test), len(y_test))
    if len(np.unique(y_test.iloc[idx])) < 2:
        continue
    f, t, _ = roc_curve(y_test.iloc[idx], y_pred_prob[idx])
    aucs.append(auc(f, t))
print("AUC 95%% CI: %.3f - %.3f" % (np.percentile(aucs, 2.5), np.percentile(aucs, 97.5)))

explainer = shap.TreeExplainer(xgb_model)
shap_values = explainer.shap_values(X_test)
mean_abs = np.abs(shap_values).mean(axis=0)
ranking = pd.Series(mean_abs, index=X_test.columns).sort_values(ascending=False)
print("\nTop 12 features by mean |SHAP|:")
for i, (name, val) in enumerate(ranking.head(12).items(), 1):
    print(f"{i:2d}. {name:40s} {val:.4f}")
fiber_rank = list(ranking.index).index("fiber_adj") + 1
print("\nfiber_adj rank:", fiber_rank)
