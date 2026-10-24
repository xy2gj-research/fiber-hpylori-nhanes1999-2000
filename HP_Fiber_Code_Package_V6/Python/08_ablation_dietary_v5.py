# Ablation: XGBoost performance with vs without dietary variables (reviewer major #9 request)
import pandas as pd
import numpy as np
import xgboost as xgb
from sklearn.model_selection import train_test_split
from sklearn.metrics import roc_auc_score
from sklearn.linear_model import LogisticRegression

df = pd.read_csv("final_data_for_ml.csv")
y = df['hp_status']
categorical_cols = ['gender', 'Race_Label', 'Education_Label', 'smoking',
                    'alcohol_group', 'Diabetes', 'Hypertension',
                    'Hyperlipidemia', 'Physical_Activity']
X_full = df.drop(columns=['hp_status'])

def run(X, tag):
    Xe = pd.get_dummies(X, columns=[c for c in categorical_cols if c in X.columns], drop_first=True)
    Xtr, Xte, ytr, yte = train_test_split(Xe, y, test_size=0.3, random_state=42, stratify=y)
    m = xgb.XGBClassifier(n_estimators=100, learning_rate=0.05, max_depth=4,
                          subsample=0.8, colsample_bytree=0.8, objective='binary:logistic',
                          random_state=42, eval_metric='logloss')
    m.fit(Xtr, ytr)
    p = m.predict_proba(Xte)[:, 1]
    auc_xgb = roc_auc_score(yte, p)
    lr = LogisticRegression(max_iter=2000)
    lr.fit(Xtr, ytr)
    auc_lr = roc_auc_score(yte, lr.predict_proba(Xte)[:, 1])
    print(f"{tag:45s} XGB AUC={auc_xgb:.3f}   LR AUC={auc_lr:.3f}")
    return auc_xgb

run(X_full, "All predictors (incl. dietary)")
dietary = [c for c in X_full.columns if c.lower() in ('fiber_adj','energy') or 'vitc' in c.lower() or 'fiber' in c.lower()]
print("dietary cols removed:", dietary)
run(X_full.drop(columns=dietary), "Without dietary variables")
run(X_full.drop(columns=['fiber_adj']), "Without fiber_adj only")
