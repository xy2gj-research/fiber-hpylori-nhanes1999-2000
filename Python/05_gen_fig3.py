# =====================================================================
# 脚本名称: 05_XGBoost_Train_and_ROC_Fig3.py
# 目的: 训练 XGBoost 模型并绘制 ROC 曲线 (Figure 3)
# 特征: 终极 14 变量 (无维C)
# =====================================================================

import pandas as pd
import numpy as np
import xgboost as xgb
from sklearn.model_selection import train_test_split
from sklearn.metrics import roc_curve, auc
import matplotlib.pyplot as plt
import joblib

print("开始加载数据并预处理...")

# 1. 读取从 R 导出的数据
df = pd.read_csv("final_data_for_ml.csv")

# 2. 分离特征 (X) 和标签 (y)
X = df.drop(columns=['hp_status'])
y = df['hp_status']

# 3. 分类变量的独热编码 (One-Hot Encoding)
# XGBoost 需要数值型输入，我们将文本类型的分类变量转换为 0/1 虚拟变量
categorical_cols = ['gender', 'Race_Label', 'Education_Label', 'smoking',
                    'alcohol_group', 'Diabetes', 'Hypertension',
                    'Hyperlipidemia', 'Physical_Activity']
X_encoded = pd.get_dummies(X, columns=categorical_cols, drop_first=True)

# 4. 划分训练集和测试集 (70% 训练, 30% 测试，固定随机种子保证可重复性)
X_train, X_test, y_train, y_test = train_test_split(X_encoded, y, test_size=0.3, random_state=42, stratify=y)

print(f"数据划分完成: 训练集 {X_train.shape[0]} 人, 测试集 {X_test.shape[0]} 人")

# 5. 训练 XGBoost 模型
print("正在训练 XGBoost 模型...")
xgb_model = xgb.XGBClassifier(
    n_estimators=100,
    learning_rate=0.05,
    max_depth=4,
    subsample=0.8,
    colsample_bytree=0.8,
    objective='binary:logistic',
    random_state=42,
    eval_metric='logloss'
)
xgb_model.fit(X_train, y_train)

# 6. 在测试集上预测概率
y_pred_prob = xgb_model.predict_proba(X_test)[:, 1]

# 7. 计算 ROC 和 AUC
fpr, tpr, thresholds = roc_curve(y_test, y_pred_prob)
roc_auc = auc(fpr, tpr)
print(f"模型在测试集上的 AUC 值为: {roc_auc:.4f}")

# 8. 绘制出版级高分辨率 ROC 曲线 (Figure 3)
plt.figure(figsize=(8, 8))
plt.plot(fpr, tpr, color='#D55E00', lw=2.5, label=f'XGBoost (AUC = {roc_auc:.3f})')
plt.plot([0, 1], [0, 1], color='navy', lw=2, linestyle='--')

# 设置坐标轴和标题
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False Positive Rate (1 - Specificity)', fontsize=14, fontweight='bold')
plt.ylabel('True Positive Rate (Sensitivity)', fontsize=14, fontweight='bold')
plt.title('Receiver Operating Characteristic (ROC) Curve', fontsize=16, fontweight='bold')
plt.legend(loc="lower right", fontsize=14)

# 调整刻度字体
plt.xticks(fontsize=12)
plt.yticks(fontsize=12)

# 移除顶部和右侧边框 (符合 SCI 风格)
ax = plt.gca()
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.spines['left'].set_linewidth(1.5)
ax.spines['bottom'].set_linewidth(1.5)

# 保存高分辨率图片
plt.savefig("Figure3_ROC_Curve.tif", dpi=300, bbox_inches='tight', format='tiff', pil_kwargs={"compression": "tiff_lzw"})
plt.show()

# 9. 保存模型和测试集数据，供下一步 SHAP 分析使用
joblib.dump(xgb_model, 'xgboost_model_final.pkl')
X_test.to_csv('X_test_encoded.csv', index=False)
print("Figure 3 绘制完成！模型和测试集已保存，准备进行 SHAP 分析。")
