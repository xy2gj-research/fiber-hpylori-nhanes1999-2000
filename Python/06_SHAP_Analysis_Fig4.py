# =====================================================================
# 脚本名称: 06_SHAP_Analysis_Fig4.py
# 目的: 计算 SHAP 值并绘制特征重要性图 (Fig 4A) 和 摘要图 (Fig 4B)
# =====================================================================

import pandas as pd
import joblib
import shap
import matplotlib.pyplot as plt

print("正在加载模型和测试集数据...")

# 1. 加载上一阶段保存的模型和测试集
xgb_model = joblib.load('xgboost_model_final.pkl')
X_test = pd.read_csv('X_test_encoded.csv')

# 2. 初始化 SHAP TreeExplainer 并计算 SHAP 值
print("正在计算 SHAP 值 (这可能需要几秒钟)...")
explainer = shap.TreeExplainer(xgb_model)
shap_values = explainer.shap_values(X_test)

# 3. 绘制 Figure 4A: SHAP 特征重要性柱状图 (Bar Plot)
print("正在绘制 Figure 4A (特征重要性柱状图)...")
plt.figure(figsize=(10, 8))
# plot_type="bar" 生成各特征的平均绝对 SHAP 值
shap.summary_plot(shap_values, X_test, plot_type="bar", show=False)
# 获取当前图形对象并调整排版
fig = plt.gcf()
fig.set_size_inches(10, 8)
plt.title("SHAP Feature Importance", fontsize=16, fontweight='bold')
plt.xlabel("mean(|SHAP value|) (Average impact on model output magnitude)", fontsize=12)
plt.savefig("Figure4A_SHAP_Bar.tif", dpi=300, bbox_inches='tight', format='tiff', pil_kwargs={"compression": "tiff_lzw"})
plt.close()

# 4. 绘制 Figure 4B: SHAP 摘要蜂之图 (Summary Dot Plot)
print("正在绘制 Figure 4B (SHAP 摘要图)...")
plt.figure(figsize=(10, 8))
# 默认生成 dot plot，展示每个样本的特征值高低与 SHAP 值的关系
shap.summary_plot(shap_values, X_test, show=False)
fig = plt.gcf()
fig.set_size_inches(10, 8)
plt.title("SHAP Summary Plot", fontsize=16, fontweight='bold')
plt.xlabel("SHAP value (Impact on model output)", fontsize=12)
plt.savefig("Figure4B_SHAP_Summary.tif", dpi=300, bbox_inches='tight', format='tiff', pil_kwargs={"compression": "tiff_lzw"})
plt.close()

print("图 4 绘制完成！已成功保存 Figure4A_SHAP_Bar.tif 和 Figure4B_SHAP_Summary.tif")
