# =====================================================================
# 脚本名称: 07_SHAP_Dependence_Fig5.py
# 目的: 绘制膳食纤维的 SHAP 依赖图 (探索非线性阈值)
# =====================================================================

import pandas as pd
import joblib
import shap
import matplotlib.pyplot as plt

print("正在加载模型和数据...")
xgb_model = joblib.load('xgboost_model_final.pkl')
X_test = pd.read_csv('X_test_encoded.csv')

print("正在计算 SHAP 值...")
explainer = shap.TreeExplainer(xgb_model)
shap_values = explainer.shap_values(X_test)

print("正在绘制 Figure 5 (SHAP 依赖图)...")
plt.figure(figsize=(8, 6))

# 绘制 fiber_adj 的依赖图，并自动寻找最强交互变量进行着色
shap.dependence_plot(
    "fiber_adj",
    shap_values,
    X_test,
    show=False,
    # 如果不想看交互作用，可以将 interaction_index 设为 None，这里我们让它自动寻找
    interaction_index="auto",
    alpha=0.7, # 透明度，防止点重叠
    dot_size=30
)

# 获取当前图形对象并调整排版
fig = plt.gcf()
fig.set_size_inches(9, 7)
plt.title("SHAP Dependence Plot for Dietary Fiber", fontsize=16, fontweight='bold')
plt.xlabel("Dietary Fiber Intake (g/day)", fontsize=14, fontweight='bold')
plt.ylabel("SHAP value for Dietary Fiber\n(Impact on H. pylori risk)", fontsize=14, fontweight='bold')

# 添加一条 y=0 的水平虚线，方便观察阈值
plt.axhline(y=0, color='red', linestyle='--', linewidth=1.5)

# 调整刻度字体
plt.xticks(fontsize=12)
plt.yticks(fontsize=12)

# 保存高分辨率图片
plt.savefig("Figure5_SHAP_Dependence.tif", dpi=300, bbox_inches='tight', format='tiff', pil_kwargs={"compression": "tiff_lzw"})
plt.close()

print("图 5 绘制完成！已成功保存 Figure5_SHAP_Dependence.tif")
