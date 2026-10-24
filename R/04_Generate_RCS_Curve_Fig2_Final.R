# 7. 使用 ggplot2 绘制出版级高分辨率 RCS 曲线 (优化排版版)
p_rcs <- ggplot(pred_rcs) +
  # 绘制 95% 置信区间阴影
  geom_ribbon(aes(x = fiber_adj, ymin = lower, ymax = upper), 
              fill = "#0073C2", alpha = 0.2) +
  # 绘制 OR 曲线
  geom_line(aes(x = fiber_adj, yhat), color = "#0073C2", size = 1.2) +
  # 添加 OR = 1 的水平参考线
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 0.8) +
  # 添加 Reference = 10 的垂直参考线
  geom_vline(xintercept = 10, linetype = "dashed", color = "black", size = 0.8) +
  # 坐标轴标签 (强制清空 caption 以去除 rms 自动生成的冗余文本)
  labs(x = "Dietary Fiber Intake (g/day)", 
       y = "Odds Ratio (OR) for H. pylori Infection",
       title = "Dose-Response Relationship between Dietary Fiber and H. pylori",
       caption = NULL) + 
  # SCI 论文常用主题
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "black"),
    axis.line = element_line(size = 0.8, color = "black"),
    axis.ticks = element_line(size = 0.8, color = "black"),
    # 彻底隐藏 caption 元素
    plot.caption = element_blank() 
  ) +
  # 【关键修改】：缩小 Y 轴范围，放大曲线的有效部分，增强视觉效果
  coord_cartesian(ylim = c(0.4, 1.5)) 

# 8. 输出高分辨率 TIFF 图片 (Figure 2)
tiff("Figure2_RCS_Curve_Updated.tif", width = 3000, height = 2400, res = 300, compression = "lzw")
print(p_rcs)
dev.off()

cat("RCS 曲线绘制完成！已保存为 Figure2_RCS_Curve_Updated.tif\n")

