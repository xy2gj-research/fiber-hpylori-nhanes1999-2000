# =====================================================================
# 脚本名称: 03_Generate_ForestPlot_Fig3_V2.R
# 目的: 绘制 SCI 出版级高分辨率森林图 (修复边缘裁剪与拥挤问题)
# =====================================================================

library(forestplot)
library(stringr)
library(grid) # 用于设置边距 unit

cat("开始生成优化版出版级森林图...\n")

# 1. 读取数据
df <- read.csv("Table4_All_Formatted.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))

# 2. 准备文本矩阵 (包含表头)
tabletext <- cbind(
  c("Subgroup", df$Subgroup),
  c("OR (95% CI)", ifelse(is.na(df[[2]]), "", df[[2]])),
  c("P value", ifelse(is.na(df[[3]]), "", df[[3]])),
  c("P for interaction", ifelse(is.na(df[[4]]), "", df[[4]]))
)

# 3. 提取数值
mean_val <- c(NA)
lower_val <- c(NA)
upper_val <- c(NA)
is_summary <- c(TRUE) # 表头加粗

for (i in 1:nrow(df)) {
  or_str <- df[[2]][i]
  if (is.na(or_str) || or_str == "") {
    mean_val <- c(mean_val, NA)
    lower_val <- c(lower_val, NA)
    upper_val <- c(upper_val, NA)
    is_summary <- c(is_summary, TRUE) # 亚组大类标题加粗
  } else {
    nums <- as.numeric(unlist(str_extract_all(or_str, "[0-9]+\\.[0-9]+")))
    mean_val <- c(mean_val, nums[1])
    lower_val <- c(lower_val, nums[2])
    upper_val <- c(upper_val, nums[3])
    is_summary <- c(is_summary, FALSE)
  }
}

# 4. 绘制并导出高分辨率 TIFF 图片
tiff_file <- "Figure3_ForestPlot_V2.tif"
# 增加 height 到 15，确保上下不被裁剪
tiff(tiff_file, width = 12, height = 15, units = "in", res = 300, compression = "lzw")

forestplot(
  labeltext = tabletext,
  mean = mean_val,
  lower = lower_val,
  upper = upper_val,
  is.summary = is_summary,
  zero = 1,
  boxsize = 0.25,
  lineheight = unit(7.5, "mm"),     # 稍微缩小行距，配合大画布
  colgap = unit(6, "mm"),           # 增加列距，让文字不拥挤
  mar = unit(c(1.5, 1, 1.5, 1), "cm"), # 强制设置边距：下, 左, 上, 右留白
  lwd.zero = 1.5,
  lwd.ci = 1.5,
  xticks = c(0.4, 0.6, 0.8, 1.0, 1.5, 2.0, 4.0), # 定制美观的 X 轴刻度
  clip = c(0.3, 4.5),               # 限制图形两端，防止个别极值拉扯图形
  col = fpColors(box = "#00468B", line = "#00468B", zero = "gray50"),
  xlab = "Odds Ratio (95% CI) Per quartile increase",
  txt_gp = fpTxtGp(
    label = gpar(cex = 0.95, fontfamily = "sans"),
    ticks = gpar(cex = 0.9, fontfamily = "sans"),
    xlab  = gpar(cex = 1.1, fontfamily = "sans", fontface = "bold")
  ),
  align = c("l", "c", "c", "c"),
  graph.pos = 2
)

dev.off()
cat("✅ 优化版森林图已成功生成并保存为:", tiff_file, "\n")
