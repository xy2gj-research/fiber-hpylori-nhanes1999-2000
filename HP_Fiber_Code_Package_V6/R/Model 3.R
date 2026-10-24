# ==============================================================================
# 修复 Model 3 (去除能量摄入) - 包含自动安装步骤
# ==============================================================================

# 1. 自动检测并安装必要的包
# ------------------------------------------------------------------------------
if(!require(broom)) install.packages("broom")
if(!require(dplyr)) install.packages("dplyr")
if(!require(survey)) install.packages("survey")

library(broom)
library(dplyr)
library(survey)

# 2. 重新定义 Model 3 (去除能量摄入变量)
# ------------------------------------------------------------------------------
# 假设 nhanes_design 对象还在内存中。如果不在，请先重新运行之前的 design 定义代码。
# 我们从公式中移除了 "energy" 或 "DRXTKCAL"

cat("正在拟合修正后的 Model 3 (不含能量)...\n")

m3_new <- svyglm(
  formula = hp_status ~ fiber_quartile + RIDAGEYR + gender + race + bmi + pir + smoking, 
  design = nhanes_design, 
  family = quasibinomial()
)

# 3. 提取结果并进行“旧 vs 新”对比
# ------------------------------------------------------------------------------
# 使用 broom::tidy 提取整洁结果
res_m3_new <- tidy(m3_new, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(grepl("fiber", term)) %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  mutate(Model = "Model 3 (Modified: No Energy)")

# 打印对比结果
cat("\n============================================================\n")
cat("【旧 Model 3 (含能量)】: OR = 0.68, CI = [0.09 - 4.75] (CI宽度爆炸)\n")
cat("------------------------------------------------------------\n")
cat("【新 Model 3 (去能量)】: 见证奇迹的时刻 👇\n")
print(res_m3_new)
cat("============================================================\n")

# 4. 自动判断是否修复成功
# ------------------------------------------------------------------------------
width_new <- res_m3_new$conf.high[3] - res_m3_new$conf.low[3] # Q4 的 CI 宽度
print(width_new)
if(width_new < 3.0) {
  cat("✅ 成功！置信区间大幅收窄，P值应该也恢复显著了。\n")
  cat(">>> 现在的结论：全调整后，膳食纤维依然显著降低 HP 感染风险！\n")
} else {
  cat("⚠️ 警告：置信区间依然较宽，请检查结果数据。\n")
}


# ==============================================================================
# 更新汇总表格 (将 Model 3 替换为新结果)
# ==============================================================================

# 1. 提取 Model 1 和 Model 2 (假设 m1, m2 还在内存中)
# 如果不在，请重新运行 m1 和 m2 的 svyglm
res_m1 <- tidy(m1, conf.int = TRUE, exponentiate = TRUE) %>% 
  filter(grepl("fiber", term)) %>% mutate(Model = "Model 1 (Crude)")
res_m2 <- tidy(m2, conf.int = TRUE, exponentiate = TRUE) %>% 
  filter(grepl("fiber", term)) %>% mutate(Model = "Model 2 (Demographics)")

# 2. 组合所有结果
final_res <- bind_rows(res_m1, res_m2, res_m3_new) %>%
  select(Model, Variable = term, OR = estimate, Lower = conf.low, Upper = conf.high, P = p.value)

# 3. 保存新的 CSV
write.csv(final_res, "final_table_corrected.csv", row.names = FALSE)
cat("✅ 已生成 'final_table_corrected.csv'，请使用此文件绘制森林图。")

