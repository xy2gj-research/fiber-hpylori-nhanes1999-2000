# ==============================================================================
# 验证步骤：能量调整后膳食纤维的 RCS 曲线与非线性检验
# ==============================================================================
library(survey)
library(rms)
library(ggplot2)

# 1. 准备数据并设置 datadist (rms包的要求)
rcs_df <- final_data %>%
  select(hp_status, fiber_adj, RIDAGEYR, gender, race_binary, bmi, pir, smoking, SDMVPSU, SDMVSTRA, WTMEC2YR) %>%
  na.omit()

dd <- datadist(rcs_df)
options(datadist = "dd")

# 2. 构建复杂抽样设计
options(survey.lonely.psu = "adjust") 
des_rcs <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR, nest = TRUE, data = rcs_df)

# 3. 拟合带有 RCS (3个节点) 的复杂抽样逻辑回归模型
# 注意：这里使用 splines::ns 来生成自然样条，兼容 svyglm
library(splines)
fit_rcs <- svyglm(hp_status ~ ns(fiber_adj, df = 2) + RIDAGEYR + gender + race_binary + bmi + pir + smoking, 
                  design = des_rcs, family = quasibinomial())

# 4. 进行非线性检验 (P for non-linearity)
# 检验样条的非线性项是否显著
non_linear_test <- regTermTest(fit_rcs, ~ns(fiber_adj, df = 2), method = "Wald")
cat("\n======================================================\n")
cat("非线性检验结果 (P for non-linearity):\n")
print(non_linear_test)
cat("======================================================\n")
cat("💡 判读标准：\n")
cat("如果 P > 0.05，说明关系是线性的，不存在拐点，不能用两段式模型！\n")
cat("如果 P < 0.05，说明存在非线性关系，我们需要进一步看图。\n")
cat("======================================================\n")
