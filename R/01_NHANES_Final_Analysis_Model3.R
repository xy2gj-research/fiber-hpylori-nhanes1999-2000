# =====================================================================
# 脚本名称: 01_NHANES_Final_Analysis_Model3.R
# 目的: 膳食纤维与Hp感染SCI论文大修 - 终极数据分析脚本
# 更新说明: 
#   1. 剔除高度共线性的 DRXTVC (维生素C)
#   2. 新增4个代谢/生活方式变量 (糖尿病、高血压、高血脂、运动状态)
#   3. 解决 survey 包自由度报错，改用加权 GLM (quasibinomial)
# 输出文件: 
#   - Table1_New_Variables_Final.csv (表1新增变量)
#   - Table2_Model3_Results_Final.csv (表2主效应)
#   - Table4_Subgroup_Results_Final.csv (表4亚组分析)
# =====================================================================

library(dplyr)
library(broom)

# 1. 定义最终的协变量 (13个变量，无 DRXTVC)
final_covariates <- "RIDAGEYR + gender + Race_Label + Education_Label + pir + smoking + alcohol_group + bmi + energy + Diabetes + Hypertension + Hyperlipidemia + Physical_Activity"

cat("开始生成 Table 1, Table 2, Table 4 的 CSV 文件...\n")

# =====================================================================
# 模块 1: 生成 Table 1 新增变量的基线特征 (加权百分比与 P 值)
# =====================================================================
new_vars <- c("Diabetes", "Hypertension", "Hyperlipidemia", "Physical_Activity")
table1_list <- list()

for (var in new_vars) {
  # 计算加权频数和百分比
  wt_table <- xtabs(clean_data$norm_weight ~ clean_data[[var]] + clean_data$hp_status)
  prop_table <- prop.table(wt_table, margin = 2) * 100
  
  # 使用加权 GLM 计算 P 值
  formula_p <- as.formula(paste("hp_status ~", var))
  model_p <- glm(formula_p, data = clean_data, family = quasibinomial(), weights = clean_data$norm_weight)
  p_val <- summary(model_p)$coefficients[2, "Pr(>|t|)"]
  
  levels_var <- rownames(prop_table)
  for (i in 1:length(levels_var)) {
    table1_list[[length(table1_list) + 1]] <- data.frame(
      Variable = var,
      Level = levels_var[i],
      Hp_Negative_0_Pct = round(prop_table[i, 1], 1),
      Hp_Positive_1_Pct = round(prop_table[i, 2], 1),
      P_value = ifelse(i == 1, round(p_val, 4), NA) 
    )
  }
}
write.csv(bind_rows(table1_list), "Table1_New_Variables_Final.csv", row.names = FALSE)
cat("✅ Table 1 (新增变量) 已导出\n")

# =====================================================================
# 模块 2: 生成 Table 2 终极 Model 3 结果 (OR, 95% CI, P for trend)
# =====================================================================
formula_str_cat <- paste("hp_status ~ Fiber_Q +", final_covariates)
formula_str_trend <- paste("hp_status ~ Fiber_Q_num +", final_covariates)

# 分类模型 (Q1-Q4)
model_final_cat <- glm(as.formula(formula_str_cat), data = clean_data, family = quasibinomial(), weights = clean_data$norm_weight)
or_ci_final <- exp(cbind(OR = coef(model_final_cat), confint.default(model_final_cat)))
fiber_res <- round(or_ci_final[grep("Fiber_Q", rownames(or_ci_final)), ], 3)

# 趋势模型 (P for trend)
model_final_trend <- glm(as.formula(formula_str_trend), data = clean_data, family = quasibinomial(), weights = clean_data$norm_weight)
p_trend_final <- summary(model_final_trend)$coefficients["Fiber_Q_num", "Pr(>|t|)"]

table2_df <- data.frame(
  Fiber_Quartile = c("Q1 (Ref)", "Q2", "Q3", "Q4"),
  OR = c("1.000", fiber_res[1, "OR"], fiber_res[2, "OR"], fiber_res[3, "OR"]),
  CI_Lower = c(NA, fiber_res[1, 2], fiber_res[2, 2], fiber_res[3, 2]),
  CI_Upper = c(NA, fiber_res[1, 3], fiber_res[2, 3], fiber_res[3, 3]),
  P_for_trend = c(NA, NA, NA, round(p_trend_final, 4))
)
write.csv(table2_df, "Table2_Model3_Results_Final.csv", row.names = FALSE)
cat("✅ Table 2 (Model 3) 已导出\n")

# =====================================================================
# 模块 3: 生成 Table 4 亚组分析 (Per quartile increase)
# =====================================================================
subgroups <- c("age_group", "gender", "Race_Label", "bmi_group", "smoking", 
               "alcohol_group", "VitC_Cat", "Energy_Cat", "Diabetes", 
               "Hypertension", "Hyperlipidemia", "Physical_Activity")
table4_list <- list()

for (sg in subgroups) {
  # 动态剔除当前亚组变量，防止共线性报错
  current_covs <- unlist(strsplit(final_covariates, " \\+ "))
  covs_for_subgroup <- paste(current_covs[current_covs != sg], collapse = " + ")
  
  # 计算交互作用 P 值
  formula_int <- as.formula(paste("hp_status ~ Fiber_Q_num *", sg, "+", covs_for_subgroup))
  model_int <- glm(formula_int, data = clean_data, family = quasibinomial(), weights = clean_data$norm_weight)
  int_terms <- grep(paste0("Fiber_Q_num:", sg), rownames(summary(model_int)$coefficients))
  p_int <- if(length(int_terms) > 0) summary(model_int)$coefficients[int_terms[1], "Pr(>|t|)"] else NA
  
  levels_sg <- na.omit(unique(clean_data[[sg]]))
  for (i in 1:length(levels_sg)) {
    lvl <- levels_sg[i]
    sub_data <- clean_data[clean_data[[sg]] == lvl, ]
    if(nrow(sub_data) < 50) next 
    
    formula_sub <- as.formula(paste("hp_status ~ Fiber_Q_num +", covs_for_subgroup))
    model_sub <- tryCatch(glm(formula_sub, data = sub_data, family = quasibinomial(), weights = sub_data$norm_weight), error = function(e) NULL)
    
    if (!is.null(model_sub)) {
      coef_val <- coef(model_sub)["Fiber_Q_num"]
      ci_val <- confint.default(model_sub)["Fiber_Q_num", ]
      p_val <- summary(model_sub)$coefficients["Fiber_Q_num", "Pr(>|t|)"]
      
      table4_list[[length(table4_list) + 1]] <- data.frame(
        Subgroup = sg, Level = as.character(lvl),
        OR_95CI = sprintf("%.3f (%.3f, %.3f)", exp(coef_val), exp(ci_val[1]), exp(ci_val[2])),
        P_value = round(p_val, 4),
        P_interaction = ifelse(i == 1, round(p_int, 4), NA)
      )
    }
  }
}
write.csv(bind_rows(table4_list), "Table4_Subgroup_Results_Final.csv", row.names = FALSE)
cat("✅ Table 4 (亚组分析) 已导出\n")
