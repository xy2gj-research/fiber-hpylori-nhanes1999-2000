library(survey)

# 确保结局变量 hp_status 是数值型 0 和 1 (如果已经是，这步不会有影响)
final_data$hp_status <- as.numeric(as.character(final_data$hp_status))

# 设置 NHANES 复杂抽样设计
# 注意：1999-2000 是单周期(2年)，通常使用 WTMEC2YR 权重。
nhanes_design <- svydesign(
  id = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  weights = ~WTMEC2YR, # 如果您之前用的是 WTMEC4YR，请改回 WTMEC4YR
  data = final_data, 
  nest = TRUE
)

