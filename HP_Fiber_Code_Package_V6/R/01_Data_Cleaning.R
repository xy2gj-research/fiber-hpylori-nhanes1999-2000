# ==============================================================================
# File Name: 01_Data_Cleaning.R
# Purpose: 加载清洗后的数据，转换为分析格式，并建立复杂抽样加权对象
# Input: "NHANES_Final_Analytic_Data.csv"
# Output: svy_design (加权对象), analytic_data (分析数据集)
# ==============================================================================

# 1. 加载必要的包
library(survey)   # 核心包：处理NHANES的复杂抽样权重
library(dplyr)    # 数据处理
library(tableone) # 制作基线表

# 2. 读取最终分析数据
# 注意：确保文件路径正确
setwd("D:/space_R/HP&dietary fiber")
dt <- read.csv("NHANES_Final_Analytic_Data.csv")

# 3. 变量类型转换
# 原理：R语言默认将数字读为连续变量，分类变量需要手动转为 Factor
dt$race_binary <- factor(dt$race_binary, levels = c("White", "Others")) # 种族二分类
dt$gender <- factor(dt$gender, levels = c("Male", "Female"))
dt$smoking <- factor(dt$smoking, levels = c(0, 1), labels = c("Non-smoker", "Smoker"))
dt$fiber_quartile <- factor(dt$fiber_quartile, levels = c("Q1", "Q2", "Q3", "Q4")) # 暴露因素

# 4. 建立复杂抽样设计对象 (Survey Design Object)
# 技术原理：
# - ids = ~SDMVPSU: 初级抽样单元 (Cluster)，用于校正聚集性误差
# - strata = ~SDMVSTRA: 分层变量，用于控制层间变异
# - weights = ~WTMEC4YR: 4年MEC检查权重，用于还原代表全美人群
# - nest = TRUE: 声明PSU嵌套在Strata中
nhanes_design <- svydesign(
  id = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  weights = ~WTMEC4YR, 
  nest = TRUE, 
  data = dt
)

print("数据加载完成，抽样设计对象 'nhanes_design' 已创建。")