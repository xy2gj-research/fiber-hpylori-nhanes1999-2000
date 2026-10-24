# ==============================================================================
# File Name: 01_Process_From_XPT.R
# Purpose: 从原始 XPT 读取数据，精确复刻 3340 人筛选流程，并生成最终分析对象
# Input Files: DEMO.XPT, DRXTOT.XPT, LAB11.XPT, BMX.XPT, SMQ.XPT
# Output: NHANES_Final_Analytic_Data.csv, nhanes_design (R对象)
# ==============================================================================

library(haven)    # 读取 XPT
library(dplyr)    # 数据处理
library(survey)   # 复杂抽样

# 1. 读取原始数据 (基于您提供的正确文件名)
# ------------------------------------------------------------------------------
# 注意：确保这些 .XPT 文件都在您的工作目录下
demo <- read_xpt("DEMO.XPT")     # 人口学
diet <- read_xpt("DRXTOT.XPT")   # 饮食 (注意文件名是 DRXTOT)
lab  <- read_xpt("LAB11.XPT")    # 实验室 (注意文件名是 LAB11)
body <- read_xpt("BMX.XPT")      # 身体测量
smq  <- read_xpt("SMQ.XPT")      # 吸烟问卷

print("正在合并数据...")

# 2. 合并数据 (Merging)
# ------------------------------------------------------------------------------
# 逻辑：将 SEQN 转为数字以防格式不匹配，使用 left_join 保留 DEMO 中的所有人
dt_raw <- demo %>%
  mutate(SEQN = as.numeric(SEQN)) %>%
  left_join(mutate(diet, SEQN = as.numeric(SEQN)), by = "SEQN") %>%
  left_join(mutate(lab,  SEQN = as.numeric(SEQN)), by = "SEQN") %>%
  left_join(mutate(body, SEQN = as.numeric(SEQN)), by = "SEQN") %>%
  left_join(mutate(smq,  SEQN = as.numeric(SEQN)), by = "SEQN")

n_total <- nrow(dt_raw)
print(paste("0. 原始总人数:", n_total))

# 3. 数据清洗与重编码 (Recoding)
# ------------------------------------------------------------------------------
dt_proc <- dt_raw %>%
  mutate(
    # --- [关键修正] 结局变量: H. pylori ---
    # 原始变量名为 LBXHP1 (1=Positive, 2=Negative)
    # 我们将其重命名为 LBXHP (1=Positive, 0=Negative) 方便后续代码通用
    LBXHP = case_when(
      LBXHP1 == 1 ~ 1,  # 阳性
      LBXHP1 == 2 ~ 0,  # 阴性
      TRUE ~ NA_real_   # 缺失
    ),
    
    # --- 暴露变量: 膳食纤维 ---
    fiber = DRXTFIBE,
    
    # --- 协变量 ---
    age = RIDAGEYR,
    
    # 性别: 1=Male, 2=Female
    gender = factor(RIAGENDR, levels = c(1, 2), labels = c("Male", "Female")),
    
    # 种族: 3=White, Others
    race_binary = case_when(
      RIDRETH1 == 3 ~ "White",
      TRUE ~ "Others"
    ),
    race_binary = factor(race_binary, levels = c("White", "Others")),
    
    # BMI
    bmi = BMXBMI,
    
    # 贫困指数 (PIR)
    pir = INDFMPIR,
    
    # 吸烟 (SMQ020): 1=Yes, 2=No
    smoking = case_when(
      SMQ020 == 1 ~ "Smoker",
      SMQ020 == 2 ~ "Non-smoker",
      TRUE ~ NA_character_
    ),
    smoking = factor(smoking, levels = c("Non-smoker", "Smoker")),
    
    # 权重变量 (确保存在)
    # 如果是 NHANES 1999-2000 单个周期，通常使用 WTMEC2YR 或 WTMEC4YR (取决于怎么合并)
    # 这里假设使用 DEMO 文件中存在的权重变量，通常为 WTMEC4YR 或 WTMEC2YR
    # 为了代码通用，我们创建一个统一的 weight 变量
    weight = if("WTMEC4YR" %in% names(.)) WTMEC4YR else WTMEC2YR 
  )

# 4. 严格按照您的逻辑进行筛选 (Step-by-Step Exclusion)
# ------------------------------------------------------------------------------

# Step 1: 排除年龄 < 20
dt_s1 <- dt_proc %>% filter(age >= 20)
print(paste("1. 排除年龄 <20 后人数:", nrow(dt_s1)))

# Step 2: 排除 Hp 缺失 (使用修正后的变量名 LBXHP1 或 LBXHP)
dt_s2 <- dt_s1 %>% filter(!is.na(LBXHP))
print(paste("2. 排除 Hp 缺失后人数:", nrow(dt_s2)))

# Step 3: 排除纤维数据缺失
dt_s3 <- dt_s2 %>% filter(!is.na(fiber))
print(paste("3. 排除纤维缺失后人数:", nrow(dt_s3)))

# Step 4: 排除协变量缺失 (BMI, PIR, Smoking)
final_data <- dt_s3 %>% filter(!is.na(bmi) & !is.na(pir) & !is.na(smoking))
print(paste("4. 最终纳入分析人数 (Target: 3340):", nrow(final_data)))

# 检查点：如果这里的数字不是 3340，说明上面的逻辑有微小偏差，但根据您提供的代码应为 3340。

# 5. 生成四分位数 (Quartiles)
# ------------------------------------------------------------------------------
# 这一步必须在筛选完人之后做，保证分层准确
final_data$fiber_quartile <- ntile(final_data$fiber, 4)
final_data$fiber_quartile <- factor(final_data$fiber_quartile, 
                                    levels = c(1, 2, 3, 4), 
                                    labels = c("Q1", "Q2", "Q3", "Q4"))

# 6. 保存数据 (备份)
# ------------------------------------------------------------------------------
write.csv(final_data, "NHANES_Final_Analytic_Data.csv", row.names = FALSE)
print("已保存: NHANES_Final_Analytic_Data.csv")

# 7. 建立 Survey Design 对象 (用于后续所有分析)
# ------------------------------------------------------------------------------
# 注意：这里 id 使用 SDMVPSU, strata 使用 SDMVSTRA, 权重使用 weight
nhanes_design <- svydesign(
  id = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  weights = ~weight, 
  nest = TRUE, 
  data = final_data
)

print("✅ 数据处理完成！对象 'nhanes_design' 已准备好，可以运行 02-07 号脚本了。")