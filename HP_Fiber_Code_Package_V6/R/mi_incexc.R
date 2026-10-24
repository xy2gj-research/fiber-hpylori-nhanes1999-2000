# =====================================================================
# Included vs Excluded comparison + Multiple Imputation sensitivity
# =====================================================================
suppressMessages({library(haven); library(dplyr); library(survey)})
setwd("D:/space_R/HP&dietary fiber")

# ---- rebuild eligible pool from raw XPT ----
demo <- read_xpt("DEMO.XPT"); lab <- read_xpt("LAB11.XPT"); diet <- read_xpt("DRXTOT.XPT")
body <- read_xpt("BMX.XPT"); smq <- read_xpt("SMQ.XPT"); diq <- read_xpt("DIQ.XPT")
bpq <- read_xpt("BPQ.XPT"); paq <- read_xpt("PAQ.XPT")

n0 <- nrow(demo)
d <- demo %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2, INDFMPIR,
         WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  left_join(lab %>% select(SEQN, LBXHP1), by = "SEQN") %>%
  left_join(diet %>% select(SEQN, DRXTFIBE, DRXTKCAL, DRXTVC, DRXTALCO, WTDRD1), by = "SEQN") %>%
  left_join(body %>% select(SEQN, BMXBMI), by = "SEQN") %>%
  left_join(smq %>% select(SEQN, SMQ020), by = "SEQN") %>%
  left_join(diq %>% select(SEQN, DIQ010), by = "SEQN") %>%
  left_join(bpq %>% select(SEQN, BPQ020, BPQ080), by = "SEQN") %>%
  left_join(paq %>% select(SEQN, PAD200, PAD320), by = "SEQN")

cat("0. Total NHANES 1999-2000 participants:", n0, "\n")

# recode
d <- d %>% mutate(
  age = RIDAGEYR,
  gender = factor(RIAGENDR, levels = c(1,2), labels = c("Male","Female")),
  race = factor(RIDRETH1, levels = c(1,2,3,4,5),
                labels = c("Mexican American","Other Hispanic","Non-Hispanic White",
                           "Non-Hispanic Black","Other Race")),
  education = case_when(DMDEDUC2 %in% c(1,2) ~ "< High School",
                        DMDEDUC2 == 3 ~ "High School Grad",
                        DMDEDUC2 %in% c(4,5) ~ "> High School",
                        TRUE ~ NA_character_),
  pir = INDFMPIR,
  bmi = BMXBMI,
  smoking = case_when(SMQ020 == 1 ~ "Smoker", SMQ020 == 2 ~ "Non-Smoker", TRUE ~ NA_character_),
  fiber = DRXTFIBE, energy = DRXTKCAL, vitc = DRXTVC, alco = DRXTALCO,
  diabetes = case_when(DIQ010 == 1 ~ "Yes", DIQ010 %in% c(2,3) ~ "No", TRUE ~ NA_character_),
  hypertension = case_when(BPQ020 == 1 ~ "Yes", BPQ020 == 2 ~ "No", TRUE ~ NA_character_),
  hyperlipidemia = case_when(BPQ080 == 1 ~ "Yes", BPQ080 == 2 ~ "No", TRUE ~ NA_character_),
  physical = case_when(PAD200 == 1 | PAD320 == 1 ~ "Active",
                       !is.na(PAD200) | !is.na(PAD320) ~ "Inactive",
                       TRUE ~ NA_character_),
  hp = case_when(LBXHP1 >= 1.10 ~ 1, LBXHP1 <= 0.90 ~ 0, TRUE ~ NA_real_),
  equivocal = !is.na(LBXHP1) & LBXHP1 > 0.90 & LBXHP1 < 1.10
)

# ---- exclusion cascade ----
s1 <- d %>% filter(age >= 20)
cat("1. Age >= 20:", nrow(s1), "\n")
s2 <- s1 %>% filter(!is.na(LBXHP1))
cat("2. With H. pylori serology:", nrow(s2), " (excluded missing serology:", nrow(s1)-nrow(s2), ")\n")
cat("   Equivocal (ISR 0.91-1.09):", sum(s2$equivocal), "\n")
s3 <- s2 %>% filter(!equivocal)
cat("3. After excluding equivocal:", nrow(s3), "\n")
s4 <- s3 %>% filter(!is.na(fiber) & !is.na(energy))
cat("4. With dietary fiber/energy data:", nrow(s4), " (excluded:", nrow(s3)-nrow(s4), ")\n")

v2 <- read.csv("NHANES_Final_Analytic_Data_V2.csv")
d$included <- d$SEQN %in% v2$SEQN
s4$included <- s4$SEQN %in% v2$SEQN
cat("5. Final analytic sample (V2):", sum(s4$included), " (excluded for covariates:",
    nrow(s4) - sum(s4$included), ")\n")

# ---- included vs excluded comparison (eligible = age>=20, valid serostatus) ----
elig <- s3 %>% mutate(group = ifelse(SEQN %in% v2$SEQN, "Included", "Excluded"))
cat("\nEligible (age>=20, valid serostatus):", nrow(elig),
    "| Included:", sum(elig$group=="Included"), "| Excluded:", sum(elig$group=="Excluded"), "\n")

# missingness per variable among eligible
miss_vars <- c("fiber","energy","bmi","pir","smoking","education","alco","vitc",
               "diabetes","hypertension","hyperlipidemia","physical")
miss_tab <- data.frame(
  Variable = miss_vars,
  N_missing = sapply(miss_vars, function(v) sum(is.na(elig[[v]]))),
  Pct = round(100 * sapply(miss_vars, function(v) mean(is.na(elig[[v]]))), 1))
cat("\nMissingness among eligible:\n"); print(miss_tab)
write.csv(miss_tab, "AI_Missingness_Eligible.csv", row.names = FALSE)

# weighted comparison with WTMEC2YR (available for all MEC participants)
cmp <- elig %>% filter(!is.na(WTMEC2YR) & WTMEC2YR > 0)
options(survey.lonely.psu = "adjust")
des_c <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR,
                   nest = TRUE, data = cmp)
cmp$group <- factor(cmp$group, levels = c("Included","Excluded"))
des_c <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR,
                   nest = TRUE, data = cmp)

fmt <- function(x, d=1) sprintf(paste0("%.", d, "f"), x)
rows <- list()
add_cont <- function(v, label) {
  m <- svyby(as.formula(paste("~", v)), ~group, des_c, svymean, na.rm = TRUE, vartype = c("se"))
  tst <- svyttest(as.formula(paste(v, "~ group")), des_c)
  rows[[length(rows)+1]] <<- data.frame(
    Variable = label,
    Included = fmt(coef(m)[1]), Excluded = fmt(coef(m)[2]),
    P = sprintf("%.4f", tst$p.value), check.names = FALSE)
}
add_cat <- function(v, label) {
  tab <- svytable(as.formula(paste("~", v, "+ group")), des_c)
  pct <- round(100 * prop.table(tab, 2), 1)
  tst <- svychisq(as.formula(paste("~", v, "+ group")), des_c)
  lv <- rownames(tab)
  for (i in seq_along(lv)) {
    rows[[length(rows)+1]] <<- data.frame(
      Variable = paste0(label, ": ", lv[i]),
      Included = sprintf("%.1f%%", pct[i, "Included"]),
      Excluded = sprintf("%.1f%%", pct[i, "Excluded"]),
      P = ifelse(i == 1, sprintf("%.4f", tst$p.value), ""), check.names = FALSE)
  }
}
add_cont("age", "Age (years), weighted mean")
add_cat("gender", "Gender")
add_cat("race", "Race/Ethnicity")
add_cat("education", "Education")
add_cont("pir", "Family income-to-poverty ratio, weighted mean")
add_cont("bmi", "BMI (kg/m2), weighted mean")
add_cat("smoking", "Smoking status")
add_cont("energy", "Total energy intake (kcal/day), weighted mean*")
add_cont("fiber", "Dietary fiber (g/day), weighted mean*")
# weighted seropositivity
hpv <- svyby(~hp, ~group, des_c, svymean, na.rm = TRUE, vartype = c("se"))
tst_hp <- svyttest(hp ~ group, des_c)
rows[[length(rows)+1]] <- data.frame(
  Variable = "H. pylori seropositive, weighted %",
  Included = fmt(100*coef(hpv)[1]), Excluded = fmt(100*coef(hpv)[2]),
  P = sprintf("%.4f", tst_hp$p.value), check.names = FALSE)
inc_exc <- do.call(rbind, rows)
cat("\n===== INCLUDED vs EXCLUDED (WTMEC2YR-weighted) =====\n"); print(inc_exc)
write.csv(inc_exc, "AI_SuppTable_IncludedVsExcluded.csv", row.names = FALSE)

# =====================================================================
# Multiple imputation sensitivity analysis
# =====================================================================
suppressMessages(library(mice))
mi_data <- elig %>%
  select(SEQN, hp, age, gender, race, education, pir, bmi, smoking,
         fiber, energy, vitc, alco, diabetes, hypertension, hyperlipidemia,
         physical, WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  as.data.frame()
mi_data$education <- factor(mi_data$education, levels = c("< High School","High School Grad","> High School"))
mi_data$smoking <- factor(mi_data$smoking)
for (v in c("diabetes","hypertension","hyperlipidemia","physical")) mi_data[[v]] <- factor(mi_data[[v]])
mi_data$SDMVSTRA <- factor(mi_data$SDMVSTRA); mi_data$SDMVPSU <- factor(mi_data$SDMVPSU)

cat("\nMI sample size:", nrow(mi_data), "\n")

meth <- make.method(mi_data)
meth[c("SEQN","hp","age","gender","race","WTMEC2YR","SDMVPSU","SDMVSTRA")] <- ""
pred <- make.predictorMatrix(mi_data)
pred[, c("SEQN")] <- 0
pred["hp", ] <- 0  # outcome not imputed
pred[, "hp"] <- 1  # outcome used as predictor
pred["WTMEC2YR", ] <- 0; pred[, "WTMEC2YR"] <- 1
pred["SDMVPSU", ] <- 0; pred[, "SDMVPSU"] <- 1
pred["SDMVSTRA", ] <- 0; pred[, "SDMVSTRA"] <- 1

M <- 20
t0 <- Sys.time()
imp <- mice(mi_data, m = M, maxit = 5, method = meth, predictorMatrix = pred,
            seed = 42, printFlag = FALSE)
cat("mice done in", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")

# per-imputation: energy-adjust fiber, quartiles, svyglm Model 3 (WTMEC2YR)
cov_string <- "age + gender + race + education + pir + smoking + alcohol_group + bmi + energy + diabetes + hypertension + hyperlipidemia + physical"
ests <- list()
for (i in 1:M) {
  di <- complete(imp, i)
  di$alcohol_group <- factor(ifelse(di$alco > 0, "Drinker", "Non-Drinker"))
  di$fiber_adj <- residuals(lm(fiber ~ energy, data = di))
  qs <- quantile(di$fiber_adj, c(.25,.5,.75), na.rm = TRUE)
  di$fiber_q <- cut(di$fiber_adj, c(-Inf, qs, Inf), labels = c("Q1","Q2","Q3","Q4"))
  di$fiber_q_num <- as.numeric(di$fiber_q)
  di$fiber5 <- di$fiber_adj / 5
  des_i <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR,
                     nest = TRUE, data = di)
  f_q <- as.formula(paste("hp ~ fiber_q +", cov_string))
  f_c <- as.formula(paste("hp ~ fiber5 +", cov_string))
  f_t <- as.formula(paste("hp ~ fiber_q_num +", cov_string))
  m_q <- svyglm(f_q, design = des_i, family = quasibinomial())
  m_c <- svyglm(f_c, design = des_i, family = quasibinomial())
  m_t <- svyglm(f_t, design = des_i, family = quasibinomial())
  grab <- function(m, pat) {
    s <- summary(m)$coefficients; idx <- grep(pat, rownames(s))
    data.frame(term = rownames(s)[idx], est = s[idx,1], var = s[idx,2]^2)
  }
  ests[[i]] <- rbind(grab(m_q, "^fiber_qQ"), grab(m_c, "^fiber5"), grab(m_t, "^fiber_q_num"))
}
all_est <- do.call(rbind, ests)
terms <- unique(all_est$term)
pooled <- data.frame(term = terms, est = NA, se = NA, p = NA)
for (t in terms) {
  e <- all_est$est[all_est$term == t]; v <- all_est$var[all_est$term == t]
  qbar <- mean(e); ubar <- mean(v); b <- var(e)
  tv <- ubar + (1 + 1/M) * b
  pooled$est[pooled$term == t] <- qbar
  pooled$se[pooled$term == t] <- sqrt(tv)
  pooled$p[pooled$term == t] <- 2 * pnorm(-abs(qbar / sqrt(tv)))
}
pooled$OR <- exp(pooled$est)
pooled$lo <- exp(pooled$est - 1.96 * pooled$se)
pooled$hi <- exp(pooled$est + 1.96 * pooled$se)
pooled$OR_CI <- sprintf("%.3f (%.3f, %.3f)", pooled$OR, pooled$lo, pooled$hi)
pooled$P <- sprintf("%.4f", pooled$p)
cat("\n===== MI-POOLED MODEL 3 (m=20, WTMEC2YR, n per imputation:", nrow(mi_data), ") =====\n")
print(pooled[, c("term","OR_CI","P")])
write.csv(pooled[, c("term","OR_CI","P")], "AI_MI_Model3_Pooled.csv", row.names = FALSE)
cat("\nALL DONE\n")
