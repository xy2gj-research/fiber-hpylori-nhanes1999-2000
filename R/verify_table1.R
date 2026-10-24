# Table 1 design-based P values + per-5g OR + subgroup n/events
suppressMessages({library(survey); library(splines)})
setwd("D:/space_R/HP&dietary fiber")

df <- read.csv("NHANES_Final_Analytic_Data_V2.csv")
df$alcohol_group <- ifelse(is.na(df$DRXTALCO), NA,
                           ifelse(df$DRXTALCO > 0, "Drinker", "Non-Drinker"))
df$Hyperlipidemia[is.na(df$Hyperlipidemia)] <- "Unknown"
df$Physical_Activity[is.na(df$Physical_Activity)] <- "Unknown"
df$hp <- as.numeric(df$hp_status)

# --- full baseline sample (n=3340, matches Table 1) ---
covs <- c("RIDAGEYR","gender","Race_Label","Education_Label","pir","smoking",
          "alcohol_group","bmi","energy","Diabetes","Hypertension",
          "Hyperlipidemia","Physical_Activity")
vars <- c("hp_status","fiber_adj","fiber_adj_quartile", covs, "WTDRD1","SDMVPSU","SDMVSTRA")
base <- df[complete.cases(df[, setdiff(vars, c("WTDRD1","SDMVPSU","SDMVSTRA"))]), ]
cat("Baseline n (Table 1):", nrow(base), " events:", sum(base$hp), "\n")

fac_vars <- c("gender","Race_Label","Education_Label","smoking","alcohol_group",
              "Diabetes","Hypertension","Hyperlipidemia","Physical_Activity")
for (v in fac_vars) base[[v]] <- factor(base[[v]])

# weighted subset for design-based tests
wsub <- base[complete.cases(base[, c("WTDRD1","SDMVPSU","SDMVSTRA")]), ]
cat("Weighted subset n:", nrow(wsub), " events:", sum(wsub$hp), "\n")
options(survey.lonely.psu = "adjust")
des <- svydesign(ids=~SDMVPSU, strata=~SDMVSTRA, weights=~WTDRD1, nest=TRUE, data=wsub)

fmt_p <- function(p) ifelse(is.na(p), "ERR", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))

cat("\n=== Continuous: design-based (svyglm Wald | svyranktest) vs unweighted ===\n")
for (v in c("RIDAGEYR","pir","bmi","energy","fiber_adj")) {
  f <- as.formula(paste(v, "~ hp"))
  m <- svyglm(f, design = des)
  p_wald <- summary(m)$coefficients[2, 4]
  p_rank <- tryCatch(svyranktest(f, design = des)$p.value, error = function(e) NA)
  p_uw <- wilcox.test(base[[v]] ~ base$hp)$p.value
  cat(sprintf("%-10s Wald P=%s | rank P=%s | unweighted Wilcoxon P=%s\n", v, fmt_p(p_wald), fmt_p(p_rank), fmt_p(p_uw)))
}

cat("\n=== Categorical: Rao-Scott (design) vs unweighted chi-square ===\n")
for (v in c(fac_vars, "fiber_adj_quartile")) {
  base[[v]] <- factor(base[[v]]); wsub[[v]] <- factor(wsub[[v]])
  des2 <- svydesign(ids=~SDMVPSU, strata=~SDMVSTRA, weights=~WTDRD1, nest=TRUE, data=wsub)
  p <- tryCatch(svychisq(as.formula(paste("~ hp +", v)), design = des2, statistic = "F")$p.value,
                error = function(e) NA)
  p_uw <- chisq.test(table(base$hp, base[[v]]))$p.value
  cat(sprintf("%-20s Rao-Scott P=%s | unweighted P=%s\n", v, fmt_p(p), fmt_p(p_uw)))
}

# --- per 5 g/day OR (Model 3) ---
cat("\n=== per 5 g/day (Model 3, design-based) ===\n")
m3 <- svyglm(hp ~ fiber_adj + RIDAGEYR + gender + Race_Label + Education_Label +
             pir + smoking + alcohol_group + bmi + energy + Diabetes + Hypertension +
             Hyperlipidemia + Physical_Activity, design = des, family = quasibinomial())
b <- coef(m3)["fiber_adj"]; se <- sqrt(vcov(m3)["fiber_adj", "fiber_adj"])
cat(sprintf("per 1 g/day: OR=%.3f (%.3f, %.3f)\n", exp(b), exp(b-1.96*se), exp(b+1.96*se)))
cat(sprintf("per 5 g/day: OR=%.3f (%.3f, %.3f)\n", exp(5*b), exp(5*(b-1.96*se)), exp(5*(b+1.96*se))))

# --- subgroup n / events (unweighted, baseline n=3340) ---
cat("\n=== subgroup n / events ===\n")
base$age_group <- cut(base$RIDAGEYR, c(-Inf, 45, 65, Inf), labels = c("< 45", "45-64", ">= 65"), right = FALSE)
base$bmi_group <- cut(base$bmi, c(-Inf, 25, 30, Inf), labels = c("Normal", "Overweight", "Obese"), right = FALSE)
base$VitC_Cat <- factor(ifelse(base$DRXTVC >= median(base$DRXTVC, na.rm=TRUE), ">= Median", "< Median"))
base$Energy_Cat <- factor(ifelse(base$energy >= median(base$energy, na.rm=TRUE), ">= Median", "< Median"))
res <- list()
for (v in c("age_group","gender","Race_Label","bmi_group","smoking","alcohol_group",
            "VitC_Cat","Energy_Cat","Diabetes","Hypertension","Hyperlipidemia","Physical_Activity")) {
  tab <- table(base[[v]], base$hp)
  cat(v, ":\n"); print(tab)
  for (lv in rownames(tab)) {
    res[[length(res)+1]] <- data.frame(var=v, level=lv, n_neg=tab[lv,"0"], n_pos=tab[lv,"1"],
                                       n_total=tab[lv,"0"]+tab[lv,"1"])
  }
}
sgn <- do.call(rbind, res)
write.csv(sgn, "AI_Table3_Subgroup_N.csv", row.names = FALSE)
cat("\nsaved AI_Table3_Subgroup_N.csv\n")
