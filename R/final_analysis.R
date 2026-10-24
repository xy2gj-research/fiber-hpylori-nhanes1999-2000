# =====================================================================
# FINAL design-based reanalysis (svyglm, WTDRD1, strata+PSU)
# Outputs: AI_Table2_DesignBased.csv, AI_Table3_Subgroup_DesignBased.csv,
#          AI_Table3_ForForest.csv, AI_Figure2_RCS.tif
# =====================================================================
suppressMessages({library(survey); library(splines); library(Hmisc); library(ggplot2)})
setwd("D:/space_R/HP&dietary fiber")

df <- read.csv("NHANES_Final_Analytic_Data_V2.csv")
df$alcohol_group <- ifelse(is.na(df$DRXTALCO), NA,
                           ifelse(df$DRXTALCO > 0, "Drinker", "Non-Drinker"))
df$Hyperlipidemia[is.na(df$Hyperlipidemia)] <- "Unknown"
df$Physical_Activity[is.na(df$Physical_Activity)] <- "Unknown"

covs <- c("RIDAGEYR","gender","Race_Label","Education_Label","pir","smoking",
          "alcohol_group","bmi","energy","Diabetes","Hypertension",
          "Hyperlipidemia","Physical_Activity")
vars <- c("hp_status","fiber_adj","fiber_adj_quartile", covs,
          "WTDRD1","SDMVPSU","SDMVSTRA")
cc <- df[complete.cases(df[, vars]), ]
cat("Analytic n:", nrow(cc), "\n")

fac_vars <- c("gender","Race_Label","Education_Label","smoking","alcohol_group",
              "Diabetes","Hypertension","Hyperlipidemia","Physical_Activity")
for (v in fac_vars) cc[[v]] <- factor(cc[[v]])
cc$fiber_q <- factor(cc$fiber_adj_quartile, levels = c("Q1","Q2","Q3","Q4"))
cc$fiber_q_num <- as.numeric(cc$fiber_q)
cc$hp <- as.numeric(cc$hp_status)

# subgroup variables
cc$age_group <- cut(cc$RIDAGEYR, c(-Inf, 45, 65, Inf),
                    labels = c("< 45", "45-64", ">= 65"), right = FALSE)
cc$bmi_group <- cut(cc$bmi, c(-Inf, 25, 30, Inf),
                    labels = c("Normal", "Overweight", "Obese"), right = FALSE)
cc$VitC_Cat <- factor(ifelse(cc$DRXTVC >= median(cc$DRXTVC, na.rm=TRUE), ">= Median", "< Median"))
cc$Energy_Cat <- factor(ifelse(cc$energy >= median(cc$energy, na.rm=TRUE), ">= Median", "< Median"))

options(survey.lonely.psu = "adjust")
des <- svydesign(ids=~SDMVPSU, strata=~SDMVSTRA, weights=~WTDRD1, nest=TRUE, data=cc)
cat("design df:", degf(des), " survey pkg:", as.character(packageVersion("survey")), "\n")

get_or <- function(model, pattern) {
  s <- summary(model)$coefficients
  est <- s[,1]; se <- s[,2]; z <- est/se; p <- 2*pnorm(-abs(z))
  idx <- grep(pattern, rownames(s))
  data.frame(term=rownames(s)[idx], OR=exp(est[idx]),
             lo=exp(est[idx]-1.96*se[idx]), hi=exp(est[idx]+1.96*se[idx]),
             P=p[idx], row.names=NULL)
}
wald_chi <- function(model, idx) {
  b <- coef(model)[idx]; V <- vcov(model)[idx, idx, drop=FALSE]
  chi2 <- as.numeric(t(b) %*% solve(V) %*% b)
  pchisq(chi2, df=length(idx), lower.tail=FALSE)
}

f_main <- hp ~ fiber_q + RIDAGEYR + gender + Race_Label + Education_Label +
  pir + smoking + alcohol_group + bmi + energy + Diabetes + Hypertension +
  Hyperlipidemia + Physical_Activity

# ---------------- Table 2 ----------------
m1 <- svyglm(hp ~ fiber_q, design=des, family=quasibinomial())
m2 <- svyglm(hp ~ fiber_q + RIDAGEYR + gender + Race_Label + Education_Label + pir,
             design=des, family=quasibinomial())
m3 <- svyglm(f_main, design=des, family=quasibinomial())
m3_cont <- svyglm(update(f_main, . ~ . - fiber_q + fiber_adj), design=des, family=quasibinomial())
m3_tr <- svyglm(update(f_main, . ~ . - fiber_q + fiber_q_num), design=des, family=quasibinomial())

cat("\n===== TABLE 2 (design-based, WTDRD1, n=3295) =====\n")
res2 <- list()
for (nm in c("m1","m2","m3")) {
  r <- get_or(get(nm), "fiber_q")
  r$model <- nm
  res2[[nm]] <- r
  cat("---", nm, "---\n"); print(r, digits=3)
}
cont <- get_or(m3_cont, "fiber_adj"); cat("--- continuous (M3) ---\n"); print(cont, digits=3)
tr <- get_or(m3_tr, "fiber_q_num"); cat("--- trend (M3) ---\n"); print(tr, digits=3)

tab2_out <- data.frame(
  Row = c("M1_Q2","M1_Q3","M1_Q4","M2_Q2","M2_Q3","M2_Q4","M3_Q2","M3_Q3","M3_Q4",
          "M3_continuous","M3_trend"),
  OR_CI = sprintf("%.3f (%.3f, %.3f)",
                  c(res2$m1$OR, res2$m2$OR, res2$m3$OR, cont$OR, tr$OR),
                  c(res2$m1$lo, res2$m2$lo, res2$m3$lo, cont$lo, tr$lo),
                  c(res2$m1$hi, res2$m2$hi, res2$m3$hi, cont$hi, tr$hi)),
  P = sprintf("%.4f", c(res2$m1$P, res2$m2$P, res2$m3$P, cont$P, tr$P)))
write.csv(tab2_out, "AI_Table2_DesignBased.csv", row.names=FALSE)

# ---------------- Table 3 subgroups ----------------
sg_list <- list(
  "Age (years)" = "age_group", "Gender" = "gender", "Race/Ethnicity" = "Race_Label",
  "Body mass index" = "bmi_group", "Smoking status" = "smoking",
  "Alcohol consumption" = "alcohol_group", "Vitamin C intake" = "VitC_Cat",
  "Total energy intake" = "Energy_Cat", "Diabetes" = "Diabetes",
  "Hypertension" = "Hypertension", "Hyperlipidemia" = "Hyperlipidemia",
  "Physical Activity" = "Physical_Activity")
cov_string <- "RIDAGEYR + gender + Race_Label + Education_Label + pir + smoking + alcohol_group + bmi + energy + Diabetes + Hypertension + Hyperlipidemia + Physical_Activity"
# mapping from subgroup var to the covariate name to drop
drop_map <- c(age_group="RIDAGEYR", gender="gender", Race_Label="Race_Label",
              bmi_group="bmi", smoking="smoking", alcohol_group="alcohol_group",
              VitC_Cat="", Energy_Cat="energy", Diabetes="Diabetes",
              Hypertension="Hypertension", Hyperlipidemia="Hyperlipidemia",
              Physical_Activity="Physical_Activity")

sg_rows <- list(); k <- 0
for (sg_label in names(sg_list)) {
  sg <- sg_list[[sg_label]]
  covs_vec <- trimws(unlist(strsplit(cov_string, "\\+")))
  drop_v <- drop_map[[sg]]
  if (nzchar(drop_v)) covs_vec <- covs_vec[covs_vec != drop_v]
  covs_s <- paste(covs_vec, collapse = " + ")
  # interaction test on full design
  f_int <- as.formula(paste("hp ~ fiber_q_num *", sg, "+", covs_s))
  m_int <- tryCatch(svyglm(f_int, design=des, family=quasibinomial()), error=function(e) NULL)
  p_int <- NA
  if (!is.null(m_int)) {
    idx <- grep("fiber_q_num:", names(coef(m_int)))
    if (length(idx) > 0) p_int <- wald_chi(m_int, idx)
  }
  k <- k + 1
  sg_rows[[k]] <- data.frame(Subgroup=sg_label, OR_CI="", P="", P_int=sprintf("%.4f", p_int))
  for (lvl in levels(factor(cc[[sg]]))) {
    sub_des <- subset(des, cc[[sg]] == lvl & !is.na(cc[[sg]]))
    if (nrow(sub_des$variables) < 50) next
    f_sub <- as.formula(paste("hp ~ fiber_q_num +", covs_s))
    m_sub <- tryCatch(svyglm(f_sub, design=sub_des, family=quasibinomial()), error=function(e) NULL)
    if (is.null(m_sub)) next
    r <- get_or(m_sub, "fiber_q_num")
    k <- k + 1
    sg_rows[[k]] <- data.frame(
      Subgroup = paste0("    ", lvl),
      OR_CI = sprintf("%.3f (%.3f, %.3f)", r$OR, r$lo, r$hi),
      P = sprintf("%.4f", r$P), P_int = "")
  }
}
tab3 <- do.call(rbind, sg_rows)
write.csv(tab3, "AI_Table3_ForForest.csv", row.names=FALSE)
cat("\n===== TABLE 3 subgroups (design-based) =====\n")
print(tab3)

# ---------------- Figure 2: RCS via svyglm + ns, contrast delta method ----------------
wtd_q <- function(x, w, p) as.numeric(wtd.quantile(x, weights=w, probs=p, normwt=TRUE))
k1 <- wtd_q(cc$fiber_adj, cc$WTDRD1, 0.10)
k2 <- wtd_q(cc$fiber_adj, cc$WTDRD1, 0.50)
k3 <- wtd_q(cc$fiber_adj, cc$WTDRD1, 0.90)
ref_x <- 10
cc$fiber_c <- cc$fiber_adj - ref_x
k1c <- k1 - ref_x; k2c <- k2 - ref_x; k3c <- k3 - ref_x
# precompute spline basis columns (avoids svyglm all.vars() check on knot objects)
ns_basis <- ns(cc$fiber_c, knots = k2c, Boundary.knots = c(k1c, k3c))
cc$fiber_ns1 <- ns_basis[, 1]
cc$fiber_ns2 <- ns_basis[, 2]
# rebuild design so spline columns are available inside the design object
des <- svydesign(ids=~SDMVPSU, strata=~SDMVSTRA, weights=~WTDRD1, nest=TRUE, data=cc)

f_ns <- as.formula(paste("hp ~ fiber_ns1 + fiber_ns2 +", cov_string))
m_ns <- svyglm(f_ns, design=des, family=quasibinomial())
p_nonlin <- wald_chi(m_ns, 2)       # non-linear component
p_overall <- wald_chi(m_ns, 1:2)    # overall
cat(sprintf("\nFigure 2 spline tests: P overall = %.4f, P non-linearity = %.4f\n",
            p_overall, p_nonlin))

x_lo <- wtd_q(cc$fiber_adj, cc$WTDRD1, 0.01)
x_hi <- wtd_q(cc$fiber_adj, cc$WTDRD1, 0.99)
grid_x <- seq(x_lo, x_hi, length.out = 200)
# reference profile: mode of factors, weighted mean of continuous
ref_profile <- list(
  RIDAGEYR = wtd_q(cc$RIDAGEYR, cc$WTDRD1, 0.5),
  gender = factor(levels(cc$gender)[1], levels=levels(cc$gender)),
  Race_Label = factor("Non-Hispanic White", levels=levels(cc$Race_Label)),
  Education_Label = factor(levels(cc$Education_Label)[1], levels=levels(cc$Education_Label)),
  pir = wtd_q(cc$pir, cc$WTDRD1, 0.5),
  smoking = factor(levels(cc$smoking)[1], levels=levels(cc$smoking)),
  alcohol_group = factor(levels(cc$alcohol_group)[1], levels=levels(cc$alcohol_group)),
  bmi = wtd_q(cc$bmi, cc$WTDRD1, 0.5),
  energy = wtd_q(cc$energy, cc$WTDRD1, 0.5),
  Diabetes = factor("No", levels=levels(cc$Diabetes)),
  Hypertension = factor("No", levels=levels(cc$Hypertension)),
  Hyperlipidemia = factor("No", levels=levels(cc$Hyperlipidemia)),
  Physical_Activity = factor("Active", levels=levels(cc$Physical_Activity)))
make_X <- function(fx) {
  nd <- data.frame(fiber_c = fx - ref_x)
  ns_mat <- ns(nd$fiber_c, knots = k2c, Boundary.knots = c(k1c, k3c))
  nd$fiber_ns1 <- ns_mat[, 1]
  nd$fiber_ns2 <- ns_mat[, 2]
  for (v in names(ref_profile)) nd[[v]] <- ref_profile[[v]]
  X <- model.matrix(~ ., data = nd[, c("fiber_ns1","fiber_ns2", names(ref_profile))])
  # align columns with model coefficients
  cf_names <- names(coef(m_ns))
  X_full <- matrix(0, nrow=nrow(X), ncol=length(cf_names),
                   dimnames=list(NULL, cf_names))
  common <- intersect(colnames(X), cf_names)
  X_full[, common] <- X[, common]
  X_full[, "(Intercept)"] <- 1
  X_full
}
Xg <- make_X(grid_x)
Xr <- make_X(rep(ref_x, 1))
D <- sweep(Xg, 2, as.numeric(Xr), "-")
V <- vcov(m_ns)
lp <- as.numeric((Xg - matrix(as.numeric(Xr), nrow=nrow(Xg), ncol=ncol(Xg), byrow=TRUE)) %*% coef(m_ns))
se <- sqrt(pmax(rowSums((D %*% V) * D), 0))
pred <- data.frame(x = grid_x, OR = exp(lp),
                   lo = exp(lp - 1.96*se), hi = exp(lp + 1.96*se))

p_rcs <- ggplot(pred) +
  geom_ribbon(aes(x=x, ymin=lo, ymax=hi), fill="#0073C2", alpha=0.2) +
  geom_line(aes(x=x, y=OR), color="#0073C2", linewidth=1.2) +
  geom_hline(yintercept=1, linetype="dashed", color="red", linewidth=0.8) +
  geom_vline(xintercept=ref_x, linetype="dashed", color="grey40", linewidth=0.7) +
  geom_vline(xintercept=c(k1,k2,k3), linetype="dotted", color="grey70", linewidth=0.5) +
  geom_rug(data=cc[cc$fiber_adj >= x_lo & cc$fiber_adj <= x_hi, ],
           aes(x=fiber_adj), inherit.aes=FALSE, alpha=0.15, sides="b") +
  annotate("text", x=ref_x, y=1.45, label="Reference = 10 g/day", hjust=-0.05, size=4) +
  annotate("text", x=x_hi, y=1.45, hjust=1, size=4.2,
           label=sprintf("P overall = %.3f\nP non-linearity = %.3f", p_overall, p_nonlin)) +
  coord_cartesian(ylim=c(0.4, 1.5)) +
  labs(x="Energy-Adjusted Dietary Fiber Intake (g/day)",
       y=expression("Adjusted OR for" ~ italic("H. pylori") ~ "Seropositivity"),
       title=expression("Dose-Response Association between Dietary Fiber and" ~ italic("H. pylori") ~ "Seropositivity")) +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5, size=13, face="bold"),
        axis.title=element_text(size=12, face="bold"),
        axis.text=element_text(size=11, color="black"))
ggsave("AI_Figure2_RCS.tif", p_rcs, width=8, height=6.5, dpi=600,
       device="tiff", compression="lzw")
cat("\nFigure 2 saved: AI_Figure2_RCS.tif\n")

# weighted prevalence for the record
des_full <- svydesign(ids=~SDMVPSU, strata=~SDMVSTRA, weights=~WTDRD1,
                      nest=TRUE, data=df[!is.na(df$WTDRD1) & !is.na(df$hp_status), ])
df$hp <- as.numeric(df$hp_status)
cat("Weighted prevalence (n=3328):", round(100*coef(svymean(~hp, des_full, na.rm=TRUE)), 1), "%\n")
cat("knots at:", round(c(k1,k2,k3),2), " x-range:", round(c(x_lo,x_hi),2), "\n")
