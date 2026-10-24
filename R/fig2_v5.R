# =====================================================================
# Figure 2 v5: RCS curve + number at risk across exposure range
# =====================================================================
suppressMessages({library(survey); library(splines); library(Hmisc); library(ggplot2); library(gridExtra); library(grid)})
setwd("D:/space_R/HP&dietary fiber")

df <- read.csv("NHANES_Final_Analytic_Data_V2.csv")
df$alcohol_group <- ifelse(is.na(df$DRXTALCO), NA, ifelse(df$DRXTALCO > 0, "Drinker", "Non-Drinker"))
df$Hyperlipidemia[is.na(df$Hyperlipidemia)] <- "Unknown"
df$Physical_Activity[is.na(df$Physical_Activity)] <- "Unknown"
covs <- c("RIDAGEYR","gender","Race_Label","Education_Label","pir","smoking",
          "alcohol_group","bmi","energy","Diabetes","Hypertension",
          "Hyperlipidemia","Physical_Activity")
vars <- c("hp_status","fiber_adj","fiber_adj_quartile", covs, "WTDRD1","SDMVPSU","SDMVSTRA")
cc <- df[complete.cases(df[, vars]), ]
fac_vars <- c("gender","Race_Label","Education_Label","smoking","alcohol_group",
              "Diabetes","Hypertension","Hyperlipidemia","Physical_Activity")
for (v in fac_vars) cc[[v]] <- factor(cc[[v]])
cc$hp <- as.numeric(cc$hp_status)
options(survey.lonely.psu = "adjust")
cov_string <- "RIDAGEYR + gender + Race_Label + Education_Label + pir + smoking + alcohol_group + bmi + energy + Diabetes + Hypertension + Hyperlipidemia + Physical_Activity"

wtd_q <- function(x, w, p) as.numeric(wtd.quantile(x, weights=w, probs=p, normwt=TRUE))
k1 <- wtd_q(cc$fiber_adj, cc$WTDRD1, 0.10); k2 <- wtd_q(cc$fiber_adj, cc$WTDRD1, 0.50); k3 <- wtd_q(cc$fiber_adj, cc$WTDRD1, 0.90)
ref_x <- 10
cc$fiber_c <- cc$fiber_adj - ref_x
k1c <- k1 - ref_x; k2c <- k2 - ref_x; k3c <- k3 - ref_x
ns_basis <- ns(cc$fiber_c, knots = k2c, Boundary.knots = c(k1c, k3c))
cc$fiber_ns1 <- ns_basis[, 1]; cc$fiber_ns2 <- ns_basis[, 2]
des <- svydesign(ids=~SDMVPSU, strata=~SDMVSTRA, weights=~WTDRD1, nest=TRUE, data=cc)

f_ns <- as.formula(paste("hp ~ fiber_ns1 + fiber_ns2 +", cov_string))
m_ns <- svyglm(f_ns, design=des, family=quasibinomial())
wald_chi <- function(model, idx) {
  b <- coef(model)[idx]; V <- vcov(model)[idx, idx, drop=FALSE]
  pchisq(as.numeric(t(b) %*% solve(V) %*% b), df=length(idx), lower.tail=FALSE)
}
p_nonlin <- wald_chi(m_ns, 2); p_overall <- wald_chi(m_ns, 1:2)
cat(sprintf("Spline tests: P overall = %.4f, P non-linearity = %.4f\n", p_overall, p_nonlin))

x_lo <- wtd_q(cc$fiber_adj, cc$WTDRD1, 0.01); x_hi <- wtd_q(cc$fiber_adj, cc$WTDRD1, 0.99)
grid_x <- seq(x_lo, x_hi, length.out = 200)
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
  nd$fiber_ns1 <- ns_mat[, 1]; nd$fiber_ns2 <- ns_mat[, 2]
  for (v in names(ref_profile)) nd[[v]] <- ref_profile[[v]]
  X <- model.matrix(~ ., data = nd[, c("fiber_ns1","fiber_ns2", names(ref_profile))])
  cf_names <- names(coef(m_ns))
  X_full <- matrix(0, nrow=nrow(X), ncol=length(cf_names), dimnames=list(NULL, cf_names))
  common <- intersect(colnames(X), cf_names)
  X_full[, common] <- X[, common]
  X_full[, "(Intercept)"] <- 1
  X_full
}
Xg <- make_X(grid_x); Xr <- make_X(rep(ref_x, 1))
D <- sweep(Xg, 2, as.numeric(Xr), "-")
V <- vcov(m_ns)
lp <- as.numeric((Xg - matrix(as.numeric(Xr), nrow=nrow(Xg), ncol=ncol(Xg), byrow=TRUE)) %*% coef(m_ns))
se <- sqrt(pmax(rowSums((D %*% V) * D), 0))
pred <- data.frame(x = grid_x, OR = exp(lp), lo = exp(lp - 1.96*se), hi = exp(lp + 1.96*se))

# ---- number at risk table across exposure range ----
brks <- c(-3, 2, 7, 12, 17, 22, 27, 32, 48)
cc$risk_grp <- cut(cc$fiber_adj, breaks = brks, include.lowest = TRUE, right = FALSE)
risk_tab <- as.data.frame(table(cc$risk_grp))
risk_tab$events <- as.numeric(tapply(cc$hp, cc$risk_grp, sum))
mid <- (brks[-length(brks)] + brks[-1]) / 2
mid[length(mid)] <- 37  # visual centre of the merged tail bin
risk_tab$mid <- mid
cat("Number at risk table:\n"); print(risk_tab)

p_rcs <- ggplot(pred) +
  geom_ribbon(aes(x=x, ymin=lo, ymax=hi), fill="#0073C2", alpha=0.2) +
  geom_line(aes(x=x, y=OR), color="#0073C2", linewidth=1.1) +
  geom_hline(yintercept=1, linetype="dashed", color="red", linewidth=0.7) +
  geom_vline(xintercept=ref_x, linetype="dashed", color="grey40", linewidth=0.6) +
  geom_vline(xintercept=c(k1,k2,k3), linetype="dotted", color="grey70", linewidth=0.5) +
  geom_rug(data=cc[cc$fiber_adj >= x_lo & cc$fiber_adj <= x_hi, ],
           aes(x=fiber_adj), inherit.aes=FALSE, alpha=0.15, sides="b") +
  annotate("text", x=ref_x, y=1.45, label="Reference = 10 g/day", hjust=-0.05, size=3.5) +
  annotate("text", x=x_hi, y=1.45, hjust=1, size=3.8,
           label=sprintf("P overall = %.3f\nP non-linearity = %.3f", p_overall, p_nonlin)) +
  coord_cartesian(ylim=c(0.4, 1.5)) +
  scale_x_continuous(breaks = c(-3, 2, 7, 12, 17, 22, 27, 32, 37),
                     labels = c("-3","2","7","12","17","22","27","32","\u226532")) +
  labs(x="Energy-Adjusted Dietary Fiber Intake (g/day)",
       y=expression("Adjusted OR for" ~ italic("H. pylori") ~ "Seropositivity")) +
  theme_classic(base_size=10) +
  theme(axis.title=element_text(size=10, face="bold"),
        axis.text=element_text(size=9, color="black"),
        plot.margin = margin(5, 8, 0, 5))

# risk table as a grob aligned to x axis
risk_df <- data.frame(x = mid,
                      Participants = sprintf("%d", risk_tab$Freq),
                      Seropositive = sprintf("%d", risk_tab$events))
risk_long <- data.frame(
  x = rep(mid, 2),
  row = factor(rep(c("No. of participants", "No. seropositive"), each = length(mid)),
               levels = c("No. seropositive", "No. of participants")),
  val = c(risk_df$Participants, risk_df$Seropositive))
p_risk <- ggplot(risk_long, aes(x = x, y = row, label = val)) +
  geom_text(size = 3.2) +
  scale_x_continuous(limits = c(min(brks), 42), expand = c(0.01, 0)) +
  labs(x = NULL, y = NULL) +
  theme_void(base_size = 9) +
  theme(axis.text.y = element_text(size = 8.5, hjust = 1, color = "black"),
        plot.margin = margin(0, 8, 2, 5))

p_comb <- arrangeGrob(p_rcs, p_risk, ncol = 1, heights = c(4, 0.8))
ggsave("AI_Figure2_RCS_v5.tif", p_comb, width = 7.2, height = 5.6, dpi = 600, device = "tiff", compression = "lzw", bg = "white")
ggsave("AI_Figure2_RCS_v5_300dpi.tif", p_comb, width = 7.2, height = 5.6, dpi = 300, device = "tiff", compression = "lzw", bg = "white")
cat("Figure 2 v5 saved (600 + 300 dpi)\n")
