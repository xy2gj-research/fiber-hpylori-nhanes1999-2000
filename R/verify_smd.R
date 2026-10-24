# Compute weighted SMDs for Table 1 (Yang-Dalton multivariate for multi-category)
suppressMessages(library(survey))
setwd("D:/space_R/HP&dietary fiber")
df <- read.csv("NHANES_Final_Analytic_Data_V2.csv")
df$alcohol_group <- ifelse(is.na(df$DRXTALCO), NA, ifelse(df$DRXTALCO > 0, "Drinker", "Non-Drinker"))
df$Hyperlipidemia[is.na(df$Hyperlipidemia)] <- "Unknown"
df$Physical_Activity[is.na(df$Physical_Activity)] <- "Unknown"
df$hp <- as.numeric(df$hp_status)
options(survey.lonely.psu = "adjust")
need <- c("hp_status","WTDRD1","SDMVPSU","SDMVSTRA","alcohol_group","RIDAGEYR","gender",
          "Race_Label","Education_Label","pir","smoking","bmi","energy","Diabetes",
          "Hypertension","Hyperlipidemia","Physical_Activity","fiber_adj","fiber_adj_quartile")
w <- df[complete.cases(df[, need]), ]
for (v in c("gender","Race_Label","Education_Label","smoking","alcohol_group","Diabetes",
            "Hypertension","Hyperlipidemia","Physical_Activity","fiber_adj_quartile"))
  w[[v]] <- factor(w[[v]])
des <- svydesign(ids=~SDMVPSU, strata=~SDMVSTRA, weights=~WTDRD1, nest=TRUE, data=w)

# continuous SMD
smd_cont <- function(v) {
  m0 <- svymean(as.formula(paste("~", v)), subset(des, hp==0))
  m1 <- svymean(as.formula(paste("~", v)), subset(des, hp==1))
  s0 <- sqrt(svyvar(as.formula(paste("~", v)), subset(des, hp==0)))[1]
  s1 <- sqrt(svyvar(as.formula(paste("~", v)), subset(des, hp==1)))[1]
  abs(coef(m1)-coef(m0)) / sqrt((s0^2+s1^2)/2)
}
# categorical SMD (Yang-Dalton on dummies)
smd_cat <- function(v) {
  X <- model.matrix(as.formula(paste("~", v)), data=w)[, -1, drop=FALSE]
  p0 <- colSums(X[w$hp==0, , drop=FALSE] * w$WTDRD1[w$hp==0]) / sum(w$WTDRD1[w$hp==0])
  p1 <- colSums(X[w$hp==1, , drop=FALSE] * w$WTDRD1[w$hp==1]) / sum(w$WTDRD1[w$hp==1])
  p  <- colSums(X * w$WTDRD1) / sum(w$WTDRD1)
  if (ncol(X) == 1) return(abs(p1-p0) / sqrt(p*(1-p)))
  V <- diag(p*(1-p)) ; for (i in 1:ncol(X)) for (j in 1:ncol(X)) if (i!=j) V[i,j] <- -p[i]*p[j]
  sqrt(as.numeric(t(p1-p0) %*% solve(V) %*% (p1-p0)))
}
cat("--- continuous ---\n")
for (v in c("RIDAGEYR","pir","bmi","energy","fiber_adj")) cat(sprintf("%-10s %.3f\n", v, smd_cont(v)))
cat("--- categorical ---\n")
for (v in c("gender","Race_Label","Education_Label","smoking","alcohol_group","Diabetes",
            "Hypertension","Hyperlipidemia","Physical_Activity","fiber_adj_quartile"))
  cat(sprintf("%-20s %.3f\n", v, smd_cat(v)))
