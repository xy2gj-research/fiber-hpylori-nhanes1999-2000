# Recompute Table 1 descriptive columns with WTDRD1 on analytic sample (n=3,340)
suppressMessages({library(survey)})
options(survey.lonely.psu='adjust')
setwd("D:/space_R/HP&dietary fiber")
df <- read.csv("NHANES_Final_Analytic_Data_V2.csv")
df$alcohol_group <- ifelse(is.na(df$DRXTALCO), NA, ifelse(df$DRXTALCO > 0, "Drinker", "Non-Drinker"))
df$Hyperlipidemia[is.na(df$Hyperlipidemia)] <- "Unknown"
df$Physical_Activity[is.na(df$Physical_Activity)] <- "Unknown"
df$hp <- as.numeric(df$hp_status)
a <- df[!is.na(df$hp) & !is.na(df$WTDRD1) & df$WTDRD1 > 0, ]
cat("n:", nrow(a), " neg:", sum(a$hp==0), " pos:", sum(a$hp==1), "\n")
des <- svydesign(ids=~SDMVPSU, strata=~SDMVSTRA, weights=~WTDRD1, nest=TRUE, data=a)
des0 <- subset(des, hp==0); des1 <- subset(des, hp==1)

fmt_med <- function(v, d) {
  q <- svyquantile(as.formula(paste0("~",v)), d, c(.25,.5,.75), na.rm=TRUE)
  sprintf("%.2f [%.2f, %.2f]", coef(q)[2], coef(q)[1], coef(q)[3])
}
fmt_mean <- function(v, d) {
  m <- svymean(as.formula(paste0("~",v)), d, na.rm=TRUE)
  v2 <- svyvar(as.formula(paste0("~",v)), d, na.rm=TRUE)
  sprintf("%.2f ± %.2f", coef(m), sqrt(as.numeric(v2)))
}
pct <- function(var, lev, d) {
  m <- svymean(as.formula(paste0("~I(", var, "=='", lev, "')")), d, na.rm=TRUE)
  sprintf("%.1f", 100*coef(m)[2])
}

cat("\n-- Age --\nneg:", fmt_mean("RIDAGEYR", des0), " pos:", fmt_mean("RIDAGEYR", des1), "\n")
cat("\n-- Gender --\nMale neg:", pct("gender","Male",des0), " pos:", pct("gender","Male",des1),
    "\nFemale neg:", pct("gender","Female",des0), " pos:", pct("gender","Female",des1), "\n")
cat("gender levels:", paste(unique(a$gender), collapse=","), "\n")
cat("\n-- Race --\n")
for (L in c("Mexican American","Other Hispanic","Non-Hispanic White","Non-Hispanic Black","Other Race"))
  cat(L, ": neg", pct("Race_Label",L,des0), " pos", pct("Race_Label",L,des1), "\n")
cat("race levels:", paste(unique(a$Race_Label), collapse=" | "), "\n")
cat("\n-- Education --\n")
cat("edu levels:", paste(unique(a$Education_Label), collapse=" | "), "\n")
for (L in unique(a$Education_Label))
  cat(L, ": neg", pct("Education_Label",L,des0), " pos", pct("Education_Label",L,des1), "\n")
cat("\n-- PIR --\nneg:", fmt_med("pir", des0), " pos:", fmt_med("pir", des1), "\n")
cat("-- BMI --\nneg:", fmt_med("bmi", des0), " pos:", fmt_med("bmi", des1), "\n")
cat("\n-- Smoking --\n")
cat("smoking values:", paste(unique(a$smoking), collapse=","), "\n")
cat("Non-smoker neg:", pct("smoking","0",des0), " pos:", pct("smoking","0",des1),
    "\nSmoker neg:", pct("smoking","1",des0), " pos:", pct("smoking","1",des1), "\n")
cat("\n-- Alcohol --\nDrinker neg:", pct("alcohol_group","Drinker",des0), " pos:", pct("alcohol_group","Drinker",des1),
    "\nNon-drinker neg:", pct("alcohol_group","Non-Drinker",des0), " pos:", pct("alcohol_group","Non-Drinker",des1), "\n")
cat("\n-- Energy --\nneg:", fmt_med("energy", des0), " pos:", fmt_med("energy", des1), "\n")
cat("-- Fiber adj --\nneg:", fmt_med("fiber_adj", des0), " pos:", fmt_med("fiber_adj", des1), "\n")
cat("\n-- Fiber quartiles --\n")
for (L in c("Q1","Q2","Q3","Q4"))
  cat(L, ": neg", pct("fiber_adj_quartile",L,des0), " pos", pct("fiber_adj_quartile",L,des1), "\n")
cat("\n-- Diabetes --\nNo neg:", pct("Diabetes","No",des0), " pos:", pct("Diabetes","No",des1),
    "\nYes neg:", pct("Diabetes","Yes",des0), " pos:", pct("Diabetes","Yes",des1), "\n")
cat("diabetes levels:", paste(unique(a$Diabetes), collapse=" | "), "\n")
cat("\n-- Hypertension --\nNo neg:", pct("Hypertension","No",des0), " pos:", pct("Hypertension","No",des1),
    "\nYes neg:", pct("Hypertension","Yes",des0), " pos:", pct("Hypertension","Yes",des1), "\n")
cat("\n-- Hyperlipidemia --\n")
for (L in c("No","Yes","Unknown"))
  cat(L, ": neg", pct("Hyperlipidemia",L,des0), " pos", pct("Hyperlipidemia",L,des1), "\n")
cat("\n-- Physical activity --\n")
for (L in c("Active","Inactive","Unknown"))
  cat(L, ": neg", pct("Physical_Activity",L,des0), " pos", pct("Physical_Activity",L,des1), "\n")
