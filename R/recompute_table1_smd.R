suppressMessages(library(survey)); options(survey.lonely.psu='adjust')
setwd("D:/space_R/HP&dietary fiber")
df <- read.csv("NHANES_Final_Analytic_Data_V2.csv")
df$alcohol_group <- ifelse(is.na(df$DRXTALCO), NA, ifelse(df$DRXTALCO > 0, "Drinker", "Non-Drinker"))
df$Hyperlipidemia[is.na(df$Hyperlipidemia)] <- "Unknown"
df$Physical_Activity[is.na(df$Physical_Activity)] <- "Unknown"
df$hp <- as.numeric(df$hp_status)
a <- df[!is.na(df$hp) & !is.na(df$WTDRD1) & df$WTDRD1 > 0, ]
des <- svydesign(ids=~SDMVPSU, strata=~SDMVSTRA, weights=~WTDRD1, nest=TRUE, data=a)

wp <- function(var, lev, g) {
  d <- subset(des, hp==g)
  m <- svymean(as.formula(paste0("~I(", var, "=='", lev, "')")), d, na.rm=TRUE)
  as.numeric(coef(m)[2])
}
smd_cat <- function(var, levs) {
  p0 <- sapply(levs, wp, var=var, g=0)
  p1 <- sapply(levs, wp, var=var, g=1)
  S <- (diag(p0) - outer(p0,p0) + diag(p1) - outer(p1,p1)) / 2
  d <- p1 - p0
  cat(var, "SMD:", sprintf("%.3f", sqrt(as.numeric(t(d) %*% MASS::ginv(S) %*% d))), "\n")
}
smd_cont <- function(var) {
  m <- svyby(as.formula(paste0("~",var)), ~hp, des, svymean, na.rm=TRUE)
  v <- svyby(as.formula(paste0("~",var)), ~hp, des, svyvar, na.rm=TRUE)
  sp <- sqrt((as.numeric(coef(v)[1]) + as.numeric(coef(v)[2]))/2)
  cat(var, "SMD:", sprintf("%.3f", abs(as.numeric(coef(m)[2])-as.numeric(coef(m)[1]))/sp), "\n")
}
smd_cont("RIDAGEYR")
smd_cat("gender", c("Male","Female"))
smd_cat("Race_Label", c("Mexican American","Other Hispanic","Non-Hispanic White","Non-Hispanic Black","Other Race"))
smd_cat("Education_Label", c("< High School","High School Grad","> High School"))
smd_cont("pir")
smd_cont("bmi")
smd_cat("smoking", c("0","1"))
smd_cat("alcohol_group", c("Non-Drinker","Drinker"))
smd_cont("energy")
smd_cont("fiber_adj")
smd_cat("fiber_adj_quartile", c("Q1","Q2","Q3","Q4"))
smd_cat("Diabetes", c("No","Yes"))
smd_cat("Hypertension", c("No","Yes"))
smd_cat("Hyperlipidemia", c("No","Yes","Unknown"))
smd_cat("Physical_Activity", c("Active","Inactive","Unknown"))
