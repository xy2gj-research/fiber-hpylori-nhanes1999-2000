# Forest plot from AI_Table3_ForForest.csv (design-based subgroup results)
suppressMessages({library(forestplot); library(stringr); library(grid)})
setwd("D:/space_R/HP&dietary fiber")

df <- read.csv("AI_Table3_ForForest.csv", stringsAsFactors = FALSE,
               na.strings = c("", "NA"), colClasses = "character")
# label fixes
df$Subgroup <- gsub("^    0$", "    Non-Smoker", df$Subgroup)
df$Subgroup <- gsub("^    1$", "    Smoker", df$Subgroup)
df$P[df$P == "0.0000"] <- "< 0.001"
df$P_int[df$P_int == "0.0000"] <- "< 0.001"

tabletext <- cbind(
  c("Subgroup", df$Subgroup),
  c("OR (95% CI)", ifelse(is.na(df[[2]]), "", df[[2]])),
  c("P value", ifelse(is.na(df[[3]]), "", df[[3]])),
  c("P for interaction", ifelse(is.na(df[[4]]), "", df[[4]])))

mean_val <- c(NA); lower_val <- c(NA); upper_val <- c(NA); is_summary <- c(TRUE)
for (i in 1:nrow(df)) {
  or_str <- df[[2]][i]
  if (is.na(or_str) || or_str == "") {
    mean_val <- c(mean_val, NA); lower_val <- c(lower_val, NA)
    upper_val <- c(upper_val, NA); is_summary <- c(is_summary, TRUE)
  } else {
    nums <- as.numeric(unlist(str_extract_all(or_str, "[0-9]+\\.[0-9]+")))
    mean_val <- c(mean_val, nums[1]); lower_val <- c(lower_val, nums[2])
    upper_val <- c(upper_val, nums[3]); is_summary <- c(is_summary, FALSE)
  }
}

tiff("AI_Figure3_ForestPlot.tif", width = 12, height = 15, units = "in",
     res = 300, compression = "lzw")
forestplot(
  labeltext = tabletext, mean = mean_val, lower = lower_val, upper = upper_val,
  is.summary = is_summary, zero = 1, boxsize = 0.25,
  lineheight = unit(7.5, "mm"), colgap = unit(6, "mm"),
  mar = unit(c(1.5, 1, 1.5, 1), "cm"),
  lwd.zero = 1.5, lwd.ci = 1.5,
  xticks = c(0.4, 0.6, 0.8, 1.0, 1.5, 2.0),
  clip = c(0.3, 2.2),
  col = fpColors(box = "#00468B", line = "#00468B", zero = "gray50"),
  xlab = "Odds Ratio (95% CI) Per Quartile Increase",
  txt_gp = fpTxtGp(
    label = gpar(cex = 0.95, fontfamily = "sans"),
    ticks = gpar(cex = 0.9, fontfamily = "sans"),
    xlab  = gpar(cex = 1.1, fontfamily = "sans", fontface = "bold")),
  align = c("l", "c", "c", "c"), graph.pos = 2)
dev.off()
cat("saved AI_Figure3_ForestPlot.tif\n")
