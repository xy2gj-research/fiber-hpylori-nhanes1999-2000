# Dietary fiber and H. pylori seropositivity (NHANES 1999-2000): analysis code

This package contains the complete R and Python code for the manuscript:
"Energy-adjusted dietary fiber intake and Helicobacter pylori seropositivity in US adults:
NHANES 1999-2000".

## Data
Publicly available from NHANES 1999-2000:
https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=1999
Files used: demographics (DEMO), dietary day-1 recall (DRXTOT/DR1TOT), laboratory surplus sera
H. pylori IgG (LAB11/LAB11HP), body measures (BMX), blood pressure questionnaire (BPQ),
diabetes questionnaire (DIQ), physical activity (PAQ), smoking (SMQ), alcohol use.
Raw XPT files are NOT redistributed here; download them from the CDC website and place them
in the R working directory.

## Environment
- R 4.5.2; packages: survey 4.4.8, mice, haven, dplyr, splines, ggplot2, gridExtra, MASS
- Python 3.14.2; packages: xgboost 3.2.0, shap 0.51.0, scikit-learn 1.8.0, pandas, numpy, matplotlib

## Reproduction pipeline
1. R/01_Process_From_XPT.R  -- merge XPT files, derive variables, build analytic dataset
2. R/01_Data_Cleaning.R     -- exclusions, residual energy adjustment of fiber
3. R/final_analysis.R       -- design-based (svyglm) Models 1-3, trend test, RCS + non-linearity test,
                               subgroup/interaction tests (Tables 2, S3; Figure 1 data)
4. R/recompute_table1.R     -- Table 1 descriptive statistics (WTDRD1-weighted percentages,
                               means +/- SD, medians [IQR]); definitive source of published Table 1
5. R/recompute_table1_smd.R -- survey-weighted standardized mean differences (Yang-Dalton method
                               for multi-category variables; Table 1 SMD column)
6. R/verify_table1.R        -- design-based baseline P values (Table 1; svyglm/svychisq/svyranktest,
                               complete-case n = 3,295)
7. R/verify_smd.R           -- earlier SMD verification script (superseded by step 5)
8. R/mi_incexc.R            -- included vs excluded comparison (Table S1) and multiple-imputation
                               sensitivity analysis (Table S2)
9. R/fig2_v5.R              -- Figure 1 (RCS curve with CI, knots, rug, number at risk)
10. R/forest_plot.R         -- Figure 2 (subgroup forest plot)
11. Python/verify_ml_ai.py   -- XGBoost training/test evaluation, bootstrap AUC CI, SHAP ranking
12. Python/audit_ml_final.py -- independent ML re-computation audit (AUC, CI, Brier, calibration,
                               logistic comparator, ablation, SHAP ranking)
13. Python/08_ablation_dietary_v5.py -- model performance with/without dietary variables
14. Python/07_regen_fig4_v5.py -- Figure 3 panels A/B (ROC + calibration)
15. Python/09_regen_fig3_composite_v5.py -- Figure 3 composite (panels A-D; final published layout
                               with abbreviated race/ethnicity labels)
16. Python/06_regen_fig456_v4.py -- SHAP summary plots and Figure S2 (SHAP dependence)
17. Python/05_regen_fig1_v5.py   -- Figure S1 (participant flowchart)

Random seed: 42 throughout. Survey design: WTDRD1 weights, SDMVSTRA strata, SDMVPSU clusters,
lonely.psu = "adjust", Taylor linearization (design df = 14).

## Methodological notes
- P values in the regression tables are normal-approximation P values computed from
  design-based standard errors (2 * pnorm(-|z|)); with design df = 14 these differ only
  negligibly from t-distribution P values.
- Fiber quartile cut-offs (10.27, 14.29, 19.90 g/day) were derived from the unweighted
  distribution in the analytic sample (n = 3,340); RCS knots are weighted 10th/50th/90th
  percentiles (Hmisc::wtd.quantile, normwt = TRUE).
- Table S1 uses MEC examination weights (WTMEC2YR) because excluded participants lack
  day-1 dietary weights; all primary analyses use WTDRD1.
- Scripts with numeric-prefix names not listed above (e.g., "Model 3.R", "RCS_p.R",
  "03_Generate_ForestPlot_Fig3_V2.R", "05_gen_fig3.py") are earlier development versions
  retained for provenance; the numbered pipeline above is authoritative.

## Contact
Jian Gong, jiangong001@csu.edu.cn
