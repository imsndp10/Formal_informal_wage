if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library('ddecompose')
library(ggplot2)
library(dplyr)
library(tidyr)



data <- readRDS("../../../Data/Cleaned/Pooled/Pooled.RDS") %>%
    group_by(psu, hhid, year) %>% 
    mutate(HH_formal = if_else(formal_employment == 1, sum(formal_employment) - 1,
                               sum(formal_employment)))

data18 <- data %>%
  filter(year == 2018)

# #custom_aggegation <- list(
#   'Human Capital' = c(
#     "experience", "experience_sq",
#     "education_Illiterate",
#     "education_Below_primary",
#     "education_Primary",
#     "education_Tenth_grade",
#     "education_Secondary",
#     "education_Bachelor",
#     "education_Masters_above"
#   ),
#   "Household character" = c(
#     "hh_size", "caste_group_6_Khas",
#     "caste_group_6_Janajati", "caste_group_6_Adhibasi",
#     "caste_group_6_Madhesi", "caste_group_6_Dalit",
#     "caste_group_6_others",
#     "female","married", "child_12",
#     "urban", "voc_train", "migrated_fr_job",
#     "overtime_40"
#   ),
#   "Job character" = c(
#     "class_5_Elementary_occupations",
#     "class_5_Plant_operator",
#     "class_5_Agri_trade",
#     "class_5_Clerical_sales",
#     "class_5_Managers"
#   )
# )

model_rif <- log(hourly_wage) ~ experience + experience_sq +
  education_Below_primary + education_Primary +
  education_Tenth_grade +education_Secondary + education_Bachelor +
  education_Masters_above + caste_group_6_Janajati + caste_group_6_Adhibasi +
  caste_group_6_Madhesi + caste_group_6_Dalit + caste_group_6_Others +
  hh_size + female + married + child_12 + voc_train + migrated_fr_job +
  tot_chores_hrs + urban + overtime_40 + class_5_Plant_operator + class_5_Agri_trade +
  class_5_Clerical_sales + class_5_Managers| HH_formal + dep_ratio +
  education_Below_primary + education_Primary +
  education_Tenth_grade + education_Secondary + education_Bachelor +
  education_Masters_above + experience + experience_sq + caste_group_6_Janajati + caste_group_6_Adhibasi +
  caste_group_6_Madhesi + caste_group_6_Dalit + caste_group_6_Others

quantiles <- c(0.1, 0.3, 0.5, 0.7, 0.9)


quant_rif <- ob_decompose(model_rif, data = data18, group = formal_employment,
                          weights = weight,
                          reference_0 = FALSE,
                          reweighting = TRUE, reweighting_method = "logit",
                          rifreg_statistic = "quantiles",
                          rifreg_probs = c(0.9), bootstrap = TRUE, cores = 3)

