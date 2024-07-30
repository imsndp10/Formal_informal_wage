if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library(haven)
library(dplyr)
library(gtsummary)
library(kableExtra)

# Read the data
sdat <- readRDS("../../../Data/Cleaned/Pooled/Pooled.RDS")

# Filter and mutate the data for Clerical occupations
clerical_sdat <- sdat %>%
  filter(class_5 %in% c("Plant_operator", "Agri_trade", "Clerical_sales")) %>%
  mutate(class_5 = "Clerical",
         combination = interaction(year, formal_employment))

# Filter the data for Elementary occupations
elem_sdat <- sdat %>%
  filter(class_5 == "Elementary_occupations") %>%
  mutate(combination = interaction(year, formal_employment))

# Filter the data for Managers
manager_sdat <- sdat %>%
  filter(class_5 == "Managers") %>%
  mutate(combination = interaction(year, formal_employment))

# Function to create a summary table
create_summary_table <- function(data, group_label) {
  data %>% 
    select(hh_size, age, experience, education, urban, married, female, 
           hourly_wage, child_12, migrated_fr_job, tot_chores_hrs,
           job_sector, combination) %>% 
    tbl_summary(by = combination,
                label = list(
                  hh_size ~ "Household Size",
                  age ~ "Age",
                  experience ~ "Experience",
                  education ~ "Education",
                  urban ~ "Urban",
                  married ~ "Married",
                  female ~ "Female",
                  hourly_wage ~ "Hourly Wage",
                  child_12 ~ "Children",
                  migrated_fr_job ~ "Migrated For Job",
                  tot_chores_hrs ~ "Total Chore Hours",
                  job_sector ~ "Job Sector",
                  combination ~ "Combination")) %>%
    modify_caption(paste(group_label, "Group Characteristics")) %>%
    modify_header(label ~ "Variables", all_stat_cols() ~ "{level}") %>%
    modify_spanning_header(c("stat_1", "stat_2") ~ "Informal Employment",
                           c("stat_3", "stat_4") ~ "Formal Employment")
}

# Create summary tables
desc_clerical <- create_summary_table(clerical_sdat, "Clerical")
desc_elem <- create_summary_table(elem_sdat, "Elementary")
desc_manager <- create_summary_table(manager_sdat, "Managers")

# Convert the summary tables to kable objects
kable_clerical <- as_kable(desc_clerical, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("scale_down"), full_width = F) %>%
  add_header_above(c(" " = 1, "Informal Employment" = 2, "Formal Employment" = 2)) %>%
  column_spec(1, bold = TRUE)

kable_elem <- as_kable(desc_elem, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("scale_down"), full_width = F) %>%
  add_header_above(c(" " = 1, "Informal Employment" = 2, "Formal Employment" = 2)) %>%
  column_spec(1, bold = TRUE)

kable_manager <- as_kable(desc_manager, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("scale_down"), full_width = F) %>%
  add_header_above(c(" " = 1, "Informal Employment" = 2, "Formal Employment" = 2)) %>%
  column_spec(1, bold = TRUE)

# Combine the kable objects into a single LaTeX table
combined_kable <- paste0(
  kable_elem, "\\newpage\n",
  kable_clerical, "\\newpage\n",
  kable_manager
)

# Save the combined table as a LaTeX file
writeLines(combined_kable, "../../../Output/Tables/combined_descriptive.tex")
