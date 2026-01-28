if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 


library(haven)
library(dplyr)
library(gtsummary)
library(kableExtra)

# Read the RDS file and mutate the dataset
sdat <- readRDS("../../../Data/Cleaned/Pooled/Pooled.RDS") %>% 
  mutate(combination = interaction(year, formal_employment))


# Select the necessary columns and create a summary table
desc <- sdat %>% 
  select(hh_size, age, experience, education, urban, married, female, 
         hourly_wage, child_12, migrated_fr_job, tot_chores_hrs,
         job_sector, class_5, combination) %>% 
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
                job_sector ~ "Job sector",
                class_5 ~ "Occupation",
                combination ~ "Combination")) %>%
  modify_caption("Group Characteristics") %>%
  modify_header(label ~ "Variables", all_stat_cols() ~ "{level}") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "Informal Employment",
                         c("stat_3", "stat_4") ~ "Formal Employment")

# Convert the summary table to a kable object and save as LaTeX
kable_desc <- as_kable(desc, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("scale_down"), full_width = F) %>%
  add_header_above(c(" " = 1, "Informal Employment" = 2, "Formal Employment" = 2)) %>%
  column_spec(1, bold = TRUE)

# Remove unnecessary decimals and add spaces before and after variable groups
kable_desc <- gsub("2008.0", "2008", kable_desc)
kable_desc <- gsub("2018.0", "2018", kable_desc)
kable_desc <- gsub("2008.1", "2008", kable_desc)
kable_desc <- gsub("2018.1", "2018", kable_desc)

# Save the table as a LaTeX file
writeLines(kable_desc, "../../../Output/Tables/descriptive.tex")
