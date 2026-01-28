# Clear workspace and set working directory
if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library(dplyr)
library(tidyr)
library(readxl)
library(kableExtra)

# Function to read and process data for a specific job type and year
read_and_filter_data <- function(file_path, sheet_name = "xyz", required_quantiles) {
  data <- read_excel(file_path, sheet = sheet_name)
  filtered_data <- data %>% filter(qnt %in% required_quantiles)
  return(filtered_data)
}

# Function to round numerical values
rounded <- function(x) { round(x, 3) }
charac <- function(x){as.character(x)}

create_table <- function(data, year) {
  data <- data %>%
    filter(group %in% c("tdifference", "t_unexplained", "t_explained")) %>%
    mutate(group = case_when(
      group == "tdifference" ~ "Total Effect",
      group == "t_unexplained" ~ "Unexplained",
      group == "t_explained" ~ "Explained"
    )) %>%
    mutate_at(c("coef", "se", "lb", "ub"), rounded) %>%
    mutate_at(c("coef", "se", "lb", "ub"), charac) %>%
    mutate(est = paste0(coef, " ", "(", se, ")")) %>%
    mutate(bound = paste0(lb,";"," ",ub)) %>% 
    select(qnt, group, est, bound) %>%
    pivot_wider(names_from = group, values_from = c(est, bound), names_glue = "{group}_{.value}") %>%
    arrange(match(qnt, c(1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90))) %>%
    mutate(
      Year = if_else(qnt == 1, as.character(year), "")
    ) %>%
    select(Year, qnt, starts_with("Total Effect_est"), starts_with("Total Effect_bound"), 
           starts_with("Unexplained_est"), starts_with("Unexplained_bound"), 
           starts_with("Explained_est"), starts_with("Explained_bound"))
  
  return(data)
}


table.export <- function(final_table, job_title){
  final_table %>%
    kable(col.names = c("Year", "Tau",
                        "Estimate", "Bounds",
                        "Estimate", "Bounds",
                        "Estimate", "Bounds"),
          align = rep("l", 8),
          caption = paste0("Decomposition Table for ", job_title),
          booktabs = TRUE, 
          format = "latex", 
          linesep = "") %>%
    add_header_above(c(" " = 2, "Total Effect" = 2, "Unexplained" = 2, "Explained" = 2)) %>%
    kable_styling(latex_options = c("scale_down")) %>%
    landscape() %>%
    save_kable(file = paste0("../../../Output/Tables/Decomposition_", job_title, ".tex"))
}

# Function to process and export data for all job types
process_and_export_all <- function() {
  job_titles <- c("elem", "clerical", "managers", "overall")
  years <- c("08", "18")
  required_quantiles <- c(1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 99)
  
  for (job in job_titles) {
    for (year in years) {
      file_path <- paste0("../../../data/Cleaned/Pooled/", job, "_", year, ".xlsx")
      data_filtered <- read_and_filter_data(file_path, "xyz", required_quantiles)
      data_table <- create_table(data_filtered, as.numeric(paste0("20", year)))
      
      if (!exists("final_table")) {
        final_table <- data_table
      } else {
        final_table <- bind_rows(final_table, data_table)
      }
    }
    
    # Export the final table for the current job type
    table.export(final_table, job)
    
    # Clear the final_table variable for the next iteration
    rm(final_table)
  }
}

# Run the function to process and export all job types
process_and_export_all()

# ,
# Total_Effect_Coef = paste0(coef_Total Effect, " (", se_Total Effect, ")"),
# Total_Effect_Bounds = paste0(lb_Total Effect, "; ", ub_Total Effect),
# Unexplained_Coef = paste0(coef_Unexplained, " (", se_Unexplained, ")"),
# Unexplained_Bounds = paste0(lb_Unexplained, "; ", ub_Unexplained),
# Explained_Coef = paste0(coef_Explained, " (", se_Explained, ")"),
# Explained_Bounds = paste0(lb_Explained, "; ", ub_Explained)
