library(dplyr)
library(MatchIt)
library(cobalt)
library(ggplot2)
library("tidyverse")

# Function to generate data for a specific year
generateYearlyData <- function(data, Year) {
  data %>% filter(year == Year)
}

# Function to perform matching and return relevant outputs
performMatching <- function(data, formula) {
  matchit_obj <- matchit(as.formula(formula),
                         data = data,
                         method = "full",
                         exact = "year",
                         distance = "mahalanobis")
  
  matchedData <- match.data(matchit_obj)
  match_summary <- summary(matchit_obj)
  
  balance_plot <- plot(matchit_obj, type = "density", interactive = FALSE, 
                       which.xs = as.formula(formula)[-2]) # Exclude response variable
  
  balance_table <- bal.tab(matchit_obj, weights = "att.weights")
  
  set.cobalt.options(binary = "std")
  love_plot <- love.plot(matchit_obj,
                         stat = c("m"), 
                         stars = "std",
                         thresholds = c(m = 0.05),
                         sample.names = c("Unmatched", "Matched"),
                         position = "bottom",
                         var.order = "alphabetical",
                         drop.distance = TRUE,
                         title = "Balance between Formal and Informal") +
    theme(panel.grid.major = element_blank(),
          axis.text.x = element_text(angle = 90, size = 12, vjust = 0.1),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.text = element_text(size = 10),
          legend.position = "bottom",
          strip.text.x = element_text(color = "black", size = 10),
          strip.text.y = element_text(color = "black", size = 10),
          strip.background = element_rect(fill = "#EEEEEE"),
          panel.spacing = unit(0.15, "lines"),
          panel.border = element_rect(colour = "black", fill = NA),
          panel.background = element_rect(fill = 'white'),
          panel.grid = element_line(colour = "#e1e5ea"))
  
  list(matched_data = matchedData, 
       summary = match_summary, 
       balance_plot = balance_plot, 
       balance_table = balance_table, 
       love_plot = love_plot)
}

# Main function to handle the entire process for two years
matchFormalEmployment <- function(data, year1, year2, formula) {
  results <- lapply(list(year1, year2), function(year) {
    yearly_data <- generateYearlyData(data, year)
    performMatching(yearly_data, formula)
  })
  
  names(results) <- c(paste0("year", year1, "_results"), paste0("year", year2, "_results"))
  results
}

# Example usage
# Assuming your dataset is loaded into 'sdat'
sdat <- readRDS("../../../Data/Cleaned/Pooled/Pooled.RDS")

formula <- "formal_employment ~ age + I(age^2) + class_5_Elementary_occupations + class_5_Plant_operator + class_5_Agri_trade +
class_5_Clerical_sales + class_5_Managers + job_sector_Mining_utility + job_sector_Construction + job_sector_Manufacturing +
job_sector_Market_services + job_sector_Non_Market_services + job_sector_Arts_entertain"

# Run the matching for two specific years
matched_results <- matchFormalEmployment(sdat, 2008, 2018, formula)

# Access the matched data, summaries, and plots for each year
matched_data_2008 <- matched_results$year2008_results$matched_data
summary_2008 <- matched_results$year2008_results$summary
balance_plot_2008 <- matched_results$year2008_results$balance_plot
balance_table_2008 <- matched_results$year2008_results$balance_table
love_plot_2008 <- matched_results$year2008_results$love_plot

matched_data_2018 <- matched_results$year2018_results$matched_data
summary_2018 <- matched_results$year2018_results$summary
balance_plot_2018 <- matched_results$year2018_results$balance_plot
balance_table_2018 <- matched_results$year2018_results$balance_table
love_plot_2018 <- matched_results$year2018_results$love_plot

#Save Matched data in RDS and DTA for 2008
write_rds(matched_data_2008, file = "../../../Data/Cleaned/Pooled/Matched08.RDS", compress = "gz")

# Save the object in DTA format
#The object is not saved for stata files for both 08 and 18
haven::write_dta(matched_data_2008, "../../../Data/Cleaned/Pooled/Matched08.dta")

#Save Matched data in RDS and DTA for 2018
write_rds(matched_data_2018, file = "../../../Data/Cleaned/Pooled/Matched18.RDS", compress = "gz")

haven::write_dta(matched_data_2018, "../../../Data/Cleaned/Pooled/Matched18.dta")