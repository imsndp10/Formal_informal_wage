if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library(ggplot2)
library(dplyr)
library(readxl)

# Import the data
file_path <- "../../../Data/Cleaned/Pooled/rif_regression_overall.xlsx"
data <- read_excel(file_path)

# Create a vector of renamed variables
variable_names <- c("Experience", "Experience square", "Below primary", "Primary", "Tenth grade", "Secondary",
                    "Bachelor", "Masters and above", "Household size", "Female", "Married", "Child (< 12 years)", 
                    "Vocational training", "Migrated for job", "Household chores", "Urban", "Overtime (>40 hrs)", 
                    "Mining utility", "Construction", "Manufacturing", "Market services", "Non Market services", "Constant")

# Ensure the 'Variable' column is correctly renamed
data$Variable <- factor(data$Variable, levels = unique(data$Variable), labels = variable_names)

# Mutate Employment column to label 0 as "Informal" and 1 as "Formal"
data <- data %>%
  mutate(Employment = ifelse(Employment == 0, "Informal", "Formal"))

# Function to plot regression coefficients for all variables in a given year
plot_all_variables <- function(data, year) {
  
  # Ensure 'Quantile' is numeric
  data$Quantile <- as.numeric(as.character(data$Quantile))
  
  # Filter the data for the specified year
  plot_data <- data %>%
    filter(Year == year)
  
  # Create the plot with facet wrap for each variable
  ggplot(plot_data, aes(x = Quantile, y = Coef, color = Employment, group = Employment, linetype = Employment)) +
    geom_line(size = 0.6) +                       # Thicker lines for elegance
    geom_point(size = 1) +                       # Points at quantiles
    labs(x = "Quantiles", y = "Coefficient") +
    scale_x_continuous(breaks = c(20, 40, 60, 80, 100), limits = c(10, 100)) +  # Set x-axis limits and breaks
    facet_wrap(~ Variable, scales = "free_y", ncol = 3) +  # Fixed y-axis scales for all facets and 3 columns per row
    theme_minimal() +                              # Use a minimal theme for elegance
    theme(
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),  # Title in bold
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.text = element_text(size = 8),
      legend.title = element_blank(),
      legend.position = "top",                      # Legend position for clarity
      strip.text = element_text(size = 8),         # Size of facet labels
      panel.grid.minor = element_blank(),           # Remove minor grid lines
      panel.grid.major = element_line(size = 0.3, color = "grey80") # Major grid lines for clarity
    ) +
    scale_color_manual(values = c("Formal" = "#466CA6", "Informal" = "#A41D1A")) +  # Custom colors
    scale_linetype_manual(values = c("Formal" = "solid", "Informal" = "dashed"))   # Different line types
}

# Example usage: Plot coefficients for all variables in 2008
a <- plot_all_variables(data, 2008)
b <- plot_all_variables(data, 2018)


ggsave(filename = "regression_coefficients_2008.pdf",plot = a ,device = "pdf", width = 17, height = 24, units = c("cm"), path = "../../../Final Paper/final_paper/images")

ggsave(filename = "regression_coefficients_2018.pdf",plot = b ,device = "pdf", width = 17, height = 24, units = c("cm"), path = "../../../Final Paper/final_paper/images")
