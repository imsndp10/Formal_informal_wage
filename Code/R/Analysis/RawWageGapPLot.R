if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library(tidyverse)
library(ggplot2)
library(grid)
library(ggpubr)
library(readxl)

# Function to process data
process_data <- function(file_path, year_label) {
  data <- readRDS(file_path)
  
  data %>%
    mutate(log_wage = log(hourly_wage)) %>%
    filter(year == year_label) %>%
    group_by(formal_employment) %>%
    mutate(quantile = ntile(log_wage, 50) / 50) %>%
    mutate(mean_Wage = weighted.mean(log_wage, w = weight)) %>%
    ungroup() %>%
    group_by(formal_employment, quantile) %>%
    summarise(
      raw_wage = weighted.mean(log_wage, w = weight),
      mean_wage = median(mean_Wage)
    ) %>%
    mutate(year = as.character(year_label))
}

# Process data for 2008 and 2018
data_2008 <- process_data("../../../Data/Cleaned/Pooled/Pooled.RDS", 2008)
data_2018 <- process_data("../../../Data/Cleaned/Pooled/Pooled.RDS", 2018)

# Combine data
combined_data <- bind_rows(data_2008, data_2018)

# Calculate wage gaps
wdat <- combined_data %>%
  pivot_wider(names_from = formal_employment, values_from = c(raw_wage, mean_wage)) %>%
  mutate(
    wage_gap = raw_wage_1 - raw_wage_0,
    mean_gap = mean_wage_1 - mean_wage_0
  )

# Function to create plots
create_plot <- function(data) {
  ggplot(data) +
    geom_line(aes(x = quantile, y = raw_wage, color = factor(formal_employment)), linewidth = 0.4) +
    scale_color_manual(values = c(
      "0" = "#466CA6", 
      "1" = "#A41D1A"
    ),
    labels = c("0" = "Informal Employment", "1" = "Formal Employment")) +
    facet_grid(. ~ year) +
    labs(x = "Quantile", y = "Log Wage", color = "Employment Type") +
    theme_minimal(base_family = "serif") +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 9),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.4),
      panel.spacing = unit(1, "lines"),
      strip.background = element_rect(fill = "#D6CFC4"),
      strip.text.x = element_text(size = 9),
      plot.title = element_text(family = "serif", size = 9)
    ) +
    scale_y_continuous(breaks = seq(0, 6, 1), limits = c(0, 6)) +
    scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    guides(color = guide_legend(title = NULL))
}

# Create wage plot
wage_plot <- create_plot(combined_data)
wage_plot


# Function to create gap plot
create_gap_plot <- function(data) {
  ggplot(data) +
    geom_line(aes(x = quantile, y = wage_gap), color = "#d62728", linewidth = 0.3) +
    geom_hline(aes(yintercept = mean_gap), linetype = "dashed", color = "blue") +
    facet_grid(cols = vars(year)) +
    labs(x = "Quantile", y = "Wage Gap (Formal - Informal)") +
    theme(legend.position = "top", panel.grid.minor = element_blank(),
          text = element_text(family = "serif", size = 9),
          strip.background = element_rect(fill = "#D6CFC4"),
          strip.text.x = element_blank(),
          strip.placement = "outside",
          plot.title = element_text(family = "serif", size = 9)) +
    #scale_y_continuous(breaks = seq(-1, 6, 1), limits = c(-1, 6)) +
    #scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    theme_bw() +
    guides(color = guide_legend(title = NULL))
}

# Create gap plot
gap_plot <- create_gap_plot(wdat)

# Save the plot
ggsave(filename = "wage_plot.pdf", plot = wage_plot, device = "pdf", width = 14, height = 10, units = "cm", path = "../../../Final Paper/final_paper/images")

