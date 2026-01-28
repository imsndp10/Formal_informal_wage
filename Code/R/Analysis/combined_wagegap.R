library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# Read the dataset
adata <- readRDS("../../../Data/Cleaned/Pooled/Pooled.RDS")

# Define sector labels correctly before use
sector_labels <- c(
  "Elementary_occupations" = "Low-skilled",
  "Clerical_sales" = "Medium-skilled",
  "Managers" = "High-skilled"
)

sdata <- adata %>%
  mutate(sector = case_match(
    class_5,
    "Elementary_occupations" ~ "Low-skilled",
    "Clerical_sales" ~ "Medium-skilled",
    "Managers" ~ "High-skilled",
    .default = NA
  )) %>%
  filter(!is.na(sector))

n_q <- 50  # Number of quantiles

# Process the dataset for sectoral wage gap
wdat_sector <- sdata %>%
  filter(!is.na(hourly_wage), hourly_wage > 0) %>%
  mutate(log_wage = log(hourly_wage)) %>%
  group_by(sector, year, formal_employment) %>%
  mutate(quantile = ntile(log_wage, n_q) / n_q) %>%
  ungroup() %>%
  group_by(sector, year, formal_employment, quantile) %>%
  summarise(
    avg_log_wage = mean(log_wage, na.rm = TRUE), 
    raw_wage = weighted.mean(log_wage, w = weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = formal_employment, 
    values_from = c(avg_log_wage, raw_wage),
    names_glue = "{.value}_formal{formal_employment}"
  ) %>%
  mutate(
    wage_gap_avg = avg_log_wage_formal1 - avg_log_wage_formal0,
    wage_gap_raw = raw_wage_formal1 - raw_wage_formal0,
    category = sector
  )

# Process the dataset for overall wage gap
wdat_overall <- sdata %>%
  filter(!is.na(hourly_wage), hourly_wage > 0) %>%
  mutate(log_wage = log(hourly_wage)) %>%
  group_by(year, formal_employment) %>%
  mutate(quantile = ntile(log_wage, n_q) / n_q) %>%
  ungroup() %>%
  group_by(year, formal_employment, quantile) %>%
  summarise(
    avg_log_wage = mean(log_wage, na.rm = TRUE), 
    raw_wage = weighted.mean(log_wage, w = weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = formal_employment, 
    values_from = c(avg_log_wage, raw_wage),
    names_glue = "{.value}_formal{formal_employment}"
  ) %>%
  mutate(
    wage_gap_avg = avg_log_wage_formal1 - avg_log_wage_formal0,
    wage_gap_raw = raw_wage_formal1 - raw_wage_formal0,
    category = "Overall"
  )

# Combine overall and sector-specific data
wdat_combined <- bind_rows(wdat_overall, wdat_sector)

# Trim data: Exclude the lowest 5% and highest 5% quantiles
quantile_range <- quantile(wdat_combined$quantile, probs = c(0.05, 0.95))
wdat_trimmed <- wdat_combined %>%
  filter(quantile >= quantile_range[1] & quantile <= quantile_range[2])

# Ensure color mapping matches the actual sector names in the dataset
color_map <- c(
  "Overall" = "#1f78b4",
  "Low-skilled" = "#1b9e77",
  "Medium-skilled" = "#d95f02",
  "High-skilled" = "#7570b3"
)

# Compute mean wage gaps per category
category_means <- wdat_trimmed %>%
  group_by(category, year) %>%
  summarise(
    mean_wage_gap_avg = mean(wage_gap_avg, na.rm = TRUE),
    mean_wage_gap_raw = mean(wage_gap_raw, na.rm = TRUE),
    .groups = "drop"
  )

# Ensure the correct order of the 'category' factor
wdat_trimmed <- wdat_trimmed %>%
  mutate(category = factor(category, levels = c("Low-skilled", "Medium-skilled", "High-skilled", "Overall")))

# Re-create the facet grid plot with the correct category order
p_final <- ggplot(wdat_trimmed, aes(x = quantile, y = wage_gap_avg, color = category)) +
  geom_line(size = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(data = category_means, aes(yintercept = mean_wage_gap_avg, color = category), 
             linetype = "dotted", size = 1) +
  scale_color_manual(values = color_map, breaks = names(color_map)) +  
  labs(
    x = "Quantile",
    y = "Wage Gap (Formal - Informal)",
    color = "Skill Level"
  ) +
  theme_bw() +  
  theme(
    legend.position = "top",
    text = element_text(family = "serif", size = 13),  
    strip.background = element_rect(fill = "grey80", color = "black"),  
    strip.text = element_text(size = 14, face = "bold"),  
    plot.title = element_text(family = "serif", size = 16, face = "bold"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  
    panel.grid.minor = element_blank()
  ) +
  facet_grid(rows = vars(category), cols = vars(year), scales = "fixed")  # Keeping fixed y-axis scale

# Display final plot
p_final

ggsave(
  filename = "wage_gap_new.pdf",
  plot = p_final,  # Make sure 'p_final' is the correct plot object
  device = "pdf",
  width = 21,  # Width in cm for A4 page
  height = 29.7,  # Height in cm for A4 page
  units = "cm",
  path = "../../../Final Paper/final_paper/images"
)
