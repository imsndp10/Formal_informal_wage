qdat <- readxl::read_xlsx("/Users/sandeepsharma/Desktop/Research/Data Analysis/Formal Informal wage/Formal_informal_wage/data/Cleaned/Pooled/2008_rif_decompose.xlsx",
                        sheet = "Sheet3")

library(ggplot2)

qdat <- qdat %>%
  mutate(qnt = quantile/100)

# Filter for specific columns and rename them
filtered_qdat <- qdat %>%
  filter(ln_wage %in% c("group_1", "group_2", "group_c", "total difference", "explained", "unexplained")) %>%
  rename(tau = qnt, coefficient = coef1, lower = lb1, upper = ub1)

# Plot the data with enhancements including distinct linetypes for the ribbons and a single legend
plot <- ggplot(data = filtered_qdat) +
  geom_line(aes(x = tau, y = coefficient, color = ln_wage), linewidth = 0.6) +
  scale_color_manual(values = c(
    "group_1" = "#466CA6", 
    "group_2" = "#A41D1A", 
    "group_c" = "#2ca02c",
    "total difference" = "#d62728",
    "explained" = "#9467bd",
    "unexplained" = "#8c564b"
  ),
  labels = c("group_1" = "Formal Employment", "group_2" = "Informal Employment", "group_c" = "Counterfactual group",
                "total difference" = "Total Difference", "explained" = "Explained",
                "unexplained" = "Unexplained")) +
  geom_ribbon(aes(x = tau, ymin = lower, ymax = upper, fill = ln_wage), linetype = 3,
              alpha = 0.3, show.legend = FALSE) +
  scale_fill_manual(values = c(
    "group_1" = "#466CA6", 
    "group_2" = "#A41D1A", 
    "group_c" = "#2ca02c",
    "total difference" = "#d62728",
    "explained" = "#9467bd",
    "unexplained" = "#8c564b"
  ),
  labels = c("group_1" = "Group 1", "group_2" = "Group 2", "group_c" = "Group C",
                "total difference" = "Total Difference", "explained" = "Explained",
                "unexplained" = "Unexplained")) +
  facet_grid() +
  labs(x = "Quantile", y = "Normalized Earnings") +
  scale_y_continuous(breaks = seq(0, 5, 1)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  guides(color = guide_legend(title = NULL)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "right",
        strip.background = element_rect(fill = "#D6CFC4"),
        legend.text = element_text(size = 8)) # Adjust legend text size if necessary

