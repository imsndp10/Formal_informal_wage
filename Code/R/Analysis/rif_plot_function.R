if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 


library(ggplot2)
library(readxl)
library(dplyr)
library(grid)

# Define your function to process the data
process_data <- function(file_path, sheet_name, year_label, group_name) {
  read_xlsx(file_path, sheet = sheet_name) %>%
    filter(as.numeric(qnt) >= 3 & as.numeric(qnt) <= 97) %>%
    mutate(qnt = as.numeric(qnt) / 100) %>%
    mutate(year_label = year_label, group_name = group_name) %>%
    filter(group %in% c("group_1", "group_2", "group_c", "tdifference", "t_explained", "t_unexplained")) %>%
    rename(tau = qnt, coefficient = coef, lower = lb, upper = ub)
}

# Function to create plot
create_plot <- function(data) {
  ggplot(data) +
    geom_line(aes(x = tau, y = coefficient, color = group), linewidth = 0.3) +
    geom_ribbon(aes(x = tau, ymin = lower, ymax = upper, fill = group),
                linetype = 3, alpha = 0.1, show.legend = FALSE) +
    scale_color_manual(values = c(
      "group_1" = "#1f77b4", 
      "group_2" = "#ff7f0e", 
      "group_c" = "#2ca02c",
      "tdifference" = "#d62728",
      "t_explained" = "#9467bd",
      "t_unexplained" = "#8c564b"
    ),
    labels = c("group_1" = "Formal Employment", "group_2" = "Informal Employment",
               "group_c" = "Counterfactual Group", "tdifference" = "Total Difference",
               "t_explained" = "Explained", "t_unexplained" = "Unexplained")) +
    scale_fill_manual(values = c(
      "group_1" = "#1f77b4", 
      "group_2" = "#ff7f0e", 
      "group_c" = "#2ca02c",
      "tdifference" = "#d62728",
      "t_explained" = "#9467bd",
      "t_unexplained" = "#8c564b"
    )) +
    facet_grid(rows = vars(group_name), cols = vars(year_label)) +
    labs(x = element_blank(), y = element_blank(), fill = "Employment") +
    theme(legend.position = "top", panel.grid.minor = element_blank(),
          text = element_text(family = "serif", size = 9),
          strip.background = element_rect(fill = "#D6CFC4"),
          strip.text.x = element_blank(),
          strip.placement = "outside",
          plot.title = element_text(family = "serif", size = 9)) +
    scale_y_continuous(breaks = seq(-1, 6, 1), limits = c(-1, 6)) +
    scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    theme_bw() +
    guides(color = guide_legend(title = NULL))
}

# Process your data
a <- process_data("../../../data/Cleaned/Pooled/elem_08.xlsx", "xyz", "2008", "Elementary")
b <- process_data("../../../data/Cleaned/Pooled/elem_18.xlsx", "xyz", "2018", "Elementary")
c <- process_data("../../../data/Cleaned/Pooled/clerical_08.xlsx", "xyz", "2008", "Clerical")
d <- process_data("../../../data/Cleaned/Pooled/clerical_18.xlsx", "xyz", "2018", "Clerical")
e <- process_data("../../../data/Cleaned/Pooled/managers_08.xlsx", "xyz", "2008", "Managers")
f <- process_data("../../../data/Cleaned/Pooled/managers_18.xlsx", "xyz", "2018", "Managers")
j <- process_data("../../../data/Cleaned/Pooled/overall_08.xlsx", "xyz", "2008", "Overall")
k <- process_data("../../../data/Cleaned/Pooled/overall_18.xlsx", "xyz", "2018", "Overall")


ab <- rbind(a, b, c, d, e, f, j, k)
# Create plots
g <- create_plot(ab)


# Arrange the plots
h <- ggpubr::ggarrange(g, common.legend = TRUE, legend = "top")

# Annotate the combined plot
i <- ggpubr::annotate_figure(h,
                             left = grid::textGrob("Normalized earnings", rot = 90, gp = grid::gpar(fontfamily = "serif", fontsize = 10)), 
                             bottom = grid::textGrob("Quantile", gp = grid::gpar(fontfamily = "serif", fontsize = 10)))


# Save the plot
ggsave(filename = "overall_rif_decompose.pdf", plot = i, device = "pdf", width = 14, height = 18, units = "cm", path = "../../../Final Paper/final_paper/images")

