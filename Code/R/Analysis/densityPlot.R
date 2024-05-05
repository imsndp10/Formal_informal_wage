if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library("tidyverse")
library("ggplot2")
library("gridExtra")
library("grid")
library("ggpubr")


dataFunc <- function(Year, industry = NULL, job = NULL){
  data <- readRDS("../../../Data/Cleaned/Pooled/Pooled.RDS") %>%
    group_by(psu, hhid, year) %>% 
    mutate(HH_formal = if_else(formal_employment == 1, sum(formal_employment) - 1,
                               sum(formal_employment))) %>% 
    ungroup() %>% 
    filter(year == Year)
  if(is.null(industry) & is.null(job)){
    Data <- data
  }else if(!is.null(industry) & !is.null(job)){
    Data <- data %>% 
      filter(job_sector%in%industry &
               class_5%in%job)
  }
  Data <- Data %>% 
    group_by(formal_employment) %>% 
    mutate(mean_schooling = mean(yrs_schooling)) %>% 
    ungroup()
  return(Data)
}


plotFun <- function(Year, industry= NULL, job=NULL, Title = element_blank()){
  ddat <- dataFunc(Year, industry, job)
  plot <- ggplot(data = ddat)+
    geom_density(aes(x = yrs_schooling, fill = factor(formal_employment)),stat = "density", 
                 position = "identity", alpha = 0.3, linewidth = 0.2 )+
    geom_vline(aes(xintercept = mean_schooling, color = factor(formal_employment)), linetype = "longdash",
               show.legend = FALSE, linewidth = 0.2 )+
    theme_bw()+
    labs(title = Title,
         x = element_blank(),
         y = element_blank(),
         fill = "Employment")+
    theme(legend.position = "top",panel.grid.minor = element_blank(),
          text = element_text(family = "serif",size = 9),
          plot.title = element_text(family = "serif",size = 9))+
    scale_color_brewer(type = "seq",  palette ="Set1" )+
    #scale_fill_brewer(type = "seq", palette = "Set1")+
    scale_fill_discrete(labels = c("Informal", "Formal"))+
    scale_y_continuous(breaks = 0.1)+
    ylim(0,0.35)
  grob <- grid::grobTree(grid::textGrob(paste0(Year), x=0.9,  y=0.9,
                            gp=grid::gpar(fontsize=8, fontfamily="serif")))
  plot <- plot + annotation_custom(grob)
  return(plot)
}

Plot1 <- plotFun(Year = 2008, industry = c("Market_services", "Non_Market_services",
                                        "Arts_entertain"),
              job = c("Managers"), Title = "Managers")
Plot2 <- plotFun(Year = 2008, industry = c("Market_services", "Non_Market_services",
                                           "Arts_entertain"),
                 job = c("Clerical_sales"), Title = "Clerical")
Plot3 <- plotFun(Year = 2008, industry = c("Market_services", "Non_Market_services",
                                           "Arts_entertain"),
                 job = c("Elementary_occupations", "Plant_operator",
                         "Agri_trade"), Title = "Elementary")
Plot4 <- plotFun(Year = 2018, industry = c("Market_services", "Non_Market_services",
                                           "Arts_entertain"),
                 job = c("Managers"))
Plot5 <- plotFun(Year = 2018, industry = c("Market_services", "Non_Market_services",
                                           "Arts_entertain"),
                 job = c("Clerical_sales"))
Plot6 <- plotFun(Year = 2018, industry = c("Market_services", "Non_Market_services",
                                           "Arts_entertain"),
                 job = c("Elementary_occupations", "Plant_operator",
                         "Agri_trade"))
a <- ggpubr::ggarrange(Plot3, Plot2, Plot1, Plot6, Plot5, Plot4, ncol = 3, nrow = 2,
                  common.legend = TRUE, legend = "top")
b <- ggpubr::annotate_figure(a, left = grid::textGrob("Density", rot = 90,
                                                 gp = grid::gpar(fontfamily = "serif", fontsize = 10)), 
                        bottom = grid::textGrob("Years of schooling",
                                                gp = grid::gpar(fontfamily = "serif", fontsize = 10)))

ggsave(filename = "yrs_schooling.pdf",plot = b,device = "pdf",width = 14,height = 12,
       units = c("cm"),path = "../../../Final Paper/final_paper/images")

