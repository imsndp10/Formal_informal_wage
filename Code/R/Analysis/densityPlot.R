if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library("tidyverse")
library("ggplot2")
library("gridExtra")
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


plotFun <- function(Year, industry= NULL, job=NULL){
  ddat <- dataFunc(Year, industry, job)
  plot <- ggplot(data = ddat)+
    geom_density(aes(x = yrs_schooling, fill = factor(formal_employment)),stat = "density", 
                 position = "identity", alpha = 0.3 )+
    geom_vline(aes(xintercept = mean_schooling, color = factor(formal_employment)), linetype = "longdash",
               show.legend = FALSE )+
    theme_bw()+
    theme(legend.position = "top",panel.grid.minor = element_blank())+
    labs(x = "Years of schooling",
         y = "Density",
         fill = "Employment")+
    scale_color_brewer(type = "seq",  palette ="Set1" )+
    #scale_fill_brewer(type = "seq", palette = "Set1")+
    scale_fill_discrete(labels = c("Informal", "Formal"))  
  return(plot)
}

Plot1 <- plotFun(Year = 2008, industry = c("Market_services", "Non_Market_services",
                                        "Arts_entertain"),
              job = c("Managers"))
Plot2 <- plotFun(Year = 2008, industry = c("Market_services", "Non_Market_services",
                                           "Arts_entertain"),
                 job = c("Clerical_sales"))
Plot3 <- plotFun(Year = 2008, industry = c("Market_services", "Non_Market_services",
                                           "Arts_entertain"),
                 job = c("Elementary_occupations", "Plant_operator",
                         "Agri_trade"))
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


