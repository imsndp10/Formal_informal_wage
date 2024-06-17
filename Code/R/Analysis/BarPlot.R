if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library("tidyverse")
library("ggplot2")
library("gridExtra")
library("grid")
library("ggpubr")


data <- readRDS("../../../Data/Cleaned/Pooled/Pooled.RDS")

data_ <- function(Year, Data){
  sdat <- Data %>% 
    filter(year == Year)
  return(sdat)
}
a <- data_(Year = 2018, Data = data)

conf_int <- function(table){
  a <-DescTools::MultinomCI(table, conf.level = 0.95, method = c("wald"))
  var <- rownames(a)
  rownames(a) <- NULL
  data <- as.data.frame(cbind(var, a))
  return(data)
}

###### Plot data prep ####
### Industry ####
plotdat <- function(Year, Data, Variable){
  pdat <- data_(Year, Data)
  fdat <- subset(pdat, formal_employment == 1)
  idat <- subset(pdat, formal_employment == 0)
  f_prop <- table(fdat[,Variable])
  i_prop <- table(idat[,Variable])
  formal <- as.data.frame(conf_int(f_prop) %>% mutate(formal = 1))
  informal <- as.data.frame(conf_int(i_prop) %>%  mutate(formal = 0))
  final <- rbind(formal,informal) %>% mutate(year = unique(pdat$year)) %>%  
    mutate(est = as.double(format(round(as.double(est)*100,digits=1),nsmall=1)),
           lwr.ci = as.double(format(round(as.double(lwr.ci)*100,digits=1),nsmall=1)),
           upr.ci = as.double(format(round(as.double(upr.ci)*100,digits=1),nsmall=1)))
  if(Variable == "class_5"){
    final <- final %>% 
      mutate(var = factor(var,
                          levels = c("Elementary_occupations",
                                     "Plant_operator",
                                     "Agri_trade",
                                     "Clerical_sales",
                                     "Managers")))
  }
  if(Variable == "job_sector"){
    final <- final %>% 
      mutate(var = factor(var,
                          levels = c("Mining_utility",
                                     "Construction",
                                     "Manufacturing",
                                     "Market_services",
                                     "Non_Market_services",
                                     "Arts_entertain")))
  }
  if(Variable == "class_11"){
    final <- final %>% 
      mutate(var = factor(var,
                          levels = c("Armed forces occupations",
                                     "Elementary_occupations",
                                     "Plant and machine operators, and assemblers",
                                     "Plant_operator",
                                     "Skilled agricultural, forestry and fishery workers",
                                     "Craft and related trades workers",
                                     "Service and sales workers",
                                     "Clerical support workers",
                                     "Technicians and associate professionals",
                                     "Professionals",
                                     "Managers")))
  }
  if(Variable == "gdp_sector"){
    final <- final %>% 
      mutate(var = factor(var,
                          levels = c("Mining_utility",
                                     "Construction",
                                     "Manufacturing",
                                     "Trade_repair",
                                     "Transport_storage",
                                     "Food_accomodation",
                                     "Info_communication",
                                     "Financial_insurance",
                                     "Real_estate",
                                     "Professional_scientific_technical",
                                     "Adminstrative_support_service",
                                     "Public_admin_defense",
                                     "Education",
                                     "Health_social_work",
                                     "Arts_entertain_other")))
  }
  
  return(final)
}
b <- plotdat(Year = 2008, Data = data, Variable = "gdp_sector")
bar_plotter <- function(Year, Data, Variable, Labels = FALSE){
  dat <- plotdat(Year,Data,Variable)
  bar_plot <- ggplot(dat, aes(x = var, 
                               y = est,
                               ymin = lwr.ci,
                               ymax = upr.ci,
                               fill = as.factor(formal)) ) + 
    geom_bar (position = position_dodge(), stat = "identity")+
    scale_fill_manual(values = c("#466CA6", "#A41D1A"),
                      labels = c("Informal", "Formal"))+
    geom_errorbar(position = position_dodge(width=0.9), colour="black")+ 
    labs(
      x = element_blank(),
      y = element_blank(),
      fill = "Employment") + 
    theme_bw()+
    theme(#plot.margin = unit(c(0.1,0.2,0.2,0.2), "cm"),
          panel.grid.major = element_blank(),
          legend.position = "top",
          legend.direction = "horizontal",
          # legend.margin = margin(0,0,0,0),
          # legend.box.margin = margin(1,0,-10,0),
          legend.text = element_text(size = 10),
          text = element_text(size=11, family = "serif"),
          axis.text.x = element_text(hjust = 1),
          axis.title.x=element_blank())+
    scale_y_continuous(breaks = 10)+
    ylim(0,50)+
          coord_flip()
  if(isFALSE(Labels)){
    bar_plot <- bar_plot + scale_x_discrete(labels = element_blank())+
      theme(axis.ticks.y = element_blank())
  }
  if(isTRUE(Labels) & Variable == "gdp_sector"){
    bar_plot <- bar_plot +
      scale_x_discrete(labels = c("Mining & utility", "Construction", "Manufacturing",
                                  "Trade & repair", "Transport & storage",
                                  "Food & accomodation", "Information & communication",
                                  "Finance & insurance", "Real estate", "Professional services",
                                  "Administrative & support", "Public administration",
                                  "Education", "Health & social work", "Arts, entertainment & other"))
  }
  if(isTRUE(Labels) & Variable == "class_11"){
    bar_plot <- bar_plot +
      scale_x_discrete(labels = c("Armed force", "Elementary", "Plant operator",
                                                       "Agricultural, forestry & \n fishery", "Crafts & trades",
                                                       "Service & sales", "Clerical support", "Technicians", "professionals",
                                                       "Managers"))
  }
  grob <- grid::grobTree(grid::textGrob(paste0(Year), x=0.9,  y=0.95,
                                        gp=grid::gpar(fontsize=8, fontfamily="serif")))
  bar_plot <- bar_plot + annotation_custom(grob)
return(bar_plot)
}


ind_08 <- bar_plotter(Year = 2008,Data = data,Variable = "gdp_sector", Labels = T)
ind_18 <- bar_plotter(Year = 2018,Data = data,Variable = "gdp_sector", Labels = F)

occu_08 <- bar_plotter(Year = 2008,Data = data,Variable = "class_11", Labels = T)

occu_18 <- bar_plotter(Year = 2018,Data = data,Variable = "class_11",Labels = F)

ind_final <- ggpubr::ggarrange(ind_08, ind_18, ncol = 2, nrow = 1,
                       common.legend = TRUE, legend = "top", widths = c(1.8,1))

ggsave(filename = "industry_classification.pdf",plot = ind_final,device = "pdf",width = 14,height = 12,
       units = c("cm"), dpi = 300, path = "../../../Final Paper/final_paper/images")

  
occu_final <- ggpubr::ggarrange(occu_08, occu_18, ncol = 2, nrow = 1,
                               common.legend = TRUE, legend = "top", widths = c(1.8,1))
ggsave(filename = "occupation_classification.pdf",plot = occu_final,device = "pdf",width = 14,height = 12,
       units = c("cm"), dpi = 300, path = "../../../Final Paper/final_paper/images")
