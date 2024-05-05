if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library("tidyverse")
library("ggplot2")
library("fixest")


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
  return(Data)
  }

estimationFunc <- function(Year, industry = NULL, job = NULL){
  Data <- dataFunc(Year,industry,job)
  adat <- fixest::feols(formal_employment ~ HH_formal + dep_ratio +  
                          experience + experience_sq + yrs_schooling| csw0(female + urban,
                                                                           factor(caste_group_6)+factor(dist)) , data = Data)
  return(adat)
}

tableOutput <- function(Year, industry = NULL, job = NULL){
  reg1 <- estimationFunc(Year = 2008, industry, job)
  reg2 <- estimationFunc(Year = 2018, industry, job)
  etable(list(reg1,reg2), title = paste0("Formal informal","_", job[1]),
         dict = c(HH_formal = "Formal members", dep_ratio = "Dependent",
                  yrs_schooling = "Years of schooling", female = "Female",
                  caste_group_6 = "Caste", dist = "District"),
         tex = TRUE,
         adjustbox = TRUE,
         file = paste0("../../../Output/Tables/formal_informal",job[1],".tex"))
}

tableOutput(industry = c("Market_services", "Non_Market_services",
                                    "Arts_entertain"),
            job = c("Managers"))

tableOutput(industry = c("Market_services", "Non_Market_services",
                         "Arts_entertain"),
            job = c("Clerical_sales"))

tableOutput(industry = c("Market_services", "Non_Market_services",
                         "Arts_entertain"),
            job = c("Elementary_occupations", "Plant_operator",
                    "Agri_trade"))