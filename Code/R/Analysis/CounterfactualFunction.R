if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library("tidyverse")
library("MatchIt")
library("Counterfactual")

## Year; Gender; Urban-Rural; Industry ### Data subclassification function


dataGen <- function(Year, Gender = c("all", "male", "female"), 
                    region = c("all", "urban", "rural"),
                    industry = c("all", "man", "ser", "man_ser")){
  data <- readRDS("../../../Data/Cleaned/Pooled/Pooled.RDS") %>%
    mutate(log_wage = log(hourly_wage)) %>% 
    filter(year == Year)
  if(Gender == "male"){
    gendata <- data %>% 
      filter(female == 0)
  }
  if(Gender == "female"){
    gendata <- data %>% 
      filter(female == 1)
  }else{
    gendata <- data
  }
  if(region == "urban"){
    regdata <- gendata %>% 
      filter(urban ==1)
  }
  if(region == "rural"){
    regdata <- gendata %>% 
      filter(urban == 0)
  }else{
    regdata <- gendata
  }
  if(industry == "man"){
    inddata <- regdata %>% 
      filter(job_sector %in% c("Manufacturing"))
  }
  if(industry == "ser"){
    inddata <- regdata %>% 
      filter(job_sector %in% c("Market_services", "Non_Market_services",
                               "Arts_entertain"))
  }
  if(industry == "man_ser"){
    inddata <- regdata %>% 
      filter(job_sector %in% c("Manufacturing", "Market_services", "Non_Market_services",
                               "Arts_entertain"))
  }else{
    inddata <- regdata
  }
  return(inddata)
}


a <- dataGen(Year = 2008,Gender = "all", region = "all",industry = "all")

CounterFac <- function(Year, Gender = c("all", "male", "female"), 
                       region = c("all", "urban", "rural"),
                       industry = c("all", "man", "ser", "man_ser"), 
                       formType = c("HH", "JM_NoInd", "JM_Ind"), reg){
  data <- dataGen(Year, Gender,region,industry)
  HH <- as.formula("log_wage ~ education + experience + experience_sq +
                       married + hh_size + child_12 + voc_train + tot_chores_hrs")
  HH_female <- update.formula(HH, . ~ . + female)
  HH_urban <- update.formula(HH, . ~ . + urban)
  HH_female_urban <- update.formula(HH_urban, . ~ . + female)
  if(Gender != "all" & region != "all"){
    HH_formula <- HH  
  }
  if(Gender == "all" & region != "all"){
    HH_formula <- HH_female
  }
  if(Gender != "all" & region == "all"){
    HH_formula <- HH_urban
  }
  if(Gender == "all" & region == "all"){
    HH_formula <- HH_female_urban
  }
  JM_NoInd_formula <- update.formula(HH_formula, . ~ . + migrated_fr_job + overtime_40 + class_5)
  JM_Ind_formula <- update.formula(HH_formula, . ~ . + migrated_fr_job + overtime_40 + class_5 +
                                            job_sector)
  Formula <- switch(formType, HH = HH_formula, JM_NoInd = JM_NoInd_formula,
                    JM_Ind = JM_Ind_formula)
  
  taus <-c(1:99)/100
  first <- sum(as.double(taus <= .10))
  last <- sum(as.double(taus <= .80))
  rang <- c(first:last)
  logitres<- counterfactual(Formula,
                            data = data, 
                            group = formal_employment,
                            treatment = TRUE,
                            weights = weight,
                            quantiles = taus,
                            method = "qr",
                            weightedboot = TRUE,
                            trimming = 0.005,
                            nreg=reg,
                            printdeco = TRUE,
                            decomposition = TRUE,
                            sepcore = TRUE,
                            ncore= 3)
  estimates <- data.frame(
    duqf_SE = (logitres$resSE)[,1], 
    l.duqf_SE = (logitres$resSE)[,3], 
    u.duqf_SE = (logitres$resSE)[,4], 
    duqf_CE = (logitres$resCE)[,1], 
    l.duqf_CE = (logitres$resCE)[,3], 
    u.duqf_CE = (logitres$resCE)[,4], 
    duqf_TE = (logitres$resTE)[,1], 
    l.duqf_TE = (logitres$resTE)[,3], 
    u.duqf_TE = (logitres$resTE)[,4],
    tau = c(1:99)/100
  )
  file_name <- paste0(Year, "_", "gender_", Gender, "_", "region_", region, "_",
                      "industry_",industry, "_", formType)
  return(write_rds(x = estimates, file = paste0("../../../Data/Cleaned/Pooled/test/", file_name, ".RDS"),
                   compress = "gz"))
}  


a <- CounterFac(Year = 2018,Gender = "all", region = "all",industry = "all",formType = "HH",
                reg = 2)

b <- CounterFac(Year = 2018,Gender = "all", region = "all",industry = "all",formType = "JM_NoInd",
                reg = 100)

c <- CounterFac(Year = 2018,Gender = "all", region = "all", industry = "man" ,formType = "JM_NoInd",
                reg = 2)

d <- CounterFac(Year = 2018,Gender = "all", region = "all", industry = "ser" ,formType = "JM_NoInd",
                reg = 2)

e <- CounterFac(Year = 2018,Gender = "all", region = "all", industry = "man_ser" ,formType = "JM_NoInd",
                reg = 2)

f <- CounterFac(Year = 2008,Gender = "all", region = "all",industry = "all",formType = "JM_NoInd",
                reg = 100)

g <- CounterFac(Year = 2008,Gender = "all", region = "all", industry = "man" ,formType = "JM_NoInd",
                reg = 2)

h <- CounterFac(Year = 2008,Gender = "all", region = "all", industry = "ser" ,formType = "JM_NoInd",
                reg = 2)

i <- CounterFac(Year = 2008,Gender = "all", region = "all", industry = "man_ser" ,formType = "JM_NoInd",
                reg = 2)




