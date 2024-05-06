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
                    industry = c("all", "man", "ser", "man_ser"),
                    job = NULL){
  data <- readRDS("../../../Data/Cleaned/Pooled/Pooled.RDS") %>%
    mutate(log_wage = log(hourly_wage)) %>% 
    filter(year == Year)
  if(Gender == "male"){
    data <- data %>% 
      filter(female == 0)
  }
  if(Gender == "female"){
    data <- data %>% 
      filter(female == 1)
  }
  
  if(region == "urban"){
    data <- data %>% 
      filter(urban ==1)
  }
  if(region == "rural"){
    data <- data %>% 
      filter(urban == 0)
  }
  if(industry == "man"){
    data <- data %>% 
      filter(job_sector %in% c("Manufacturing"))
  }
  if(industry == "ser"){
    data <- data %>% 
      filter(job_sector %in% c("Market_services", "Non_Market_services",
                               "Arts_entertain"))
  }
  if(industry == "man_ser"){
    data <- data %>% 
      filter(job_sector %in% c("Manufacturing", "Market_services", "Non_Market_services",
                               "Arts_entertain"))
  }
  if(is.null(job)){
    Data <- data
  } else if(!is.null(job)){
    Data <- data %>% 
      filter(class_5 %in%job)
  }
  return(Data)
}

CounterFac <- function(Year, Gender = c("all", "male", "female"), 
                       region = c("all", "urban", "rural"),
                       industry = c("all", "man", "ser", "man_ser"),
                       job = NULL,
                       formType = c("HH", "JmNoInd", "JmInd", "JmNoIndNoJob"), reg){
  data <- dataGen(Year, Gender,region,industry, job)
  HH <- as.formula("log_wage ~ yrs_schooling + experience + experience_sq + caste_group_6+
                       married + hh_size + child_12 + tot_chores_hrs")
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
  JM_NoInd_NoJob_formula <- update.formula(JM_NoInd_formula, .~. -migrated_fr_job -overtime_40 -class_5)
  Formula <- switch(formType, "HH" = HH_formula, "JmNoInd" = JM_NoInd_formula,
                    "JmInd" = JM_Ind_formula, "JmNoIndNoJob" = JM_NoInd_NoJob_formula)
  
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
                            ncore= 3,
                            firs)
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
  if(is.null(job)){
    file_name <- paste0(Year, "_", "gender_", Gender, "_", "region_", region, "_",
                        "industry_",industry, "_", formType)  
  } else if(!is.null(job)){
    file_name <- paste0(Year, "_", "gender_", Gender, "_", "region_", region, "_",
                        "industry_",industry, "_", formType, "_", job[1])
  }
  
  
  return(write_rds(x = estimates, file = paste0("../../../Data/Cleaned/Pooled/test/", file_name, ".RDS"),
                   compress = "gz"))
}  

d <- CounterFac(Year = 2018,Gender = "all", region = "all", industry = "ser" ,formType = "JmNoIndNoJob",
                reg = 100)
d1 <- CounterFac(Year = 2018,Gender = "all", region = "all", industry = "ser" ,
                 job = c("Managers"), formType = "JmNoIndNoJob", reg = 100)
d2 <- CounterFac(Year = 2018,Gender = "all", region = "all", industry = "ser",
                 job = c("Clerical_sales"), formType = "JmNoIndNoJob",
                 reg = 100)
d3 <- CounterFac(Year = 2018,Gender = "all", region = "all", industry = "ser",
                 job = c("Elementary_occupations", "Plant_operator",
                         "Agri_trade"),formType = "JmNoIndNoJob",
                reg = 100)


e <- CounterFac(Year = 2018,Gender = "all", region = "all", industry = "all" ,formType = "JmNoIndNoJob",
                reg = 100)
e1 <- CounterFac(Year = 2018,Gender = "all", region = "all", industry = "all" ,
                 job = c("Managers"), formType = "JmNoIndNoJob", reg = 100)
e2 <- CounterFac(Year = 2018,Gender = "all", region = "all", industry = "all",
                 job = c("Clerical_sales"), formType = "JmNoIndNoJob",
                 reg = 100)
e3 <- CounterFac(Year = 2018,Gender = "all", region = "all", industry = "all",
                 job = c("Elementary_occupations", "Plant_operator",
                         "Agri_trade"),formType = "JmNoIndNoJob",
                 reg = 100)


h <- CounterFac(Year = 2008,Gender = "all", region = "all", industry = "ser" ,formType = "JmNoIndNoJob",
                reg = 100)
h1 <- CounterFac(Year = 2008,Gender = "all", region = "all", industry = "ser" ,
                 job = c("Managers"), formType = "JmNoIndNoJob", reg = 100)
h2 <- CounterFac(Year = 2008,Gender = "all", region = "all", industry = "ser",
                 job = c("Clerical_sales"), formType = "JmNoIndNoJob",
                 reg = 100)
h3 <- CounterFac(Year = 2008,Gender = "all", region = "all", industry = "ser",
                 job = c("Elementary_occupations", "Plant_operator",
                         "Agri_trade"),formType = "JmNoIndNoJob",
                 reg = 100)


i <- CounterFac(Year = 2008,Gender = "all", region = "all", industry = "ser" ,formType = "JmNoIndNoJob",
                reg = 100)
i1 <- CounterFac(Year = 2008,Gender = "all", region = "all", industry = "ser" ,
                 job = c("Managers"), formType = "JmNoIndNoJob", reg = 100)
i2 <- CounterFac(Year = 2008,Gender = "all", region = "all", industry = "ser",
                 job = c("Clerical_sales"), formType = "JmNoIndNoJob",
                 reg = 100)
i3 <- CounterFac(Year = 2008,Gender = "all", region = "all", industry = "ser",
                 job = c("Elementary_occupations", "Plant_operator",
                         "Agri_trade"),formType = "JmNoIndNoJob",
                 reg = 100)
