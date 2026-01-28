if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library("tidyverse")
library("MatchIt")
library("Counterfactual")



#Classification: Yearwise; Caste Classification wise
dataGen <- function(Year, caste1, caste2){
  sdat <- readRDS("../../../Data/Cleaned/Pooled/MergedData.rds")
  ddat <- sdat %>% 
    filter(year == Year & muluki_grp %in% c(caste1, caste2)) %>% 
    mutate(classify = if_else(muluki_grp == caste1, 1, 0)) %>% 
    mutate(year = factor(year),
           agesq = age * age)
  return(ddat)
}


###### Matched Data ######
MatchData <- function(Year, caste1, caste2){
  Data <- dataGen(Year, caste1, caste2)
  unmatch <- Data %>% 
    mutate(year = factor(year),
           agesq = age * age) 
  X <- paste("hh_size", "child_12",
             "female", "married", "urban", "age",
             "agesq",  sep = "+")

  balance_match <- matchit(as.formula(paste("classify~", X)),
                           data = unmatch, 
                           method = "full", 
                           exact = "year",
                           caliper = c(.02, hh_size =4),
                           std.caliper = FALSE,
                           mahvars = ~ hh_size + child_12 + age + agesq)
  matchdata <- match.data(balance_match) %>% 
    select(-c("weight")) %>% 
    rename(weight = weights)
  return(matchdata)
}
CounterFac <- function(Year, caste1, caste2, matched, reg){
  if(matched == FALSE){
    data <- dataGen(Year, caste1, caste2)
  }
  if(matched == TRUE){
    data <- MatchData(Year, caste1, caste2)
  }
  taus <-c(1:99)/100
  first <- sum(as.double(taus <= .10))
  last <- sum(as.double(taus <= .80))
  rang <- c(first:last)
  logitres<- counterfactual(loghr ~ hh_size+child_12+tot_chores_hrs+female+married+urban+age+
                              agesq+experience+experience_sq+class_5+
                              job_sector+
                              workplace+
                              sz_workplace+
                              education+dist,
                              data = data, 
                              group = classify,
                              treatment = TRUE,
                              weights = weight,
                              quantiles = taus,
                              method = "qr",
                              nreg=reg,
                              printdeco = TRUE,
                              decomposition = TRUE,
                              sepcore = TRUE,
                              ncore= 6)
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
  if(matched == TRUE){file_name <- paste0("matched",caste1, "_", caste2, "_",
                                          Year)
  }
  if(matched == FALSE){file_name <- paste0("Unmatched",caste1, "_", caste2, "_",
                                           Year)
  }
  return(write_rds(x = estimates, file = paste0("../../../Data/Cleaned/Pooled/", file_name, ".RDS"),
                   compress = "gz"))
}  

UTM2008 <- CounterFac(Year = 2008,caste1 = "Tagadhari", caste2 = "Matwali",
                    matched = FALSE, reg = 5)
UTM2018 <- CounterFac(Year = 2018,caste1 = "Tagadhari", caste2 = "Matwali",
                      matched = FALSE, reg = 5)
UTP2008 <- CounterFac(Year = 2008,caste1 = "Tagadhari", caste2 = "Paninachalne",
                      matched = FALSE, reg = 5)
UTP2018 <- CounterFac(Year = 2018,caste1 = "Tagadhari", caste2 = "Paninachalne",
                      matched = FALSE, reg = 5)
UMP2008 <- CounterFac(Year = 2008,caste1 = "Matwali", caste2 = "Paninachalne",
                      matched = FALSE, reg = 5)
UMP2018 <- CounterFac(Year = 2018,caste1 = "Matwali", caste2 = "Paninachalne",
                      matched = FALSE, reg = 5)
MTM2008 <- CounterFac(Year = 2008,caste1 = "Tagadhari", caste2 = "Matwali",
                      matched = TRUE, reg = 5)
MTM2018 <- CounterFac(Year = 2018,caste1 = "Tagadhari", caste2 = "Matwali",
                      matched = TRUE, reg = 5)
MTP2008 <- CounterFac(Year = 2008,caste1 = "Tagadhari", caste2 = "Paninachalne",
                      matched = TRUE, reg = 5)
MTP2018 <- CounterFac(Year = 2018,caste1 = "Tagadhari", caste2 = "Paninachalne",
                      matched = TRUE, reg = 5)
MMP2008 <- CounterFac(Year = 2008,caste1 = "Matwali", caste2 = "Paninachalne",
                      matched = TRUE, reg = 5)
MMP2018 <- CounterFac(Year = 2018,caste1 = "Matwali", caste2 = "Paninachalne",
                      matched = TRUE, reg = 5)




