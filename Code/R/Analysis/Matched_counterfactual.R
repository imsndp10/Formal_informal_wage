if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library("tidyverse")

source("CounterfactualFunction.R")

sdat <- readRDS("../../../Data/Cleaned/Pooled/Merged_Matched.RDS")


tictoc::tic()
d <- CounterFac(Data_ = sdat, Year = 2018,Gender = "all", region = "all", industry = "ser" ,formType = "JmNoIndNoJob",
                reg = 500, Matched = TRUE)
tictoc::toc()
d1 <- CounterFac(Data_ = sdat,Year = 2018,Gender = "all", region = "all", industry = "ser" ,
                 job = c("Managers"), formType = "JmNoIndNoJob", reg = 500, Matched = TRUE)
d2 <- CounterFac(Data_ = sdat,Year = 2018,Gender = "all", region = "all", industry = "ser",
                 job = c("Clerical_sales"), formType = "JmNoIndNoJob",
                 reg = 500, Matched = TRUE)
d3 <- CounterFac(Data_ = sdat,Year = 2018,Gender = "all", region = "all", industry = "ser",
                 job = c("Elementary_occupations", "Plant_operator",
                         "Agri_trade"),formType = "JmNoIndNoJob",
                 reg = 500, Matched = TRUE)


e <- CounterFac(Data_ = sdat, Year = 2018,Gender = "all", region = "all", industry = "all" ,formType = "JmNoIndNoJob",
                reg = 500, Matched = TRUE)
e1 <- CounterFac(Data_ = sdat, Year = 2018,Gender = "all", region = "all", industry = "all" ,
                 job = c("Managers"), formType = "JmNoIndNoJob", reg = 500, Matched = TRUE)
e2 <- CounterFac(Data_ = sdat, Year = 2018,Gender = "all", region = "all", industry = "all",
                 job = c("Clerical_sales"), formType = "JmNoIndNoJob",
                 reg = 500, Matched = TRUE)
e3 <- CounterFac(Data_ = sdat, Year = 2018,Gender = "all", region = "all", industry = "all",
                 job = c("Elementary_occupations", "Plant_operator",
                         "Agri_trade"),formType = "JmNoIndNoJob",
                 reg = 500, Matched = TRUE)


h <- CounterFac(Data_ = sdat, Year = 2008,Gender = "all", region = "all", industry = "ser" ,formType = "JmNoIndNoJob",
                reg = 500, Matched = TRUE)
h1 <- CounterFac(Data_ = sdat, Year = 2008,Gender = "all", region = "all", industry = "ser" ,
                 job = c("Managers"), formType = "JmNoIndNoJob", reg = 500, Matched = TRUE)
h2 <- CounterFac(Data_ = sdat, Year = 2008,Gender = "all", region = "all", industry = "ser",
                 job = c("Clerical_sales"), formType = "JmNoIndNoJob",
                 reg = 500, Matched = TRUE)
h3 <- CounterFac(Data_ = sdat, Year = 2008,Gender = "all", region = "all", industry = "ser",
                 job = c("Elementary_occupations", "Plant_operator",
                         "Agri_trade"),formType = "JmNoIndNoJob",
                 reg = 500, Matched = TRUE)

tictoc::tic()
i <- CounterFac(Data_ = sdat, Year = 2008,Gender = "all", region = "all", industry = "all" ,formType = "JmNoIndNoJob",
                reg = 500, Matched = TRUE)
tictoc::toc()
i1 <- CounterFac(Data_ = sdat, Year = 2008,Gender = "all", region = "all", industry = "all" ,
                 job = c("Managers"), formType = "JmNoIndNoJob", reg = 500, Matched = TRUE)
i2 <- CounterFac(Data_ = sdat, Year = 2008,Gender = "all", region = "all", industry = "all",
                 job = c("Clerical_sales"), formType = "JmNoIndNoJob",
                 reg = 500, Matched = TRUE)
i3 <- CounterFac(Data_ = sdat, Year = 2008,Gender = "all", region = "all", industry = "all",
                 job = c("Elementary_occupations", "Plant_operator",
                         "Agri_trade"),formType = "JmNoIndNoJob",
                 reg = 500, Matched = TRUE)

