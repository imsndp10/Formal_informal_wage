if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library("tidyverse")

source("CounterfactualFunction.R")

sdat <- readRDS("../../../Data/Cleaned/Pooled/Pooled.RDS")


tictoc::tic()
d <- CounterFac(Data_ = sdat, Year = 2018,Gender = "all", region = "all", industry = "ser" ,formType = "JmNoIndNoJob",
                reg = 2, Matche)
tictoc::toc()
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

tictoc::tic()
i <- CounterFac(Year = 2008,Gender = "all", region = "all", industry = "all" ,formType = "JmNoIndNoJob",
                reg = 100)
tictoc::toc()
i1 <- CounterFac(Year = 2008,Gender = "all", region = "all", industry = "all" ,
                 job = c("Managers"), formType = "JmNoIndNoJob", reg = 100)
i2 <- CounterFac(Year = 2008,Gender = "all", region = "all", industry = "all",
                 job = c("Clerical_sales"), formType = "JmNoIndNoJob",
                 reg = 100)
i3 <- CounterFac(Year = 2008,Gender = "all", region = "all", industry = "all",
                 job = c("Elementary_occupations", "Plant_operator",
                         "Agri_trade"),formType = "JmNoIndNoJob",
                 reg = 100)

