if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 
library("tidyverse")
library("dplyr")
library("tidyr")
library("haven")

##########Data Import#############
nlfs_II <- readRDS("../../../data/Cleaned/NLFS_II/NLFS_II.Rds") %>% 
  filter(!is.na(formal_employment))
nlfs_III <- readRDS("../../../data/Cleaned/NLFS_III/NLFS_III.Rds")

