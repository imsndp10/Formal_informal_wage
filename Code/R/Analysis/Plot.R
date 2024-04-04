if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library("tidyverse")
library("ggplot2")


#plot data preparation

dat <- list.files("../../../Data/Cleaned/Pooled/Counterfactual")
b <- list.files("../../../Data/Cleaned/Pooled/Counterfactual", full.names = TRUE)
objectname <- gsub(".RDS", "", dat)
sdat <- list()
for (i in c(1: length(dat))){
objectname[i] <- gsub(".RDS", "", dat[i])
sdat[[i]] <- readRDS(b[i])
names(sdat)[i] <- objectname[i]
   }

for(i in c(1:length(dat))){
  split[i] <- str_split(string = names(sdat)[i], pattern = "_")
}

for(i in c(1:length(dat))){
  sdat[[i]] <- sdat[[i]] %>% 
    mutate(year = split[[i]][1],
           gender = split[[i]][3],
           region = split[[i]][5],
           industry = split[[i]][7],
           forumla = split[[i]][9])
}

test <- do.call("rbind", sdat)
row.names(test) <- NULL

longtest <- pivot_longer(data = test, cols =  c("duqf_SE", "l.duqf_SE", 
                                                "u.duqf_SE", "duqf_CE",
                                                "l.duqf_CE", "u.duqf_CE",
                                                "duqf_TE", "l.duqf_TE",
                                                "u.duqf_TE"), names_to = "effect",
                         values_to = "value") %>% 
  mutate(Effect = case_when(
    effect %in% c("duqf_CE", "l.duqf_CE", "u.duqf_CE") ~ "Composition",
    effect %in% c("duqf_SE", "l.duqf_SE", "u.duqf_SE") ~ "Structure",
    TRUE ~ "Total"
  ),
  estimate = case_when(
    effect %in% c("l.duqf_CE", "l.duqf_SE", "l.duqf_TE") ~ "lower",
    effect %in% c("u.duqf_CE", "u.duqf_SE", "u.duqf_TE") ~ "upper",
    TRUE ~ "coefficient"
  )) %>% 
  select(-c(effect))


#PLot here



   