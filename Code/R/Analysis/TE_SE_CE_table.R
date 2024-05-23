if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library("tidyverse")
library("kableExtra")
library("dplyr")

dat <- list.files("../../../Data/Cleaned/Pooled/test2/")
b <- list.files("../../../Data/Cleaned/Pooled/test2/", full.names = TRUE)
objectname <- gsub(".RDS", "", dat)
sdat <- list()
for (i in c(1: length(dat))){
  objectname[i] <- gsub(".RDS", "", dat[i])
  sdat[[i]] <- readRDS(b[i])
  names(sdat)[i] <- objectname[i]
}

sdat <- lapply(sdat,  function (x) 
{ x %>% 
    filter(tau%in%c(seq(0.1, 0.9, 0.1)))
})

Split <- NULL
for(i in c(1:length(dat))){
  Split[i] <- str_split(string = names(sdat)[i], pattern = "_")
}

for(i in c(1:length(dat))){
  sdat[[i]] <- sdat[[i]] %>% 
    mutate(year = Split[[i]][1],
           gender = Split[[i]][3],
           region = Split[[i]][5],
           industry = Split[[i]][7],
           formula = Split[[i]][8],
           job = if_else(length(Split[[i]]) > 8, Split[[i]][9], "overall"))
}
test <- do.call("rbind", sdat)
row.names(test) <- NULL

for (i in 1:12) {
  test[[i]] <- round(test[[i]], 2)
}


effects <- test %>% 
  select(c("year", "tau", "duqf_CE", "duqf_CE_serror", "l.duqf_CE",     
           "u.duqf_CE", "duqf_SE", "duqf_SE_serror", "l.duqf_SE",     
           "u.duqf_SE", "duqf_TE", "duqf_TE_serror", "l.duqf_TE",
           "u.duqf_TE", "gender", "industry", "region", "formula", "job"))

widetest <- effects %>% 
  pivot_wider(names_from = industry,
              values_from = c("duqf_CE", "duqf_CE_serror", "l.duqf_CE",     
                              "u.duqf_CE", "duqf_SE", "duqf_SE_serror", "l.duqf_SE",     
                              "u.duqf_SE", "duqf_TE",       
                              "duqf_TE_serror", "l.duqf_TE",     
                              "u.duqf_TE")) %>%
  select(c("year", "tau", "job"))

tableDat <- function(Job = c("Managers", "Elementary_occupations", "Clerical_sales")){
  data <- effects %>% 
    filter(job == Job) %>%
    mutate(year = if_else(tau == 0.1, year, "")) %>% 
    select(-c("job"))
  
data1 <- data %>% 
    mutate(CE = paste0(duqf_CE, " ", "(", duqf_CE_serror, ")"),
           CE_bounds = paste0(l.duqf_CE, ";", " ", u.duqf_CE),
           SE = paste0(duqf_SE, " ", "(", duqf_SE_serror, ")"),
           SE_bounds = paste0(l.duqf_SE, ";", " ", u.duqf_SE),
           TE = paste0(duqf_TE, " ", "(", duqf_TE_serror, ")"),
           TE_bounds = paste0(l.duqf_TE, ";", " ", u.duqf_TE)) %>% 
    select(c("year", "tau", "CE", "CE_bounds", "SE",
             "SE_bounds", "TE", "TE_bounds")) %>% 
    mutate(year = if_else(tau == 0.1, year, "")) 
  return(data1)
}  


table.export <- function(Job = c("Managers", "Elementary_occupations", "Clerical_sales")){
  
  data <- tableDat(Job)
  data %>% 
    kable(col.names = c("Year", "tau", "Estimate", "Bounds",
                        "Estimate", "Bounds","Estimate", "Bounds"),
          align = rep("l", length(data)),
          caption = paste0("Decomposition Table", " ", Job),
          booktabs = T, 
          format = "latex", 
          linesep = "") %>% 
    add_header_above(c(" " = 2,"Composition Effect" = 2, "Structure Effect" = 2, "Total Effect" = 2)) %>% 
    kable_styling(latex_options = c("scale_down")) %>% 
    landscape() %>% 
    save_kable(file = paste0("../../../Output/Tables/","Decomposition", 
                           "_", Job, ".tex"))
  
}

managers <- table.export(Job = "Managers") 
elementary <- table.export(Job = "Elementary_occupations") 
clerical <- table.export(Job = "Clerical_sales") 


