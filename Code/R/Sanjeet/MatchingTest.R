if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library("tidyverse")
library("MatchIt")

merged <- readRDS("../../../Data/Cleaned/Pooled/Pooled.RDS")

test08 <- merged %>%
  filter(year==2008)

test18 <- merged %>%
  filter(year==2018)

m1 <- MatchIt::matchit(formal_employment ~ age + (age*age) + class_5_Elementary_occupations +
                         class_5_Plant_operator+ class_5_Agri_trade + class_5_Clerical_sales +
                         class_5_Managers + job_sector_Mining_utility + 
                         job_sector_Construction + job_sector_Manufacturing +
                         job_sector_Market_services + job_sector_Non_Market_services+
                         job_sector_Arts_entertain, data = test08,
                       method = "full", distance = "mahalanobis")
cobalt::love.plot(m1, thresholds = c(m = .1), var.order = "unadjusted")
summary(m1, un = FALSE)
sdat <- MatchIt::match.data(m1) %>% 
  filter(formal_sector == 1)
write_rds(sdat, file = "../../../Data/Cleaned/Pooled/Matched08.RDS", compress = "gz")

# Save the object in DTA format
haven::write_dta(sdat, "../../../Data/Cleaned/Pooled/Matched08.dta")

