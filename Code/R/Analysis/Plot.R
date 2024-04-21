if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library("tidyverse")
library("ggplot2")


#plot data preparation

dat <- list.files("../../../Data/Cleaned/Pooled/test")
b <- list.files("../../../Data/Cleaned/Pooled/test", full.names = TRUE)
objectname <- gsub(".RDS", "", dat)
sdat <- list()
for (i in c(1: length(dat))){
objectname[i] <- gsub(".RDS", "", dat[i])
sdat[[i]] <- readRDS(b[i])
names(sdat)[i] <- objectname[i]
   }
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
           forumla = Split[[i]][9])
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
pdat <- longtest %>% 
  filter(forumla == "NoInd" & industry == "ser")

plotdat <- pivot_wider(data = pdat, names_from = estimate,
                       values_from = value) %>% 
  filter(tau <=0.95 & tau >=0.05)

plot <- ggplot(data = plotdat)+
  geom_line(aes(x = tau, y = coefficient, color = as.factor(year)),show.legend = FALSE)+
  geom_ribbon(aes(x = tau, ymin=lower, ymax = upper, alpha = 0.2,
                  fill = as.factor(year)),
              linetype = 3, alpha = 0.1) +
  facet_grid(cols = vars(Effect))+
  labs(x = "tau",
       y = "Log hourly wage",
       fill = "Year")  +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        legend.position = "top")






   