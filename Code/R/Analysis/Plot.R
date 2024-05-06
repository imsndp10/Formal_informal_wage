if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library("tidyverse")
library("ggplot2")


#plot data preparation

dat <- list.files("../../../Data/Cleaned/Pooled/test2")
b <- list.files("../../../Data/Cleaned/Pooled/test2", full.names = TRUE)
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
           forumla = Split[[i]][8],
           job = if_else(length(dat) > 8, Split[[i]][9], NA),
           job = if_else(is.na(job), "overall", job))
}

test <- do.call("rbind", sdat) %>% 
  mutate(job = factor(job,
                      levels = c("overall", "Managers", "Clerical",
                                 "Elementary"),
                      labels = c("Overall", "Upper tier", "Middle tier",
                                 "Lower tier")))
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

plotFun <- function(Industry){
  pdat <- longtest %>% 
    filter(industry == Industry)  
  plotdat <- pivot_wider(data = pdat, names_from = estimate,
                         values_from = value) %>% 
    filter(tau <=0.95 & tau >=0.05)
  plot <- ggplot(data = plotdat)+
    geom_line(aes(x = tau, y = coefficient, color = as.factor(year)),
              linewidth = 0.6)+
      scale_color_manual(values = c("#466CA6", "#A41D1A"),
                         labels = c("2008", "2018"))+
    geom_ribbon(aes(x = tau, ymin=lower, ymax = upper, alpha = 0.2,
                    fill = as.factor(year)),
                linetype = 3, alpha = 0.3,show.legend = FALSE) +
    scale_fill_manual(values = c("#466CA6", "#A41D1A"),
                      labels = c("2008", "2018"))+
    facet_grid(cols = vars(Effect), rows = vars(job))+
    labs(x = "tau",
         y = "Log hourly wage",
         color = "Year")  +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8)+
    theme_bw()+
    theme(panel.grid.minor = element_blank(),
          legend.position = "top",
          strip.background = element_rect(fill = "#D6CFC4"))
    
  
  return(plot)
}

finalPlot <- plotFun(Industry = "ser")

ggsave(filename = "service.pdf",plot = finalPlot,device = "pdf",width = 14,height = 18,
       units = c("cm"), dpi = 300,path = "../../../Final Paper/final_paper/images")



a <- plotFun(Industry = "ser") + labs(title = "service", x = element_blank()) 
b <- plotFun(Industry = "all")+ labs(title = "overall")
c <- plotFun(Industry = "ser", Job = "Managers") + labs(title = "service managers", x = element_blank())
d <- plotFun(Industry = "ser", Job = "Clerical")+ labs(title = "service Clerical", x = element_blank())
e <- plotFun(Industry = "ser", Job = "Elementary")+ labs(title = "service Elementary", x = element_blank())
f <- plotFun(Industry = "all", Job = "Managers")+ labs(title = "overall managers")
g <- plotFun(Industry = "all", Job = "Clerical")+ labs(title = "overall Clerical")
h <- plotFun(Industry = "all", Job = "Elementary")+ labs(title = "overall Elementary")

i <- ggpubr::ggarrange(a, b, nrow = 2, ncol = 1, common.legend = TRUE,
                       legend = "top")
j <- ggpubr::ggarrange(c, f, nrow = 2, ncol = 1, common.legend = TRUE,
                       legend = "top")
k <- ggpubr::ggarrange(d, g, nrow = 2, ncol = 1, common.legend = TRUE,
                       legend = "top")
l <- ggpubr::ggarrange(f, h, nrow = 2, ncol = 1, common.legend = TRUE,
                       legend = "top")

m <- ggpubr::ggarrange(a, c,d,e, nrow = 4, ncol = 1, common.legend = TRUE,
                       legend = "top")
n <- ggpubr::ggarrange(b, f,g,h, nrow = 4, ncol = 1, common.legend = TRUE,
                       legend = "top")

ggsave(filename = "service_overall.pdf",plot = i,device = "pdf",width = 14,height = 12,
       units = c("cm"),path = "../../../Final Paper/final_paper/images")
ggsave(filename = "service_overall_managers.pdf",plot = j,device = "pdf",width = 14,height = 12,
       units = c("cm"),path = "../../../Final Paper/final_paper/images")
ggsave(filename = "service_overall_clerical.pdf",plot = k,device = "pdf",width = 14,height = 12,
       units = c("cm"),path = "../../../Final Paper/final_paper/images")
ggsave(filename = "service_overall_elementary.pdf",plot = l,device = "pdf",width = 14,height = 12,
       units = c("cm"),path = "../../../Final Paper/final_paper/images")


ggsave(filename = "service.pdf",plot = m,device = "pdf",width = 14,height = 18,
       units = c("cm"),path = "../../../Final Paper/final_paper/images")
ggsave(filename = "overall.pdf",plot = n,device = "pdf",width = 14,height = 18,
       units = c("cm"),path = "../../../Final Paper/final_paper/images")


   