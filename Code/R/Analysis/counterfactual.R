if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 
gc()

library(tidyverse)
library(Counterfactual)


dat <- readRDS("../../../data/Cleaned/Pooled/Pooled.RDS")

#### 2008 decomposition####
taus <-c(1:99)/100

logitres<- counterfactual(log(hourly_wage) ~ experience + experience_sq +
                            education + female + hh_size +
                            caste_group_6 + married + child_12 + voc_train +
                            migrated_fr_job + tot_chores_hrs + urban + overtime_40,
                          data = dat[dat$year %in% "2008",], 
                          group = formal_employment,
                          treatment = TRUE,
                          weightedboot = TRUE,
                          weights = weight,
                          quantiles = taus,
                          method = "qr",
                          nreg=100,
                          printdeco = TRUE,
                          decomposition = TRUE,
                          sepcore = TRUE,
                          ncore= 3)


estimate <- data.frame(
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

total <- estimate %>% 
  select(duqf_TE, l.duqf_TE, u.duqf_TE, tau) %>% 
  rename(estimate = duqf_TE,
         lower = l.duqf_TE,
         upper = u.duqf_TE) %>% 
  mutate(effect = "Total")

structure <- estimate %>% 
  select(duqf_SE, l.duqf_SE, u.duqf_SE, tau) %>% 
  rename(estimate = duqf_SE,
         lower = l.duqf_SE,
         upper = u.duqf_SE) %>% 
  mutate(effect = "Structure")

composition <- estimate %>% 
  select(duqf_CE, l.duqf_CE, u.duqf_CE, tau) %>% 
  rename(estimate = duqf_CE,
         lower = l.duqf_CE,
         upper = u.duqf_CE) %>% 
  mutate(effect = "Composition")


coeftable <- bind_rows(total, structure, composition) %>% mutate(year = "2008")

#### 2018 Decomposition ####

logitres1<- counterfactual(log(hourly_wage) ~ experience + experience_sq +
                             education + female + hh_size +
                             caste_group_6 + married + child_12 + voc_train +
                             migrated_fr_job +
                             tot_chores_hrs + urban + overtime_40,
                           data = dat[dat$year %in% "2018",], 
                           group = formal_employment,
                           treatment = TRUE,
                           weightedboot = TRUE,
                           weights = weight,
                           quantiles = taus,
                           method = "qr",
                           nreg=10,
                           printdeco = TRUE,
                           decomposition = TRUE,
                           sepcore = TRUE,
                           ncore= 3)

estimate_1 <- data.frame(
  duqf_SE = (logitres1$resSE)[,1],
  l.duqf_SE = (logitres1$resSE)[,3],
  u.duqf_SE = (logitres1$resSE)[,4],
  duqf_CE = (logitres1$resCE)[,1],
  l.duqf_CE = (logitres1$resCE)[,3],
  u.duqf_CE = (logitres1$resCE)[,4],
  duqf_TE = (logitres1$resTE)[,1],
  l.duqf_TE = (logitres1$resTE)[,3],
  u.duqf_TE = (logitres1$resTE)[,4],
  tau = c(1:99)/100
)

total1 <- estimate_1 %>% 
  select(duqf_TE, l.duqf_TE, u.duqf_TE, tau) %>% 
  rename(estimate = duqf_TE,
         lower = l.duqf_TE,
         upper = u.duqf_TE) %>% 
  mutate(effect = "Total")

structure1 <- estimate_1 %>% 
  select(duqf_SE, l.duqf_SE, u.duqf_SE, tau) %>% 
  rename(estimate = duqf_SE,
         lower = l.duqf_SE,
         upper = u.duqf_SE) %>% 
  mutate(effect = "Structure")

composition1 <- estimate_1 %>% 
  select(duqf_CE, l.duqf_CE, u.duqf_CE, tau) %>% 
  rename(estimate = duqf_CE,
         lower = l.duqf_CE,
         upper = u.duqf_CE) %>% 
  mutate(effect = "Composition")


coeftable_1 <- bind_rows(total1, structure1, composition1) %>% mutate(year = "2018")
coeftable_merged <- bind_rows(coeftable, coeftable_1)


{
  coeftable_merged %>% 
    ggplot(aes(x = tau, y = estimate)) +
    geom_line(aes(color = year), size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = year), alpha = .2) +
    geom_hline(yintercept = 0, linetype = "dotted", size = 1) +
    xlab("Quantiles") +
    ylab("Estimates") +
    facet_grid(~effect) +
    theme(
      axis.text.x = element_text(angle = 90, size = 12),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12),
      legend.position = "bottom",
      strip.text.x = element_text(color = "black", size = 10),
      strip.text.y = element_text(color = "black", size = 10),
      strip.background = element_rect(fill="#EEEEEE"),
      panel.spacing = unit(0.4, "lines"),
      panel.border = element_rect(colour = "black", fill=NA),
      panel.background = element_rect(fill = 'white'),
      panel.grid = element_line(colour = "#e1e5ea")
    )
}


