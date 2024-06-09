if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library("tidyverse")
library("MatchIt")
library("Counterfactual")

## Year; Gender; Urban-Rural; Industry ### Data subclassification function

data <- readRDS("../../../Data/Cleaned/Pooled/Pooled.RDS") %>% 
  filter(job_sector%in%c("Market_services", "Non_Market_services",
                         "Arts_entertain")) %>% 
  mutate(log_wage = log(hourly_wage))

managers <- data %>% 
  filter(class_11%in%c("Managers", "Professionals"))%>% 
  group_by(formal_employment, year) %>% 
  mutate(mean_wage = mean(log(hourly_wage))) %>% 
  ungroup()
data_18 <- managers %>% 
  filter(year == 2018)
data_08 <- managers %>% 
  filter(year == 2008)

dataGen <- function(Year, Gender = c("all", "male", "female"), 
                    region = c("all", "urban", "rural"),
                    industry = c("all", "man", "ser", "man_ser"),
                    job = NULL){
  data <- readRDS("../../../Data/Cleaned/Pooled/Pooled.RDS") %>%
    mutate(log_wage = log(hourly_wage)) %>% 
    filter(year == Year)
  if(Gender == "male"){
    data <- data %>% 
      filter(female == 0)
  }
  if(Gender == "female"){
    data <- data %>% 
      filter(female == 1)
  }
  
  if(region == "urban"){
    data <- data %>% 
      filter(urban ==1)
  }
  if(region == "rural"){
    data <- data %>% 
      filter(urban == 0)
  }
  if(industry == "man"){
    data <- data %>% 
      filter(job_sector %in% c("Manufacturing"))
  }
  if(industry == "ser"){
    data <- data %>% 
      filter(job_sector %in% c("Market_services", "Non_Market_services",
                               "Arts_entertain"))
  }
  if(industry == "man_ser"){
    data <- data %>% 
      filter(job_sector %in% c("Manufacturing", "Market_services", "Non_Market_services",
                               "Arts_entertain"))
  }
  if(is.null(job)){
    Data <- data
  } else if(!is.null(job)){
    Data <- data %>% 
      filter(class_5 %in%job)
  }
  return(Data)
}

CounterFac <- function(Year, Gender = c("all", "male", "female"), 
                       region = c("all", "urban", "rural"),
                       industry = c("all", "man", "ser", "man_ser"),
                       job = NULL,
                       formType = c("HH", "JmNoInd", "JmInd", "JmNoIndNoJob"), reg){
  data <- dataGen(Year, Gender,region,industry, job)
  Gender <- "all"
  region <- "all"
  industry <- "ser"
  formType <- "JmNoIndNoJob"
  HH <- as.formula("log_wage ~ yrs_schooling + experience + experience_sq + caste_group_6+
                       married + hh_size + child_12 + tot_chores_hrs")
  HH_female <- update.formula(HH, . ~ . + female)
  HH_urban <- update.formula(HH, . ~ . + urban)
  HH_female_urban <- update.formula(HH_urban, . ~ . + female)
  if(Gender != "all" & region != "all"){
    HH_formula <- HH  
  }
  if(Gender == "all" & region != "all"){
    HH_formula <- HH_female
  }
  if(Gender != "all" & region == "all"){
    HH_formula <- HH_urban
  }
  if(Gender == "all" & region == "all"){
    HH_formula <- HH_female_urban
  }
  JM_NoInd_formula <- update.formula(HH_formula, . ~ . + migrated_fr_job + overtime_40 + class_5)
  JM_Ind_formula <- update.formula(HH_formula, . ~ . + migrated_fr_job + overtime_40 + class_5 +
                                     job_sector)
  JM_NoInd_NoJob_formula <- update.formula(JM_NoInd_formula, .~. -migrated_fr_job -overtime_40 -class_5)
  Formula <- switch(formType, "HH" = HH_formula, "JmNoInd" = JM_NoInd_formula,
                    "JmInd" = JM_Ind_formula, "JmNoIndNoJob" = JM_NoInd_NoJob_formula)
  
  taus <-c(1:99)/100
  first <- sum(as.double(taus <= .10))
  last <- sum(as.double(taus <= .80))
  rang <- c(first:last)
  logitres<- counterfactual(Formula,
                            data = data_08, 
                            group = formal_employment,
                            treatment = TRUE,
                            weights = weight,
                            quantiles = taus,
                            method = "qr",
                            weightedboot = TRUE,
                            trimming = 0.025,
                            nreg=500,
                            printdeco = TRUE,
                            decomposition = TRUE,
                            sepcore = TRUE,
                            ncore= parallel::detectCores()-1)
  estimates <- data.frame(
    duqf_SE = (logitres$resSE)[,1],
    duqf_SE_serror = (logitres$resSE)[,2],
    l.duqf_SE = (logitres$resSE)[,3], 
    u.duqf_SE = (logitres$resSE)[,4], 
    duqf_CE = (logitres$resCE)[,1],
    duqf_CE_serror = (logitres$resCE)[,2],
    l.duqf_CE = (logitres$resCE)[,3], 
    u.duqf_CE = (logitres$resCE)[,4], 
    duqf_TE = (logitres$resTE)[,1],
    duqf_TE_serror = (logitres$resTE)[,2],
    l.duqf_TE = (logitres$resTE)[,3], 
    u.duqf_TE = (logitres$resTE)[,4],
    tau = c(1:99)/100
  )
  if(is.null(job)){
    file_name <- paste0(Year, "_", "gender_", Gender, "_", "region_", region, "_",
                        "industry_",industry, "_", formType)  
  } else if(!is.null(job)){
    file_name <- paste0(Year, "_", "gender_", Gender, "_", "region_", region, "_",
                        "industry_",industry, "_", formType, "_", job[1])
  }
  
  
  return(write_rds(x = estimates, file = paste0("../../../Data/Cleaned/Pooled/new_classify/", "2008_gender_all_region_all_industry_ser_JmNoIndNoJob_Managers", ".RDS"),
                   compress = "gz"))
}  
if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library("tidyverse")
library("ggplot2")


#plot data preparation

dat <- list.files("../../../Data/Cleaned/Pooled/new_classify")
b <- list.files("../../../Data/Cleaned/Pooled/new_classify", full.names = TRUE)
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
           job = Split[[i]][9])
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
  plotdat <- pivot_wider(data = longtest, names_from = estimate,
                         values_from = value) %>% 
    filter(tau <=0.90 & tau >=0.10)
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

plot <- ggplot(data = data_08)+
  geom_density(aes(x = log(hourly_wage), fill = factor(formal_employment)),stat = "density", 
               position = "identity", alpha = 0.3, linewidth = 0.2 )+
  scale_fill_manual(values = c("#466CA6", "#A41D1A"),
                    labels = c("Informal", "Formal"))+
  geom_vline(aes(xintercept = mean_wage, color = factor(formal_employment)), linetype = "longdash",
             show.legend = FALSE, linewidth = 0.2 )+
  scale_color_manual(values = c("#466CA6", "#A41D1A"),
                     labels = c("Informal", "Formal"))+
  theme_bw()+
  labs(title = "New Managers",
       x = element_blank(),
       y = element_blank(),
       fill = "Employment")+
  theme(legend.position = "top",panel.grid.minor = element_blank(),
        text = element_text(family = "serif",size = 9),
        plot.title = element_text(family = "serif",size = 9))+
  #scale_fill_brewer(type = "seq", palette = "Set1")+
  scale_y_continuous(breaks = 0.1)+
  ylim(0,2) + xlim(0, 7.5)
grob <- grid::grobTree(grid::textGrob(paste0(2008), x=0.9,  y=0.9,
                                      gp=grid::gpar(fontsize=8, fontfamily="serif")))
plot <- plot + annotation_custom(grob)


######## New test of wage ranking ######
data <- readRDS("../../../Data/Cleaned/Pooled/Pooled.RDS") %>%
  mutate(log_wage = log(hourly_wage)) %>%
  group_by(year) %>%
  mutate(wage_per = ntile(hourly_wage, 100)) %>%
  ungroup() %>%
  filter(wage_per>=3 & wage_per<=97) %>%
  group_by(formal_employment, year) %>%
  mutate(ranking = ntile(log_wage, 100)) %>%
  ungroup() %>%
  mutate(wage_group = case_when(ranking >=1 & ranking <=33 ~ "LowTier",
                                ranking >=34 & ranking <=67 ~ "MidTier",
                                TRUE ~ "UpperTier"))%>%
  group_by(wage_group, formal_employment, year) %>%
  summarise(schooling = mean(yrs_schooling),
            average_wage = mean(hourly_wage),
            occu_elem = round(mean(class_5_Elementary_occupations),2),
            occu_plant = round(mean(class_5_Plant_operator),2),
            occu_Agri = round(mean(class_5_Agri_trade),2),
            occu_Clerk = round(mean(class_5_Clerical_sales),2),
            occu_managers = round(mean(class_5_Managers),2),
            eleven_armed = round(mean(`class_11_Armed forces occupations`),2),
            eleven_clerk = round(mean(`class_11_Clerical support workers`),2),
            eleven_craft = round(mean(`class_11_Craft and related trades workers`),2),
            two_71 = round(mean(two_digit_71),2),
            two_72 = round(mean(two_digit_72),2),
            two_73 = round(mean(two_digit_73),2),
            two_74 = round(mean(two_digit_74),2),
            eleven_element = round(mean(class_11_Elementary_occupations),2),
            eleven_managers = round(mean(class_11_Managers),2),
            eleven_assembler = round(mean(`class_11_Plant and machine operators, and assemblers`),2),
            eleven_plant = round(mean(class_11_Plant_operator),2),
            eleven_professional = round(mean(class_11_Professionals),2),
            eleven_ser_sales = round(mean(`class_11_Service and sales workers`),2),
            eleven_agri = round(mean(`class_11_Skilled agricultural, forestry and fishery workers`),2),
            eleven_technicians = round(mean(`class_11_Technicians and associate professionals`),2))
  


data2 <- readRDS("../../../Data/Cleaned/Pooled/Pooled.RDS") %>%
  mutate(log_wage = log(hourly_wage)) %>%
  group_by(year) %>%
  mutate(wage_per = ntile(hourly_wage, 100)) %>%
  ungroup() %>%
  filter(wage_per>=3 & wage_per<=97) %>%
  mutate(employment_group = case_when(class_5 %in% c("Elementary_occupations") ~ "LowTier",
                                      class_5 %in% c("Managers") ~ "UpperTier",
                                      TRUE ~ "MidTier"))%>%
  group_by(employment_group, formal_employment, year) %>%
  summarise(schooling = mean(yrs_schooling),
            average_wage = mean(hourly_wage)) 

data3 <- readRDS("../../../Data/Cleaned/Pooled/Pooled.RDS")%>%
  mutate(log_wage = log(hourly_wage)) %>% 
  filter(job_sector == "Construction") %>% 
  group_by(year, formal_employment) %>% 
  summarise(schooling = mean(yrs_schooling),
            average_wage = mean(hourly_wage),
            occu_elem = round(mean(class_5_Elementary_occupations),2),
            occu_plant = round(mean(class_5_Plant_operator),2),
            occu_Agri = round(mean(class_5_Agri_trade),2),
            occu_Clerk = round(mean(class_5_Clerical_sales),2),
            occu_managers = round(mean(class_5_Managers),2),
            eleven_armed = round(mean(`class_11_Armed forces occupations`),2),
            eleven_clerk = round(mean(`class_11_Clerical support workers`),2),
            eleven_craft = round(mean(`class_11_Craft and related trades workers`),2),
            two_71 = round(mean(two_digit_71),2),
            two_72 = round(mean(two_digit_72),2),
            two_73 = round(mean(two_digit_73),2),
            two_74 = round(mean(two_digit_74),2),
            eleven_element = round(mean(class_11_Elementary_occupations),2),
            eleven_managers = round(mean(class_11_Managers),2),
            eleven_assembler = round(mean(`class_11_Plant and machine operators, and assemblers`),2),
            eleven_plant = round(mean(class_11_Plant_operator),2),
            eleven_professional = round(mean(class_11_Professionals),2),
            eleven_ser_sales = round(mean(`class_11_Service and sales workers`),2),
            eleven_agri = round(mean(`class_11_Skilled agricultural, forestry and fishery workers`),2),
            eleven_technicians = round(mean(`class_11_Technicians and associate professionals`),2))

