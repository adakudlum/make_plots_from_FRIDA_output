library(tidyverse)
library("readxl")
library(abind)
library(ggtext)

remove(list=ls())

setwd('G:/My Drive/R/plots')
# path to directory where the EMB data exist
filepath.frida <- ("G:/My Drive/Data/NumSample/figures/figures/CI-plots/equalyWeighted/plotData/")

##### -- FRIDA data ----
# Population
population         <- readRDS(paste(filepath.frida,"demographics_population-fit uncertainty-equaly-weighted.RDS",sep=""))
population.frida      <- as.data.frame(population$ciBounds)*1e-3
colnames(population.frida) <- c("low2","low1","median","high1","high2")
population.frida$mean <- population$means*1e-3
population.frida$years <- as.numeric(population$years)
# create a data frame with the median from FRIDA in the same format as rest of the IAMS
population.frida.df <- data.frame(population.frida$years,population.frida$median)
colnames(population.frida.df) <- c("Year","Data")
population.frida.df$Scenario <- "EMB"
population.frida.df$Variable  <- "Population"
population.frida.df$Unit   <- "billion"

calibrated.vars <- as.data.frame(read_excel(paste(filepath.frida,  "frida_calibration.xlsx",sep="")))
population.calibration <- data.frame(seq(1980,2130,by=1),t(filter(calibrated.vars, if_any(everything(),~str_detect(.x,"Population")))[,-1])*1e-3)

colnames(population.calibration) <-  c("Year","Data")
population.calibration$Scenario <-"Calibration"
population.calibration$Variable  <- "Population"
population.calibration$Unit   <- "Billion"

# read population cohorts
cohort.0 <- readRDS(paste(filepath.frida,"demographics_aged_0_years-fit uncertainty-equaly-weighted.RDS",sep=""))
cohort.0.frida      <- as.data.frame(cohort.0$ciBounds)*1e-3
colnames(cohort.0.frida) <- c("low2","low1","median","high1","high2")
cohort.0.frida$mean <- cohort.0$means*1e-3
cohort.0.frida$years <- as.numeric(cohort.0$years)
# create a data frame with the median from FRIDA in the same format as rest of the IAMS
cohort.0.frida.df <- data.frame(cohort.0.frida$years,cohort.0.frida$median)
colnames(cohort.0.frida.df) <- c("Year","Data")
cohort.0.frida.df$Scenario <- "EMB"
cohort.0.frida.df$Variable  <- "cohort.0"
cohort.0.frida.df$Unit   <- "billion"

cohort.0.to.20 <- readRDS(paste(filepath.frida,"demographics_aged_to_20_years-fit uncertainty-equaly-weighted.RDS",sep=""))
cohort.0.to.20.frida      <- as.data.frame(cohort.0.to.20$ciBounds)*1e-3
colnames(cohort.0.to.20.frida) <- c("low2","low1","median","high1","high2")
cohort.0.to.20.frida$mean <- cohort.0.to.20$means*1e-3
cohort.0.to.20.frida$years <- as.numeric(cohort.0.to.20$years)
# create a data frame with the median from FRIDA in the same format as rest of the IAMS
cohort.0.to.20.frida.df <- data.frame(cohort.0.to.20.frida$years,cohort.0.to.20.frida$median)
colnames(cohort.0.to.20.frida.df) <- c("Year","Data")
cohort.0.to.20.frida.df$Scenario <- "EMB"
cohort.0.to.20.frida.df$Variable  <- "cohort.0.to.20"
cohort.0.to.20.frida.df$Unit   <- "billion"

cohort.20.to.40 <- readRDS(paste(filepath.frida,"demographics_aged_20_to_40_years-fit uncertainty-equaly-weighted.RDS",sep=""))
cohort.20.to.40.frida      <- as.data.frame(cohort.20.to.40$ciBounds)*1e-3
colnames(cohort.20.to.40.frida) <- c("low2","low1","median","high1","high2")
cohort.20.to.40.frida$mean <- cohort.20.to.40$means*1e-3
cohort.20.to.40.frida$years <- as.numeric(cohort.20.to.40$years)
# create a data frame with the median from FRIDA in the same format as rest of the IAMS
cohort.20.to.40.frida.df <- data.frame(cohort.20.to.40.frida$years,cohort.20.to.40.frida$median)
colnames(cohort.20.to.40.frida.df) <- c("Year","Data")
cohort.20.to.40.frida.df$Scenario <- "EMB"
cohort.20.to.40.frida.df$Variable  <- "cohort.20.to.40"
cohort.20.to.40.frida.df$Unit   <- "billion"

cohort.40.to.60 <- readRDS(paste(filepath.frida,"demographics_aged_40_to_60_years-fit uncertainty-equaly-weighted.RDS",sep=""))
cohort.40.to.60.frida      <- as.data.frame(cohort.40.to.60$ciBounds)*1e-3
colnames(cohort.40.to.60.frida) <- c("low2","low1","median","high1","high2")
cohort.40.to.60.frida$mean <- cohort.40.to.60$means*1e-3
cohort.40.to.60.frida$years <- as.numeric(cohort.40.to.60$years)
# create a data frame with the median from FRIDA in the same format as rest of the IAMS
cohort.40.to.60.frida.df <- data.frame(cohort.40.to.60.frida$years,cohort.40.to.60.frida$median)
colnames(cohort.40.to.60.frida.df) <- c("Year","Data")
cohort.40.to.60.frida.df$Scenario <- "EMB"
cohort.40.to.60.frida.df$Variable  <- "cohort.40.to.60"
cohort.40.to.60.frida.df$Unit   <- "billion"

cohort.60.to.65 <- readRDS(paste(filepath.frida,"demographics_aged_60_to_65_years-fit uncertainty-equaly-weighted.RDS",sep=""))
cohort.60.to.65.frida      <- as.data.frame(cohort.60.to.65$ciBounds)*1e-3
colnames(cohort.60.to.65.frida) <- c("low2","low1","median","high1","high2")
cohort.60.to.65.frida$mean <- cohort.60.to.65$means*1e-3
cohort.60.to.65.frida$years <- as.numeric(cohort.60.to.65$years)
# create a data frame with the median from FRIDA in the same format as rest of the IAMS
cohort.60.to.65.frida.df <- data.frame(cohort.60.to.65.frida$years,cohort.60.to.65.frida$median)
colnames(cohort.60.to.65.frida.df) <- c("Year","Data")
cohort.60.to.65.frida.df$Scenario <- "EMB"
cohort.60.to.65.frida.df$Variable  <- "cohort.60.to.65"
cohort.60.to.65.frida.df$Unit   <- "billion"

cohort.65.to.75 <- readRDS(paste(filepath.frida,"demographics_aged_65_to_75_years-fit uncertainty-equaly-weighted.RDS",sep=""))
cohort.65.to.75.frida      <- as.data.frame(cohort.65.to.75$ciBounds)*1e-3
colnames(cohort.65.to.75.frida) <- c("low2","low1","median","high1","high2")
cohort.65.to.75.frida$mean <- cohort.65.to.75$means*1e-3
cohort.65.to.75.frida$years <- as.numeric(cohort.65.to.75$years)
# create a data frame with the median from FRIDA in the same format as rest of the IAMS
cohort.65.to.75.frida.df <- data.frame(cohort.65.to.75.frida$years,cohort.65.to.75.frida$median)
colnames(cohort.65.to.75.frida.df) <- c("Year","Data")
cohort.65.to.75.frida.df$Scenario <- "EMB"
cohort.65.to.75.frida.df$Variable  <- "cohort.65.to.65"
cohort.65.to.75.frida.df$Unit   <- "billion"

cohort.75.plus <- readRDS(paste(filepath.frida,"demographics_aged_over_75_years-fit uncertainty-equaly-weighted.RDS",sep=""))
cohort.75.plus.frida      <- as.data.frame(cohort.75.plus$ciBounds)*1e-3
colnames(cohort.75.plus.frida) <- c("low2","low1","median","high1","high2")
cohort.75.plus.frida$mean <- cohort.75.plus$means*1e-3
cohort.75.plus.frida$years <- as.numeric(cohort.75.plus$years)
# create a data frame with the median from FRIDA in the same format as rest of the IAMS
cohort.75.plus.frida.df <- data.frame(cohort.75.plus.frida$years,cohort.75.plus.frida$median)
colnames(cohort.75.plus.frida.df) <- c("Year","Data")
cohort.75.plus.frida.df$Scenario <- "EMB"
cohort.75.plus.frida.df$Variable  <- "cohort.75.plus"
cohort.75.plus.frida.df$Unit   <- "billion"

cohorts   <- map_df( .x=list(
                        "75 plus" = cohort.75.plus.frida.df,
                        "65 to 75 years" = cohort.65.to.75.frida.df,
                        "60 to 65 years" = cohort.60.to.65.frida.df,
                        "40 to 60 years" = cohort.40.to.60.frida.df,
                        "20 to 40 years" = cohort.20.to.40.frida.df,
                        "1 to 20 years"= cohort.0.to.20.frida.df,                        
                        "0 years" = cohort.0.frida.df),
.f=bind_rows, .id="type")

ggplot()+
  geom_area(data=cohorts,mapping=aes(x=Year,y=Data,
                                            fill=as.factor(type)),position=position_stack(reverse = T))+
  geom_line(data=population.frida.df, 
            mapping=aes(x=Year,y=Data),color="black", linewidth=1.5)+
  geom_point(data=population.calibration,mapping=aes(x=Year,y=Data, size=Scenario,shape=Scenario, color=Scenario))+
  geom_line(data=population.calibration,mapping=aes(x=Year,y=Data, linetype=Scenario, color=Scenario))+
  geom_ribbon(data=population.frida,aes(x=years, ymin=low2, ymax=high2),alpha=0.2)+
  geom_line(data=population.frida,aes(x=years, y=low2),linetype="dashed",color="black",linewidth=1)+
  geom_line(data=population.frida,aes(x=years, y=high2),linetype="dashed",color="black",linewidth=1)+
  theme_bw()+
  scale_fill_manual(values=c("grey","lightblue2","green","magenta2","blue","purple","yellow3"))+
  scale_linetype_manual(values="solid")+
  scale_shape_manual(values=19)+
  scale_color_manual(values="red")+
  scale_size_manual(values=2)+
  labs(x=NULL,y=NULL,title="Population (billion)")+
  scale_x_continuous(breaks = seq(1980,2130,20),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme(strip.background =element_rect(fill="white"),
        strip.text = element_text(size=13, family="serif"),
        axis.text.x = element_text(size=12, angle = 90, family="serif"),
        axis.text.y = element_text(size=12, family="serif"),
        legend.text = element_text(size=12), 
        legend.key.width = unit(.8,"cm"),
        legend.key.height = unit(.5,"cm"),
        legend.position = "bottom",
        legend.key.spacing.x= unit(1.5,"cm"),
        legend.box="horizontal",
        legend.box.just="left",
        legend.title=element_blank())+
  guides(fill= guide_legend(ncol = 4, nrow = 2))


  
                                 
