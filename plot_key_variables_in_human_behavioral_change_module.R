library(tidyverse)
library("readxl")
library(abind)
library(ggpubr)

remove(list=ls())

setwd('G:/My Drive/R/plots')
# path to directory where the EMB data exist
filepath.frida <- ("G:/My Drive/Data/NumSample/figures/figures/CI-plots/equalyWeighted/plotData/")

##### -- FRIDA data ----
# vegetal products demand
veg.products         <- readRDS(paste(filepath.frida,"food_demand_vegetal_product_demand_per_person_per_day-fit uncertainty-equaly-weighted.RDS",sep=""))
veg.products.frida      <- as.data.frame(veg.products$ciBounds)
colnames(veg.products.frida) <- c("low2","low1","median","high1","high2")
veg.products.frida$mean <- veg.products$means
veg.products.frida$Year <- as.numeric(veg.products$years)
veg.products.frida.df <- data.frame(veg.products.frida$Year,veg.products.frida$median)
colnames(veg.products.frida.df) <- c("Year","Data")
veg.products.frida.df$Scenario <- "EMB"
veg.products.frida.df$Variable  <- "vegetal products demand per person per day"
veg.products.frida.df$Unit   <- "kCal/person/day"

# animal products demand
animal.products   <- readRDS(paste(filepath.frida,"food_demand_animal_product_demand_per_person_per_day-fit uncertainty-equaly-weighted.RDS",sep=""))
animal.products.frida <- as.data.frame(animal.products$ciBounds)
colnames(animal.products.frida) <- c("low2","low1","median","high1","high2")
animal.products.frida$mean <- animal.products$means
animal.products.frida$Year <- as.numeric(animal.products$years)
animal.products.frida.df <- data.frame(animal.products.frida$Year,animal.products.frida$median)
colnames(animal.products.frida.df) <- c("Year","Data")
animal.products.frida.df$Scenario <- "EMB"
animal.products.frida.df$Variable  <- "animal products demand per person per day"
animal.products.frida.df$Unit   <- "kCal/person/day"

# perceived climate risk 
perceived.risk         <- readRDS(paste(filepath.frida,"climate_risk_perception_perceived_climate_risk-fit uncertainty-equaly-weighted.RDS",sep=""))
perceived.risk.frida      <- as.data.frame(perceived.risk$ciBounds)
colnames(perceived.risk.frida) <- c("low2","low1","median","high1","high2")
perceived.risk.frida$mean <- perceived.risk$means
perceived.risk.frida$Year <- as.numeric(perceived.risk$years)
perceived.risk.frida.df <- data.frame(perceived.risk.frida$Year,perceived.risk.frida$median)
colnames(perceived.risk.frida.df) <- c("Year","Data")
perceived.risk.frida.df$Scenario <- "EMB"
perceived.risk.frida.df$Variable  <- "Perceived climate risk"
perceived.risk.frida.df$Unit   <- "1"

# Demands from personal norms
personal.norm         <- readRDS(paste(filepath.frida,"animal_products_demand_desired_demand_from_personal_norm-fit uncertainty-equaly-weighted.RDS",sep=""))
personal.norm.frida      <- as.data.frame(personal.norm$ciBounds)
colnames(personal.norm.frida) <- c("low2","low1","median","high1","high2")
personal.norm.frida$mean <- personal.norm$means
personal.norm.frida$Year <- as.numeric(personal.norm$years)
personal.norm.frida.df <- data.frame(personal.norm.frida$Year,personal.norm.frida$median)
colnames(personal.norm.frida.df) <- c("Year","Data")
personal.norm.frida.df$Scenario <- "EMB"
personal.norm.frida.df$Variable  <- "demand from personal norm"
personal.norm.frida.df$Unit   <- "kCal/person/Day"

# demands from descriptive norm
descriptive.norm         <- readRDS(paste(filepath.frida,"animal_products_demand_desired_demand_from_descriptive_norm-fit uncertainty-equaly-weighted.RDS",sep=""))
descriptive.norm.frida      <- as.data.frame(descriptive.norm$ciBounds)
colnames(descriptive.norm.frida) <- c("low2","low1","median","high1","high2")
descriptive.norm.frida$mean <- descriptive.norm$means
descriptive.norm.frida$Year <- as.numeric(descriptive.norm$years)
descriptive.norm.frida.df <- data.frame(descriptive.norm.frida$Year,descriptive.norm.frida$median)
colnames(descriptive.norm.frida.df) <- c("Year","Data")
descriptive.norm.frida.df$Scenario <- "EMB"
descriptive.norm.frida.df$Variable  <- "demand from descriptive norm"
descriptive.norm.frida.df$Unit   <- "kCal/person/Day"

# demand from accessibility
accessibility         <- readRDS(paste(filepath.frida,"animal_products_demand_desired_demand_from_accessibility-fit uncertainty-equaly-weighted.RDS",sep=""))
accessibility.frida      <- as.data.frame(accessibility$ciBounds)
colnames(accessibility.frida) <- c("low2","low1","median","high1","high2")
accessibility.frida$mean <- accessibility$means
accessibility.frida$Year <- as.numeric(accessibility$years)
accessibility.frida.df <- data.frame(accessibility.frida$Year,accessibility.frida$median)
colnames(accessibility.frida.df) <- c("Year","Data")
accessibility.frida.df$Scenario <- "EMB"
accessibility.frida.df$Variable  <- "demand from accessibility"
accessibility.frida.df$Unit   <- "kCal/person/Day"

# Average daily demand per capita
average.demand         <- readRDS(paste(filepath.frida,"animal_products_demand_average_daily_demand_per_capita-fit uncertainty-equaly-weighted.RDS",sep=""))
average.demand.frida      <- as.data.frame(average.demand$ciBounds)
colnames(average.demand.frida) <- c("low2","low1","median","high1","high2")
average.demand.frida$mean <- average.demand$means
average.demand.frida$Year <- as.numeric(average.demand$years)
average.demand.frida.df <- data.frame(average.demand.frida$Year,average.demand.frida$median)
colnames(average.demand.frida.df) <- c("Year","Data")
average.demand.frida.df$Scenario <- "EMB"
average.demand.frida.df$Variable  <- "Average daily demand per capita"
average.demand.frida.df$Unit   <- "kCal/person/Day"

# plot data

plot.animal.products <- animal.products.frida.df
plot.veg.products   <- veg.products.frida.df
plot.perceived.risk <- perceived.risk.frida.df
plot.perceived.risk.ci <- perceived.risk.frida

plot.stacks <- map_df(.x=list("Vegetal poducts"=plot.veg.products,
                              "Animal products"=plot.animal.products),
                               .f=bind_rows,.id="type")

plot.lines <- map_df(.x=list("Demand from personal norm"=personal.norm.frida.df,
                             "Demand from descriptive norm"=descriptive.norm.frida.df,
                             "Demand from accessibility"=accessibility.frida.df,
                             "Average daily demand from capita"=average.demand.frida.df),
                     .f=bind_rows, .id="type")


p1 <- ggplot() +
      geom_line(data=plot.perceived.risk, mapping=aes(x=Year,y=Data), linewidth=2)+
      geom_ribbon(data=plot.perceived.risk.ci,aes(x=Year,ymax=high1,ymin=low1),alpha=0.4)+
      geom_ribbon(data=plot.perceived.risk.ci, aes(x=Year,ymax=high2,ymin=low2),alpha=0.2)+
      theme_bw()+
      labs(x=NULL,y=NULL,title="(a) Perceived climate risk")+
      scale_x_continuous(breaks = seq(1980,2130,20),expand=c(0,0))+
      scale_y_continuous(expand=c(0,0))+
      theme(axis.text.x = element_text(size=12, angle = 90, family="sans"),
            axis.text.y = element_text(size=12, family="sans"),
            legend.text = element_text(size=12), 
            legend.key.width = unit(1.0,"cm"),
            legend.position = "inside",
            legend.position.inside = c(0.2,0.8),
            legend.key.spacing.x= unit(1.5,"cm"),
            legend.box="vertical",
            legend.box.just="left",
            legend.title=element_blank())

p2 <- ggplot()+
  geom_area(data=plot.stacks,mapping=aes(x=Year,y=Data,fill=type))+
  theme_bw()+
  scale_fill_manual(values=c("red","blue"))+
  labs(x=NULL,y=NULL,title="(b) Food demand per person per day (kCal/person/day)")+
  scale_x_continuous(breaks = seq(1980,2130,20),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme(axis.text.x = element_text(size=12, angle = 90, family="sans"),
        axis.text.y = element_text(size=12, family="sans"),
        legend.text = element_text(size=10), 
        legend.background = element_blank(),
        legend.key.width = unit(1,"cm"),
        legend.key.height = unit(.5,"cm"),
        legend.position = "inside",
        legend.position.inside = c(0.25,0.8),
        legend.key.spacing.x= unit(.5,"cm"),
        legend.box="vertical",
        legend.direction="vertical",
        legend.box.just="left",
        legend.title=element_blank())

p3 <- ggplot()+
     geom_line(data=plot.lines, mapping=aes(x=Year,y=Data, color=type, linetype=type), linewidth=1.5)+
     #geom_point(data=plot.lines, mapping=aes(x=Year, y=Data, shape=type, fill= type), size=2)+
     theme_bw()+
     labs(x=NULL,y=NULL,title="(c) Food demand by norm (kCal/person/day)")+
     #scale_shape_manual(values=c(19,NA,NA,NA),breaks = c(2000,2020,2040,2060,2080,2100))+
     scale_linetype_manual(values=c("solid","dotdash","dotdash","dotdash"))+
     scale_x_continuous(breaks = seq(1980,2130,20),expand=c(0,0))+
     scale_y_continuous(expand=c(0,0))+
     theme(axis.text.x = element_text(size=12, angle = 90, family="sans"),
        axis.text.y = element_text(size=12, family="sans"),
        legend.text = element_text(size=10),  
        legend.background = element_blank(),
        legend.key.width = unit(1.0,"cm"),
        legend.position = "inside",
        legend.position.inside = c(0.36,0.7),
        legend.key.spacing.x= unit(.5,"cm"),
        legend.direction="vertical",
        legend.title=element_blank())+
  guides(color=guide_legend(nrow=4),byrow=T)

ggarrange(p1, p2, p3, 
          labels = NULL,
          ncol = 2, nrow = 2)

