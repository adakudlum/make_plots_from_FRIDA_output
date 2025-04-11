library(tidyverse)
library("readxl")
library(abind)

remove(list=ls())

setwd('G:/My Drive/R/plots')
# path to directory where the EMB data exist
filepath.frida <- ("G:/My Drive/Data/NumSample/figures/figures/CI-plots/equalyWeighted/plotData/")

##### -- FRIDA data ----
# real GDP
gdp         <- readRDS(paste(filepath.frida,"gdp_real_gdp_in_2021c-fit uncertainty-equaly-weighted.RDS",sep=""))
gdp.frida      <- as.data.frame(gdp$ciBounds)*1e-3
colnames(gdp.frida) <- c("low2","low1","median","high1","high2")
gdp.frida$Year <- as.numeric(gdp$years)
gdp.frida.df <- data.frame(gdp.frida$Year,gdp.frida$median)
colnames(gdp.frida.df) <- c("Year","Data")
gdp.frida.df$Scenario <- "EMB"
gdp.frida.df$Variable  <- "Real GDP in 2021"
gdp.frida.df$Unit   <- "T$/Yr"

# inflation rate
inflation.rate   <- readRDS(paste(filepath.frida,"inflation_inflation_rate-fit uncertainty-equaly-weighted.RDS",sep=""))
inflation.rate.frida <- as.data.frame(inflation.rate$ciBounds)*100
colnames(inflation.rate.frida) <- c("low2","low1","median","high1","high2")
inflation.rate.frida$Year <- as.numeric(inflation.rate$years)
inflation.rate.frida.df <- data.frame(inflation.rate.frida$Year,inflation.rate.frida$median)
colnames(inflation.rate.frida.df) <- c("Year","Data")
inflation.rate.frida.df$Scenario <- "EMB"
inflation.rate.frida.df$Variable  <- "Inflation rate"
inflation.rate.frida.df$Unit   <- "%/Yr"

# Productivity
productivity   <- readRDS(paste(filepath.frida,"employment_productivity-fit uncertainty-equaly-weighted.RDS",sep=""))
productivity.frida <- as.data.frame(productivity$ciBounds)
colnames(productivity.frida) <- c("low2","low1","median","high1","high2")
productivity.frida$Year <- as.numeric(productivity$years)
productivity.frida.df <- data.frame(productivity.frida$Year,productivity.frida$median)
colnames(productivity.frida.df) <- c("Year","Data")
productivity.frida.df$Scenario <- "EMB"
productivity.frida.df$Variable  <- "Productivity"
productivity.frida.df$Unit   <- "1"

# unemployment rate
unemployment.rate   <- readRDS(paste(filepath.frida,"employment_unemployment_rate-fit uncertainty-equaly-weighted.RDS",sep=""))
unemployment.rate.frida <- as.data.frame(unemployment.rate$ciBounds)*100
colnames(unemployment.rate.frida) <- c("low2","low1","median","high1","high2")
unemployment.rate.frida$Year <- as.numeric(unemployment.rate$years)
unemployment.rate.frida.df <- data.frame(unemployment.rate.frida$Year,unemployment.rate.frida$median)
colnames(unemployment.rate.frida.df) <- c("Year","Data")
unemployment.rate.frida.df$Scenario <- "EMB"
unemployment.rate.frida.df$Variable  <- "Unemployment rate"
unemployment.rate.frida.df$Unit   <- "%"

# worker share of net income
worker.share   <- readRDS(paste(filepath.frida,"circular_flow_worker_share_of_net_income-fit uncertainty-equaly-weighted.RDS",sep=""))
worker.share.frida <- as.data.frame(worker.share$ciBounds)
colnames(worker.share.frida) <- c("low2","low1","median","high1","high2")
worker.share.frida$Year <- as.numeric(worker.share$years)
worker.share.frida.df <- data.frame(worker.share.frida$Year,worker.share.frida$median)
colnames(worker.share.frida.df) <- c("Year","Data")
worker.share.frida.df$Scenario <- "EMB"
worker.share.frida.df$Variable  <- "Worker share of net income"
worker.share.frida.df$Unit   <- "%"

calibrated.vars <- as.data.frame(read_excel(paste(filepath.frida,  "frida_calibration.xlsx",sep="")))
gdp.calibration <- data.frame(seq(1980,2130,by=1),t(filter(calibrated.vars, if_any(everything(),~str_detect(.x,"Real GDP in 2021c")))[,-1])*1e-3)
colnames(gdp.calibration) <- c("Year","Data")

gdp.calibration$Scenario <-"Calibration"
gdp.calibration$Variable  <- "Real GDP in 2021"
gdp.calibration$Unit   <- "T$/Yr"

# plot data

plot.gdp <- map_df(.x=list(gdp.frida.df,
                           gdp.calibration),
                   .f=bind_rows)

plot.inflation <- inflation.rate.frida.df
plot.productivity <- productivity.frida.df
plot.unemployment <- unemployment.rate.frida.df
plot.worker.share <- worker.share.frida.df

data.for.lines <- map_df(.x=list("(a) Real GDP (2021c T$/Yr)"=plot.gdp,
                                 "(b) Inflation rate (%/Yr)"=plot.inflation,
                                 "(c) Productivity (dmnl)"=plot.productivity,
                                 "(d) Unemployment rate (%)"=plot.unemployment,
                                 "(e) Worker share of net income (%)"=plot.worker.share),
                         .f=bind_rows,.id="name")

# data for shading
plot.data.ci <- map_df(.x=list("(a) Real GDP (2021c T$/Yr)"=gdp.frida,
                               "(b) Inflation rate (%/Yr)"=inflation.rate.frida,
                               "(c) Productivity (dmnl)"=productivity.frida,
                               "(d) Unemployment rate (%)"=unemployment.rate.frida,
                               "(e) Worker share of net income (%)"=worker.share.frida),
                       .f=bind_rows,.id="name")


ggplot()+
  geom_line(data=data.for.lines,mapping=aes(x=as.numeric(Year),y=Data,
                                           color=Scenario,linetype=Scenario,linewidth=Scenario))+
  geom_point(data=data.for.lines,mapping=aes(x=as.numeric(Year),y=Data,
                                            shape=Scenario, size=Scenario, fill=Scenario))+
  facet_wrap(~name,scale="free_y")+
  geom_ribbon(data=plot.data.ci,aes(x=Year,ymax=high1,ymin=low1),alpha=0.4)+
  geom_ribbon(data=plot.data.ci, aes(x=Year,ymax=high2,ymin=low2),alpha=0.2)+
  theme_bw()+
  scale_color_manual(values=c("red","black"))+
  scale_linetype_manual(values=c("dotted","solid"))+
  scale_linewidth_manual(values=c(1,1))+
  scale_shape_manual(values=c(21,NA))+
  scale_size_manual(values=c(1.5,NA))+
  scale_fill_manual(values=c("red",NA))+
  labs(x=NULL,y=NULL,title=NULL)+
  scale_x_continuous(breaks = seq(1980,2130,20),expand=c(0,0))+
  theme(strip.background =element_rect(fill="white"),
        strip.text = element_text(size=13, family="serif"),
        axis.text.x = element_text(size=12, angle = 90, family="serif"),
        axis.text.y = element_text(size=12, family="serif"),
        legend.text = element_text(size=12), 
        legend.key.width = unit(1.0,"cm"),
        legend.position = "inside",
        legend.position.inside = c(0.11,0.8),
        legend.key.spacing.x= unit(1.5,"cm"),
        legend.box="vertical",
        legend.box.just="left",
        legend.title=element_blank()
  )
