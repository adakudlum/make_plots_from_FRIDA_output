library(tidyverse)
library("readxl")
library(abind)

remove(list=ls())

setwd('G:/My Drive/R/plots')
# path to directory where the EMB data exists
filepath.frida <- ("G:/My Drive/Data/NumSample/plotData_EMB/")

# CO2 emissions
co2.concentration         <- readRDS(paste(filepath.frida,"co2_forcing_atmospheric_co2_concentration-fit uncertainty-equaly-weighted.RDS",sep=""))
co2.concentration.frida      <- as.data.frame(co2.concentration$ciBounds)
colnames(co2.concentration.frida) <- c("low2","low1","median","high1","high2")
co2.concentration.frida$Year <- as.numeric(co2.concentration$years)
co2.concentration.frida.df <- data.frame(co2.concentration.frida$Year,co2.concentration.frida$median)
colnames(co2.concentration.frida.df) <- c("Year","Data")
co2.concentration.frida.df$Scenario <- "EMB"
co2.concentration.frida.df$Variable  <- "Atm. CO2 concentration"
co2.concentration.frida.df$Unit   <- "ppm"

# Sea level rise
slr   <- readRDS("G:/My Drive/Data/NumSample/figures/figures/CI-plots/equalyWeighted/plotData/sea_level_total_global_slr-equaly-weighted.RDS")
slr.frida <- as.data.frame(slr$ciBounds)
colnames(slr.frida) <- c("low2","low1","median","high1","high2")
slr.frida$Year <- as.numeric(slr$years)
slr.frida.df <- data.frame(slr.frida$Year,slr.frida$median)
colnames(slr.frida.df) <- c("Year","Data")
slr.frida.df$Scenario <- "EMB"
slr.frida.df$Variable  <- "Sea Level Rise"
slr.frida.df$Unit   <- "meters"

# Surface temperature anomaly
sta             <- readRDS(paste(filepath.frida,"energy_balance_model_surface_temperature_anomaly-equaly-weighted.RDS",sep=""))
sta.frida      <- as.data.frame(sta$ciBounds)
colnames(sta.frida) <- c("low2","low1","median","high1","high2")
sta.frida$Year <- as.numeric(sta$years)
sta.frida.df <- data.frame(sta.frida$Year,sta.frida$median)
colnames(sta.frida.df) <- c("Year","Data")
sta.frida.df$Scenario <- "EMB"
sta.frida.df$Variable  <- "Surface Temperature Anomaly"
sta.frida.df$Unit   <- "C"

calibrated.vars <- as.data.frame(read_excel(paste(filepath.frida,  "frida_calibration.xlsx",sep="")))
sta.calibration <- data.frame(seq(1980,2130,by=1),t(filter(calibrated.vars, if_any(everything(),~str_detect(.x,"Surface Temperature Anomaly")))[,-1]))
slr.calibration <- data.frame(seq(1980,2130,by=1),t(filter(calibrated.vars, if_any(everything(),~str_detect(.x,"Total global SLR")))[,-1]))
co2.calibration <- data.frame(seq(1980,2130,by=1),t(filter(calibrated.vars, if_any(everything(),~str_detect(.x,"Atmospheric CO2 Concentration")))[,-1]))
colnames(slr.calibration) <- colnames(sta.calibration) <- colnames(co2.calibration) <- c("Year","Data")

sta.calibration$Scenario <-"Calibration"
sta.calibration$Variable  <- "Surface Temperature Anomaly"
sta.calibration$Unit   <- "C"

co2.calibration$Scenario <-"Calibration"
co2.calibration$Variable  <- "Atm. CO2 concentration"
co2.calibration$Unit   <- "ppm"

slr.calibration$Scenario <-"Calibration"
slr.calibration$Variable  <- "Sea Level Rise"
slr.calibration$Unit   <- "meters"

# Ocean surface pH (take the median of cold/warm surface pH and add)
# Cold surface pH
cold.ph   <- readRDS(paste(filepath.frida,"ocean_cold_surface_ocean_ph-fit uncertainty-equaly-weighted.RDS",sep=""))
cold.ph.frida <- as.data.frame(cold.ph$ciBounds)
colnames(cold.ph.frida) <- c("low2","low1","median","high1","high2")

# warm surface pH
warm.ph             <- readRDS(paste(filepath.frida,"ocean_warm_surface_ocean_ph-fit uncertainty-equaly-weighted.RDS",sep=""))
warm.ph.frida      <- as.data.frame(warm.ph$ciBounds)
colnames(warm.ph.frida) <- c("low2","low1","median","high1","high2")


# Estimate mean ocean surface pH
mean.surface.pH.frida <- data.frame(apply(rbind(cold.ph.frida$low2,warm.ph.frida$low2),2,mean),
                                    apply(rbind(cold.ph.frida$low1,warm.ph.frida$low1),2,mean),
                                    apply(rbind(cold.ph.frida$median,warm.ph.frida$median),2,mean),
                                    apply(rbind(cold.ph.frida$high1,warm.ph.frida$high1),2,mean),
                                    apply(rbind(cold.ph.frida$high2,warm.ph.frida$high2),2,mean),
                                    seq(1980,2130,by=1))
colnames(mean.surface.pH.frida) <- c("low2","low1","median","high1","high2","Year")
mean.surface.pH.frida.df <- data.frame(seq(1980,2130,by=1),mean.surface.pH.frida$median)
colnames(mean.surface.pH.frida.df) <- c("Year","Data")
mean.surface.pH.frida.df$Scenario <- "EMB"
mean.surface.pH.frida.df$Variable  <- "Mean ocean surface pH"
mean.surface.pH.frida.df$Unit   <- "1"

# plot data

plot.sta <- map_df(.x=list(sta.frida.df,
                           sta.calibration),
                   .f=bind_rows)

plot.co2 <- map_df(.x=list(co2.concentration.frida.df,
                           co2.calibration),
                   .f=bind_rows)

plot.slr <- map_df(.x=list(slr.frida.df,
                           slr.calibration),
                   .f=bind_rows)

plot.ph <-  map_df(.x=list(mean.surface.pH.frida.df),
                   .f=bind_rows)


data.for.lines <- map_df(.x=list("(a) Atm. CO2 concentration (ppm)"=plot.co2,
                                 "(b) Surface Temp. Anomaly (°C)"=plot.sta,
                                 "(c) Sea Level Rise (meters)"=plot.slr,
                                 "(d) Mean Surface Ocean pH"=plot.ph),
                         .f=bind_rows,.id="name")

# data for shading
plot.data.ci <- map_df(.x=list("(a) Atm. CO2 concentration (ppm)"=co2.concentration.frida,
                               "(b) Surface Temp. Anomaly (°C)"=sta.frida,
                               "(c) Sea Level Rise (meters)"=slr.frida,
                               "(d) Mean Surface Ocean pH"=mean.surface.pH.frida),
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