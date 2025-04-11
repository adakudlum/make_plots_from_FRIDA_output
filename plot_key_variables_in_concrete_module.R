library(tidyverse)
library("readxl")
library(abind)

remove(list=ls())

setwd('G:/My Drive/R/plots')
# path to directory where the EMB data exist
filepath.frida <- ("G:/My Drive/Data/NumSample/figures/figures/CI-plots/equalyWeighted/plotData/")

##### -- FRIDA data ----
# Yearly concrete use
concrete.use         <- readRDS(paste(filepath.frida,"concrete_total_yearly_concrete_use-equaly-weighted.RDS",sep=""))
concrete.use.frida      <- as.data.frame(concrete.use$ciBounds)*1e-3
colnames(concrete.use.frida) <- c("low2","low1","median","high1","high2")
concrete.use.frida$Year <- as.numeric(concrete.use$years)
concrete.use.frida.df <- data.frame(concrete.use.frida$Year,concrete.use.frida$median)
colnames(concrete.use.frida.df) <- c("Year","Data")
concrete.use.frida.df$Scenario <- "EMB"
concrete.use.frida.df$Variable  <- "yearly concrete use"
concrete.use.frida.df$Unit   <- "Gt/Yr"

# in use concrete use
concrete.stock   <- readRDS(paste(filepath.frida,"concrete_in_use_concrete_stock-equaly-weighted.RDS",sep=""))
concrete.stock.frida <- as.data.frame(concrete.stock$ciBounds)*1e-3
colnames(concrete.stock.frida) <- c("low2","low1","median","high1","high2")
concrete.stock.frida$Year <- as.numeric(concrete.stock$years)
concrete.stock.frida.df <- data.frame(concrete.stock.frida$Year,concrete.stock.frida$median)
colnames(concrete.stock.frida.df) <- c("Year","Data")
concrete.stock.frida.df$Scenario <- "EMB"
concrete.stock.frida.df$Variable  <- "concrete stock"
concrete.stock.frida.df$Unit   <- "Gt"

# CO2 emissions
co2.emissions   <- readRDS(paste(filepath.frida,"concrete_co2_emissions-equaly-weighted.RDS",sep=""))
co2.emissions.frida <- as.data.frame(co2.emissions$ciBounds)*1e-3
colnames(co2.emissions.frida) <- c("low2","low1","median","high1","high2")
co2.emissions.frida$Year <- as.numeric(co2.emissions$years)
co2.emissions.frida.df <- data.frame(co2.emissions.frida$Year,co2.emissions.frida$median)
colnames(co2.emissions.frida.df) <- c("Year","Data")
co2.emissions.frida.df$Scenario <- "EMB"
co2.emissions.frida.df$Variable  <- "CO2 emissions from concrete"
co2.emissions.frida.df$Unit   <- "GtCO2/Yr"


calibrated.vars <- as.data.frame(read_excel(paste(filepath.frida,  "frida_calibration.xlsx",sep="")))
concrete.use.calibration <- data.frame(seq(1980,2130,by=1),t(filter(calibrated.vars, if_any(everything(),~str_detect(.x,"total yearly concrete use")))[,-1])*1e-3)
concrete.stock.calibration <- data.frame(seq(1980,2130,by=1),t(filter(calibrated.vars, if_any(everything(),~str_detect(.x,"In use concrete stock")))[,-1])*1e-3)
co2.emissions.calibration <- data.frame(seq(1980,2130,by=1),t(filter(calibrated.vars, if_any(everything(),~str_detect(.x,"CO2 emissions")))[,-1])*1e-3)

colnames(concrete.use.calibration) <- colnames(concrete.stock.calibration) <- colnames(co2.emissions.calibration) <- c("Year","Data")

concrete.use.calibration$Scenario <- "Calibration"
concrete.use.calibration$Variable  <- "yearly concrete use"
concrete.use.calibration$Unit   <- "Gt/Yr"

co2.emissions.calibration$Scenario <- "Calibration"
co2.emissions.calibration$Variable  <- "CO2 emissions from concrete"
co2.emissions.calibration$Unit   <- "GtCO2/Yr"


concrete.stock.calibration$Scenario <- "Calibration"
concrete.stock.calibration$Variable  <- "concrete stock"
concrete.stock.calibration$Unit   <- "Gt"


# plot data

plot.concrete.use <- map_df(.x=list(concrete.use.frida.df,
                           concrete.use.calibration),
                   .f=bind_rows)
plot.concrete.stock <- map_df(.x=list(concrete.stock.frida.df,
                           concrete.stock.calibration),
                   .f=bind_rows)
plot.co2.emissions <- map_df(.x=list(co2.emissions.frida.df,
                           co2.emissions.calibration),
                   .f=bind_rows)


data.for.lines <- map_df(.x=list("(a) Concrete production (Gt/Yr)"=plot.concrete.use,
                                 "(b) In use concrete (Gt)"=plot.concrete.stock,
                                 "(c) concrete CO2 emissions (GtCO2/Yr)"=plot.co2.emissions),
                         .f=bind_rows,.id="name")

# data for shading
plot.data.ci <- map_df(.x=list("(a) Concrete production (Gt/Yr)"=concrete.use.frida,
                               "(b) In use concrete (Gt)"=concrete.stock.frida,
                               "(c) concrete CO2 emissions (GtCO2/Yr)"=co2.emissions.frida),
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
        strip.text = element_text(size=11, family="serif"),
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
