library(tidyverse)
library("readxl")
library(abind)
library(ggpubr)

remove(list=ls())

setwd('G:/My Drive/R/plots')
# path to directory where the EMB and carbon tax experiment data exist
filepath.frida.exp1 <- ("G:/My Drive/Data/NumSample/plotData_EXP1/")
filepath.frida.emb <- ("G:/My Drive/Data/NumSample/plotData_EMB/")

file.names    <- c("gdp_real_gdp_in_2021c-fit uncertainty-equaly-weighted.RDS",
                   "demographics_population-fit uncertainty-equaly-weighted.RDS",
                   "energy_balance_model_surface_temperature_anomaly-fit uncertainty-equaly-weighted.RDS",
                   "forcing_total_effective_radiative_forcing-fit uncertainty-equaly-weighted.RDS")
variable.names <- c("(a) GDP (2021c t$/Yr)", "(b) Population (billion)","(c) Surface Temperature Anomaly (°C)","(d) Total Radiative Forcing (Wm-2)")


emb.data <- list(NA)
emb.data <- sapply(1:length(variable.names), function(i) {
                  data <- readRDS(paste(filepath.frida.emb, file.names[i],sep=""))
                  if (str_detect(variable.names[i],"GDP") || str_detect(variable.names[i],"Population")) { 
                  conv.factor <- 1e-3 } else {conv.factor <- 1}
                  emb.data[[variable.names[i]]] <- as.data.frame(data$ciBounds)*conv.factor
                  }, simplify=FALSE, USE.NAMES = TRUE)
names(emb.data) <- variable.names
emb.data.df <- as.data.frame(do.call(rbind, emb.data))
colnames(emb.data.df) <- c("low2","low1","median","high1","high2")
emb.data.df$variable <- rownames(emb.data.df)
emb.data.df <- tidyr::separate(emb.data.df, variable, into = c('variable', 'year'), sep = '\\.')

exp1.data <- list(NA)
exp1.data <- sapply(1:length(variable.names), function(i) {
  data <- readRDS(paste(filepath.frida.exp1, file.names[i],sep=""))
  if (str_detect(variable.names[i],"GDP") || str_detect(variable.names[i],"Population")) { 
    conv.factor <- 1e-3 } else {conv.factor <- 1}
  exp1.data[[variable.names[i]]] <- as.data.frame(data$ciBounds)*conv.factor
}, simplify=FALSE, USE.NAMES = TRUE)
names(exp1.data) <- variable.names
exp1.data.df <- as.data.frame(do.call(rbind, exp1.data))
colnames(exp1.data.df) <- c("low2","low1","median","high1","high2")
exp1.data.df$variable <- rownames(exp1.data.df)
exp1.data.df <- tidyr::separate(exp1.data.df, variable, into = c('variable', 'year'), sep = '\\.')

# Calibrated variables
calibrated.vars <- as.data.frame(read_excel(paste(filepath.frida.emb,  "frida_calibration.xlsx",sep="")))
sta.calibration <- data.frame(as.character(seq(1980,2130,by=1)),t(filter(calibrated.vars, if_any(everything(),~str_detect(.x,"Surface Temperature Anomaly")))[,-1]))
gdp.calibration <- data.frame(as.character(seq(1980,2130,by=1)),t(filter(calibrated.vars, if_any(everything(),~str_detect(.x,"Real GDP in 2021c")))[,-1])*1e-3)
population.calibration <- data.frame(as.character(seq(1980,2130,by=1)),t(filter(calibrated.vars, if_any(everything(),~str_detect(.x,"Population")))[,-1])*1e-3)
colnames(sta.calibration) <- colnames(gdp.calibration) <- colnames(population.calibration) <- c("year","median")

# combine calibration data
plot.calibration <- map_df(.x=list("(a) GDP (2021c t$/Yr)"=gdp.calibration,
                                "(b) Population (billion)"=population.calibration,
                                "(c) Surface Temperature Anomaly (°C)"=sta.calibration),
                        .f=bind_rows, .id="variable")

# prepare subplots medians
plot.data.ci <- map_df(.x=list("EMB"=emb.data.df,
                            "Carbon tax exp."=exp1.data.df),
                       .f=bind_rows, .id="Experiment")

plot.data.median <- map_df(.x=list("EMB"=emb.data.df,
                                   "Carbon tax exp."=exp1.data.df,
                                   "Calibration"=plot.calibration),
                           .f=bind_rows, .id="Experiment")

# make the figures
ggplot()+
  geom_line(data=plot.data.median,mapping=aes(x=year,y=median,
                                            color=Experiment,linetype=Experiment,group=Experiment,color=Experiment,linewidth=Experiment),linetype=1)+
  geom_point(data=plot.data.median,mapping=aes(x=year,y=median,
                                            shape=Experiment, size=Experiment, group=Experiment,color=Experiment))+
  facet_wrap(~variable,scale="free_y")+
  geom_ribbon(data=plot.data.ci, aes(x=year, ymin=low1, ymax=high1, group=Experiment, fill=Experiment), alpha=0.2)+
  theme_bw()+
  scale_color_manual(values=c("red","green","black"))+
  scale_linewidth_manual(values=c(.5,1.5,1.5))+
  scale_shape_manual(values=c(21,NA,NA))+
  scale_size_manual(values=c(1.5,NA,NA))+
  #scale_fill_manual(values=c("red",NA,NA))+
  scale_fill_manual(values=c("green","grey"))+
  labs(x=NULL,y=NULL,title=NULL)+
  scale_x_discrete(breaks = seq(1980,2130,20),expand=c(0,0))+
  theme(strip.background =element_rect(fill="white"),
        strip.text = element_text(size=13, family="serif"),
        axis.text.x = element_text(size=13, angle = 90, family="serif"),
        axis.text.y = element_text(size=13, family="serif"),
        legend.background=element_blank(),
        legend.text = element_text(size=12), 
        legend.key.width = unit(1.0,"cm"),
        legend.position = "inside",
        legend.position.inside = c(0.15,0.8),
        legend.key.spacing.x= unit(1.5,"cm"),
        legend.box="vertical",
        legend.box.just="left",
        legend.title=element_blank()
  )
