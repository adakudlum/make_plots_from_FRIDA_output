library(tidyverse)
library("readxl")
library(abind)
library(ggpubr)

remove(list=ls())

setwd('G:/My Drive/R/plots')
# path to directory where the EMB data exists
filepath.frida <- ("G:/My Drive/Data/NumSample/plotData_EMB/")

# Files/variables
files.investments.on.extraction <- c("energy_investments_coal_gross_investments_in_fossil_fuel_extraction_capital-fit uncertainty-equaly-weighted.RDS",
                "energy_investments_oil_gross_investments_in_fossil_fuel_extraction_capital-fit uncertainty-equaly-weighted.RDS",
                "energy_investments_gas_gross_investments_in_fossil_fuel_extraction_capital-fit uncertainty-equaly-weighted.RDS",
                "energy_investments_gross_investments_in_solar_and_wind_energy_capacity-fit uncertainty-equaly-weighted.RDS",
                "energy_investments_investments_in_nuclear_capacity-fit uncertainty-equaly-weighted.RDS",
                "energy_investments_gross_investments_in_biofuel_capacity-fit uncertainty-equaly-weighted.RDS",
                "energy_investments_gross_investments_in_hydropower_energy_capacity-fit uncertainty-equaly-weighted.RDS",
                "energy_investments_gross_investments_in_other_capacity-fit uncertainty-equaly-weighted.RDS")
variables.extraction <- c("Coal","Oil","Gas","Solar/Wind","Nuclear","Biofuel","Hydropower","Other") 

files.investments.on.conversion <- c("energy_investments_coal_gross_investments_in_fossil_energy_capital-fit uncertainty-equaly-weighted.RDS",
                                     "energy_investments_oil_gross_investments_in_fossil_energy_capital-fit uncertainty-equaly-weighted.RDS",
                                     "energy_investments_gas_gross_investments_in_fossil_energy_capital-fit uncertainty-equaly-weighted.RDS")
variables.conversion <- c("Coal_to_energy","Oil_to_energy","Gas_to_energy") 

files.energy.output <- c("fossil_energy_coal_secondary_fossil_energy_output-fit uncertainty-equaly-weighted.RDS",
                         "fossil_energy_oil_secondary_fossil_energy_output-fit uncertainty-equaly-weighted.RDS",
                         "fossil_energy_gas_secondary_fossil_energy_output-fit uncertainty-equaly-weighted.RDS",
                         "wind_and_solar_energy_wind_and_solar_energy_output-fit uncertainty-equaly-weighted.RDS",
                         "hydropower_energy_hydropower_energy_output-fit uncertainty-equaly-weighted.RDS",
                         "nuclear_energy_nuclear_energy_output-fit uncertainty-equaly-weighted.RDS",
                         "bio_fuel_energy_bio_fuel_secondary_energy_output-fit uncertainty-equaly-weighted.RDS",
                         "other_energy_other_energy_output-fit uncertainty-equaly-weighted.RDS")

variables.output <- c("Coal","Oil","Gas","Solar/Wind","Hydropower","Nuclear","Biofuel","Other")

# Read and process the variables - extraction capital
extraction.data <- list(NA)
extraction.data <- sapply(1:length(variables.extraction), function(i) {
                  data <- readRDS(paste(filepath.frida,files.investments.on.extraction[i],sep=""))
                  extraction.data[[variables.extraction[i]]] <- as.data.frame(data$ciBounds)
                  }, simplify=FALSE, USE.NAMES = TRUE)
names(extraction.data)<- variables.extraction
extraction.data.df <- as.data.frame(do.call(rbind,extraction.data), stringAsFactors=True)
colnames(extraction.data.df) <- c("low2","low1","median","high1","high2")
extraction.data.df$variable <- rownames(extraction.data.df)
extraction.data.df <- tidyr::separate(extraction.data.df, variable, into = c('variable', 'year'), sep = '\\.')

# Read and process the variables - conversion capital
conversion.data <- list(NA)
conversion.data <- sapply(1:length(variables.conversion), function(i) {
  data <- readRDS(paste(filepath.frida, files.investments.on.conversion[i],sep=""))
  conversion.data[[variables.conversion[i]]] <- as.data.frame(data$ciBounds)
}, simplify=FALSE, USE.NAMES = TRUE)
names(conversion.data)<- variables.conversion
conversion.data.df <- as.data.frame(do.call(rbind,conversion.data), stringAsFactors=True)
colnames(conversion.data.df) <- c("low2","low1","median","high1","high2")
conversion.data.df$variable <- rownames(conversion.data.df)
conversion.data.df <- tidyr::separate(conversion.data.df, variable, into = c('variable', 'year'), sep = '\\.')

# Read and process the variables - energy output
output.data <- list(NA)
output.data <- sapply(1:length(variables.output), function(i) {
  data <- readRDS(paste(filepath.frida, files.energy.output[i],sep=""))
  output.data[[variables.output[i]]] <- as.data.frame(data$ciBounds)
}, simplify=FALSE, USE.NAMES = TRUE)
names(output.data)<- variables.output
output.data.df <- as.data.frame(do.call(rbind,output.data), stringAsFactors=True)
colnames(output.data.df) <- c("low2","low1","median","high1","high2")
output.data.df$variable <- rownames(output.data.df)
output.data.df <- tidyr::separate(output.data.df, variable, into = c('variable', 'year'), sep = '\\.')

# create the plots
p1 <- ggplot(extraction.data.df) +
  geom_area(mapping=aes(x=as.numeric(year),y=median*1e-12,fill=factor(variable)),
            color="grey4",linetype=1, linewidth=1,show.legend=TRUE,
            position=position_stack(reverse = T))+
  theme_bw()+
  labs(x=NULL,y=NULL,title="(a) Energy investments by type (2021c T$/Yr)")+
  #scale_fill_manual(values=c("green","grey","orange4","darkgreen", "red","red4","magenta","yellow4"))+
  #scale_color_manual(values=c("grey","brown"))+
  #scale_linetype_manual(values=c("dashed","dashed"))+
  scale_x_continuous(breaks = seq(1980,2130,20),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme(axis.text.x = element_text(size=12, angle = 90, family="sans"),
        axis.text.y = element_text(size=12, family="sans"),
        legend.text = element_text(size=11), 
        legend.key.width = unit(.3,"cm"),
        legend.key.height = unit(.1,"cm"),
        legend.position = "bottom",
        legend.key.spacing.x= unit(.5,"cm"),
        legend.title=element_blank())+
  guides(fill=guide_legend(nrow=2,ncol=4))

p2 <- ggplot(data=output.data.df) +
  geom_area(mapping=aes(x=as.numeric(year),y=median*1e-3, fill=factor(variable)),
            position=position_stack(reverse = T),
            color="black",linetype=1, linewidth=1,show.legend=TRUE)+
  theme_bw()+
  labs(x=NULL,y=NULL,title="(b) Energy output by type (PWh/Yr)")+
  #scale_fill_manual(values=c("green","grey","orange4","darkgreen", "red","red4","magenta","yellow4"))+
  scale_x_continuous(breaks = seq(1980,2130,20),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme(axis.text.x = element_text(size=12, angle = 90, family="sans"),
        axis.text.y = element_text(size=12, family="sans"),
        legend.text = element_text(size=11), 
        legend.key.width = unit(.3,"cm"),
        legend.key.height = unit(.1,"cm"),
        legend.position = "bottom",
        legend.key.spacing.x= unit(.5,"cm"),
        legend.direction="horizontal",
        legend.box.just="left",
        legend.title=element_blank())+
  guides(fill=guide_legend(nrow=2,ncol=4))

p3 <- ggplot(data=conversion.data.df) +
  geom_area(mapping=aes(x=as.numeric(year),y=median*1e-12,fill=factor(variable)),
            color="grey4",linetype=1, linewidth=1,show.legend=TRUE, 
            position=position_stack(reverse = T))+
  theme_bw()+
  labs(x=NULL,y=NULL,title="(c) Fossil energy conversion capital (2021c T$/Yr)")+
  scale_fill_manual(values=c("orange3","green4","lightblue"))+
  scale_x_continuous(breaks = seq(1980,2130,20),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme(axis.text.x = element_text(size=12, angle = 90, family="sans"),
        axis.text.y = element_text(size=12, family="sans"),
        legend.text = element_text(size=11), 
        legend.key.width = unit(.3,"cm"),
        legend.key.height = unit(.1,"cm"),
        legend.position = "bottom",
        #legend.position.inside = c(0.2,0.8),
        legend.key.spacing.x= unit(1,"cm"),
        legend.box.just="left",
        legend.title=element_blank())+
  guides(fill=guide_legend(nrow=1,ncol=3))

ggarrange(p1, p2, p3,
          labels = NULL,
          ncol = 2, nrow = 2)
