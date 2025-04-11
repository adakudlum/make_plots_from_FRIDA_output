library(tidyverse)
library("readxl")
library(abind)
library(ggpubr)

remove(list=ls())

setwd('G:/My Drive/R/plots')
# path to directory where the EMB and carbon tax experiment data exist
filepath.frida.exp1 <- ("G:/My Drive/Data/NumSample/plotData_EXP1/")
filepath.frida.emb <- ("G:/My Drive/Data/NumSample/plotData_EMB/")

# Energy output by type - EXP1 
coal.output         <- readRDS(paste(filepath.frida.exp1,"fossil_energy_coal_secondary_fossil_energy_output-fit uncertainty-equaly-weighted.RDS",sep=""))
coal.output.frida.exp1      <- as.data.frame(coal.output$ciBounds)*1e-3
colnames(coal.output.frida.exp1) <- c("low2","low1","median","high1","high2")
coal.output.frida.exp1$Year <- as.numeric(coal.output$years)
coal.output.frida.exp1.df <- data.frame(coal.output.frida.exp1$Year,coal.output.frida.exp1$median)
colnames(coal.output.frida.exp1.df) <- c("Year","Data")

gas.output         <- readRDS(paste(filepath.frida.exp1,"fossil_energy_gas_secondary_fossil_energy_output-fit uncertainty-equaly-weighted.RDS",sep=""))
gas.output.frida.exp1      <- as.data.frame(gas.output$ciBounds)*1e-3
colnames(gas.output.frida.exp1) <- c("low2","low1","median","high1","high2")
gas.output.frida.exp1$Year <- as.numeric(gas.output$years)
gas.output.frida.exp1.df <- data.frame(gas.output.frida.exp1$Year,gas.output.frida.exp1$median)
colnames(gas.output.frida.exp1.df) <- c("Year","Data")

oil.output         <- readRDS(paste(filepath.frida.exp1,"fossil_energy_oil_secondary_fossil_energy_output-fit uncertainty-equaly-weighted.RDS",sep=""))
oil.output.frida.exp1      <- as.data.frame(oil.output$ciBounds)*1e-3
colnames(oil.output.frida.exp1) <- c("low2","low1","median","high1","high2")
oil.output.frida.exp1$Year <- as.numeric(oil.output$years)
oil.output.frida.exp1.df <- data.frame(oil.output.frida.exp1$Year,oil.output.frida.exp1$median)
colnames(oil.output.frida.exp1.df) <- c("Year","Data")

renewable.output         <- readRDS(paste(filepath.frida.exp1,"wind_and_solar_energy_wind_and_solar_energy_output-fit uncertainty-equaly-weighted.RDS",sep=""))
renewable.output.frida.exp1      <- as.data.frame(renewable.output$ciBounds)*1e-3
colnames(renewable.output.frida.exp1) <- c("low2","low1","median","high1","high2")
renewable.output.frida.exp1$Year <- as.numeric(renewable.output$years)
renewable.output.frida.exp1.df <- data.frame(renewable.output.frida.exp1$Year,renewable.output.frida.exp1$median)
colnames(renewable.output.frida.exp1.df) <- c("Year","Data")

hydro.output         <- readRDS(paste(filepath.frida.exp1,"hydropower_energy_hydropower_energy_output-fit uncertainty-equaly-weighted.RDS",sep=""))
hydro.output.frida.exp1      <- as.data.frame(hydro.output$ciBounds)*1e-3
colnames(hydro.output.frida.exp1) <- c("low2","low1","median","high1","high2")
hydro.output.frida.exp1$Year <- as.numeric(hydro.output$years)
hydro.output.frida.exp1.df <- data.frame(hydro.output.frida.exp1$Year,hydro.output.frida.exp1$median)
colnames(hydro.output.frida.exp1.df) <- c("Year","Data")

nuclear.output         <- readRDS(paste(filepath.frida.exp1,"nuclear_energy_nuclear_energy_output-fit uncertainty-equaly-weighted.RDS",sep=""))
nuclear.output.frida.exp1      <- as.data.frame(nuclear.output$ciBounds)*1e-3
colnames(nuclear.output.frida.exp1) <- c("low2","low1","median","high1","high2")
nuclear.output.frida.exp1$Year <- as.numeric(nuclear.output$years)
nuclear.output.frida.exp1.df <- data.frame(nuclear.output.frida.exp1$Year,nuclear.output.frida.exp1$median)
colnames(nuclear.output.frida.exp1.df) <- c("Year","Data")

bio.output         <- readRDS(paste(filepath.frida.exp1,"bio_fuel_energy_bio_fuel_secondary_energy_output-fit uncertainty-equaly-weighted.RDS",sep=""))
bio.output.frida.exp1      <- as.data.frame(bio.output$ciBounds)*1e-3
colnames(bio.output.frida.exp1) <- c("low2","low1","median","high1","high2")
bio.output.frida.exp1$Year <- as.numeric(bio.output$years)
bio.output.frida.exp1.df <- data.frame(bio.output.frida.exp1$Year,bio.output.frida.exp1$median)
colnames(bio.output.frida.exp1.df) <- c("Year","Data")

other.output         <- readRDS(paste(filepath.frida.exp1,"other_energy_other_energy_output-fit uncertainty-equaly-weighted.RDS",sep=""))
other.output.frida.exp1      <- as.data.frame(other.output$ciBounds)*1e-3
colnames(other.output.frida.exp1) <- c("low2","low1","median","high1","high2")
other.output.frida.exp1$Year <- as.numeric(other.output$years)
other.output.frida.exp1.df <- data.frame(other.output.frida.exp1$Year,other.output.frida.exp1$median)
colnames(other.output.frida.exp1.df) <- c("Year","Data")

# Energy output by type - EMB 
coal.output         <- readRDS(paste(filepath.frida.emb,"fossil_energy_coal_secondary_fossil_energy_output-fit uncertainty-equaly-weighted.RDS",sep=""))
coal.output.frida.emb      <- as.data.frame(coal.output$ciBounds)*1e-3
colnames(coal.output.frida.emb) <- c("low2","low1","median","high1","high2")
coal.output.frida.emb$Year <- as.numeric(coal.output$years)
coal.output.frida.emb.df <- data.frame(coal.output.frida.emb$Year,coal.output.frida.emb$median)
colnames(coal.output.frida.emb.df) <- c("Year","Data")

gas.output         <- readRDS(paste(filepath.frida.emb,"fossil_energy_gas_secondary_fossil_energy_output-fit uncertainty-equaly-weighted.RDS",sep=""))
gas.output.frida.emb      <- as.data.frame(gas.output$ciBounds)*1e-3
colnames(gas.output.frida.emb) <- c("low2","low1","median","high1","high2")
gas.output.frida.emb$Year <- as.numeric(gas.output$years)
gas.output.frida.emb.df <- data.frame(gas.output.frida.emb$Year,gas.output.frida.emb$median)
colnames(gas.output.frida.emb.df) <- c("Year","Data")

oil.output         <- readRDS(paste(filepath.frida.emb,"fossil_energy_oil_secondary_fossil_energy_output-fit uncertainty-equaly-weighted.RDS",sep=""))
oil.output.frida.emb      <- as.data.frame(oil.output$ciBounds)*1e-3
colnames(oil.output.frida.emb) <- c("low2","low1","median","high1","high2")
oil.output.frida.emb$Year <- as.numeric(oil.output$years)
oil.output.frida.emb.df <- data.frame(oil.output.frida.emb$Year,oil.output.frida.emb$median)
colnames(oil.output.frida.emb.df) <- c("Year","Data")

renewable.output         <- readRDS(paste(filepath.frida.emb,"wind_and_solar_energy_wind_and_solar_energy_output-fit uncertainty-equaly-weighted.RDS",sep=""))
renewable.output.frida.emb      <- as.data.frame(renewable.output$ciBounds)*1e-3
colnames(renewable.output.frida.emb) <- c("low2","low1","median","high1","high2")
renewable.output.frida.emb$Year <- as.numeric(renewable.output$years)
renewable.output.frida.emb.df <- data.frame(renewable.output.frida.emb$Year,renewable.output.frida.emb$median)
colnames(renewable.output.frida.emb.df) <- c("Year","Data")

hydro.output         <- readRDS(paste(filepath.frida.emb,"hydropower_energy_hydropower_energy_output-fit uncertainty-equaly-weighted.RDS",sep=""))
hydro.output.frida.emb      <- as.data.frame(hydro.output$ciBounds)*1e-3
colnames(hydro.output.frida.emb) <- c("low2","low1","median","high1","high2")
hydro.output.frida.emb$Year <- as.numeric(hydro.output$years)
hydro.output.frida.emb.df <- data.frame(hydro.output.frida.emb$Year,hydro.output.frida.emb$median)
colnames(hydro.output.frida.emb.df) <- c("Year","Data")

nuclear.output         <- readRDS(paste(filepath.frida.emb,"nuclear_energy_nuclear_energy_output-fit uncertainty-equaly-weighted.RDS",sep=""))
nuclear.output.frida.emb      <- as.data.frame(nuclear.output$ciBounds)*1e-3
colnames(nuclear.output.frida.emb) <- c("low2","low1","median","high1","high2")
nuclear.output.frida.emb$Year <- as.numeric(nuclear.output$years)
nuclear.output.frida.emb.df <- data.frame(nuclear.output.frida.emb$Year,nuclear.output.frida.emb$median)
colnames(nuclear.output.frida.emb.df) <- c("Year","Data")

bio.output         <- readRDS(paste(filepath.frida.emb,"bio_fuel_energy_bio_fuel_secondary_energy_output-fit uncertainty-equaly-weighted.RDS",sep=""))
bio.output.frida.emb      <- as.data.frame(bio.output$ciBounds)*1e-3
colnames(bio.output.frida.emb) <- c("low2","low1","median","high1","high2")
bio.output.frida.emb$Year <- as.numeric(bio.output$years)
bio.output.frida.emb.df <- data.frame(bio.output.frida.emb$Year,bio.output.frida.emb$median)
colnames(bio.output.frida.emb.df) <- c("Year","Data")

other.output         <- readRDS(paste(filepath.frida.emb,"other_energy_other_energy_output-fit uncertainty-equaly-weighted.RDS",sep=""))
other.output.frida.emb      <- as.data.frame(other.output$ciBounds)*1e-3
colnames(other.output.frida.emb) <- c("low2","low1","median","high1","high2")
other.output.frida.emb$Year <- as.numeric(other.output$years)
other.output.frida.emb.df <- data.frame(other.output.frida.emb$Year,other.output.frida.emb$median)
colnames(other.output.frida.emb.df) <- c("Year","Data")

# Combine all sources

plot.exp1 <- map_df(.x=list("(a) Coal"=coal.output.frida.exp1.df,
                            "(b) Oil"=oil.output.frida.exp1.df,
                            "(c) gas"=gas.output.frida.exp1.df,
                            "(d) Hydropower"=hydro.output.frida.exp1.df,
                            "(e) Nuclear"=nuclear.output.frida.exp1.df,
                            "(f) Solar/wind"=renewable.output.frida.exp1.df,
                            "(g) Biofuel"=bio.output.frida.exp1.df,
                            "(h) Other"=other.output.frida.exp1.df),
                    .f=bind_rows, .id="type")

plot.emb <- map_df(.x=list("(a) Coal"=coal.output.frida.emb.df,
                           "(b) Oil"=oil.output.frida.emb.df,
                           "(c) gas"=gas.output.frida.emb.df,
                           "(d) Hydropower"=hydro.output.frida.emb.df,
                           "(e) Nuclear"=nuclear.output.frida.emb.df,
                           "(f) Solar/wind"=renewable.output.frida.emb.df,
                           "(g) Biofuel"=bio.output.frida.emb.df,
                           "(h) Other"=other.output.frida.emb.df),
                    .f=bind_rows, .id="type")



plot.exp1.sub <- plot.exp1[plot.exp1$Year %in% c(1980,1990,2000,2010,2020,2030,2040,2050,2060,2070,2080,2090,2100),]
plot.emb.sub <- plot.emb[plot.exp1$Year %in% c(1980,1990,2000,2010,2020,2030,2040,2050,2060,2070,2080,2090,2100),]

# to change the order of stacks (doesn't )
#plot.exp1.sub$type <- factor(as.factor(plot.exp1.sub$type), levels = levels(as.factor(plot.exp1.sub$type))[c(7,1,2,4,5,6,8,3)])
#plot.emb.sub$type <- factor(as.factor(plot.emb.sub$type), levels = levels(as.factor(plot.emb.sub$type))[c(7,1,2,4,5,6,8,3)])

#plot.data <- map_df(.x=list("FRIDA EMB"=plot.emb.sub,
#                            "Carbon tax experiment"=plot.exp1.sub),
#                    .f=bind_rows,.id="Experiment")

text.data.exp1 <- plot.exp1.sub %>% group_by(Year) %>% mutate(pos=sum(Data))
exp.pos <- data.frame(text.data.exp1$Year[1:13],text.data.exp1$pos[1:13])
colnames(exp.pos) <- c("Year","Data")

text.data.emb <- plot.emb.sub %>% group_by(Year) %>% mutate(pos=sum(Data))
emb.pos <- data.frame(text.data.emb$Year[1:13],text.data.emb$pos[1:13])
colnames(emb.pos) <- c("Year","Data")

ggplot()+
  geom_bar(data=plot.exp1.sub,mapping=aes(x=Year,y=Data,fill=type,group=type),
           position=position_stack(reverse = T), stat = "identity",width=3)+
  geom_text(data=exp.pos,aes(x=Year-1,y=round(Data)+5), label="EXP1",angle=90)+
  geom_bar(data=plot.emb.sub,mapping=aes(x=Year+3.5,y=Data,fill=type,group=type),
           position=position_stack(reverse = T),stat = "identity",width=3)+
  geom_text(data=emb.pos,aes(x=Year+3.5,y=round(Data)+5), label="EMB",angle=90)+
  theme_bw()+
  scale_fill_manual(values=c("grey4","red4","grey","blue","red","green4", "orange","magenta"),
                    labels=c("Coal","Oil","Gas","Hydropower","Nuclear","Solar/Wind","Biofuel","Other"))+
  scale_x_continuous(breaks=seq(1980,2100,by=10))+
  labs(x=NULL,y=NULL,title="Energy output by type (PWh/Yr)")+
  theme(strip.background =element_rect(fill="white"),
        strip.text = element_text(size=13, family="serif"),
        axis.text.x = element_text(size=12, angle = 90, family="serif"),
        axis.text.y = element_text(size=12, family="serif"),
        legend.background=element_blank(),
        legend.text = element_text(size=12), 
        legend.key.width = unit(1.0,"cm"),
        legend.position = "inside",
        legend.position.inside = c(0.15,0.8),
        legend.key.spacing.x= unit(1.5,"cm"),
        legend.box="vertical",
        legend.box.just="left",
        legend.title=element_blank(),
        
  )

# make the figure like 9b
ggplot() +
  geom_area(data=plot.exp1, mapping=aes(x=Year,y=Data, fill=factor(type)),position=position_stack(reverse = T),
            color="black",linetype=1, linewidth=1,show.legend=TRUE)+
  theme_bw()+
  labs(x=NULL,y=NULL,title="Energy output by type in the carbon tax experiment (PWh/Yr)")+
  #scale_fill_manual(values=c("green","grey","orange4","darkgreen", "red","red4","magenta","yellow4"))+
  scale_x_continuous(breaks = seq(1980,2130,20),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme(axis.text.x = element_text(size=12, angle = 90, family="sans"),
        axis.text.y = element_text(size=12, family="sans"),
        legend.text = element_text(size=12), 
        legend.key.width = unit(.4,"cm"),
        legend.key.height = unit(.1,"cm"),
        legend.position = "bottom",
        legend.key.spacing.x= unit(.6,"cm"),
        legend.direction="horizontal",
        legend.box.just="left",
        legend.title=element_blank())+
  guides(fill=guide_legend(nrow=1,ncol=8))


