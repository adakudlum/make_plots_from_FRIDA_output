library(tidyverse)
library("readxl")
library(abind)
library(ggpubr)

remove(list=ls())

setwd('G:/My Drive/R/plots')
# path to directory where the EMB data exists
filepath.frida <- ("G:/My Drive/Data/NumSample/plotData_EMB/")

##### -- FRIDA data ----
# Net primary production
npp.mature.forest         <- readRDS(paste(filepath.frida,"forest_mature_forest_net_primary_production-fit uncertainty-equaly-weighted.RDS",sep=""))
npp.mature.forest.frida      <- as.data.frame(npp.mature.forest$ciBounds)
colnames(npp.mature.forest.frida) <- c("low2","low1","median","high1","high2")
npp.mature.forest.frida$Year <- as.numeric(npp.mature.forest$years)
npp.mature.forest.frida.df <- data.frame(npp.mature.forest.frida$Year,npp.mature.forest.frida$median)
colnames(npp.mature.forest.frida.df) <- c("Year","Data")
npp.mature.forest.frida.df$Scenario <- "EMB"
npp.mature.forest.frida.df$Variable  <- "Net primary production from mature forest"
npp.mature.forest.frida.df$Unit   <- "GtC/Yr"

npp.young.forest         <- readRDS(paste(filepath.frida,"forest_young_forest_net_primary_production-fit uncertainty-equaly-weighted.RDS",sep=""))
npp.young.forest.frida      <- as.data.frame(npp.young.forest$ciBounds)
colnames(npp.young.forest.frida) <- c("low2","low1","median","high1","high2")
npp.young.forest.frida$Year <- as.numeric(npp.young.forest$years)
npp.young.forest.frida.df <- data.frame(npp.young.forest.frida$Year,npp.young.forest.frida$median)
colnames(npp.young.forest.frida.df) <- c("Year","Data")
npp.young.forest.frida.df$Scenario <- "EMB"
npp.young.forest.frida.df$Variable  <- "Net primary production from young forest"
npp.young.forest.frida.df$Unit   <- "GtC/Yr"

npp.cropland         <- readRDS(paste(filepath.frida,"crop_cropland_net_primary_production-fit uncertainty-equaly-weighted.RDS",sep=""))
npp.cropland.frida      <- as.data.frame(npp.cropland$ciBounds)
colnames(npp.cropland.frida) <- c("low2","low1","median","high1","high2")
npp.cropland.frida$Year <- as.numeric(npp.cropland$years)
npp.cropland.frida.df <- data.frame(npp.cropland.frida$Year,npp.cropland.frida$median)
colnames(npp.cropland.frida.df) <- c("Year","Data")
npp.cropland.frida.df$Scenario <- "EMB"
npp.cropland.frida.df$Variable  <- "Net primary production from cropland"
npp.cropland.frida.df$Unit   <- "GtC/Yr"

npp.grassland         <- readRDS(paste(filepath.frida,"grass_grassland_net_primary_production-fit uncertainty-equaly-weighted.RDS",sep=""))
npp.grassland.frida      <- as.data.frame(npp.grassland$ciBounds)
colnames(npp.grassland.frida) <- c("low2","low1","median","high1","high2")
npp.grassland.frida$Year <- as.numeric(npp.grassland$years)
npp.grassland.frida.df <- data.frame(npp.grassland.frida$Year,npp.grassland.frida$median)
colnames(npp.grassland.frida.df) <- c("Year","Data")
npp.grassland.frida.df$Scenario <- "EMB"
npp.grassland.frida.df$Variable  <- "Net primary production from grassland"
npp.grassland.frida.df$Unit   <- "GtC/Yr"

# Landuse type area
area.cropland         <- readRDS(paste(filepath.frida,"land_use_cropland-fit uncertainty-equaly-weighted.RDS",sep=""))
area.cropland.frida      <- as.data.frame(area.cropland$ciBounds)
colnames(area.cropland.frida) <- c("low2","low1","median","high1","high2")
area.cropland.frida$Year <- as.numeric(area.cropland$years)
area.cropland.frida.df <- data.frame(area.cropland.frida$Year,area.cropland.frida$median)
colnames(area.cropland.frida.df) <- c("Year","Data")
area.cropland.frida.df$Scenario <- "EMB"
area.cropland.frida.df$Variable  <- "cropland area"
area.cropland.frida.df$Unit   <- "mha"

area.mature.forest         <- readRDS(paste(filepath.frida,"land_use_mature_forest-fit uncertainty-equaly-weighted.RDS",sep=""))
area.mature.forest.frida      <- as.data.frame(area.mature.forest$ciBounds)
colnames(area.mature.forest.frida) <- c("low2","low1","median","high1","high2")
area.mature.forest.frida$Year <- as.numeric(area.mature.forest$years)
area.mature.forest.frida.df <- data.frame(area.mature.forest.frida$Year,area.mature.forest.frida$median)
colnames(area.mature.forest.frida.df) <- c("Year","Data")
area.mature.forest.frida.df$Scenario <- "EMB"
area.mature.forest.frida.df$Variable  <- "mature forest area"
area.mature.forest.frida.df$Unit   <- "mha"

area.young.forest         <- readRDS(paste(filepath.frida,"land_use_young_forest-fit uncertainty-equaly-weighted.RDS",sep=""))
area.young.forest.frida      <- as.data.frame(area.young.forest$ciBounds)
colnames(area.young.forest.frida) <- c("low2","low1","median","high1","high2")
area.young.forest.frida$Year <- as.numeric(area.young.forest$years)
area.young.forest.frida.df <- data.frame(area.young.forest.frida$Year,area.young.forest.frida$median)
colnames(area.young.forest.frida.df) <- c("Year","Data")
area.young.forest.frida.df$Scenario <- "EMB"
area.young.forest.frida.df$Variable  <- "young forest area"
area.young.forest.frida.df$Unit   <- "mha"

area.grassland         <- readRDS(paste(filepath.frida,"land_use_grassland-fit uncertainty-equaly-weighted.RDS",sep=""))
area.grassland.frida      <- as.data.frame(area.grassland$ciBounds)
colnames(area.grassland.frida) <- c("low2","low1","median","high1","high2")
area.grassland.frida$Year <- as.numeric(area.grassland$years)
area.grassland.frida.df <- data.frame(area.grassland.frida$Year,area.grassland.frida$median)
colnames(area.grassland.frida.df) <- c("Year","Data")
area.grassland.frida.df$Scenario <- "EMB"
area.grassland.frida.df$Variable  <- "grassland area"
area.grassland.frida.df$Unit   <- "mha"

area.degraded.land         <- readRDS(paste(filepath.frida,"land_use_degraded_land-fit uncertainty-equaly-weighted.RDS",sep=""))
area.degraded.land.frida      <- as.data.frame(area.degraded.land$ciBounds)
colnames(area.degraded.land.frida) <- c("low2","low1","median","high1","high2")
area.degraded.land.frida$Year <- as.numeric(area.degraded.land$years)
area.degraded.land.frida.df <- data.frame(area.degraded.land.frida$Year,area.degraded.land.frida$median)
colnames(area.degraded.land.frida.df) <- c("Year","Data")
area.degraded.land.frida.df$Scenario <- "EMB"
area.degraded.land.frida.df$Variable  <- "Degraded land area"
area.degraded.land.frida.df$Unit   <- "mha"

# Crop demand for food 
crop.demand.for.food         <- readRDS(paste(filepath.frida,"food_demand_crop_demand_for_food-fit uncertainty-equaly-weighted.RDS",sep=""))
crop.demand.for.food.frida      <- as.data.frame(crop.demand.for.food$ciBounds)
colnames(crop.demand.for.food.frida) <- c("low2","low1","median","high1","high2")
crop.demand.for.food.frida$Year <- as.numeric(crop.demand.for.food$years)
crop.demand.for.food.frida.df <- data.frame(crop.demand.for.food.frida$Year,crop.demand.for.food.frida$median)
colnames(crop.demand.for.food.frida.df) <- c("Year","Data")
crop.demand.for.food.frida.df$Scenario <- "EMB"
crop.demand.for.food.frida.df$Variable  <- "crop demand.for.food"
crop.demand.for.food.frida.df$Unit   <- "PCal/Yr"

# animal products production
animal.products   <- readRDS(paste(filepath.frida,"animal_products_animal_products_production-fit uncertainty-equaly-weighted.RDS",sep=""))
animal.products.frida <- as.data.frame(animal.products$ciBounds)
colnames(animal.products.frida) <- c("low2","low1","median","high1","high2")
animal.products.frida$Year <- as.numeric(animal.products$years)
animal.products.frida.df <- data.frame(animal.products.frida$Year,animal.products.frida$median)
colnames(animal.products.frida.df) <- c("Year","Data")
animal.products.frida.df$Scenario <- "EMB"
animal.products.frida.df$Variable  <- "animal products production"
animal.products.frida.df$Unit   <- "PCal/Yr"

# Crop production by use (feed production, crop demand for food, crops used for seed and loss, crop used for non energy other)

feed.production         <- readRDS(paste(filepath.frida,"food_demand_feed_production-fit uncertainty-equaly-weighted.RDS",sep=""))
feed.production.frida      <- as.data.frame(feed.production$ciBounds)
colnames(feed.production.frida) <- c("low2","low1","median","high1","high2")
feed.production.frida$Year <- as.numeric(feed.production$years)
feed.production.frida.df <- data.frame(feed.production.frida$Year,feed.production.frida$median)
colnames(feed.production.frida.df) <- c("Year","Data")
feed.production.frida.df$Scenario <- "EMB"
feed.production.frida.df$Variable  <- "crop production for food"
feed.production.frida.df$Unit   <- "PCal/Yr"

crop.production.seed.lost         <- readRDS(paste(filepath.frida,"food_demand_crops_used_for_seed_or_lost-fit uncertainty-equaly-weighted.RDS",sep=""))
crop.production.seed.lost.frida      <- as.data.frame(crop.production.seed.lost$ciBounds)
colnames(crop.production.seed.lost.frida) <- c("low2","low1","median","high1","high2")
crop.production.seed.lost.frida$Year <- as.numeric(crop.production.seed.lost$years)
crop.production.seed.lost.frida.df <- data.frame(crop.production.seed.lost.frida$Year,crop.production.seed.lost.frida$median)
colnames(crop.production.seed.lost.frida.df) <- c("Year","Data")
crop.production.seed.lost.frida.df$Scenario <- "EMB"
crop.production.seed.lost.frida.df$Variable  <- "crop production for biofuel"
crop.production.seed.lost.frida.df$Unit   <- "PCal/Yr"

#crop.used.for.non.energy         <- readRDS(paste(filepath.frida,"food_demand_crops_used_for_seed_or_lost-fit uncertainty-equaly-weighted.RDS",sep=""))
#crop.used.for.non.energy.frida      <- as.data.frame(crop.used.for.non.energy$ciBounds)
#colnames(crop.used.for.non.energy.frida) <- c("low2","low1","median","high1","high2")
#crop.used.for.non.energy.frida$Year <- as.numeric(crop.used.for.non.energy$years)
#crop.used.for.non.energy.frida.df <- data.frame(crop.used.for.non.energy.frida$Year,crop.used.for.non.energy.frida$median)
#colnames(crop.used.for.non.energy.frida.df) <- c("Year","Data")
#crop.used.for.non.energy.frida.df$Scenario <- "EMB"
#crop.used.for.non.energy.frida.df$Variable  <- "crop production for biofuel"
#crop.used.for.non.energy.frida.df$Unit   <- "PCal/Yr"


# water use by type
water.use.agriculture         <- readRDS(paste(filepath.frida,"freshwater_agricultural_water_withdrawal-fit uncertainty-equaly-weighted.RDS",sep=""))
water.use.agriculture.frida      <- as.data.frame(water.use.agriculture$ciBounds)
colnames(water.use.agriculture.frida) <- c("low2","low1","median","high1","high2")
water.use.agriculture.frida$Year <- as.numeric(water.use.agriculture$years)
water.use.agriculture.frida.df <- data.frame(water.use.agriculture.frida$Year,water.use.agriculture.frida$median)
colnames(water.use.agriculture.frida.df) <- c("Year","Data")
water.use.agriculture.frida.df$Scenario <- "EMB"
water.use.agriculture.frida.df$Variable  <- "water use agriculture"
water.use.agriculture.frida.df$Unit   <- "m3/Yr"

water.use.other         <- readRDS(paste(filepath.frida,"freshwater_non_agricultural_water_withdrawl-fit uncertainty-equaly-weighted.RDS",sep=""))
water.use.other.frida      <- as.data.frame(water.use.other$ciBounds)
colnames(water.use.other.frida) <- c("low2","low1","median","high1","high2")
water.use.other.frida$Year <- as.numeric(water.use.other$years)
water.use.other.frida.df <- data.frame(water.use.other.frida$Year,water.use.other.frida$median)
colnames(water.use.other.frida.df) <- c("Year","Data")
water.use.other.frida.df$Scenario <- "EMB"
water.use.other.frida.df$Variable  <- "water use non-agriculture"
water.use.other.frida.df$Unit   <- "m3/Yr"

# Fertilizer by type
fertilizer.man.made         <- readRDS(paste(filepath.frida,"land_nutrients_man_made_fertilizer_use-fit uncertainty-equaly-weighted.RDS",sep=""))
fertilizer.man.made.frida      <- as.data.frame(fertilizer.man.made$ciBounds)
colnames(fertilizer.man.made.frida) <- c("low2","low1","median","high1","high2")
fertilizer.man.made.frida$Year <- as.numeric(fertilizer.man.made$years)
fertilizer.man.made.frida.df <- data.frame(fertilizer.man.made.frida$Year,fertilizer.man.made.frida$median)
colnames(fertilizer.man.made.frida.df) <- c("Year","Data")
fertilizer.man.made.frida.df$Scenario <- "EMB"
fertilizer.man.made.frida.df$Variable  <- "man made fertilizer use"
fertilizer.man.made.frida.df$Unit   <- "MtN/Yr"

fertilizer.manure         <- readRDS(paste(filepath.frida,"land_nutrients_manure_fertilizer_use-fit uncertainty-equaly-weighted.RDS",sep=""))
fertilizer.manure.frida      <- as.data.frame(fertilizer.manure$ciBounds)
colnames(fertilizer.manure.frida) <- c("low2","low1","median","high1","high2")
fertilizer.manure.frida$Year <- as.numeric(fertilizer.manure$years)
fertilizer.manure.frida.df <- data.frame(fertilizer.manure.frida$Year,fertilizer.manure.frida$median)
colnames(fertilizer.manure.frida.df) <- c("Year","Data")
fertilizer.manure.frida.df$Scenario <- "EMB"
fertilizer.manure.frida.df$Variable  <- "manure fertilizer use"
fertilizer.manure.frida.df$Unit   <- "MtN/Yr"

# Add the variables by type for plotting
plot.npp <- map_df(.x=list("mature forest"=npp.mature.forest.frida.df,
                           "young forest"=npp.young.forest.frida.df,
                           "cropland"=npp.cropland.frida.df,
                           "grassland"=npp.grassland.frida.df),
                   .f=bind_rows, .id="name")

plot.area <- map_df(.x=list("mature forest"=area.mature.forest.frida.df,
                            "young forest"=area.young.forest.frida.df,
                            "cropland"=area.cropland.frida.df,
                            "grassland"=area.grassland.frida.df,
                            "degraded land"=area.degraded.land.frida.df),
                    .f=bind_rows, .id="name")

plot.food.production <- map_df(.x=list("vegetal products demand"=crop.demand.for.food.frida.df,
                            "animal products"=animal.products.frida.df),
                    .f=bind_rows, .id="name")

# add crop used for non energy here
plot.crop.production <- map_df(.x=list("vegetal products demand"=crop.demand.for.food.frida.df,
                                       "feed production"=feed.production.frida.df,
                                       "crops used for seed and lost"= crop.production.seed.lost.frida.df),
                               .f=bind_rows, .id="name")

plot.water.use <- map_df(.x=list("agriculture"=water.use.agriculture.frida.df,
                                 "non-agriculture"=water.use.other.frida.df),
                               .f=bind_rows, .id="name")

plot.fertilizer <- map_df(.x=list("man made"=fertilizer.man.made.frida.df,
                                 "manure"=fertilizer.manure.frida.df),
                         .f=bind_rows, .id="name")

p1 <- ggplot() +
  geom_area(data=plot.npp, mapping=aes(x=Year,y=Data, fill=name))+
  theme_bw()+
  labs(x=NULL,y=NULL,title="(a) Net primary production (GtC/Yr)")+
  scale_x_continuous(breaks = seq(1980,2130,20),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme(axis.text.x = element_text(size=12, angle = 90, family="sans"),
        axis.text.y = element_text(size=12, family="sans"),
        legend.text = element_text(size=10), 
        legend.key.width = unit(.4,"cm"),
        legend.key.height = unit(.02,"cm"),
        legend.direction="horizontal",
        legend.position = "bottom",
        #legend.position.inside = c(0.2,0.8),
        legend.key.spacing.x= unit(.6,"cm"),
        legend.title=element_blank())+
  guides(fill=guide_legend(nrow=1,ncol=4))

p2 <- ggplot() +
  geom_area(data=plot.area, mapping=aes(x=Year,y=Data*1e-3, fill=name))+
  theme_bw()+
  labs(x=NULL,y=NULL,title="(b) Landuse type (billion ha)")+
  scale_x_continuous(breaks = seq(1980,2130,20),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme(axis.text.x = element_text(size=12, angle = 90, family="sans"),
        axis.text.y = element_text(size=12, family="sans"),
        legend.text = element_text(size=10), 
        legend.key.width = unit(.4,"cm"),
        legend.key.height = unit(.02,"cm"),
        legend.position = "bottom",
        #legend.position.inside = c(0.2,0.8),
        legend.key.spacing.x= unit(.1,"cm"),
        legend.direction="horizontal",
        legend.title=element_blank())+
  guides(fill=guide_legend(nrow=1,ncol=5))

p3 <- ggplot() +
  geom_area(data=plot.food.production, mapping=aes(x=Year,y=Data*1e-3, fill=name))+
  theme_bw()+
  labs(x=NULL,y=NULL,title="(c) Human food production (ECal/Yr)")+
  scale_x_continuous(breaks = seq(1980,2130,20),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0), limits=c(0,30))+
  scale_fill_manual(values=c("red3","lightblue"))+
  theme(axis.text.x = element_text(size=12, angle = 90, family="sans"),
        axis.text.y = element_text(size=12, family="sans"),
        legend.text = element_text(size=12),  
        legend.background = element_blank(),
        legend.key.size = unit(.6,"cm"),
        legend.position = "inside",
        legend.position.inside = c(0.32,0.8),
        legend.key.spacing.x= unit(.6,"cm"),
        legend.direction="vertical",
        legend.title=element_blank())+
  guides(fill=guide_legend(nrow=2,ncol=3))

p4 <- ggplot() +
  geom_area(data=plot.crop.production, mapping=aes(x=Year,y=Data*1e-3, fill=name))+
  theme_bw()+
  labs(x=NULL,y=NULL,title="(d) Crop production by type (ECal/Yr)")+
  scale_x_continuous(breaks = seq(1980,2130,20),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0), limits=c(0,30))+
  scale_fill_manual(values=c("red4","green4","lightblue"))+
  theme(axis.text.x = element_text(size=12, angle = 90, family="sans"),
        axis.text.y = element_text(size=12, family="sans"),
        legend.text = element_text(size=12), 
        legend.background = element_blank(), 
        legend.key.size = unit(.6,"cm"),
        legend.position = "inside",
        legend.position.inside = c(0.3,0.7),
        legend.key.spacing.x= unit(1,"cm"),
        legend.direction="vertical",
        legend.title=element_blank())+
  guides(fill=guide_legend(nrow=3,ncol=1))

p5 <- ggplot() +
  geom_area(data=plot.water.use, mapping=aes(x=Year,y=Data*1e-12, fill=name))+
  theme_bw()+
  labs(x=NULL,y=NULL,title="(e) Water use (terra m3/Yr)")+
  scale_x_continuous(breaks = seq(1980,2130,20),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme(axis.text.x = element_text(size=12, angle = 90, family="sans"),
        axis.text.y = element_text(size=12, family="sans"),
        legend.text = element_text(size=12),  
        legend.background = element_blank(),
        legend.key.size = unit(.6,"cm"),
        legend.position = "inside",
        legend.position.inside = c(0.32,0.8),
        legend.key.spacing.x= unit(1,"cm"),
        legend.direction="horizontal",
        legend.box.just="left",
        legend.title=element_blank())+
  guides(fill=guide_legend(nrow=2,ncol=3))

p6 <- ggplot() +
  geom_area(data=plot.fertilizer, mapping=aes(x=Year,y=Data, fill=name))+
  theme_bw()+
  labs(x=NULL,y=NULL,title="(f) Fertilzer use (MtN/Yr)")+
  scale_x_continuous(breaks = seq(1980,2130,20),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme(axis.text.x = element_text(size=12, angle = 90, family="sans"),
        axis.text.y = element_text(size=12, family="sans"),
        legend.text = element_text(size=12), 
        legend.background = element_blank(),
        legend.key.size = unit(.6,"cm"),
        legend.position = "inside",
        legend.position.inside = c(0.3,0.8),
        legend.key.spacing.x= unit(1,"cm"),
        legend.direction="horizontal",
        legend.box.just="left",
        legend.title=element_blank())+
  guides(fill=guide_legend(nrow=2,ncol=3))

ggarrange(p1, p2, p3,p4,p5,p6, 
          labels = NULL,
          ncol = 2, nrow = 3)
