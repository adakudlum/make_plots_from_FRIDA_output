# Compare FRIDA's endogenous model behavior (EMB) with that of 6 other IAMs, including the uncertainty range represented by FRIDA

# 3 different datasets are used, the percentiles from the uncertainty output, variables from 6 IAMs (obtained from IIASA)
# and the FRIDA calibration set
# Muralidhar Adakudlu, 15.01.2025

library(tidyverse)
library("readxl")
library(abind)
library(ggtext)   # for highlighting legend title
library(gridExtra) # nice arrangement of panels
library(lemon)
library(ggpubr)

remove(list=ls())

# Change the paths to respective data catalogues
# Data from uncertainty sampling has to be obtained separately

setwd('G:/My Drive/R/plots')
filepath.frida.emb  <- ("G:/My Drive/Data/frida_uncertainty_sample/figures/CI-plots/equalyWeighted/plotData")
filepath.frida.calib <- ("G:/My Drive/Data/frida_uncertainty_sample")
filepath.iams       <- ("G:/My Drive/Data/SSPs_all-other_models/")

file.names   <- c("demographics_population-fit uncertainty-equaly-weighted.RDS",
                 "food_demand_animal_products_demand-fit uncertainty-equaly-weighted.RDS",
                 "demographics_real_gdp_per_person-fit uncertainty-equaly-weighted.RDS",
                 "energy_balance_model_surface_temperature_anomaly-fit uncertainty-equaly-weighted.RDS",
                 "land_use_forest_land-fit uncertainty-equaly-weighted.RDS", 
                 "renewable_energy_renewable_energy_output-fit uncertainty-equaly-weighted.RDS",
                 "fossil_energy_secondary_fossil_energy_output-fit uncertainty-equaly-weighted.RDS",
                 "energy_supply_total_energy_output-fit uncertainty-equaly-weighted.RDS",
                 "sea_level_total_global_sea_level_anomaly-fit uncertainty-equaly-weighted.RDS")

variable.names <- c("Population","Animal_Products_Demand","GDP_per_person","Surface_temperature_anomaly",
                    "Forest_land","Renewable_energy","Secondary_fossil_energy","Total_energy","Sea_level_rise") 

emb.data <- list(NA)
emb.data <- sapply(1:length(file.names), function(i) {
            data <- readRDS(paste(filepath.frida.emb,"/",file.names[i],sep=""))
            emb.data[[variable.names[i]]] <- as.data.frame(data$ciBounds)}, 
            simplify=FALSE, USE.NAMES = TRUE)
names(emb.data)<- variable.names
emb.data.df <- as.data.frame(do.call(rbind,emb.data), stringAsFactors=True)
colnames(emb.data.df) <- c("low2","low1","median","high1","high2")
emb.data.df$variable <- rownames(emb.data.df)
emb.data.df <- tidyr::separate(emb.data.df, variable, into = c('variable', 'year'), sep = '\\.')
emb.data.df$Scenario <- "EMB"

# We combine any other experiments/policy results here
frida.data.df <- map_df(.x=list("FRIDA"=emb.data.df),
                        .f=bind_rows,.id="Model")
  
# Extract the medians for each variable in FRIDA
population.frida.df <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[1]], 
                                  frida.data.df$Model[frida.data.df$variable==variable.names[1]],
                                  frida.data.df$Scenario[frida.data.df$variable==variable.names[1]],
                                  frida.data.df$median[frida.data.df$variable==variable.names[1]]*1e-3)
colnames(population.frida.df) <- c("Year","Model","Scenario","Data")

animal.products.demand.frida.df <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[2]],
                                                  frida.data.df$Model[frida.data.df$variable==variable.names[2]],
                                                  frida.data.df$Scenario[frida.data.df$variable==variable.names[2]],
                                                  frida.data.df$median[frida.data.df$variable==variable.names[2]]*1e-3)
colnames(animal.products.demand.frida.df) <- c("Year","Model","Scenario","Data")

deflator.value.2005  <- 1.784793301  # Deflating the GDP to year 2005 to be consistent with rest of the IAMs
gdp.per.person.frida.df <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[3]],
                                          frida.data.df$Model[frida.data.df$variable==variable.names[3]],
                                          frida.data.df$Scenario[frida.data.df$variable==variable.names[3]],
                                          frida.data.df$median[frida.data.df$variable==variable.names[3]]/deflator.value.2005)
colnames(gdp.per.person.frida.df) <- c("Year","Model","Scenario","Data")

sta.frida.df <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[4]],
                               frida.data.df$Model[frida.data.df$variable==variable.names[4]],
                               frida.data.df$Scenario[frida.data.df$variable==variable.names[4]],
                               frida.data.df$median[frida.data.df$variable==variable.names[4]])
colnames(sta.frida.df) <- c("Year","Model","Scenario","Data")

forest.land.frida.df <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[5]],
                                       frida.data.df$Model[frida.data.df$variable==variable.names[5]],
                                       frida.data.df$Scenario[frida.data.df$variable==variable.names[5]],
                                       frida.data.df$median[frida.data.df$variable==variable.names[5]]*1e-3)
colnames(forest.land.frida.df) <- c("Year","Model","Scenario","Data")

energy.TWh.to.EJ <- 1/277.8   # Unit conversion to be consistent with rest of the IAMs
ren.energy.frida.df <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[6]],
                                      frida.data.df$Model[frida.data.df$variable==variable.names[6]],
                                      frida.data.df$Scenario[frida.data.df$variable==variable.names[6]],
                                      frida.data.df$median[frida.data.df$variable==variable.names[6]]*energy.TWh.to.EJ)
colnames(ren.energy.frida.df) <- c("Year","Model","Scenario","Data")


fossil.energy.frida.df <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[7]],
                                         frida.data.df$Model[frida.data.df$variable==variable.names[7]],
                                         frida.data.df$Scenario[frida.data.df$variable==variable.names[7]],
                                         frida.data.df$median[frida.data.df$variable==variable.names[7]]*energy.TWh.to.EJ)
colnames(fossil.energy.frida.df) <- c("Year","Model","Scenario","Data")

total.energy.frida.df <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[8]],
                                        frida.data.df$Model[frida.data.df$variable==variable.names[8]],
                                        frida.data.df$Scenario[frida.data.df$variable==variable.names[8]],
                                        frida.data.df$median[frida.data.df$variable==variable.names[8]]*energy.TWh.to.EJ)
colnames(total.energy.frida.df) <- c("Year","Model","Scenario","Data")

slr.frida.df <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[9]],
                               frida.data.df$Model[frida.data.df$variable==variable.names[9]],
                               frida.data.df$Scenario[frida.data.df$variable==variable.names[9]],
                               frida.data.df$median[frida.data.df$variable==variable.names[9]])
colnames(slr.frida.df) <- c("Year","Model","Scenario","Data")


#### ---- end of FRIDA data. Start with data from other IAMS

# Population
population      <- read_excel(paste(filepath.iams, "population.xlsx",sep=""))
population.iams   <- head(population,-2)[,!names(population) %in% c("Notes","Region","Variable","Unit")]  
colnames(population.iams) <- c(colnames(population.iams)[1:2],as.character(c(2005,seq(2010,2100,by=10))))
population.iams.long <- reshape2::melt(population.iams, id.vars=c("Model","Scenario"))                   
colnames(population.iams.long) = c("Model","Scenario","Year","Data")
population.iams.long$Data <- population.iams.long$Data*1e-3

# Real GDP per person
gdp.per.person   <- read_excel(paste(filepath.iams, "gdp_per_capita.xlsx",sep=""))
gdp.per.person.iams         <- head(gdp.per.person,-2)[,!names(gdp.per.person) %in% c("Notes","Region","Variable","Unit")]
colnames(gdp.per.person.iams) <- c(colnames(gdp.per.person.iams)[1:2],as.character(c(2005,seq(2010,2100,by=10))))
gdp.per.person.iams.long <- reshape2::melt(gdp.per.person.iams, id.vars=c("Model","Scenario"))                   
colnames(gdp.per.person.iams.long) = c("Model","Scenario","Year","Data")

# surface temperature anomaly
sta      <- read_excel(paste(filepath.iams, "surface_temperature_anomaly.xlsx",sep=""))
sta.iams   <- head(sta,-2)[,!names(sta) %in% c("Notes","Region","Variable","Unit")]  
colnames(sta.iams) <- c(colnames(sta.iams)[1:2],as.character(c(2005,seq(2010,2100,by=10))))
sta.iams.long <- reshape2::melt(sta.iams, id.vars=c("Model","Scenario"))                   
colnames(sta.iams.long) = c("Model","Scenario","Year","Data")

# Forest land
forest.land      <- read_excel(paste(filepath.iams, "forest.xlsx",sep=""))
forest.land.iams   <- head(forest.land,-2)[,!names(forest.land) %in% c("Notes","Region","Variable","Unit")]  
colnames(forest.land.iams) <- c(colnames(forest.land.iams)[1:2],as.character(c(2005,seq(2010,2100,by=10))))
forest.land.iams.long <- reshape2::melt(forest.land.iams, id.vars=c("Model","Scenario"))                   
colnames(forest.land.iams.long) = c("Model","Scenario","Year","Data")
forest.land.iams.long$Data <- forest.land.iams.long$Data*1e-3

# secondary fossil energy
fossil.energy      <- read_excel(paste(filepath.iams, "secondary_fossil_energy_liquids.xlsx",sep=""))
fossil.energy.iams   <- head(fossil.energy,-2)[,!names(fossil.energy) %in% c("Notes","Region","Variable","Unit")]  
colnames(fossil.energy.iams) <- c(colnames(fossil.energy.iams)[1:2],as.character(c(2005,seq(2010,2100,by=10))))
fossil.energy.iams.long <- reshape2::melt(fossil.energy.iams, id.vars=c("Model","Scenario"))                   
colnames(fossil.energy.iams.long) = c("Model","Scenario","Year","Data")

# renewable energy
ren_energy_solar      <- read_excel(paste(filepath.iams,  "final_solar.xlsx",sep="")) 
ren_energy_hydro      <- read_excel(paste(filepath.iams, "final_hydrogen.xlsx",sep="")) 
ren_energy_solar.df   <- head(ren_energy_solar,-2)[,!names(ren_energy_solar) %in% c("Notes","Region","Variable","Unit")]  
colnames(ren_energy_solar.df) <- c(colnames(ren_energy_solar.df)[1:2],as.character(c(2005,seq(2010,2100,by=10))))
ren_energy_hydro.df   <- head(ren_energy_hydro,-2)[,!names(ren_energy_hydro) %in% c("Notes","Region","Variable","Unit")]  
colnames(ren_energy_hydro.df) <- c(colnames(ren_energy_hydro.df)[1:2],as.character(c(2005,seq(2010,2100,by=10))))

# add renewable energy sources
ren.energy <- rbind(ren_energy_hydro.df,ren_energy_solar.df)
ren.energy.iams <- aggregate(list(ren.energy$'2005',ren.energy$'2010',ren.energy$'2020',ren.energy$'2030',
                                  ren.energy$'2040',ren.energy$'2050',ren.energy$'2060',ren.energy$'2070',ren.energy$'2080',ren.energy$'2090',ren.energy$'2100'),
                              by=list(ren.energy$Model,ren.energy$Scenario), FUN=sum )
colnames(ren.energy.iams) <- c("Model","Scenario",as.character(c(2005,seq(2010,2100,by=10))))
ren.energy.iams.long <- reshape2::melt(ren.energy.iams, id.vars=c("Model","Scenario"))                   
colnames(ren.energy.iams.long) = c("Model","Scenario","Year","Data")

final.energy.liquids      <- read_excel(paste(filepath.iams,  "final_energy_liquids.xlsx",sep="")) 
final.energy.gases      <- read_excel(paste(filepath.iams, "final_energy_gases.xlsx",sep="")) 
final.energy.liquids.df   <- head(final.energy.liquids,-2)[,!names(final.energy.liquids) %in% c("Notes","Region","Variable","Unit")]  
colnames(final.energy.liquids.df) <- c(colnames(final.energy.liquids.df)[1:2],as.character(c(2005,seq(2010,2100,by=10))))
final.energy.gases.df   <- head(final.energy.gases,-2)[,!names(final.energy.gases) %in% c("Notes","Region","Variable","Unit")]  
colnames(final.energy.gases.df) <- c(colnames(final.energy.gases.df)[1:2],as.character(c(2005,seq(2010,2100,by=10))))
final.energy <- rbind(ren_energy_hydro.df,ren_energy_solar.df, final.energy.liquids.df, final.energy.gases.df)
final.energy.iams <- aggregate(list(final.energy$'2005',final.energy$'2010',final.energy$'2020',final.energy$'2030',
                                  final.energy$'2040',final.energy$'2050',final.energy$'2060',final.energy$'2070',final.energy$'2080',final.energy$'2090',final.energy$'2100'),
                             by=list(final.energy$Model,final.energy$Scenario), FUN=sum )
colnames(final.energy.iams) <- c("Model","Scenario",as.character(c(2005,seq(2010,2100,by=10))))
final.energy.iams.long <- reshape2::melt(final.energy.iams, id.vars=c("Model","Scenario"))                   
colnames(final.energy.iams.long) = c("Model","Scenario","Year","Data")

### -- end of IAMs data. Now we need the FRIDA calibration data
len.frida.data <- length(seq(1980,2150,1))
calib.df <- readRDS(paste(filepath.frida.calib,"/","calDat.RDS",sep=""))
gdp.per.person.calibration <- data.frame(as.factor(seq(1980,2150,by=1)),abind(calib.df$calDat$demographics_real_gdp_per_person/deflator.value.2005,rep(NA,len.frida.data-44)))
sta.calibration <- data.frame(as.factor(seq(1980,2150,by=1)),abind(calib.df$calDat$energy_balance_model_surface_temperature_anomaly,rep(NA,len.frida.data-44)))
forest.land.calibration <- data.frame(as.factor(seq(1980,2150,by=1)),abind(calib.df$calDat$land_use_forest_land*1e-3,rep(NA,len.frida.data-44)))
population.calibration <- data.frame(as.factor(seq(1980,2150,by=1)),abind(calib.df$calDat$demographics_population*1e-3,rep(NA,len.frida.data-44)))
ren.energy.calibration <- data.frame(as.factor(seq(1980,2150,by=1)),abind(calib.df$calDat$renewable_energy_renewable_energy_output*energy.TWh.to.EJ,rep(NA,len.frida.data-44)))
fossil.energy.calibration <- data.frame(as.factor(seq(1980,2150,by=1)),abind(calib.df$calDat$fossil_energy_secondary_fossil_energy_output*energy.TWh.to.EJ,rep(NA,len.frida.data-44)))
total.energy.calibration  <- data.frame(as.factor(seq(1980,2150,by=1)),abind(calib.df$calDat$energy_supply_total_energy_output*energy.TWh.to.EJ,rep(NA,len.frida.data-44)))
animal.products.demand.calibration <- data.frame(as.factor(seq(1980,2150,by=1)),abind(calib.df$calDat$food_demand_animal_products_demand*1e-3,rep(NA,len.frida.data-44)))
slr.calibration <- data.frame(as.factor(seq(1980,2150,by=1)),abind(calib.df$calDat$sea_level_total_global_sea_level_anomaly,rep(NA,len.frida.data-44)))

colnames(population.calibration) <- colnames(sta.calibration) <- colnames(forest.land.calibration) <- colnames(gdp.per.person.calibration) <-
colnames(ren.energy.calibration) <- colnames(fossil.energy.calibration) <- colnames(total.energy.calibration) <- colnames(animal.products.demand.calibration) <-
colnames(slr.calibration) <- c("Year","Data")

population.calibration$Model <- population.calibration$Scenario <- 
  sta.calibration$Model <- sta.calibration$Scenario <- forest.land.calibration$Model <-
  forest.land.calibration$Scenario <- ren.energy.calibration$Model <-
  ren.energy.calibration$Scenario <- fossil.energy.calibration$Model <- 
  fossil.energy.calibration$Scenario <- total.energy.calibration$Model <-  total.energy.calibration$Scenario <-
  animal.products.demand.calibration$Model <- animal.products.demand.calibration$Scenario <- 
  gdp.per.person.calibration$Model <- gdp.per.person.calibration$Scenario <- slr.calibration$Model <- 
  slr.calibration$Scenario <- "Calibration"

# ---- plotting preprocessing

plot.population <- map_df(.x=list(population.iams.long,
                                  population.frida.df,
                                  population.calibration),
                    .f=bind_rows)

plot.sta <- map_df(.x=list(sta.iams.long,
                            sta.frida.df,
                           sta.calibration),
                               .f=bind_rows)

plot.slr     <- map_df(.x=list(slr.frida.df,
                               slr.calibration),
                       .f=bind_rows)

plot.gdp.per.person <- map_df(.x=list(gdp.per.person.iams.long,
                                       gdp.per.person.frida.df,
                                      gdp.per.person.calibration),
                               .f=bind_rows)

plot.animal.products.demand <- map_df(.x=list(animal.products.demand.frida.df,
                                       animal.products.demand.calibration),
                               .f=bind_rows)

plot.forest.land <- map_df(.x=list(forest.land.iams.long,
                                   forest.land.frida.df,
                                   forest.land.calibration),
                               .f=bind_rows)

plot.ren.energy <- map_df(.x=list(ren.energy.frida.df,
                                       ren.energy.calibration),
                               .f=bind_rows)

plot.fossil.energy <- map_df(.x=list(fossil.energy.iams.long,
                                     fossil.energy.frida.df,
                                     fossil.energy.calibration),
                               .f=bind_rows)

plot.total.energy <- map_df(.x=list(final.energy.iams.long,
                                       total.energy.frida.df,
                                    total.energy.calibration),
                               .f=bind_rows)


####################################################################################################################
# Merge all data together
plot.data.all <- map_df(.x=list("(a) Surf. Temp. Anomaly (°C)"=plot.sta,
                                "(b) GDP per per. (2005b$/mp/Yr)"=plot.gdp.per.person,
                                "(c) Sea Level Rise (meters)"=plot.slr,
                                "(d) Population (Billion)"=plot.population,
                                "(e) Animal Pr. Dem. (ECal/Yr)"=plot.animal.products.demand,
                                "(f) Forest (bHa)"= plot.forest.land,
                                "(g) Ren. Energy Output (EJ/Yr)"=plot.ren.energy,
                                "(h) Fossil Energy Output (EJ/Yr)"=plot.fossil.energy,
                                "(i) Total Energy Output (EJ/Yr)"=plot.total.energy),
                        .f=bind_rows, .id = "name")
plot.data.all$Scenario[plot.data.all$Scenario=="SSP5-Baseline"]<-"SSP5-85"
plot.data.all$Scenario[plot.data.all$Scenario=="SSP3-Baseline"]<-"SSP3-70"

# confidence intervals data for shading
plot.population.ci <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[1]], 
                                 frida.data.df$Model[frida.data.df$variable==variable.names[1]],
                                 frida.data.df$Scenario[frida.data.df$variable==variable.names[1]],
                                 frida.data.df$low2[frida.data.df$variable==variable.names[1]]*1e-3,
                                 frida.data.df$low1[frida.data.df$variable==variable.names[1]]*1e-3,
                                 frida.data.df$high1[frida.data.df$variable==variable.names[1]]*1e-3,
                                 frida.data.df$high2[frida.data.df$variable==variable.names[1]]*1e-3)

plot.animal.products.demand.ci <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[2]], 
                                             frida.data.df$Model[frida.data.df$variable==variable.names[2]],
                                             frida.data.df$Scenario[frida.data.df$variable==variable.names[2]],
                                             frida.data.df$low2[frida.data.df$variable==variable.names[2]]*1e-3,
                                             frida.data.df$low1[frida.data.df$variable==variable.names[2]]*1e-3,
                                             frida.data.df$high1[frida.data.df$variable==variable.names[2]]*1e-3,
                                            frida.data.df$high2[frida.data.df$variable==variable.names[2]]*1e-3)
plot.forest.land.ci <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[5]], 
                                  frida.data.df$Model[frida.data.df$variable==variable.names[5]],
                                  frida.data.df$Scenario[frida.data.df$variable==variable.names[5]],
                                  frida.data.df$low2[frida.data.df$variable==variable.names[5]]*1e-3,
                                  frida.data.df$low1[frida.data.df$variable==variable.names[5]]*1e-3,
                                  frida.data.df$high1[frida.data.df$variable==variable.names[5]]*1e-3,
                                  frida.data.df$high2[frida.data.df$variable==variable.names[5]]*1e-3)
plot.sta.ci <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[4]], 
                          frida.data.df$Model[frida.data.df$variable==variable.names[4]],
                          frida.data.df$Scenario[frida.data.df$variable==variable.names[4]],
                          frida.data.df$low2[frida.data.df$variable==variable.names[4]],
                          frida.data.df$low1[frida.data.df$variable==variable.names[4]],
                          frida.data.df$high1[frida.data.df$variable==variable.names[4]],
                          frida.data.df$high2[frida.data.df$variable==variable.names[4]])
plot.slr.ci <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[9]], 
                          frida.data.df$Model[frida.data.df$variable==variable.names[9]],
                          frida.data.df$Scenario[frida.data.df$variable==variable.names[9]],
                          frida.data.df$low2[frida.data.df$variable==variable.names[9]],
                          frida.data.df$low1[frida.data.df$variable==variable.names[9]],
                          frida.data.df$high1[frida.data.df$variable==variable.names[9]],
                          frida.data.df$high2[frida.data.df$variable==variable.names[9]])
plot.ren.energy.ci <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[6]], 
                                 frida.data.df$Model[frida.data.df$variable==variable.names[6]],
                                 frida.data.df$Scenario[frida.data.df$variable==variable.names[6]],
                                 frida.data.df$low2[frida.data.df$variable==variable.names[6]]*energy.TWh.to.EJ,
                                 frida.data.df$low1[frida.data.df$variable==variable.names[6]]*energy.TWh.to.EJ,
                                 frida.data.df$high1[frida.data.df$variable==variable.names[6]]*energy.TWh.to.EJ,
                                 frida.data.df$high2[frida.data.df$variable==variable.names[6]]*energy.TWh.to.EJ)
plot.fossil.energy.ci <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[7]], 
                                    frida.data.df$Model[frida.data.df$variable==variable.names[7]],
                                    frida.data.df$Scenario[frida.data.df$variable==variable.names[7]],
                                    frida.data.df$low2[frida.data.df$variable==variable.names[7]]*energy.TWh.to.EJ,
                                    frida.data.df$low1[frida.data.df$variable==variable.names[7]]*energy.TWh.to.EJ,
                                    frida.data.df$high1[frida.data.df$variable==variable.names[7]]*energy.TWh.to.EJ,
                                    frida.data.df$high2[frida.data.df$variable==variable.names[7]]*energy.TWh.to.EJ)
plot.total.energy.ci <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[8]], 
                                   frida.data.df$Model[frida.data.df$variable==variable.names[8]],
                                   frida.data.df$Scenario[frida.data.df$variable==variable.names[8]],
                                   frida.data.df$low2[frida.data.df$variable==variable.names[8]]*energy.TWh.to.EJ,
                                   frida.data.df$low1[frida.data.df$variable==variable.names[8]]*energy.TWh.to.EJ,
                                   frida.data.df$high1[frida.data.df$variable==variable.names[8]]*energy.TWh.to.EJ,
                                   frida.data.df$high2[frida.data.df$variable==variable.names[8]]*energy.TWh.to.EJ)
plot.gdp.per.person.ci <-  data.frame(frida.data.df$year[frida.data.df$variable==variable.names[3]], 
                                      frida.data.df$Model[frida.data.df$variable==variable.names[3]],
                                      frida.data.df$Scenario[frida.data.df$variable==variable.names[3]],
                                      frida.data.df$low2[frida.data.df$variable==variable.names[3]]/deflator.value.2005,
                                      frida.data.df$low1[frida.data.df$variable==variable.names[3]]/deflator.value.2005,
                                      frida.data.df$high1[frida.data.df$variable==variable.names[3]]/deflator.value.2005,
                                      frida.data.df$high2[frida.data.df$variable==variable.names[3]]/deflator.value.2005)

colnames(plot.sta.ci) <- colnames(plot.forest.land.ci) <- colnames(plot.animal.products.demand.ci) <-
  colnames(plot.gdp.per.person.ci) <- colnames(plot.population.ci) <-
  colnames(plot.slr.ci) <- colnames(plot.total.energy.ci) <- 
  colnames(plot.ren.energy.ci) <- colnames(plot.fossil.energy.ci) <- c("Year","Model","Scenario","low2","low1","high1","high2")

plot.data.ci <- map_df(.x=list("(a) Surf. Temp. Anomaly (°C)"=plot.sta.ci,
                               "(b) GDP per per. (2005b$/mp/Yr)"=plot.gdp.per.person.ci,
                               "(c) Sea Level Rise (meters)"=plot.slr.ci,
                               "(d) Population (Billion)"= plot.population.ci,
                               "(e) Animal Pr. Dem. (ECal/Yr)"=plot.animal.products.demand.ci,
                               "(f) Forest (bHa)"=plot.forest.land.ci,
                               "(g) Ren. Energy Output (EJ/Yr)"=plot.ren.energy.ci,
                               "(h) Fossil Energy Output (EJ/Yr)"=plot.fossil.energy.ci,
                               "(i) Total Energy Output (EJ/Yr)"=plot.total.energy.ci),
                        .f=bind_rows, .id="name")


# Separate energy variables from the rest for the sake of adjusting y-scales in the plot.
panel.rest.emb.ci <-  plot.data.ci[!plot.data.ci$name %in% c("(g) Ren. Energy Output (EJ/Yr)",
                                                              "(h) Fossil Energy Output (EJ/Yr)",
                                                              "(i) Total Energy Output (EJ/Yr)") & plot.data.ci$Scenario %in% "EMB",]
panel.energy.emb.ci <-  plot.data.ci[plot.data.ci$name %in% c("(g) Ren. Energy Output (EJ/Yr)",
                                                               "(h) Fossil Energy Output (EJ/Yr)",
                                                               "(i) Total Energy Output (EJ/Yr)") & plot.data.ci$Scenario %in% "EMB",]

panel.rest.median <- plot.data.all[!plot.data.all$name %in% c("(g) Ren. Energy Output (EJ/Yr)",
                                                                 "(h) Fossil Energy Output (EJ/Yr)",
                                                                 "(i) Total Energy Output (EJ/Yr)"),]

panel.energy.median <- plot.data.all[plot.data.all$name %in% c("(g) Ren. Energy Output (EJ/Yr)",
                                                              "(h) Fossil Energy Output (EJ/Yr)",
                                                              "(i) Total Energy Output (EJ/Yr)"),]


panel.plot.rest <- ggplot()+
  geom_line(data=panel.rest.median,mapping=aes(x=as.numeric(Year),y=Data,
                                           color=Scenario,linetype=Model,linewidth=Scenario))+
  geom_point(data=panel.rest.median,mapping=aes(x=as.numeric(Year),y=Data,
                                            shape=Model, size=Model, fill=Model))+
  facet_wrap(~name,scale="free_y")+
  geom_ribbon(data=panel.rest.emb.ci,aes(x=as.numeric(Year),ymax=high1,ymin=low1),alpha=0.4)+
  geom_ribbon(data=panel.rest.emb.ci, aes(x=as.numeric(Year),ymax=high2,ymin=low2),alpha=0.2)+
  theme_bw()+
  scale_color_manual(values=c("red","black","green","green4","magenta","orange","brown","blue"))+
  scale_linetype_manual(values=c("dotted","dotted","solid","dashed","longdash","dotdash","twodash","dotted"))+
  scale_shape_manual(values=c(0,21,NA,5,1,2,3,4))+
  scale_size_manual(values=c(2,2,NA,2,2,2,2,2,2))+
  scale_fill_manual(values=c(NA,"red",NA,NA,NA,NA,NA,NA,NA))+
  scale_linewidth_manual(values=c(1,1,1,1,1,1,1,1))+
  labs(x=NULL,y=NULL,title=NULL)+
  scale_x_continuous(breaks = seq(1980,2150,20),expand=c(0,0))+
  theme(strip.background =element_rect(fill="white"),
        strip.text = element_text(size=14, family="serif"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=14, family="serif"),
        legend.position = "none")


panel.plot.energy <- ggplot()+
  geom_line(data=panel.energy.median,mapping=aes(x=as.numeric(Year),y=Data,
                                               color=Scenario,linetype=Model,linewidth=Scenario))+
  geom_point(data=panel.energy.median,mapping=aes(x=as.numeric(Year),y=Data,
                                                shape=Model, size=Model, fill=Model))+
  facet_rep_wrap(~name,scales="fixed", repeat.tick.labels = "left")+
  geom_ribbon(data=panel.energy.emb.ci,aes(x=as.numeric(Year),ymax=high1,ymin=low1),alpha=0.4)+
  geom_ribbon(data=panel.energy.emb.ci, aes(x=as.numeric(Year),ymax=high2,ymin=low2),alpha=0.2)+
  theme_bw()+
  scale_color_manual(values=c("red","black","green","green4","magenta","orange","brown","blue"))+
  scale_linetype_manual(values=c("dotted","dotted","solid","dashed","longdash","dotdash","twodash","dotted"))+
  scale_shape_manual(values=c(0,21,NA,5,1,2,3,4))+
  scale_size_manual(values=c(2,2,NA,2,2,2,2,2))+
  scale_fill_manual(values=c(NA,"red",NA,NA,NA,NA,NA,NA,NA))+
  scale_linewidth_manual(values=c(1,1,1,1,1,1,1,1))+
  labs(x=NULL,y=NULL,title=NULL)+
  scale_x_continuous(breaks = seq(1980,2150,20),expand=c(0,0))+
  theme(strip.background =element_rect(fill="white"),
        strip.text = element_text(size=14, family="serif"),
        axis.text.x = element_text(size=14, angle = 90, family="serif"),
        axis.text.y = element_text(size=14, family="serif"),
        legend.text = element_text(size=14), 
        legend.key.size = unit(1.5,"cm"),
        legend.key.spacing.x= unit(.05,"cm"),
        legend.key.spacing.y= unit(.01,"mm"),
        legend.spacing.y = unit(.01,"cm"),
        legend.box="vertical",
        legend.direction = "vertical",
        legend.title = element_blank()
  )+
  guides(color= guide_legend(nrow =1, ncol=7, position = "bottom"),
         linetype=guide_legend(nrow = 1, ncol=8, position = "bottom"))

ggarrange(panel.plot.rest,panel.plot.energy,nrow=2,ncol=1)

# To plot any particular FRIDA variable, to check the uncertainty range (if its too narrow compared to the range of all the IAMs)
var.of.interest <- "(h) Fossil Energy Output (EJ/Yr)"
df.median <- panel.energy.median[panel.energy.median$name %in% var.of.interest & 
                                   panel.energy.median$Model %in% "FRIDA",]
df.ci <- panel.energy.emb.ci[panel.energy.emb.ci$name %in% var.of.interest & 
                                   panel.energy.emb.ci$Model %in% "FRIDA",]

ggplot()+
  geom_line(data=df.median,mapping=aes(x=as.numeric(Year),y=Data,
                                                 color=Scenario,linetype=Model,linewidth=Scenario))+
  geom_ribbon(data=df.ci,aes(x=as.numeric(Year),ymax=high1,ymin=low1),alpha=0.4)+
  geom_ribbon(data=df.ci, aes(x=as.numeric(Year),ymax=high2,ymin=low2),alpha=0.2)+
  theme_bw()+
  scale_color_manual(values=c("black"))+
  scale_linetype_manual(values=c("solid"))+
  scale_linewidth_manual(values=c(2))+
  labs(x=NULL,y=NULL,title=var.of.interest)+
  scale_x_continuous(breaks = seq(1980,2150,20),expand=c(0,0))+
  theme(strip.background =element_rect(fill="white"),
        strip.text = element_text(size=14, family="serif"),
        axis.text.x = element_text(size=14, angle = 90, family="serif"),
        axis.text.y = element_text(size=14, family="serif"),
        legend.position = "none")
