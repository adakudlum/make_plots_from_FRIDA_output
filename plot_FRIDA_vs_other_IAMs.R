library(tidyverse)
library("readxl")
library(abind)
library(ggtext)   # for highlighting legend title
library(gridExtra) # nice arrangement of panels
library(lemon)
library(ggpubr)

setwd('G:/My Drive/R/plots')

remove(list=ls())

# Get the energy variables from other IAMs
source('G:/My Drive/R/r-scripts/worldTrans/get_variables_from_other_IAMs.R')
source('G:/My Drive/R/r-scripts/worldTrans/get_variables_from_FRIDA.R')

# frida.output <- get.frida.vars() 
# iams.output <- get.iams.vars()

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
                                   forest.land.frida.df),
                                   forest.land.calibration,
                           .f=bind_rows)

plot.ren.energy <- map_df(.x=list(ren.energy.frida.df,
                                  total.ren.energy,
                                  ren.energy.calibration),
                          .f=bind_rows)

plot.fossil.energy <- map_df(.x=list(total.sec.fossil.energy,
                                     fossil.energy.frida.df,
                                     fossil.energy.calibration),
                             .f=bind_rows)

plot.total.energy <- map_df(.x=list(total.energy.output.iams,
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
