library(tidyverse)
library("readxl")
library(abind)
library(ggpubr)
remove(list=ls())

setwd('G:/My Drive/R/plots')
# path to directory where the EMB data exists
filepath.frida <- ("G:/My Drive/Data/NumSample/plotData_EMB/")

# real gross income
non.bank.profits <- readRDS(paste(filepath.frida,"circular_flow_non_bank_profits_in_2021c-fit uncertainty-equaly-weighted.RDS",sep=""))
bank.profits <- readRDS(paste(filepath.frida,"circular_flow_bank_profits_in_2021c-fit uncertainty-equaly-weighted.RDS",sep=""))
rent.after.tax <- readRDS(paste(filepath.frida,"circular_flow_rent_in_in_2021c-fit uncertainty-equaly-weighted.RDS",sep=""))
owner.savings.interest <- readRDS(paste(filepath.frida,"circular_flow_owner_savings_interest_in_2021c-fit uncertainty-equaly-weighted.RDS",sep=""))
transfers <- readRDS(paste(filepath.frida,"circular_flow_transfers_in_2021c-fit uncertainty-equaly-weighted.RDS",sep=""))
worker.savings.interest <- readRDS(paste(filepath.frida,"circular_flow_worker_savings_interest_in_2021c-fit uncertainty-equaly-weighted.RDS",sep=""))
wages.after.tax <- readRDS(paste(filepath.frida,"circular_flow_wages_in_2021c-fit uncertainty-equaly-weighted.RDS",sep=""))
profit.tax <- readRDS(paste(filepath.frida,"circular_flow_profit_tax_in_2021c-fit uncertainty-equaly-weighted.RDS",sep=""))
rent.tax <- readRDS(paste(filepath.frida,"circular_flow_rent_tax_in_2021c-fit uncertainty-equaly-weighted.RDS",sep=""))
wage.tax <- readRDS(paste(filepath.frida,"circular_flow_wage_tax_in_2021c-fit uncertainty-equaly-weighted.RDS",sep=""))

# Real GDP
owner.consumption <- readRDS(paste(filepath.frida,"gdp_owner_consumption_in_2021c-fit uncertainty-equaly-weighted.RDS",sep=""))
worker.consumption <- readRDS(paste(filepath.frida,"gdp_worker_consumption_excluding_transfers_in_2021c-fit uncertainty-equaly-weighted.RDS",sep=""))
gov.consumption.without.climate.repair <-readRDS(paste(filepath.frida,"government_government_spending_without_climate_impact_in_2021c-fit uncertainty-equaly-weighted.RDS",sep=""))
gov.consumption.with.climate.repair <-readRDS(paste(filepath.frida,"government_government_spending_because_of_climate_in_2021c-fit uncertainty-equaly-weighted.RDS",sep=""))
gov.transfers <- readRDS(paste(filepath.frida,"gdp_government_transfers_in_2021c-fit uncertainty-equaly-weighted.RDS",sep=""))
gov.investment <- readRDS(paste(filepath.frida,"gdp_public_investment_in_2021c-fit uncertainty-equaly-weighted.RDS",sep=""))
private.investment <- readRDS(paste(filepath.frida,"gdp_private_investment_in_in_2021c-fit uncertainty-equaly-weighted.RDS",sep=""))

# Process variable: income
non.bank.profits.frida      <- as.data.frame(non.bank.profits$ciBounds)
colnames(non.bank.profits.frida) <- c("low2","low1","median","high1","high2")
non.bank.profits.frida$Year <- as.numeric(non.bank.profits$years)
non.bank.profits.frida.df <- data.frame(non.bank.profits.frida$Year,non.bank.profits.frida$median)
colnames(non.bank.profits.frida.df) <- c("Year","Data")
non.bank.profits.frida.df$Scenario <- "EMB"
non.bank.profits.frida.df$Variable  <- "Real non.bank.profits in 2021"
non.bank.profits.frida.df$Unit   <- "T$/Yr"

bank.profits.frida      <- as.data.frame(bank.profits$ciBounds)
colnames(bank.profits.frida) <- c("low2","low1","median","high1","high2")
bank.profits.frida$Year <- as.numeric(bank.profits$years)
bank.profits.frida.df <- data.frame(bank.profits.frida$Year,bank.profits.frida$median)
colnames(bank.profits.frida.df) <- c("Year","Data")
bank.profits.frida.df$Scenario <- "EMB"
bank.profits.frida.df$Variable  <- "Real bank.profits in 2021"
bank.profits.frida.df$Unit   <- "T$/Yr"

rent.after.tax.frida      <- as.data.frame(rent.after.tax$ciBounds)
colnames(rent.after.tax.frida) <- c("low2","low1","median","high1","high2")
rent.after.tax.frida$Year <- as.numeric(rent.after.tax$years)
rent.after.tax.frida.df <- data.frame(rent.after.tax.frida$Year,rent.after.tax.frida$median)
colnames(rent.after.tax.frida.df) <- c("Year","Data")
rent.after.tax.frida.df$Scenario <- "EMB"
rent.after.tax.frida.df$Variable  <- "Real rent.after.tax in 2021"
rent.after.tax.frida.df$Unit   <- "T$/Yr"

owner.savings.interest.frida      <- as.data.frame(owner.savings.interest$ciBounds)
colnames(owner.savings.interest.frida) <- c("low2","low1","median","high1","high2")
owner.savings.interest.frida$Year <- as.numeric(owner.savings.interest$years)
owner.savings.interest.frida.df <- data.frame(owner.savings.interest.frida$Year,owner.savings.interest.frida$median)
colnames(owner.savings.interest.frida.df) <- c("Year","Data")
owner.savings.interest.frida.df$Scenario <- "EMB"
owner.savings.interest.frida.df$Variable  <- "Real owner.savings.interest in 2021"
owner.savings.interest.frida.df$Unit   <- "T$/Yr"

transfers.frida      <- as.data.frame(transfers$ciBounds)
colnames(transfers.frida) <- c("low2","low1","median","high1","high2")
transfers.frida$Year <- as.numeric(transfers$years)
transfers.frida.df <- data.frame(transfers.frida$Year,transfers.frida$median)
colnames(transfers.frida.df) <- c("Year","Data")
transfers.frida.df$Scenario <- "EMB"
transfers.frida.df$Variable  <- "Real transfers in 2021"
transfers.frida.df$Unit   <- "T$/Yr"

worker.savings.interest.frida      <- as.data.frame(worker.savings.interest$ciBounds)
colnames(worker.savings.interest.frida) <- c("low2","low1","median","high1","high2")
worker.savings.interest.frida$Year <- as.numeric(worker.savings.interest$years)
worker.savings.interest.frida.df <- data.frame(worker.savings.interest.frida$Year,worker.savings.interest.frida$median)
colnames(worker.savings.interest.frida.df) <- c("Year","Data")
worker.savings.interest.frida.df$Scenario <- "EMB"
worker.savings.interest.frida.df$Variable  <- "Real worker.savings.interest in 2021"
worker.savings.interest.frida.df$Unit   <- "T$/Yr"

wages.after.tax.frida      <- as.data.frame(wages.after.tax$ciBounds)
colnames(wages.after.tax.frida) <- c("low2","low1","median","high1","high2")
wages.after.tax.frida$Year <- as.numeric(wages.after.tax$years)
wages.after.tax.frida.df <- data.frame(wages.after.tax.frida$Year,wages.after.tax.frida$median)
colnames(wages.after.tax.frida.df) <- c("Year","Data")
wages.after.tax.frida.df$Scenario <- "EMB"
wages.after.tax.frida.df$Variable  <- "Real wages.after.tax in 2021"
wages.after.tax.frida.df$Unit   <- "T$/Yr"

profit.tax.frida      <- as.data.frame(profit.tax$ciBounds)
colnames(profit.tax.frida) <- c("low2","low1","median","high1","high2")
profit.tax.frida$Year <- as.numeric(profit.tax$years)
profit.tax.frida.df <- data.frame(profit.tax.frida$Year,profit.tax.frida$median)
colnames(profit.tax.frida.df) <- c("Year","Data")
profit.tax.frida.df$Scenario <- "EMB"
profit.tax.frida.df$Variable  <- "Real profit.tax in 2021"
profit.tax.frida.df$Unit   <- "T$/Yr"

rent.tax.frida      <- as.data.frame(rent.tax$ciBounds)
colnames(rent.tax.frida) <- c("low2","low1","median","high1","high2")
rent.tax.frida$Year <- as.numeric(rent.tax$years)
rent.tax.frida.df <- data.frame(rent.tax.frida$Year,rent.tax.frida$median)
colnames(rent.tax.frida.df) <- c("Year","Data")
rent.tax.frida.df$Scenario <- "EMB"
rent.tax.frida.df$Variable  <- "Real rent.tax in 2021"
rent.tax.frida.df$Unit   <- "T$/Yr"

wage.tax.frida      <- as.data.frame(wage.tax$ciBounds)
colnames(wage.tax.frida) <- c("low2","low1","median","high1","high2")
wage.tax.frida$Year <- as.numeric(wage.tax$years)
wage.tax.frida.df <- data.frame(wage.tax.frida$Year,wage.tax.frida$median)
colnames(wage.tax.frida.df) <- c("Year","Data")
wage.tax.frida.df$Scenario <- "EMB"
wage.tax.frida.df$Variable  <- "Real wage.tax in 2021"
wage.tax.frida.df$Unit   <- "T$/Yr"

# Process variable: gdp
owner.consumption.frida      <- as.data.frame(owner.consumption$ciBounds)
colnames(owner.consumption.frida) <- c("low2","low1","median","high1","high2")
owner.consumption.frida$Year <- as.numeric(owner.consumption$years)
owner.consumption.frida.df <- data.frame(owner.consumption.frida$Year,owner.consumption.frida$median)
colnames(owner.consumption.frida.df) <- c("Year","Data")
owner.consumption.frida.df$Scenario <- "EMB"
owner.consumption.frida.df$Variable  <- "Real owner.consumption in 2021"
owner.consumption.frida.df$Unit   <- "T$/Yr"

worker.consumption.frida      <- as.data.frame(worker.consumption$ciBounds)
colnames(worker.consumption.frida) <- c("low2","low1","median","high1","high2")
worker.consumption.frida$Year <- as.numeric(worker.consumption$years)
worker.consumption.frida.df <- data.frame(worker.consumption.frida$Year,worker.consumption.frida$median)
colnames(worker.consumption.frida.df) <- c("Year","Data")
worker.consumption.frida.df$Scenario <- "EMB"
worker.consumption.frida.df$Variable  <- "Real worker.consumption in 2021"
worker.consumption.frida.df$Unit   <- "T$/Yr"

gov.consumption.without.climate.repair.frida      <- as.data.frame(gov.consumption.without.climate.repair$ciBounds)
colnames(gov.consumption.without.climate.repair.frida) <- c("low2","low1","median","high1","high2")
gov.consumption.without.climate.repair.frida$Year <- as.numeric(gov.consumption.without.climate.repair$years)
gov.consumption.without.climate.repair.frida.df <- data.frame(gov.consumption.without.climate.repair.frida$Year,gov.consumption.without.climate.repair.frida$median)
colnames(gov.consumption.without.climate.repair.frida.df) <- c("Year","Data")
gov.consumption.without.climate.repair.frida.df$Scenario <- "EMB"
gov.consumption.without.climate.repair.frida.df$Variable  <- "Real gov.consumption.without.climate.repair in 2021"
gov.consumption.without.climate.repair.frida.df$Unit   <- "T$/Yr"

gov.consumption.with.climate.repair.frida      <- as.data.frame(gov.consumption.with.climate.repair$ciBounds)
colnames(gov.consumption.with.climate.repair.frida) <- c("low2","low1","median","high1","high2")
gov.consumption.with.climate.repair.frida$Year <- as.numeric(gov.consumption.with.climate.repair$years)
gov.consumption.with.climate.repair.frida.df <- data.frame(gov.consumption.with.climate.repair.frida$Year,gov.consumption.with.climate.repair.frida$median)
colnames(gov.consumption.with.climate.repair.frida.df) <- c("Year","Data")
gov.consumption.with.climate.repair.frida.df$Scenario <- "EMB"
gov.consumption.with.climate.repair.frida.df$Variable  <- "Real gov.consumption.with.climate.repair in 2021"
gov.consumption.with.climate.repair.frida.df$Unit   <- "T$/Yr"

gov.transfers.frida      <- as.data.frame(gov.transfers$ciBounds)
colnames(gov.transfers.frida) <- c("low2","low1","median","high1","high2")
gov.transfers.frida$Year <- as.numeric(gov.transfers$years)
gov.transfers.frida.df <- data.frame(gov.transfers.frida$Year,gov.transfers.frida$median)
colnames(gov.transfers.frida.df) <- c("Year","Data")
gov.transfers.frida.df$Scenario <- "EMB"
gov.transfers.frida.df$Variable  <- "Real gov.transfers in 2021"
gov.transfers.frida.df$Unit   <- "T$/Yr"

private.investment.frida      <- as.data.frame(private.investment$ciBounds)
colnames(private.investment.frida) <- c("low2","low1","median","high1","high2")
private.investment.frida$Year <- as.numeric(private.investment$years)
private.investment.frida.df <- data.frame(private.investment.frida$Year,private.investment.frida$median)
colnames(private.investment.frida.df) <- c("Year","Data")
private.investment.frida.df$Scenario <- "EMB"
private.investment.frida.df$Variable  <- "Real private.investment in 2021"
private.investment.frida.df$Unit   <- "T$/Yr"

gov.investment.frida      <- as.data.frame(gov.investment$ciBounds)
colnames(gov.investment.frida) <- c("low2","low1","median","high1","high2")
gov.investment.frida$Year <- as.numeric(gov.investment$years)
gov.investment.frida.df <- data.frame(gov.investment.frida$Year,gov.investment.frida$median)
colnames(gov.investment.frida.df) <- c("Year","Data")
gov.investment.frida.df$Scenario <- "EMB"
gov.investment.frida.df$Variable  <- "Real gov.investment in 2021"
gov.investment.frida.df$Unit   <- "T$/Yr"

plot.income <- map_df(.x=list("Owner profits after tax (non-bank)"=non.bank.profits.frida.df,
                              "Owner profits after tax (bank)"=bank.profits.frida.df,
                              "Owner rent income after tax"=rent.after.tax.frida.df,
                              "Owner income savings interest"=owner.savings.interest.frida.df,
                              "Worker income gov. transfers"=transfers.frida.df,
                              "Worker income savings interest"=worker.savings.interest.frida.df,
                              "Worker income wages after tax"=wages.after.tax.frida.df,
                              "Gov. income profit tax"=profit.tax.frida.df,
                              "Gov. income rent tax"=rent.tax.frida.df,
                              "Gov. income wage tax"=wage.tax.frida.df),
                      .f=bind_rows, .id="name")

plot.gdp  <- map_df(.x=list("Owner consumption"=owner.consumption.frida.df,
                            "Worker consumption excl. gov. transfers"=worker.consumption.frida.df,
                            "Gov. consumption without climate repairs"=gov.consumption.without.climate.repair.frida.df,
                            "Gov. consumption with climate repairs"=gov.consumption.with.climate.repair.frida.df,
                            "Gov. transfers"=gov.transfers.frida.df,
                            "Gov. investment"=gov.investment.frida.df,
                            "Private investment"=private.investment.frida.df),
                    .f=bind_rows,.id="name")

p1 <- ggplot() +
  geom_area(data=plot.income, mapping=aes(x=Year,y=Data*1e-3, 
                                               fill=factor(name)),
            color="grey4",linetype=1, linewidth=.5,show.legend=TRUE)+
  theme_bw()+
  labs(x=NULL,y=NULL,title="(a) Real gross income sources (2021c trill. $/Yr)")+
  #scale_fill_manual(values=c("green","grey","orange4","darkgreen", "red","red4","magenta","yellow4"))+
  #scale_color_manual(values=c("grey","brown"))+
  #scale_linetype_manual(values=c("dashed","dashed"))+
  scale_x_continuous(breaks = seq(1980,2130,20),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0),limits = c(0,1200))+
  theme(axis.text.x = element_text(size=12, angle = 90, family="sans"),
        axis.text.y = element_text(size=12, family="sans"),
        legend.text = element_text(size=11), 
        legend.background = element_blank(),
        legend.key.width = unit(.8,"cm"),
        legend.key.height = unit(.2,"cm"),
        legend.position = "inside",
        legend.position.inside = c(0.35,0.8),
        legend.key.spacing.x= unit(1,"cm"),
        legend.direction = "vertical",
        legend.box.just="left",
        legend.title=element_blank())

p2 <- ggplot() +
  geom_area(data=plot.gdp, mapping=aes(x=Year,y=Data*1e-3, 
                                          fill=factor(name)),
            color="grey4",linetype=1, linewidth=.5,show.legend=TRUE)+
  theme_bw()+
  labs(x=NULL,y=NULL,title="(b) Income usage (2021c trill. $/Yr)")+
  scale_x_continuous(breaks = seq(1980,2130,20),expand=c(0,0))+
  scale_y_continuous(breaks = seq(0,1000,200),expand=c(0,0),limits = c(0,1000))+
  theme(axis.text.x = element_text(size=12, angle = 90, family="sans"),
        axis.text.y = element_text(size=12, family="sans"),
        legend.text = element_text(size=11), 
        legend.background = element_blank(),
        legend.key.width = unit(.8,"cm"),
        legend.key.height = unit(.2,"cm"),
        legend.position = "inside",
        legend.position.inside = c(0.4,0.8),
        legend.key.spacing.x= unit(1,"cm"),
        legend.direction = "vertical",
        legend.box.just="left",
        legend.title=element_blank())

ggarrange(p1, p2,
            labels = NULL,
            ncol = 2, nrow = 1)
  