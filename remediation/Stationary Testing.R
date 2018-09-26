# 2018 DFAST Production Run Ending Balance CRE and C&I
# Bank of Hope
# Developer: Omar Lopez
# Start Date: 03/02/2018
# R version 3.4.3 (2017-11-30)

library(dplyr)
library(lubridate)
library(zoo)
library(data.table)
library(ggplot2)



setwd("C:/Users/OL07805/Desktop/Desktop Things/Ending Balance Model Final/Ending Balance Remediation Plan 09_25_18/Stationary Testing/")
source("dev-support.R")
#########################################################################################################################
### Read in Call Report Data

# Read in raw file of RCCI Schedule
cr1 <- fread("FFIEC CDR Call Schedule RCCI 12312017.txt")
# Bank of Hope IDRSSD
idRSSD <- 671464
cr1 <- cr1[IDRSSD == idRSSD]

# MF, NOO, OO, CnI
# RCON1460, RCONF160, RCONF161, RCON1766
startBal <- as.data.frame(cbind(c("mf","oo","no","ci"),c("RCON1460","RCONF160","RCONF161","RCON1766")))
names(startBal) <- c("segment","crID")

# Get relevant columns, I also checked them against the actual pdf as well, they match
startBal$balance <- as.numeric(cr1[,mget(as.character(startBal$crID))])
write.csv(startBal,"startBal.csv",row.names = F)
#########################################################################################################################
### Read in relevant datasets

# Read in training data
boh <- readRDS("data-boh.RDS")

# Read in new macroeconomic data
base <- readRDS("econ-data-baseline.RDS")
adverse <- readRDS("econ-data-adverse.RDS")
severe <- readRDS("econ-data-severe.RDS")
idios <- readRDS("econ-data-severe_Idiosyncratic.RDS")

#########################################################################################################################
### Train the models

# Define model variables and collapse with '+' for formula
ciVars <- paste(c("gdp_ag_lag1","ca_rinc_ag_lag3","inc_qg_lag2"),collapse = "+")
ipVars <- paste(c("crei_eg_lag4","dow_ya"),collapse = "+")
ooVars <- "crei_eg_lag2"

# Train models 

ciModel <- lm(formula = paste0("ldiff_ci ~ ",ciVars)
              ,data = boh[qtr_dt >= "2003-03-31" & qtr_dt <= "2016-12-31"])
ipModel <- lm(formula = paste0("ldiff_ip ~ ",ipVars)
              ,data = boh[qtr_dt >= "2007-06-30" & qtr_dt <= "2016-12-31"])
ooModel <- lm(formula = paste0("ldiff_oo ~ ",ooVars)
              ,data = boh[qtr_dt >= "2007-06-30" & qtr_dt <= "2016-12-31"])



# Final stationary table
finalStationaryTable <- as.data.frame(c())
# loop for each model
for(i in c("ci","ip","oo")){
  if(i == "ci"){
    begDate <- "2003-03-31"
  }
  else{
    begDate <- "2007-06-30"
  }
  # First the y variable
  depVar <- get.Stationary.Results(paste0("ldiff_",i),boh[qtr_dt >= begDate & qtr_dt <= "2016-12-31"],sigLevel = 0.1)
  # Then the residuals
  residVar <- get.Stationary.Results("residuals",get(paste0(i,"Model")),sigLevel = 0.1)
  residVar$Variable <- paste0(i,"_resid")
  finalStationaryTable <- rbind(finalStationaryTable,depVar,residVar)
  
}
#########################################################################################################################
# do stationary tests 

# Define vars to pull and perform testing on (keep CI separate since different timeframe)
ciVarStat <- c("gdp_ag_lag1","ca_rinc_ag_lag3","inc_qg_lag2")
modelVarStat <- c("crei_eg_lag4","dow_ya","crei_eg_lag2")
# Perform testing 
ciStationary <- get.Stationary.Results(ciVarStat,boh[qtr_dt >= "2003-03-31" & qtr_dt <= "2016-12-31"],sigLevel = 0.1)
restStationary <- get.Stationary.Results(modelVarStat,boh[qtr_dt >= "2007-06-30" & qtr_dt <= "2016-12-31"],sigLevel = 0.1)

finalStationaryTable <- rbind(finalStationaryTable, ciStationary,restStationary)


write.csv(finalStationaryTable,"finalStationaryTable.csv",row.names = F)
#########################################################################################################################

### Predict new growth rates and apply to ending balance. 

# Filter data so we only have relevant 9 quarter forecast
base <- base[qtr_dt >= "2018-03-31"]
adverse <- adverse[qtr_dt >= "2018-03-31"]
severe <- severe[qtr_dt >= "2018-03-31"]
idios <- idios[qtr_dt >= "2018-03-31"]

# Define segs for loop
segs <- c("ci","ip","oo")

# Define table to store forecasting information
forecastTable <- as.data.frame(c())
forecastTable[1:13,"qtr_dt"] <- as.yearqtr(unique(base$qtr_dt))
forecastTable$qtr_dt <- as.yearqtr(forecastTable$qtr_dt)

# Loop through each segment
for(i in segs){
  # If statements to choose the right model
  if(i == "ci"){
    fit <- ciModel
  }
  else if(i == "ip"){
    fit <- ipModel
  }
  else if(i == "oo"){
    fit <- ooModel
  }
  # After choosing model, get new forecast information for growth rates
  forecastTable[1:13,paste0("grw_base_",i)] <- predict(fit,base)
  forecastTable[1:13,paste0("grw_adverse_",i)] <- predict(fit,adverse)
  forecastTable[1:13,paste0("grw_severe_",i)] <- predict(fit,severe)
  forecastTable[1:13,paste0("grw_idiosyncratic_",i)] <- predict(fit,idios)
  # After getting growth rates, get ending balance
  # If income producing, get ending balance for mf and non owner using ip growth rates
  if(i == "ip"){
    for(j in c("mf","no")){
      forecastTable[1:13,paste0("eb_base_",j)] <- get_bal_forecast(startBal$balance[startBal$segment == j],forecastTable[1:13,paste0("grw_base_",i)])/1000000
      forecastTable[1:13,paste0("eb_adverse_",j)] <- get_bal_forecast(startBal$balance[startBal$segment == j],forecastTable[1:13,paste0("grw_adverse_",i)])/1000000
      forecastTable[1:13,paste0("eb_severe_",j)] <- get_bal_forecast(startBal$balance[startBal$segment == j],forecastTable[1:13,paste0("grw_severe_",i)])/1000000
      forecastTable[1:13,paste0("eb_idiosyncratic_",j)] <- get_bal_forecast(startBal$balance[startBal$segment == j],forecastTable[1:13,paste0("grw_idiosyncratic_",i)])/1000000
    }
  }
  # Otherwise just use relevant segment to get ending balance
  else{
    forecastTable[1:13,paste0("eb_base_",i)] <- get_bal_forecast(startBal$balance[startBal$segment == i],forecastTable[1:13,paste0("grw_base_",i)])/1000000
    forecastTable[1:13,paste0("eb_adverse_",i)] <- get_bal_forecast(startBal$balance[startBal$segment == i],forecastTable[1:13,paste0("grw_adverse_",i)])/1000000
    forecastTable[1:13,paste0("eb_severe_",i)] <- get_bal_forecast(startBal$balance[startBal$segment == i],forecastTable[1:13,paste0("grw_severe_",i)])/1000000
    forecastTable[1:13,paste0("eb_idiosyncratic_",i)] <- get_bal_forecast(startBal$balance[startBal$segment == i],forecastTable[1:13,paste0("grw_idiosyncratic_",i)])/1000000
  }
  
}

write.csv(forecastTable,"2018_DFAST_EB_Forecasts.csv",row.names = F)

#########################################################################################################################
### Plot each segments growth rates and ending balances


forecastTable$qtr_dt <- as.Date(forecastTable$qtr_dt)

segLabels <- c("Commercial and Industrial","Multifamily","Non Owner Occupied","Owner Occupied")
segs <- c("ci","mf","no","oo")
rowCt <- 1

# Start with growth rates
for(i in segs){
  plotA <- (ggplot(aes(x = qtr_dt),data = forecastTable)
            + geom_line(aes(y = get(paste0("eb_base_",i)),color = "Baseline"))
            + geom_line(aes(y = get(paste0("eb_adverse_",i)),color = "Adverse"))
            + geom_line(aes(y = get(paste0("eb_severe_",i)),color = "Severe"))
            + geom_line(aes(y = get(paste0("eb_idiosyncratic_",i)),color = "Idiosyncratic"),linetype = "dashed")
            + scale_color_manual(values = c("Baseline" = "green","Adverse" = "orange","Severe" = "red","Idiosyncratic" = "lightcoral"))
            + labs(y = "Ending Balance ($ Billions)",x = "Date",title = paste0(segLabels[rowCt]," Ending Balance"),color = "Scenario")
            + theme_minimal()
            + theme(plot.title = element_text(hjust = 0.5)))
  #plot(plotA)
  ggsave(paste0("plot_EB_Forecast_",segLabels[rowCt],".png"), plot=plotA, width=20, height=11, unit="cm", dpi=500)
  rowCt <- rowCt + 1
}













#########################################################################################################################

