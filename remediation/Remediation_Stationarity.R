# MRM Inquiry Response Support (Inquiry 18)
# Bank of Hope
# Original Developer: Omar Lopez
# Start Date: 01/24/2018

setwd("C:/Users/OL07805/Desktop/Desktop Things/Net Charge Off Models/MRM Inquiries/MRM Inquiry 18/")

source("BOH_dev_support.R")

library(dplyr)
library(data.table)
library(ggplot2)
library(lubridate)
library(zoo)

# Read in data used to train models

load("S3_00_Estimation_Sample_with_MEV_20171117")

########################################################################################################################
### Dependent Variable Stationary Testing
nooPeers <- c("Bank of Hope","Cathay General Bancorp","EAST WEST BANCORP","UMPQUA BANK","Western Alliance")
mfPeers <- c("Bank of Hope","Banner Corporation","Cathay General Bancorp","Columbia Banking System","EAST WEST BANCORP","PacWest Bancorp","UMPQUA BANK")
# Design dataframe to put into stationary function

datesA <- unique(data3$Date)
data4 <- as.data.frame(c())
data4[1:64,"fileDate"] <- as.yearqtr(datesA[1:64])
data4$fileDate <- as.yearqtr(data4$fileDate)

# Define segments to get data from main datatable
segs <- c("CnI","OOCRE","NOOCRE","MF")
# Loop and get data from main table and put into different columns
for(i in segs){
  if(i %in% c("MF","NOOCRE")){
    if(i == "MF"){
      peersA <- mfPeers
    }
    else{
      peersA <- nooPeers
    }
    for(peer in peersA){
      tempData <- data3 %>% filter(Portfolio2 == i,ENTITY_NAME == peer) %>% select(Date,NCOR)
      names(tempData) <- c("fileDate",paste0("NCOR_",i,"_",peer))
      # Join to designed dataframe
      data4 <- left_join(data4,tempData,by = "fileDate")
    }
    
  }
  else{
    tempData <- data3 %>% filter(Portfolio2 == i,ENTITY_NAME == "Bank of Hope") %>% select(Date,NCOR)
    names(tempData) <- c("fileDate",paste0("NCOR_",i))
    # Join to designed dataframe
    data4 <- left_join(data4,tempData,by = "fileDate")
  }

}

stationaryDependent <- get.Stationary.Results(names(data4)[!grepl("fileDate",names(data4))],data4,
                                           sigLevel = 0.1)


########################################################################################################################
### Residual Stationary Testing

# First, train the models
data3[is.na(ENTITY_NAME)] <- "Bank of Hope"

# NOO CRE
# Peers are Cathay, East West, Umpqua, and western alliance
nooPeers <- c("Bank of Hope","Cathay General Bancorp","EAST WEST BANCORP","UMPQUA BANK","Western Alliance")
nooModel <- paste(c("ENTITY_NAME","empl_yg_EWMA4_lag1","rgdp_grw_NL_lag3","crei_yg_EWMA4_lag3"), collapse = "+")

# MF CRE
#Peers are Banner, Cathay, Columbia, East West, PacWest, and Umpqua 
mfPeers <- c("Bank of Hope","Banner Corporation","Cathay General Bancorp","Columbia Banking System","EAST WEST BANCORP","PacWest Bancorp","UMPQUA BANK")
mfModel <- paste(c("ENTITY_NAME","empl_qg_EWMA2_lag3","gdp_grw_yoy_NL_lag2"),collapse = "+")

# C&I
ciModel <- paste(c("ca_rgsp_yg_EWMA4","ca_unemp_yd_EWMA4","vix_qd_lag4","prime_spread_log_qd","ca_hpi_yg_EWMA4_lag4"),collapse = "+")

# OOCRE
ooModel <- paste(c("empl_yg_EWMA4_lag4","crei_yg_EWMA2_lag4","prime_spread_log_qd_EWMA2_lag4"),collapse = "+")

# train models on relevant data

PANEL_NOO_FIT <- lm(paste0(c("NCOR ~ ",nooModel)),data = data3[(ENTITY_NAME %in% nooPeers) & (Date >= "2007 Q1" & Date <= "2016 Q4") & (Portfolio2 == "NOOCRE")])
PANEL_MF_FIT <- lm(paste0(c("NCOR ~ ",mfModel)),data = data3[(ENTITY_NAME %in% mfPeers) & (Date >= "2007 Q1" & Date <= "2016 Q4") & (Portfolio2 == "MF")])

OLS_CI_FIT <- lm(paste0(c("NCOR ~ ",ciModel)),data = data3[(ENTITY_NAME == "Bank of Hope") &(Date >= "2001 Q1" & Date <= "2016 Q4") & (Portfolio2 == "CnI")])
OLS_OO_FIT <- lm(paste0(c("NCOR ~ ",ooModel)),data = data3[(ENTITY_NAME == "Bank of Hope") &(Date >= "2007 Q1" & Date <= "2016 Q4") & (Portfolio2 == "OOCRE")])

# Loop across segments and get residuals for BOH
for(i in segs){
  # Specify which model to use for each segment
  if(i == "CnI"){
    fit <- OLS_CI_FIT
  }
  else if(i == "OOCRE"){
    fit <- OLS_OO_FIT
  }
  else if(i == "NOOCRE"){
    fit <- PANEL_NOO_FIT
  }
  else{
    fit <- PANEL_MF_FIT
  }
  # Do another loop for all panel banks
  if(i %in% c("MF","NOOCRE")){
    if(i == "MF"){
      peersA <- mfPeers
    }
    else{
      peersA <- nooPeers
    }
    for(peer in peersA){
      # Make training dataset
      train <- filter(data3,Portfolio2 == i,ENTITY_NAME == peer,Scenario == "Historic")
      # Get model predictions
      train[["predict"]] <- predict(fit,train)
      # Get residuals
      train[[paste0("resid_",i,"_",peer)]] <- train[["NCOR"]] - train[["predict"]]
      # Extract only Date and residuals
      train <- train[,c("Date",paste0("resid_",i,"_",peer))]
      names(train)[1] <- "fileDate"
      # Store residuals in other dataframe
      data4 <- left_join(data4,train,by = "fileDate")
    }
    
  }
  else{
    # Make training dataset
    train <- filter(data3,Portfolio2 == i,ENTITY_NAME == "Bank of Hope",Scenario == "Historic")
    # Get model predictions
    train[["predict"]] <- predict(fit,train)
    # Get residuals
    train[[paste0("resid_",i)]] <- train[["NCOR"]] - train[["predict"]]
    # Extract only Date and residuals
    train <- train[,c("Date",paste0("resid_",i))]
    names(train)[1] <- "fileDate"
    # Store residuals in other dataframe
    data4 <- left_join(data4,train,by = "fileDate")
  }
}

# Get results and merge
stationaryResiduals <- get.Stationary.Results(names(data4)[grepl("resid",names(data4))],
                                              data4,sigLevel = 0.1)


stationaryTable <- rbind(stationaryDependent,stationaryResiduals)
write.csv(stationaryTable,"stationaryTable.csv",row.names = F)

########################################################################################################################
### Macroeconomic Data

# Read in MEV data

mev <- fread("S0_09_MEV_data_transformed_111717.csv")

# Define macrovars used in models
nooMEV <- c("empl_yg_EWMA4_lag1","rgdp_grw_NL_lag3","crei_yg_EWMA4_lag3")
mfMEV <- c("empl_qg_EWMA2_lag3","gdp_grw_yoy_NL_lag2")
ciMEV <- c("ca_rgsp_yg_EWMA4","ca_unemp_yd_EWMA4","vix_qd_lag4","prime_spread_log_qd","ca_hpi_yg_EWMA4_lag4")
ooMEV <- c("empl_yg_EWMA4_lag4","crei_yg_EWMA2_lag4","prime_spread_log_qd_EWMA2_lag4")

# change col format to Date and then later filter based on deveelopment timeframe 
mev$Date <- as.Date(mev$Date)

# Get stationary results
mfStationary <- get.Stationary.Results(mfMEV,mev[Scenario == "Historic" & Date >= "2007-03-31"],sigLevel = 0.1)
nooStationary <- get.Stationary.Results(nooMEV,mev[Scenario == "Historic" & Date >= "2007-03-31"],sigLevel = 0.1)
ooStationary <- get.Stationary.Results(ooMEV,mev[Scenario == "Historic" & Date >= "2007-03-31"],sigLevel = 0.1)
ciStationary <- get.Stationary.Results(ciMEV,mev[Scenario == "Historic" & Date >= "2001-03-31"],sigLevel = 0.1)

finalStationary <- rbind(mfStationary,nooStationary,ooStationary,ciStationary)
write.csv(finalStationary,"finalStationary.csv",row.names = F)





