# 2018 DFAST Production Run Net Charge Off Models CRE and C&I
# Bank of Hope
# Developer: Omar Lopez
# Start Date: 02/26/2018
# R version 3.4.3 (2017-11-30)

library(dplyr)
library(lubridate)
library(zoo)
library(data.table)

setwd("C:/Users/OL07805/Desktop/Desktop Things/Net Charge Off Models/DFAST Production Run 2018/")

# Load in dev dataset

load("S3_00_Estimation_Sample_with_MEV_20171117")

# Read in MEV Data

mev <- fread("S0_09_MEV_data_transformed_111717.csv")

# SEt up peer banks to train models
mfPeers <- c("Bank of Hope","Banner Corporation","Cathay General Bancorp","Columbia Banking System"
             ,"EAST WEST BANCORP","PacWest Bancorp","UMPQUA BANK")

nooPeers <- c("Bank of Hope","Cathay General Bancorp","EAST WEST BANCORP","UMPQUA BANK","Western Alliance")

# Set up variable formulas for models
ciVars <- paste(c("ca_rgsp_yg_EWMA4","ca_unemp_yd_EWMA4","vix_qd_lag4","prime_spread_log_qd","ca_hpi_yg_EWMA4_lag4")
                ,collapse = "+")

ooVars <- paste(c("empl_yg_EWMA4_lag4","crei_yg_EWMA2_lag4","prime_spread_log_qd_EWMA2_lag4"),collapse = "+")

nooVars <- paste(c("ENTITY_NAME","empl_yg_EWMA4_lag1","rgdp_grw_NL_lag3","crei_yg_EWMA4_lag3"),collapse = "+")

mfVars <- paste(c("ENTITY_NAME","empl_qg_EWMA2_lag3","gdp_grw_yoy_NL_lag2"),collapse = "+")

# Train the models
ciModel <- lm(formula = paste(c("NCOR ~ ",ciVars))
              ,data = data3[Portfolio2 == "CnI" & ENTITY_NAME == "Bank of Hope" & Scenario == "Historic"])

ooModel <- lm(formula = paste(c("NCOR ~ ",ooVars))
              ,data = data3[Portfolio2 == "OOCRE" & ENTITY_NAME == "Bank of Hope" & Scenario == "Historic"])

nooModel <- lm(formula = paste(c("NCOR ~ ",nooVars))
               ,data = data3[Portfolio2 == "NOOCRE" & ENTITY_NAME %in% nooPeers & Scenario == "Historic"])

mfModel <- lm(formula = paste(c("NCOR ~ ",mfVars))
              ,data = data3[Portfolio2 == "MF" & ENTITY_NAME %in% mfPeers & Scenario == "Historic"])

# Forecast on MEV Data and store in a new table
mev$ENTITY_NAME <- "Bank of Hope"
mev$Date <- as.Date(mev$Date)

forecastTable <- as.data.frame(c())
forecastTable[1:13,"qtr_dt"] <- as.Date(mev[Scenario == "Baseline",Date])
forecastTable$qtr_dt <- as.Date(forecastTable$qtr_dt)

segs <- c("CnI","OOCRE","NOOCRE","MF")

for(i in segs){
  if(i == "CnI"){
    fit <- ciModel
  }
  else if(i == "OOCRE"){
    fit <- ooModel
  }
  else if(i == "NOOCRE"){
    fit <- nooModel
  }
  else{
    fit <- mfModel
  }
  
  forecastTable[1:13,paste0("NCOR_Baseline",i)] <- predict(fit,mev[Scenario == "Baseline"])
  forecastTable[1:13,paste0("NCOR_Adverse",i)] <- predict(fit,mev[Scenario == "Adverse"])
  forecastTable[1:13,paste0("NCOR_Severe",i)] <- predict(fit,mev[Scenario == "Severe"])
  
}

write.csv(forecastTable,"2018_DFAST_NCOR_Forecasts.csv",row.names = F)

#########################################################################################################################
### Plot each segments growth rates and ending balances


forecastTable$qtr_dt <- as.Date(forecastTable$qtr_dt)



segLabels <- c("Commercial and Industrial","Multifamily","Non Owner Occupied","Owner Occupied")
segs <- c("CnI","MF","NOOCRE","OOCRE")
rowCt <- 1

# Start with growth rates
for(i in segs){
  plotA <- (ggplot(aes(x = qtr_dt),data = forecastTable)
            + geom_line(aes(y = get(paste0("NCOR_Baseline",i)),color = "Baseline"))
            + geom_line(aes(y = get(paste0("NCOR_Adverse",i)),color = "Adverse"))
            + geom_line(aes(y = get(paste0("NCOR_Severe",i)),color = "Severe"))
            + scale_color_manual(values = c("Baseline" = "green","Adverse" = "orange","Severe" = "red"))
            + labs(y = "NCOR",x = "Date",title = paste0(segLabels[rowCt]," Net Charge Off Rate"),color = "Scenario")
            + theme_minimal()
            + theme(plot.title = element_text(hjust = 0.5))
            + scale_y_continuous(labels = scales::percent))
  #plot(plotA)
  ggsave(paste0("plot_NCOR_Forecast_",segLabels[rowCt],".png"), plot=plotA, width=20, height=11, unit="cm", dpi=500)
  rowCt <- rowCt + 1
}






