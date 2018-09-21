##############################################################################
## File Name: R03_Merging.R
## Author: KZ
## Date: 5/1/2017 Created
## Purpose: To import and merge BBCN and Wilshire data accoring to 
##          "03 - merging.sas"
##############################################################################


requirements <- c("dplyr", "reshape2", "data.table","zoo")
for(rr in requirements){
  if(! rr %in% installed.packages()) install.packages(rr)
}
require(dplyr)
require(reshape2)
require(data.table)
require(zoo)

setwd("C:/Users/OL07805/Desktop/DFAST/")

## Import BBCN and Wilshire Data (SAS File 03, Line 1 to 49)
df_final_bbcn <- read.csv("PD/Code to Create input file/df_final_bbcn.csv")
df_final_wilshire <- read.csv("PD/Code to Create input file/df_final_wilshire.csv")
df_boh <- rbind(df_final_bbcn, df_final_wilshire)
df_boh$fileDate <- as.Date(df_boh$fileDate, "%Y-%m-%d")
df_boh$origination_date <- as.Date(df_boh$origination_date, "%Y-%m-%d")
df_boh$maturity_date <- as.Date(df_boh$maturity_date,"%Y-%m-%d")

## Expand data from 2016Q2 to 2018Q2 (SAS File 03, Line 123 to 174)
df_boh_1Q <- filter(df_boh, fileDate == "2016-03-31")
dateSeq <- seq(as.Date("2016-07-01"),as.Date("2018-07-01"), by="quarter")-1

df_boh_9Q <- NULL
for(ii in 1:9 ){
  df_boh_1Q$fileDate <- dateSeq[ii]
  df_boh_9Q <- rbind(df_boh_9Q, df_boh_1Q)
}

## deleted obs after maturity and create new variables
df_boh_9Q$loan_age_q <- (as.yearqtr(df_boh_9Q$fileDate) - as.yearqtr(df_boh_9Q$origination_date)
                         ) * 4
df_boh_9Q$term_q <- (as.yearqtr(df_boh_9Q$maturity_date) - as.yearqtr(df_boh_9Q$origination_date)
                     ) * 4
df_boh_9Q <- filter(df_boh_9Q, loan_age_q <= term_q)
df_boh_9Q$year <- year(df_boh_9Q$fileDate) 
df_boh_9Q$month <- month(df_boh_9Q$fileDate)
df_boh_9Q$q <- quarter(df_boh_9Q$fileDate)
df_boh_9Q$POB <- 100 * df_boh_9Q$loan_age_q / df_boh_9Q$term_q

## combind data before 2016Q1 and after 2016Q1 (SAS File 03, Line 177 to 179)
df_boh_9Q <- subset(df_boh_9Q, select = -c(term_q))
df_boh_dev <- filter(df_boh, as.Date(fileDate) <= as.Date("2016-03-31"))
df_boh_2018 <- rbind(df_boh_dev, df_boh_9Q)
## !! in the SAS code, Interim.df_boh_merged2 also contains data after 2016Q1
##    I deleted those obs here

## add macroeconomic variables (SAS File 03, Line 184 to 231)
for(scenario in c("base", "adverse", "severe")){
  
  print(paste0("==== ", scenario, " ===="))
  
  macro_var <- fread(paste0("Raw Data/Macrovars/", scenario, ".csv"))
  
  macro_var <- subset(macro_var, select = c(year, quarter,rgdp_qg_lag_2, CAUR_yd, CAUR_yd_lag_1, CAUR_yd_lag_2,
                                            CAUR_yd_lag_3, CAUR_yd_lag_4, CAHPI_ag, CAHPI_ag_lag_1, CAHPI_ag_lag_2,
                                            CAHPI_ag_lag_3, CAHPI_ag_lag_4))
  
  names(macro_var)[names(macro_var)=="quarter"] <- "q"
  
  df_boh_macro_var <- merge(x = df_boh_2018, y = macro_var, by = c("year","q"), all.x = TRUE)
  
  assign(paste0("df_boh_",scenario), df_boh_macro_var)
}


## Save data 
save(df_boh_base, file = "df_boh_base.RData")
save(df_boh_adverse, file = "df_boh_adverse.RData")
save(df_boh_severe, file = "df_boh_severe.RData")
write.csv(df_boh_base, file = "df_boh_base.csv", row.names = FALSE)
write.csv(df_boh_adverse, file = "df_boh_adverse.csv", row.names = FALSE)
write.csv(df_boh_severe, file = "df_boh_severe.csv", row.names = FALSE)

