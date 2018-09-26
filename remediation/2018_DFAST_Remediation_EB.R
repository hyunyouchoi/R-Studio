# 2018 DFAST EB Model remediation - variable selection for CRE 
# Bank of Hope
# Developer: Irene Choi
# Start Date: 09/24/2018
# R version 3.4.3 (2017-11-30)

library(dplyr)
library(lubridate)
library(zoo)
library(data.table)
library(ggplot2)

source("dev-support.R")

setwd("C:/Users/ic07949/Desktop/Model Development Code Package v2/DFAST Production Run 2018 EB/DFAST Production Run 2018")

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

#########################################################################################################################
### Read in relevant datasets

# Read in training data
boh <- readRDS("data-boh.RDS")

# Read in new macroeconomic data
base <- readRDS("econ-data-baseline.RDS")
adverse <- readRDS("econ-data-adverse.RDS")
severe <- readRDS("econ-data-severe.RDS")

#########################################################################################################################
### Train the models

# Define model variables and collapse with '+' for formula
#ciVars <- paste(c("gdp_ag_lag1","ca_rinc_ag_lag3","inc_qg_lag2"),collapse = "+")
ipVars <- paste(c("crei_eg_lag4","dow_ya"),collapse = "+")
ooVars <- "crei_eg_lag2"

# Train models and check coefficient

ipModel_8 <- lm(formula = paste0("ldiff_ip ~ ",ipVars)
              ,data = boh[qtr_dt >= "2007-06-30" & qtr_dt <= "2015-06-30"])
ipModel_8
ipModel_4 <- lm(formula = paste0("ldiff_ip ~ ",ipVars)
                ,data = boh[qtr_dt >= "2007-06-30" & qtr_dt <= "2015-12-31"])
ipModel_4
ipModel_full <- lm(formula = paste0("ldiff_ip ~ ",ipVars)
                ,data = boh[qtr_dt >= "2007-06-30" & qtr_dt <= "2016-12-31"])
ipModel_full


ooModel_8 <- lm(formula = paste0("ldiff_oo ~ ",ooVars)
              ,data = boh[qtr_dt >= "2007-06-30" & qtr_dt <= "2015-06-30"])
ooModel_8
ooModel_4 <- lm(formula = paste0("ldiff_oo ~ ",ooVars)
                ,data = boh[qtr_dt >= "2007-06-30" & qtr_dt <= "2015-12-31"])
ooModel_4
ooModel_full <- lm(formula = paste0("ldiff_oo ~ ",ooVars)
                ,data = boh[qtr_dt >= "2007-06-30" & qtr_dt <= "2016-12-3"])
ooModel_full

#########################################################################################################################
 
boh_oos_8 <- boh[qtr_dt >= "2015-09-30"]

ip_oos_8 <- predict(ipModel_8, boh_oos_8)
oo_oos_8 <- predict(ooModel_8, boh_oos_8)

boh_oos_4 <- boh[qtr_dt >= "2016-03-31"]

ip_oos_4 <- predict(ipModel_4, boh_oos_4)
oo_oos_4 <- predict(ooModel_4, boh_oos_4)

boh_oos_full <- boh[qtr_dt >= "2007-06-30"]

ip_oos_full <- predict(ipModel_full, boh_oos_full)
oo_oos_full <- predict(ooModel_full, boh_oos_full)

result <- boh[,c("qtr_dt", "ldiff_ip", "ldiff_oo")]

result[,"ip_oos_8"] <- NA
result[,"oo_oos_8"] <- NA
result[,"ip_oos_4"] <- NA
result[,"oo_oos_4"] <- NA
result[,"ip_oos_full"] <- NA
result[,"oo_oos_full"] <- NA

result$ip_oos_8[59:66] <- ip_oos_8
result$oo_oos_8[59:66] <- oo_oos_8

result$ip_oos_4[63:66] <- ip_oos_4
result$oo_oos_4[63:66] <- oo_oos_4

result$ip_oos_full[27:66] <- ip_oos_full
result$oo_oos_full[27:66] <- oo_oos_full

final_result <- filter(result, qtr_dt >= "2007-06-30")

write.csv(final_result,"remedication_plan_result.csv",row.names = F)
































