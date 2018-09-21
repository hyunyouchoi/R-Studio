
################################################################################
# Bank of Hope
# Commercial Real Estate Ending Balances
# Program: <>.R
# Author(s): KPMG, LLP
# Purpose:
# Data Dependences:
#
#
# R-version: R version 3.3.1 (2016-06-21)
# -- "Bug in Your Hair" Copyright (C) 2016 The R Foundation
# for Statistical Computing Platform: x86_64-apple-darwin13.4.0 (64-bit)
################################################################################

### Environment Settings #######################################################
pth_inputs = "C:/Users/ic07949/Desktop/KPMG/Model Development/development code and data/ending-balance_combined/combined/read-only-inputs"
pth_lib = "C:/Users/ic07949/Desktop/KPMG/Model Development/development code and data/ending-balance_combined/combined/library"
pth_out = "C:/Users/ic07949/Desktop/KPMG/Model Development/development code and data/ending-balance_combined/combined"
### No need to make changes below after this line ##############################

### Dependencies
source(paste(pth_lib,"/dev-support.R", sep=""))
source(paste(pth_lib,"/dfast-support.R", sep=""))
# source has the following functions:
#  - stack()
#  - get_bal_forecast()
#  - concat()
#  - bin_interval_variable()
#  - calc_rsq(), calc_mape(), calc_mad(), calc_rmset()
#  - cv_step(), cv_select()

library("openxlsx")
library("data.table")
library("lubridate")
library("ggplot2")
library("scales")
library("tseries")
library("quantreg")
library("car")
library("urca")
library("lmtest")
library("stats")

################################################################################
### Combine CRE and C-I
cre = readRDS(concat(pth_out, "/data-cre.RDS"))
c_i = readRDS(concat(pth_out, "/data-c_i.RDS"))
boh = cre[c_i, on="qtr_dt"]

### Transform Balance Variables

boh[, boh := cre_boh + ci]

segments = c("ip", "oo", "ci", "no", "mf", "cre_boh", "boh")

# log-diff
# log-diff-lag1
# pct

### Patch oo and no 2016Q4 with values based on mix of 2007.
boh[["is_patched_data"]] = FALSE
boh_2007 = boh[qtr_dt >= as.Date("2007-03-31") & qtr_dt <= as.Date("2007-12-31"), c("boh", "ci", "mf", "no", "oo")]

boh_2007Q1_bal = boh_2007[["boh"]][1]
boh_2007_growth_factor = exp(mean(log_diff(boh_2007[["boh"]]), na.rm=TRUE))
boh[qtr_dt == as.Date("2006-12-31"), "boh"] = boh_2007Q1_bal/boh_2007_growth_factor
# bal_t = bal_t-1 x exp(log_diff)
# bat_t-1 = bal_t/exp(log_diff)

mix_2007Q1 = (boh_2007[1, c("no", "oo")]/boh_2007[["boh"]][1])

boh[qtr_dt == as.Date("2006-12-31"),"is_patched_data"] = TRUE
boh[qtr_dt == as.Date("2006-12-31"),c("no", "oo")] = mix_2007Q1 * boh[qtr_dt == as.Date("2006-12-31"),"boh"][[1]]


for (seg in segments) {

  tf_name = concat("ldiff_", seg)
  Tf_name = concat("Ldiff_", seg)
  pct_name = concat("pct_", seg)
  tf_name_lag = concat(tf_name, "_lag1")
  boh[[tf_name]] = log_diff(boh[[seg]])
  boh[[Tf_name]] = delta(boh[[tf_name]])
  boh[[tf_name_lag]] = shift(boh[[tf_name]], n=1)
  boh[[pct_name]] = boh[[seg]]/boh[["boh"]]


}

### Data flags:
FST_TRAIN_DT = "2003-03-31"
CRE_FST_TRAIN_DT = "2007-06-30"


NTH_TRAIN_DT = "2016-12-31"
RECESSION_START = "2007-12-31"
RECESSION_END = "2009-06-30"


boh[["is_oot"]] = ifelse(boh[["qtr_dt"]] >= as.Date(NTH_TRAIN_DT), TRUE, FALSE)
boh[["is_train_ci"]] = ifelse(boh[["qtr_dt"]] >= as.Date(FST_TRAIN_DT) & boh[["qtr_dt"]] <= as.Date(NTH_TRAIN_DT), TRUE, FALSE)
boh[["is_train_mf"]] = ifelse(boh[["qtr_dt"]] >= as.Date(FST_TRAIN_DT) & boh[["qtr_dt"]] <= as.Date(NTH_TRAIN_DT), TRUE, FALSE)
boh[["is_train_no"]] = ifelse(boh[["qtr_dt"]] >= as.Date(CRE_FST_TRAIN_DT) & boh[["qtr_dt"]] <= as.Date(NTH_TRAIN_DT), TRUE, FALSE)
boh[["is_train_oo"]] = ifelse(boh[["qtr_dt"]] >= as.Date(CRE_FST_TRAIN_DT) & boh[["qtr_dt"]] <= as.Date(NTH_TRAIN_DT), TRUE, FALSE)
boh[["is_train_ip"]] = ifelse(boh[["qtr_dt"]] >= as.Date(CRE_FST_TRAIN_DT) & boh[["qtr_dt"]] <= as.Date(NTH_TRAIN_DT), TRUE, FALSE)
boh[["is_train_boh"]] = ifelse(boh[["qtr_dt"]] >= as.Date(CRE_FST_TRAIN_DT) & boh[["qtr_dt"]] <= as.Date(NTH_TRAIN_DT), TRUE, FALSE)
boh[["is_train_cre_boh"]] = ifelse(boh[["qtr_dt"]] >= as.Date(CRE_FST_TRAIN_DT) & boh[["qtr_dt"]] <= as.Date(NTH_TRAIN_DT), TRUE, FALSE)

boh[["is_recession"]] = ifelse(boh[["qtr_dt"]] >= as.Date(RECESSION_START) & boh[["qtr_dt"]] <= as.Date(RECESSION_END), TRUE, FALSE)
boh[["is_9q_data"]] = FALSE


### Add Econ Data
econ_data = readRDS(concat(pth_out, "/econ-data-baseline.RDS"))

for (seg in c("ci", "ip", "oo")) {

  tf_name = concat("ldiff_", seg)
  outlier_name = concat("is_",seg,"_outlier")
  boh[[outlier_name]] = ifelse(is_outlier_ia(boh[[tf_name]]) == 1, TRUE, FALSE)

}

boh = econ_data[boh, on="qtr_dt"]

saveRDS(boh, concat(pth_out, "/data-boh.RDS"))
################################################################################
