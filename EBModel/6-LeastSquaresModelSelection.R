
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
library("RGraphics")
library("gridExtra")
library("ggplot2")
library("scales")
library("tseries")
library("car")
library("urca")
library("lmtest")
library("stats")
library("orcutt")

################################################################################

### Import Data ################################################################
boh = readRDS(concat(pth_out, "/data-boh.RDS"))
boh_train = function(seg) {
  train_ind = concat("is_train_", seg)
  boh_cp = copy(boh)
  setnames(boh_cp, train_ind, "is_train")
  boh_cp[is_train == TRUE,]
}
boh_train_ce = boh_train("ip")
boh_train_ci = boh_train("ci")
################################################################################
info = readRDS(concat(pth_inputs, "/table-variable_information_rho.RDS"))

################################################################################
################################################################################

#ci_smape = cv_select_bal(
#   boh_train_ci
# , info[c_i_tier != 0,]
# , resp="ldiff_ci"
# , bal="ci"
# , use_cochrane_orcutt=TRUE
# , modl=""
# , iter=5
# , criteria="smape"
# , vif_tol=3
# , sig_tol=0.05
# , from_yr=2003
# , to_yr=2016
#)

ip_smape = cv_select_bal(
   boh_train_ce
 , info[cre_tier != 0,]
 , resp="ldiff_ip"
 , bal="ip"
 , use_cochrane_orcutt=TRUE
 , modl=""
 , iter=5
 , criteria="smape"
 , vif_tol=3
 , sig_tol=0.05
 , from_yr=2007
 , to_yr=2016
)

oo_smape = cv_select_bal(
   boh_train_ce
 , info[cre_tier != 0,]
 , resp="ldiff_oo"
 , bal="oo"
 , use_cochrane_orcutt=TRUE
 , modl=""
 , iter=5
 , criteria="smape"
 , vif_tol=3
 , sig_tol=0.05
 , from_yr=2007
 , to_yr=2016
)

################################################################################
################################################################################
#saveRDS(ci_smape, concat(pth_out, "/robj-LeastSquaresSelectionSmape-ci.RDS"))
saveRDS(ip_smape, concat(pth_out, "/robj-LeastSquaresSelectionSmape-ip.RDS"))
saveRDS(oo_smape, concat(pth_out, "/robj-LeastSquaresSelectionSmape-oo.RDS"))
################################################################################

### Output Iteration Data for Review

#ci_itr_1 = ci_smape[["summary"]][["iteration-1"]]
#ci_itr_2 = ci_smape[["summary"]][["iteration-2"]]
ip_itr_1 = ip_smape[["summary"]][["iteration-1"]]
ip_itr_2 = ip_smape[["summary"]][["iteration-2"]]
oo_itr_1 = oo_smape[["summary"]][["iteration-1"]]
oo_itr_2 = oo_smape[["summary"]][["iteration-2"]]

#ci_itr_1[["iter"]] = 1
#ci_itr_2[["iter"]] = 2
ip_itr_1[["iter"]] = 1
ip_itr_2[["iter"]] = 2
oo_itr_1[["iter"]] = 1
oo_itr_2[["iter"]] = 2

#ci = rbind(ci_itr_1, ci_itr_2)
ip = rbind(ip_itr_1, ip_itr_2)
oo = rbind(oo_itr_1, oo_itr_2)

#setnames(ci, "var", "name")
setnames(ip, "var", "name")
setnames(oo, "var", "name")

#write.csv(ci[info, on="name"], concat(pth_out, "/ci-selections.csv"))
write.csv(ip[info, on="name"], concat(pth_out, "/ip-selections.csv"))
write.csv(oo[info, on="name"], concat(pth_out, "/oo-selections.csv"))
################################################################################
