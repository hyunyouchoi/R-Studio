
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
source(paste(pth_lib,"/colors.R", sep=""))

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
library("nortest")
library("stats")
library("orcutt")
library("quantreg")

### Import Data ################################################################
boh = readRDS(concat(pth_out, "/data-boh.RDS"))
boh_train = function(seg) {
  train_ind = concat("is_train_", seg)
  boh_cp = copy(boh)
  setnames(boh_cp, train_ind, "is_train")
  boh_cp[is_train == TRUE,]
}
boh_train_ce = boh_train("ip")
################################################################################

### Any statistical outliers ? #################################################

boh_train_ce =  boh_train_ce[, exclude_for_ChowTest := ifelse(is_outlier_ia(ldiff_mf) == 1, TRUE, FALSE)]
boh_train_ce[exclude_for_ChowTest == TRUE, c("qtr_dt", "ldiff_mf")]

ChowTest_data = boh_train_ce[exclude_for_ChowTest == FALSE,]

### Chow Test ##################################################################
library("gap")

ols_ip_model = c("crei_eg_lag4","dow_ya")

X = as.matrix(ChowTest_data[, ols_ip_model, with=FALSE])
y_mf = as.matrix(ChowTest_data[["ldiff_mf"]])
y_no = as.matrix(ChowTest_data[["ldiff_no"]])
chow.test(y_no, X, y_mf, X)


### chow test assumptions
mf_fit = lm(ldiff_mf~., data=ChowTest_data[, c("ldiff_mf", ols_ip_model), with=FALSE])
length(mf_fit$residuals) # removed three observations that would prevent normal residuals
no_fit = lm(ldiff_no~., data=ChowTest_data[, c("ldiff_no", ols_ip_model), with=FALSE])

ip_model_obj_list = list(mf=mf_fit, no=no_fit)

### Export coefficient data ####################################################

step = 1
for (seg in c("mf", "no")) {

  coef_names = names(ip_model_obj_list[[seg]]$coefficients)
  coef_data = data.table(summary(ip_model_obj_list[[seg]])$coefficients)
  oc_coef_data = data.table(summary(cochrane.orcutt(ip_model_obj_list[[seg]]))$coefficients)

  names(coef_data) = c("ols_est", "se", "t_value", "p_value")
  names(oc_coef_data) = c("oc_est", "oc_se", "oc_t_value", "oc_p_value")
  coef_data[["parameter"]] = coef_names
  coef_data[["segment"]] = seg


  if (step == 1) {
    combined_parameter_data = coef_data
  } else {
    combined_parameter_data = rbind(combined_parameter_data, coef_data)
  }
  step = step + 1
}

write.csv(combined_parameter_data, file=concat(pth_out, "/table-SegmentTesting-boh-paramater_data.csv"))

### Chow Test Assumptions ######################################################

# MF Normality
shapiro.test(mf_fit$residuals)
ad.test(mf_fit$residuals)
# Homoskedasticity
ncvTest(mf_fit)

# NO Normality
shapiro.test(no_fit$residuals)
ad.test(no_fit$residuals)
# Homoskedasticity
ncvTest(no_fit)

# Kruskal Wallace (common dist)

mf_resid = data.table(resid=mf_fit$residuals, seg_id=1, segment="Multifamily")
no_resid = data.table(resid=no_fit$residuals, seg_id=2, segment="Non-Owner Occupied")
ip_resid = rbind(mf_resid, no_resid)
kruskal.test(resid ~ seg_id, data=ip_resid)
