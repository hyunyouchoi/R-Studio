
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
library("stats")

################################################################################
GOLDEN_RATIO = 1.61803398875
STACK_RATIO = 1.20


### Import Data ################################################################
baseline = readRDS(concat(pth_out, "/econ-data-baseline.RDS"))
adverse = readRDS(concat(pth_out, "/econ-data-adverse.RDS"))
severe = readRDS(concat(pth_out, "/econ-data-severe.RDS"))

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

################################################################################
### Selected Models ############################################################
### OLS

ols_ip_model = c("crei_eg_lag4","dow_ya")
ols_oo_model = c("crei_eg_lag2")


ols_ip_desc = c("CRE Index 8Q Growth", "Dow 1Q Growth MA(4)")
ols_oo_desc = c("CRE Index 8Q Growth")


### Linear Testing Model Fit ###################################################

ols_ip = lm(ldiff_ip~., data=boh_train_ce[, c(ols_ip_model, "ldiff_ip"), with=FALSE])
ols_oo = lm(ldiff_oo~., data=boh_train_ce[, c(ols_oo_model, "ldiff_oo"), with=FALSE])
### combine model info into lists for later usage.
model_obj_list = list(ip=ols_ip, oo=ols_oo)
model_var_list = list(ip=ols_ip_model, oo=ols_oo_model)
model_desc_list = list(ip=ols_ip_desc, oo=ols_oo_desc)

### Forecasts ##################################################################

boh_2007q2_to_2016q4 = boh[
  qtr_dt >= as.Date("2007-06-30", "%Y-%m-%d")
  & qtr_dt <= as.Date("2016-12-31", "%Y-%m-%d")
,]


ma_n=1
j = 1
for (seg in c("ip", "oo")) {
  fit = model_obj_list[[seg]]
  var_names = model_var_list[[seg]]



  fcst_data = get_forecasts(
      boh_2007q2_to_2016q4
    , baseline
    , adverse
    , severe
    , bal_var=seg
    , dt_var="qtr_dt"
    , model=var_names
    , model_obj=fit
    , resp=concat("ldiff_", seg)
    , type="ols"
    , ar_term=""
    , ma_n=ma_n
  )

  a = fcst_data[["balance"]][, c("dt", "bal", "baseline", "adverse", "severe")]
  setnames(a, "bal", "resp")
  a[["grp"]] = "Balance (billions)"

  b = fcst_data[["ldiff"]][, c("dt", "resp", "baseline", "adverse", "severe")]


  b[["grp"]] = "Balance Log-difference"

  b[!is.na(resp), `:=`(baseline = resp, adverse = resp, severe = resp)]


  c = rbind(a,b)

  f_c = (
      ggplot(data=c)
    + facet_wrap(~grp, ncol=2, scales="free_y")
    + theme_minimal()
    + theme(
        legend.title=element_blank()
      , legend.position="bottom"
      , panel.grid.minor=element_blank()
      , strip.text = element_text(size = 14)
    )
    + geom_line(aes(x=dt, y=baseline, color="Baseline"))
    + geom_line(aes(x=dt, y=adverse, color="Adverse"))
    + geom_line(aes(x=dt, y=severe, color="Severe"))
    + geom_line(aes(x=dt, y=resp, color="Actual"))
    + labs(x=NULL, y=NULL)
    + scale_y_continuous(label=comma_format())
    + theme(legend.title=element_blank(), legend.position="bottom")
    + guides(col=guide_legend(nrow=1))
    + scale_colour_manual(values=c(hxDRed, hxDGreen, hxDAqua, hxGray, hxDPurple))
  )
  ggsave(concat(pth_out, "/image-Forecasts-", seg,".png"), plot=f_c, height=10, width=12*GOLDEN_RATIO, unit="cm")



  

  j = j + 1
  write.csv(fcst_data[["balance"]], file=concat(pth_out, "/table-Forecasts-", seg,".csv"))
}



