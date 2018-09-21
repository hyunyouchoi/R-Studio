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

################################################################################
GOLDEN_RATIO = 1.61803398875
STACK_RATIO = 1.20

### Import Econ Data ###########################################################
baseline = readRDS(concat(pth_out, "/econ-data-baseline.RDS"))
adverse = readRDS(concat(pth_out, "/econ-data-adverse.RDS"))
severe = readRDS(concat(pth_out, "/econ-data-severe.RDS"))
boh = readRDS(concat(pth_out, "/data-boh.RDS"))

### Import Balance Data ########################################################
boh = readRDS(concat(pth_out, "/data-boh.RDS"))
boh_train = function(seg) {
  train_ind = concat("is_train_", seg)
  boh_cp = copy(boh)
  setnames(boh_cp, train_ind, "is_train")
  boh_cp[is_train == TRUE,]
}
boh_train_ce = boh_train("ip")

################################################################################


### Selected Models ############################################################
### OLS

ols_ip_model = c("crei_eg_lag4","dow_ya")
ols_oo_model = c("crei_eg_lag2")


ols_ip_desc = c("CRE Index 8Q Growth", "Dow 1Q Growth MA(4)")
ols_oo_desc = c("CRE Index 8Q Growth")

### Quantile Regression

qrg_ip_model = ols_ip_model
qrg_oo_model = ols_oo_model


### Linear Testing Model Fit ###################################################

ols_ip = lm(ldiff_ip~., data=boh_train_ce[, c(ols_ip_model, "ldiff_ip"), with=FALSE])
ols_oo = lm(ldiff_oo~., data=boh_train_ce[, c(ols_oo_model, "ldiff_oo"), with=FALSE])


qrg_ip = lm(ldiff_ip~., data=boh_train_ce[, c(qrg_ip_model, "ldiff_ip"), with=FALSE])
qrg_oo = lm(ldiff_oo~., data=boh_train_ce[, c(qrg_oo_model, "ldiff_oo"), with=FALSE])


### combine model info into lists for later usage.
model_obj_list = list(ip=ols_ip, oo=ols_oo)
model_var_list = list(ip=ols_ip_model, oo=ols_oo_model)
model_desc_list = list(ip=ols_ip_desc, oo=ols_oo_desc)

### Find best tau for quantile regressions #####################################
# Find Tau for quantile regression
tau_list = seq(0.01,0.99,0.01)
seg_list = c("ip", "oo")
n_tau = length(tau_list)
qrg_tau_summary = data.table(tau=tau_list,ip=0, oo=0)

i = 1
for (tau in tau_list) {
  for (seg in seg_list) {
    resp = concat("ldiff_", seg)
    train = boh_train_ce
    frm = 2007
    

    qrg_tau_summary[i, seg] = cv_step_qr_bal(
          train
        , tau=tau
        , bal=seg
        , resp=resp
        , model=model_var_list[[seg]]
        , from_yr=frm
        , to_yr=2016
      )[["bmape"]]
   }
   i = i + 1
}

tau_selections = list()
for (seg in seg_list) {
  tau_selections[[seg]] = tau_list[which.min(qrg_tau_summary[[seg]])]
}


qrg_ip = rq(ldiff_ip~., tau=tau_selections[["ip"]], data=boh_train_ce[, c(qrg_ip_model, "ldiff_ip"), with=FALSE])
qrg_oo = rq(ldiff_oo~., tau=tau_selections[["oo"]], data=boh_train_ce[, c(qrg_oo_model, "ldiff_oo"), with=FALSE])

qrg_model_obj_list = list(ip=qrg_ip, oo=qrg_oo)

### Export coefficient data ####################################################

step = 1
for (seg in c("ip", "oo")) {

  coef_names = names(model_obj_list[[seg]]$coefficients)
  coef_data = data.table(summary(model_obj_list[[seg]])$coefficients)
  oc_coef_data = data.table(summary(cochrane.orcutt(model_obj_list[[seg]]))$coefficients)

  names(coef_data) = c("ols_est", "se", "t_value", "p_value")
  names(oc_coef_data) = c("oc_est", "oc_se", "oc_t_value", "oc_p_value")
  coef_data[["parameter"]] = coef_names
  coef_data[["segment"]] = seg

  oc_coef_data[["parameter"]] = coef_names
  oc_coef_data[["segment"]] = seg

  qrg_coef_names = names(qrg_model_obj_list[[seg]]$coefficients)
  qrg_coef_data = data.table(summary(qrg_model_obj_list[[seg]])$coefficients)[,1]
  setnames(qrg_coef_data, "coefficients", "qrg_est")
  qrg_coef_data[["parameter"]] = qrg_coef_names
  qrg_coef_data[["tau"]] = tau_selections[[seg]]

  parameter_data = qrg_coef_data[coef_data[oc_coef_data, on="parameter"], on="parameter"][, c("segment","parameter", "ols_est", "oc_est", "qrg_est", "se", "oc_se", "t_value", "oc_t_value", "p_value", "oc_p_value", "tau")]

  if (step == 1) {
    combined_parameter_data = parameter_data
  } else {
    combined_parameter_data = rbind(combined_parameter_data, parameter_data)
  }
  step = step + 1
}

write.csv(combined_parameter_data, file=concat(pth_out, "/table-BackTesting-boh-paramater_data.csv"))

### Linearity Testing ##########################################################

for (seg in c("ip", "oo")) {

  k = length(model_var_list[[seg]])
  resp = concat("ldiff_", seg)
  train = boh_train_ce
  
  for (i in 1:k) {

    var = model_var_list[[seg]][i]
    dsc = model_desc_list[[seg]][i]

    plot_data = train[, c(resp, var), with=FALSE]
    setnames(plot_data, c(var, resp), c("xvar", "resp"))
    plot_data[["grp"]] = dsc

    if (i == 1) {
      line_plot_data = plot_data
    } else {
      line_plot_data = rbind(line_plot_data, plot_data)
    }

  }

  linearity_plot = (
      ggplot(data=line_plot_data, aes(x=xvar, y=resp))
    + facet_wrap(~grp, ncol=2, scales="free_x")
    + theme_minimal()
    + theme(
          panel.grid.minor=element_blank()
        , strip.text = element_text(size = 14)
      )
    + geom_point()
    + geom_smooth(method="lm", se=FALSE, linetype=2, size=0.50, color=hxBlue)
    + labs(x=NULL, y=NULL)
    + scale_y_continuous(label=comma_format())
  )


  if (k > 1) {
    ggsave(concat(pth_out, "/image-BackTesting-", seg, "-linearity.png"), plot=linearity_plot, height=10, width=12*GOLDEN_RATIO, unit="cm")
  } else {
    ggsave(concat(pth_out, "/image-BackTesting-", seg, "-linearity.png"), plot=linearity_plot, height=10, width=8*GOLDEN_RATIO, unit="cm")
  }

}


### VIF Testing ################################################################

write.csv(vif(ols_ip), file=concat(pth_out, "/table-BackTesting-ip-vifs.csv"))
### Stationarity Testing #######################################################

### Saved output to table-BackTesting-ci_model_variable_stationarity.txt

for (var in ols_ip_model) {
  print(concat("###::ip::",var))
  print(summary(ur.df(boh_train_ce[[var]], type='drift', lags=6, selectlags = 'BIC')))
}
### Saved output to table-BackTesting-ip_model_variable_stationarity.txt

for (var in ols_oo_model) {
  print(concat("###::oo::",var))
  print(summary(ur.df(boh_train_ce[[var]], type='drift', lags=6, selectlags = 'BIC')))
}
### Saved output to table-BackTesting-oo_model_variable_stationarity.txt


### Plot Model Drivers #########################################################

for (seg in c("ip", "oo")) {
  k = length(model_var_list[[seg]])
  train = boh_train_ce
  

  for (i in 1:k) {

    var = model_var_list[[seg]][i]
    dsc = model_desc_list[[seg]][i]

    act = train[, c("qtr_dt", var), with=FALSE]
    bsl = baseline[, c("qtr_dt", var), with=FALSE]
    adv = adverse[, c("qtr_dt", var), with=FALSE]
    sev = severe[, c("qtr_dt", var), with=FALSE]
    setnames(act, var, "Actual")
    setnames(bsl, var, "Baseline")
    setnames(adv, var, "Adverse")
    setnames(sev, var, "Severe")

    plot_data = act[bsl[adv[sev, on="qtr_dt"], on="qtr_dt"], on="qtr_dt"][qtr_dt >= as.Date("2007-09-30"), ]
    plot_data[["grp"]] = dsc

    if (i == 1) {
      driver_plot_data = plot_data
    } else {
      driver_plot_data = rbind(driver_plot_data, plot_data)
    }

  }

  driver_plot = (
      ggplot(data=driver_plot_data)
    + facet_wrap(~grp, ncol=2, scales="free_y")
    + theme_minimal()
    + theme(
        legend.title=element_blank()
      , legend.position="bottom"
      , panel.grid.minor=element_blank()
      , strip.text = element_text(size = 14)
    )
    + geom_line(aes(x=qtr_dt, y=Baseline, color="Baseline"))
    + geom_line(aes(x=qtr_dt, y=Adverse, color="Adverse"))
    + geom_line(aes(x=qtr_dt, y=Severe, color="Severe"))
    + geom_line(aes(x=qtr_dt, y=Actual, color="Actual"))
    + labs(x=NULL, y=NULL)
    + scale_y_continuous(label=comma_format())
    + theme(legend.title=element_blank(), legend.position="bottom")
    + guides(col=guide_legend(nrow=1))
  )

  if (k > 1) {
    ggsave(concat(pth_out, "/image-BackTesting-", seg, "-drivers.png"), plot=driver_plot, height=10, width=12*GOLDEN_RATIO, unit="cm")
  } else {
    ggsave(concat(pth_out, "/image-BackTesting-", seg, "-drivers.png"), plot=driver_plot, height=10, width=8*GOLDEN_RATIO, unit="cm")
  }
}


### Scenario talking points ####################################################
# recession_data = baseline[is_recession == TRUE, c("qtr_dt",unique(c(ols_ci_model, ols_ip_model, ols_oo_model))), with=FALSE]
# baseline_data = baseline[is_9q_data == TRUE, c("qtr_dt",unique(c(ols_ci_model, ols_ip_model, ols_oo_model))), with=FALSE]
# adverse_data = adverse[is_9q_data == TRUE, c("qtr_dt",unique(c(ols_ci_model, ols_ip_model, ols_oo_model))), with=FALSE]
# severe_data = severe[is_9q_data == TRUE, c("qtr_dt",unique(c(ols_ci_model, ols_ip_model, ols_oo_model))), with=FALSE]
# apply(recession_data,2,min)[ols_ci_model]
# adverse_data[, c("qtr_dt", ols_ci_model), with=FALSE]

### Residuals Testing ##########################################################
### Normality
# Restore output to console

################################################################################



### Income Producing Residual Tests ############################################
# Normailty
shapiro.test(ols_ip$residuals)
ad.test(ols_ip$residuals)

# Homoskedasticity
ncvTest(ols_ip)

# Autocorrelation
Box.test(ols_ip$residuals, type="Ljung-Box", lag=1)
Box.test(ols_ip$residuals, type="Ljung-Box", lag=2)
Box.test(ols_ip$residuals, type="Ljung-Box", lag=3)

### Owner Occupied Residual Tests ##############################################
# Normailty
shapiro.test(ols_oo$residuals)
ad.test(ols_oo$residuals)

# Homoskedasticity
ncvTest(ols_oo)

# Autocorrelation
Box.test(ols_oo$residuals, type="Ljung-Box", lag=1)
Box.test(ols_oo$residuals, type="Ljung-Box", lag=2)
Box.test(ols_oo$residuals, type="Ljung-Box", lag=3)



# Adjusted for outliers
ols_oo_no_outlier = lm(ldiff_oo~., data=boh_train_ce[is_oo_outlier == FALSE, c(ols_oo_model, "ldiff_oo"), with=FALSE])
# Normailty
shapiro.test(ols_oo_no_outlier$residuals)
ad.test(ols_oo_no_outlier$residuals)

# Homoskedasticity
ncvTest(ols_oo_no_outlier)




################################################################################
### saved output to table-BackTesting-<seg>_residual_tests.txt

#### Residuals ACF, Scatter, Histograms, and QQPlots ############################


### Income Producting

acf_obj = acf(ols_ip$residuals, plot=FALSE)
acf_df = with(acf_obj, data.frame(lag, acf))
acf_hi = 1.96/sqrt(length(ols_ip$residuals))
acf_lo = -acf_hi

r_acf = (
      ggplot(data = acf_df, mapping = aes(x = lag, y = acf))
    + geom_hline(aes(yintercept = 0))
    + geom_hline(aes(yintercept = acf_hi), linetype=2, color=hxBlue)
    + geom_hline(aes(yintercept = acf_lo), linetype=2, color=hxBlue)
    + geom_segment(mapping = aes(xend = lag, yend = 0))
    + theme_minimal()
    + theme(panel.grid.minor=element_blank(), plot.title = element_text(hjust = 0.50))
    + labs(x="Lag", y="ACF",title="Residual ACF")
)

res_data = data.table(r=ols_ip$residuals, fitted=ols_ip$fitted.values)
r = res_data[["r"]]
p_rng = c(0.25, 0.75)
r_points = quantile(r, p_rng)
n_points = qnorm(p_rng)

slope = diff(r_points)/diff(n_points)
int = r_points[2] - slope * n_points[2]


r_qq = (
      ggplot(data=res_data, aes(sample=r))
    + stat_qq()
    + geom_abline(intercept = int, slope=slope, linetype=2, color=hxBlue)
    + labs(x="Theoretical Quantiles", y="Residuals", title="Residual QQ Plot")
    + theme_minimal()
    + theme(panel.grid.minor=element_blank(), plot.title = element_text(hjust = 0.50))
)

r_hist = (
    ggplot(data=res_data, aes(r))
  + theme_minimal()
  + theme(panel.grid.minor=element_blank(), plot.title = element_text(hjust = 0.50))
  + geom_histogram(color="white", bins=10, fill=hxBlue)
  + labs(x="Residuals", y=NULL,title="Residual Distribution")
  + scale_y_continuous(label=comma_format())
  + scale_x_continuous(label=comma_format())
)

r_scat = (
    ggplot(data=res_data)
  + geom_point(aes(x=fitted, y=r))
  + geom_hline(yintercept=0, linetype=2, color=hxBlue)
  + theme_minimal()
  + theme(panel.grid.minor=element_blank(),plot.title = element_text(hjust = 0.50))
  + labs(x="Fitted Values", y="Residuals", title="Residual Scatter Plot")
  + scale_y_continuous(label=comma_format())
  + scale_x_continuous(label=comma_format())
)

r_top = grid.arrange(r_scat, r_acf,ncol=2)
r_bot = grid.arrange(r_qq, r_hist,ncol=2)
residual_plot = grid.arrange(r_top, r_bot, nrow=2)
ggsave(concat(pth_out, "/image-BackTesting-ip-residuals.png"), plot=residual_plot, height=12*GOLDEN_RATIO, width=12*GOLDEN_RATIO, unit="cm")


### Owner Occupied
acf_obj = acf(ols_oo$residuals, plot=FALSE)
acf_df = with(acf_obj, data.frame(lag, acf))
acf_hi = 1.96/sqrt(length(ols_oo$residuals))
acf_lo = -acf_hi

r_acf = (
      ggplot(data = acf_df, mapping = aes(x = lag, y = acf))
    + geom_hline(aes(yintercept = 0))
    + geom_hline(aes(yintercept = acf_hi), linetype=2, color=hxBlue)
    + geom_hline(aes(yintercept = acf_lo), linetype=2, color=hxBlue)
    + geom_segment(mapping = aes(xend = lag, yend = 0))
    + theme_minimal()
    + theme(panel.grid.minor=element_blank(), plot.title = element_text(hjust = 0.50))
    + labs(x="Lag", y="ACF",title="Residual ACF")
)

res_data = data.table(r=ols_oo$residuals, fitted=ols_oo$fitted.values)
r = res_data[["r"]]
p_rng = c(0.25, 0.75)
r_points = quantile(r, p_rng)
n_points = qnorm(p_rng)

slope = diff(r_points)/diff(n_points)
int = r_points[2] - slope * n_points[2]


r_qq = (
      ggplot(data=res_data, aes(sample=r))
    + stat_qq()
    + geom_abline(intercept = int, slope=slope, linetype=2, color=hxBlue)
    + labs(x="Theoretical Quantiles", y="Residuals", title="Residual QQ Plot")
    + theme_minimal()
    + theme(panel.grid.minor=element_blank(), plot.title = element_text(hjust = 0.50))
)

r_hist = (
    ggplot(data=res_data, aes(r))
  + theme_minimal()
  + theme(panel.grid.minor=element_blank(), plot.title = element_text(hjust = 0.50))
  + geom_histogram(color="white", bins=10, fill=hxBlue)
  + labs(x="Residuals", y=NULL,title="Residual Distribution")
  + scale_y_continuous(label=comma_format())
  + scale_x_continuous(label=comma_format())
)

r_scat = (
    ggplot(data=res_data)
  + geom_point(aes(x=fitted, y=r))
  + geom_hline(yintercept=0, linetype=2, color=hxBlue)
  + theme_minimal()
  + theme(panel.grid.minor=element_blank(),plot.title = element_text(hjust = 0.50))
  + labs(x="Fitted Values", y="Residuals", title="Residual Scatter Plot")
  + scale_y_continuous(label=comma_format())
  + scale_x_continuous(label=comma_format())
)

r_top = grid.arrange(r_scat, r_acf,ncol=2)
r_bot = grid.arrange(r_qq, r_hist,ncol=2)
residual_plot = grid.arrange(r_top, r_bot, nrow=2)
ggsave(concat(pth_out, "/image-BackTesting-oo-residuals.png"), plot=residual_plot, height=12*GOLDEN_RATIO, width=12*GOLDEN_RATIO, unit="cm")

################################################################################

### Performance Assesssment ####################################################
### Setup OOT Test Data
boh_2007q2_to_2017q2 = boh[
  qtr_dt >= as.Date("2007-06-30", "%Y-%m-%d")
  & qtr_dt <= as.Date("2017-06-30", "%Y-%m-%d")
,]
oot_testing = baseline[boh_2007q2_to_2017q2, on="qtr_dt"]
### Setup hold-out test data
### give one of 2007s data points to 2006 so we can do holdout for 2007.
boh_2006_adj = boh[qtr_dt != as.Date("2006-12-31"), ]
adj_row = ifelse(boh_2006_adj[["qtr_dt"]] == as.Date("2007-03-31"), TRUE, FALSE)
boh_2006_adj[adj_row,][["qtr_dt"]] = as.Date("2006-12-31")
################################################################################

### To do: In-sample-Performance
### To do: Out-of-time Performance
MAX_TRAIN_DATE = "2015-06-30"

### C-I
ma_n = 1
step = 1
hold_out_est_list = list()
for(seg in c("ip", "oo")) {
  resp = concat("ldiff_", seg)
  model = model_var_list[[seg]]
  ols_model_obj = model_obj_list[[seg]]
  qrg_model_obj = qrg_model_obj_list[[seg]]

  train = boh_train_ce
  cv_data = boh_2006_adj
  

  ### OLS Performance

  ols_perf = get_oot_data(
      train
    , oot_testing
    , bal_var=seg
    , dt_var="qtr_dt"
    , resp=resp
    , model=model
    , type="ols"
    , ar_term=""
    , max_train_dt=MAX_TRAIN_DATE
    , ma_n=ma_n
  )

  ols_perf[["ns_resp_hat"]] = predict(ols_model_obj, ols_perf)
  ols_hold_out = get_hold_out_perf(
      cv_data
    , train
    , bal_var=seg
    , dt_var="qtr_dt"
    , resp=resp
    , model=model
    , type="ols"
    , ar_term=""
  )

  # this will be use in coefficient stability analysis below
  hold_out_est_list[[seg]] = ols_hold_out[["hold_out_coefs"]][var != "(Intercept)",]

  ns = ols_perf[, .(
      Ldiff_Insample_Rsq = calc_rsq(resp, ns_resp_hat)
    , Ldiff_Insample_MAPE = calc_mape(resp, ns_resp_hat)
    , Ldiff_InSample_UR = calc_under_rate(resp, ns_resp_hat)
  )]

  oot = ols_perf[is_oot == 1, .(
      Balance_OOTime_Rsq = calc_rsq(is_oot * bal, is_oot * bal_est)
    , Balance_OOTime_MAPE = calc_mape(bal, bal_est)
    , Balance_OOTime_UR = calc_under_rate(bal, bal_est)
    , Ldiff_OOTime_Rsq = calc_rsq(resp, resp_hat)
    , Ldiff_OOTime_MAPE = calc_mape(resp, resp_hat)
    , Ldiff_OOTime_UR = calc_under_rate(resp, resp_hat)
  )]

  ns_oot = data.table(ns, oot)

  cv = ols_hold_out[["hold_out_data"]][, .(
      Ldiff_CV_Rsq = calc_rsq(resp, resp_hat)
    , Ldiff_CV_MAPE = calc_mape(resp, resp_hat)
    , Ldiff_CV_UR = calc_under_rate(resp, resp_hat)
    , Balance_CV_Rsq = calc_rsq(bal, bal_est)
    , Balance_CV_MAPE = calc_mape(bal, bal_est)
    , Balance_CV_UR = calc_under_rate(bal, bal_est)
  )]

  ols_metrics = data.table(ns_oot, cv)
  ols_metrics[["id"]] = 1
  ols_metrics = melt(ols_metrics, id.vars="id", value.name="ols", variable.name="metric")[, c("metric", "ols")]

  ### Quantile Regression Performance

  qrg_perf = get_oot_data(
      train
    , oot_testing
    , bal_var=seg
    , dt_var="qtr_dt"
    , resp=resp
    , tau=tau_selections[[seg]]
    , model=model
    , type="qrg"
    , ar_term=""
    , max_train_dt=MAX_TRAIN_DATE
    , ma_n=ma_n
  )


  qrg_perf[["ns_resp_hat"]] = predict(qrg_model_obj, qrg_perf)
  qrg_hold_out = get_hold_out_perf(
      cv_data
    , train
    , bal_var=seg
    , dt_var="qtr_dt"
    , resp=resp
    , tau=tau_selections[[seg]]
    , model=model
    , type="qrg"
    , ar_term=""
  )

  ns= qrg_perf[, .(
      Ldiff_Insample_Rsq = calc_rsq(resp, ns_resp_hat)
    , Ldiff_Insample_MAPE = calc_mape(resp, ns_resp_hat)
    , Ldiff_InSample_UR = calc_under_rate(resp, ns_resp_hat)
  )]

  oot = qrg_perf[is_oot == 1, .(
      Balance_OOTime_Rsq = calc_rsq(bal, bal_est)
    , Balance_OOTime_MAPE = calc_mape(bal, bal_est)
    , Balance_OOTime_UR = calc_under_rate(bal, bal_est)
    , Ldiff_OOTime_Rsq = calc_rsq(resp, resp_hat)
    , Ldiff_OOTime_MAPE = calc_mape(resp, resp_hat)
    , Ldiff_OOTime_UR = calc_under_rate(resp, resp_hat)
  )]

  ns_oot = data.table(ns, oot)

  cv = qrg_hold_out[["hold_out_data"]][, .(
      Ldiff_CV_Rsq = calc_rsq(resp, resp_hat)
    , Ldiff_CV_MAPE = calc_mape(resp, resp_hat)
    , Ldiff_CV_UR = calc_under_rate(resp, resp_hat)
    , Balance_CV_Rsq = calc_rsq(bal, bal_est)
    , Balance_CV_MAPE = calc_mape(bal, bal_est)
    , Balance_CV_UR = calc_under_rate(bal, bal_est)
  )]

  qrg_metrics = data.table(ns_oot, cv)
  qrg_metrics[["id"]] = 1
  qrg_metrics = melt(qrg_metrics, id.vars="id", value.name="qrg", variable.name="metric")[, c("metric", "qrg")]

  ### Combine metrics
  metrics = ols_metrics[qrg_metrics, on="metric"]
  metrics[["segment"]] = seg

  ### visualization
  ### plot in-sample up to oot period and show oot for ldiff and balance


  ### Log-difference
  ols_resp = ols_perf[, c("dt", "is_oot", "resp", "ns_resp_hat", "resp_hat")]
  setnames(ols_resp, c("resp_hat", "ns_resp_hat"), c("ols_resp_hat", "ols_ns_resp_hat"))
  qrg_resp = qrg_perf[, c("dt", "ns_resp_hat", "resp_hat")]
  setnames(qrg_resp, c("resp_hat", "ns_resp_hat"), c("qrg_resp_hat", "qrg_ns_resp_hat"))
  resp_data = qrg_resp[ols_resp, on="dt"]

  resp_data[,
  `:=`(
      bal = NA
    , ols_bal_est = NA
    , qrg_bal_est = NA
    , bal_oot = NA
    , bal_ns = NA
  )
  ]

  resp_data[, ols_ns_resp_hat := ifelse(is_oot == 0, ols_ns_resp_hat, NA)]
  resp_data[, qrg_ns_resp_hat := ifelse(is_oot == 0, qrg_ns_resp_hat, NA)]
  resp_data[, ols_resp_hat := ifelse(is_oot == 1 | shift(is_oot, n=1, type="lead") == 1, ols_resp_hat, NA)]
  resp_data[, qrg_resp_hat := ifelse(is_oot == 1 | shift(is_oot, n=1, type="lead") == 1, qrg_resp_hat, NA)]


  resp_data[["grp"]] = "Balance Log-difference"

  ### Balance
  ols_bal = ols_perf[, c("dt", "is_oot", "bal", "bal_est")]
  setnames(ols_bal, c("bal_est"), c("ols_bal_est"))
  qrg_bal = qrg_perf[, c("dt", "bal_est")]
  setnames(qrg_bal, c("bal_est"), c("qrg_bal_est"))
  bal_data = qrg_bal[ols_bal, on="dt"]
  bal_data[,
    `:=`(
        resp = NA
      , ols_resp_hat = NA
      , qrg_resp_hat = NA
      , ols_ns_resp_hat = NA
      , qrg_ns_resp_hat = NA
      , bal_oot = NA
      , bal_ns = NA
    )
  ]

  bal_data[, bal_oot := ifelse(is_oot == 1 | shift(is_oot, n=1, type="lead") == 1, bal, NA)]
  bal_data[, ols_bal_est := ifelse(is_oot == 1 | shift(is_oot, n=1, type="lead") == 1, ols_bal_est, NA)]
  bal_data[, qrg_bal_est := ifelse(is_oot == 1 | shift(is_oot, n=1, type="lead") == 1, qrg_bal_est, NA)]
  bal_data[, bal_ns := ifelse(is_oot == 0, bal, NA)]

  bal_data[["grp"]] = "Balance (billions)"

  perf_series = rbind(resp_data, bal_data)



  perf_plot = (
      ggplot(data=perf_series)
    + facet_wrap(~grp, ncol=2, scales="free_y")
    + theme_minimal()
    + theme(
        legend.title=element_blank()
      , legend.position="bottom"
      , panel.grid.minor=element_blank()
      , strip.text = element_text(size = 14)
    )

    + geom_point(aes(x=dt, y=resp, color="Actual"))
    + geom_line(aes(x=dt, y=ols_ns_resp_hat, color="OLS"))
    + geom_line(aes(x=dt, y=ols_resp_hat, color="OLS"), linetype=2)
    + geom_line(aes(x=dt, y=qrg_ns_resp_hat, color="QR"))
    + geom_line(aes(x=dt, y=qrg_resp_hat, color="QR"), linetype=2)

    + geom_line(aes(x=dt, y=bal_ns, color="Actual"))
    + geom_line(aes(x=dt, y=bal_ns, color="Actual"))

    + geom_line(aes(x=dt, y=ols_bal_est, color="OLS"), linetype=2)
    + geom_line(aes(x=dt, y=qrg_bal_est, color="QR"), linetype=2)
    + geom_point(aes(x=dt, y=bal_oot, color="Actual"))
    + labs(x=NULL, y=NULL)
    + scale_y_continuous(label=comma_format())
    + theme(legend.title=element_blank(), legend.position="bottom")
    + guides(col=guide_legend(nrow=1))
    #+ scale_colour_manual(values=c(hxDRed, hxDGreen, hxDAqua, hxGray, hxDPurple))
  )


  ggsave(concat(pth_out, "/image-BackTesting-", seg, "-performance.png"), plot=perf_plot, height=10, width=12*GOLDEN_RATIO, unit="cm")

  if (step == 1) {
    combined_metrics = metrics
  } else {
    combined_metrics = rbind(combined_metrics, metrics)
  }
  step = step + 1
}

write.csv(combined_metrics, concat(pth_out, "/table-BackTesting-boh-performance_metrics.csv"))


### Stability (use hold_out_est_list from above) ###############################

### To do: More Stability Testing (using yearly hold-out)
### MAPE between final estimates and hold-out estimates.
### Plot Estimates for each hold-out year.
### MAPE from final coefficient


step = 1
for (seg in c("ip", "oo")) {

  hold_out_coefs = hold_out_est_list[[seg]]
  var_names = model_var_list[[seg]]
  var_descs = model_desc_list[[seg]]
  final_coefs = summary(model_obj_list[[seg]])$coefficients
  coef_names = row.names(final_coefs)
  final_coefs = data.table(final_coefs)
  final_coefs[["var"]] = coef_names
  final_coefs = final_coefs[, c("var", "Estimate")]
  setnames(final_coefs, "Estimate", "final_est")

  var_lookup = data.table(var=var_names, name=var_descs)
  hold_out_coefs = var_lookup[hold_out_coefs, on="var"]
  hold_out_coefs = final_coefs[hold_out_coefs, on="var"]
  hold_out_coefs[, ape := abs(est/final_est - 1)]

  coefs_mape = hold_out_coefs[,mean(ape), by="var"]
  coefs_mape[["segment"]] = seg
  setnames(coefs_mape, "V1", "mape")

  if (step == 1) {
    stacked_coefs_mape = coefs_mape
  } else {
    stacked_coefs_mape = rbind(stacked_coefs_mape, coefs_mape)
  }
  step = step + 1



  write.csv(hold_out_coefs, concat(pth_out, "/table-BackTesting-", seg, "-hold_out_coefs.csv"))


  final_coef_row = dcast(final_coefs[var != "(Intercept)", c("var", "final_est")], .~var, value.var="final_est")[, var_names, with=FALSE]
  k = length(var_names)


  x_marks = 2007:2016
  


  if (k == 3) {
    p = (
        ggplot()
      + theme_minimal()
      + theme(panel.grid.minor=element_blank())
      + geom_line(data=hold_out_coefs, aes(x=yr_out, y=est, color=name))
      + labs(x="Hold-out Year",y="Coefficient Estimate")
      + scale_x_discrete(limits=x_marks, labels=x_marks)
      + guides(col=guide_legend(ncol=2))
      + theme(legend.title=element_blank(), legend.position="bottom")
      + geom_hline(yintercept = final_coef_row[[var_names[1]]], linetype=2, color=hxGray)
      + geom_hline(yintercept = final_coef_row[[var_names[2]]], linetype=2, color=hxGray)
      + geom_hline(yintercept = final_coef_row[[var_names[3]]], linetype=2, color=hxGray)

    )
  } else if (k == 2) {
    p = (
        ggplot()
      + theme_minimal()
      + theme(panel.grid.minor=element_blank())
      + geom_line(data=hold_out_coefs, aes(x=yr_out, y=est, color=name))
      + labs(x="Hold-out Year",y="Coefficient Estimate")
      + scale_x_discrete(limits=x_marks, labels=x_marks)
      + guides(col=guide_legend(ncol=2))
      + theme(legend.title=element_blank(), legend.position="bottom")
      + geom_hline(yintercept = final_coef_row[[var_names[1]]], linetype=2, color=hxGray)
      + geom_hline(yintercept = final_coef_row[[var_names[2]]], linetype=2, color=hxGray)
    )
  } else {
    p = (
        ggplot()
      + theme_minimal()
      + theme(panel.grid.minor=element_blank())
      + geom_line(data=hold_out_coefs, aes(x=yr_out, y=est, color=name))
      + labs(x="Hold-out Year",y="Coefficient Estimate")
      + scale_x_discrete(limits=x_marks, labels=x_marks)
      + guides(col=guide_legend(ncol=2))
      + theme(legend.title=element_blank(), legend.position="bottom")
      + geom_hline(yintercept = final_coef_row[[var_names[1]]], linetype=2, color=hxGray)
    )
  }

  ggsave(concat(pth_out, "/image-BackTesting-", seg, "-coefficient_stability.png"), plot=p, height=8, width=10*GOLDEN_RATIO, unit="cm")
}

write.csv(stacked_coefs_mape, concat(pth_out, "/table-BackTesting-boh-hold_out_coefs_mape.csv"))

### To do: Sensitivity #########################################################

for (seg in c("ip", "oo")) {

  # Note: this section employ lists defined way, way up at around model fitting step
  # they are model_var_list(), model_desc_list(), and model_obj_list()

  train = boh_train_ce
  

  var_names = model_var_list[[seg]]
  var_descs = model_desc_list[[seg]]

  coef_names = c("(Intercept)",model_var_list[[seg]])
  fit = model_obj_list[[seg]]

  for (name in coef_names) {
    coef = fit$coefficients[name][[1]]
    if (name == coef_names[1]) {
      lo = coef
      hi = coef
      sens_df = data.frame(parm=name, coef=coef, lo=1, hi=1, mu=1, sd=1, xb_comp_lo=lo, xb_comp_hi=hi)
    } else  {
      lo = min(train[[name]])
      hi = max(train[[name]])
      mu = mean(train[[name]])
      sd = sd(train[[name]])
      xb_lo = min(coef * train[[name]])
      xb_hi = max(coef * train[[name]])
      sens_df = rbind(sens_df, data.frame(parm=name, lo=lo, hi=hi, mu=mu, sd=sd, coef=coef, xb_comp_lo=xb_lo, xb_comp_hi=xb_hi))
    }
  }
  sens_dt = data.table(sens_df)
  sens_dt$xb_range = max(fit$fitted.values) - min(fit$fitted.values)
  sens_dt[, `:=`(
      xb_comp_range = xb_comp_hi - xb_comp_lo
      , xb_comp_total = sum(xb_comp_hi - xb_comp_lo)
  )]
  sens_dt[, importance := xb_comp_range/xb_comp_total]
  write.csv(sens_dt, file=concat(pth_out, "/table-BackTesting-", seg, "_variable_importance.csv"))


  ### stress average case

  case_template = dcast(sens_dt[parm != "(Intercept)", c("parm", "mu")], .~parm, value.var="mu")[, var_names, with=FALSE]
  case_template[["id"]] = 1
  sigmas = data.table(id=1, sigma_factor=seq(-5, 5))
  case_template = sigmas[case_template, on="id", allow.cartesian=TRUE]


  sens_list = list()
  for (name in var_names) {
    cases = case_template
    sd = sens_dt[parm == name, sd]
    cases[[name]] = cases[[name]] + cases[["sigma_factor"]] * sd
    cases[["growth_rate"]] = exp(predict(fit, cases)) - 1
    sens_list[[name]] = cases
  }

  sens_list = list()
  for (name in var_names) {
    cases = case_template
    sd = sens_dt[parm == name, sd]
    cases[[name]] = cases[[name]] + cases[["sigma_factor"]] * sd
    cases[["growth_rate"]] = exp(predict(fit, cases)) - 1
    sens_list[[name]] = cases
  }

  avg_growth = exp(mean(train[[concat("ldiff_", seg)]])) - 1

  k = length(var_names)
  ### Sensitivity Plots (shocks)
  if (k == 3) {
    p_shock = (
      ggplot()
      + geom_line(data=sens_list[[var_names[1]]], aes(x=sigma_factor, y=growth_rate, color=var_descs[1]))
      + geom_line(data=sens_list[[var_names[2]]], aes(x=sigma_factor, y=growth_rate, color=var_descs[2]))
      + geom_line(data=sens_list[[var_names[3]]], aes(x=sigma_factor, y=growth_rate, color=var_descs[3]))
      + theme_minimal()
      + theme(panel.grid.minor=element_blank())
      + labs(x="Standard Deviations", y="Growth Rate")
      + geom_hline(yintercept = avg_growth, linetype=2, color=hxGray)
      + scale_y_continuous(label=percent_format())
      + guides(col=guide_legend(ncol=2))
      + theme(legend.title=element_blank(), legend.position="bottom")
      + scale_x_discrete(limits = seq(-5,5), labels=
          c(
              expression(paste("-5",sigma, sep=""))
            , expression(paste("-4",sigma, sep=""))
            , expression(paste("-3",sigma, sep=""))
            , expression(paste("-2",sigma, sep=""))
            , expression(paste("-1",sigma, sep=""))
            , expression(mu)
            , expression(paste("+1",sigma, sep=""))
            , expression(paste("+2",sigma, sep=""))
            , expression(paste("+3",sigma, sep=""))
            , expression(paste("+4",sigma, sep=""))
            , expression(paste("+5",sigma, sep=""))
          )
        )
    )
  } else if (k == 2) {
    p_shock = (
      ggplot()
      + geom_line(data=sens_list[[var_names[1]]], aes(x=sigma_factor, y=growth_rate, color=var_descs[1]))
      + geom_line(data=sens_list[[var_names[2]]], aes(x=sigma_factor, y=growth_rate, color=var_descs[2]))
      + theme_minimal()
      + theme(panel.grid.minor=element_blank())
      + labs(x="Standard Deviations", y="Growth Rate")
      + geom_hline(yintercept = avg_growth, linetype=2, color=hxGray)
      + scale_y_continuous(label=percent_format())
      + guides(col=guide_legend(ncol=2))
      + theme(legend.title=element_blank(), legend.position="bottom")
      + scale_x_discrete(limits = seq(-5,5), labels=
          c(
              expression(paste("-5",sigma, sep=""))
            , expression(paste("-4",sigma, sep=""))
            , expression(paste("-3",sigma, sep=""))
            , expression(paste("-2",sigma, sep=""))
            , expression(paste("-1",sigma, sep=""))
            , expression(mu)
            , expression(paste("+1",sigma, sep=""))
            , expression(paste("+2",sigma, sep=""))
            , expression(paste("+3",sigma, sep=""))
            , expression(paste("+4",sigma, sep=""))
            , expression(paste("+5",sigma, sep=""))
          )
        )
    )
  } else {
    p_shock = (
      ggplot()
      + geom_line(data=sens_list[[var_names[1]]], aes(x=sigma_factor, y=growth_rate, color=var_descs[1]))
      + theme_minimal()
      + theme(panel.grid.minor=element_blank())
      + labs(x="Standard Deviations", y="Growth Rate")
      + geom_hline(yintercept = avg_growth, linetype=2, color=hxGray)
      + scale_y_continuous(label=percent_format())
      + guides(col=guide_legend(ncol=2))
      + theme(legend.title=element_blank(), legend.position="bottom")
      + scale_x_discrete(limits = seq(-5,5), labels=
          c(
              expression(paste("-5",sigma, sep=""))
            , expression(paste("-4",sigma, sep=""))
            , expression(paste("-3",sigma, sep=""))
            , expression(paste("-2",sigma, sep=""))
            , expression(paste("-1",sigma, sep=""))
            , expression(mu)
            , expression(paste("+1",sigma, sep=""))
            , expression(paste("+2",sigma, sep=""))
            , expression(paste("+3",sigma, sep=""))
            , expression(paste("+4",sigma, sep=""))
            , expression(paste("+5",sigma, sep=""))
          )
        )
    )
  }

  ggsave(concat(pth_out, "/image-BackTesting-", seg,"-sensitivity_shocks.png"), plot=p_shock, height=12, width=12*GOLDEN_RATIO, unit="cm")

}
