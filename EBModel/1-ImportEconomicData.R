
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
pth_inputs = "/Users/jerrywatkins/Projects/Engagements/BOH/ending-balance/combined/read-only-inputs"
pth_lib = "/Users/jerrywatkins/Projects/Engagements/BOH/ending-balance/combined/library"
pth_out = "/Users/jerrywatkins/Projects/Engagements/BOH/ending-balance/combined"
### No need to make changes below after this line ##############################

### Dependencies
source(paste(pth_lib,"/dev-support.R", sep=""))
source(paste(pth_lib,"/dfast-support.R", sep=""))

library("openxlsx")
library("data.table")
library("lubridate")
library("ggplot2")
library("scales")
library("zoo")
library("tseries")

# Step 1 of 4
### Regional Data ##############################################################
# needs openxlsx package
# ! Caution ! make sure the column order still matches the following
# column	variable	scenaro	description
# x1	qtr_dt	scenario	none
# x2	ca_unemp	baseline	FRB CCAR 2017 - Baseline : Labor: Unemployment Rate, (%, SA)
# x3	ca_unemp	adverse	FRB CCAR 2017 - Adverse : Labor: Unemployment Rate, (%, SA)
# x4	ca_unemp	severe	FRB CCAR 2017 - Severely Adverse : Labor: Unemployment Rate, (%, SA)
# x5	ca_hpi	baseline	FRB CCAR 2017 - Baseline : FHFA  All Transactions Home Price Index, (1980Q1 = 100, SA)
# x6	ca_hpi	adverse	FRB CCAR 2017 - Adverse : FHFA  All Transactions Home Price Index, (1980Q1 = 100, SA)
# x7	ca_hpi	severe	FRB CCAR 2017 - Severely Adverse : FHFA  All Transactions Home Price Index, (1980Q1 = 100, SA)
# x8	ca_gsp	baseline	FRB CCAR 2017 - Baseline : Gross State Product: Total, (Bil. $, SAAR) Nominal
# x9	ca_gsp	adverse	FRB CCAR 2017 - Adverse : Gross State Product: Total, (Bil. $, SAAR) Nominal
# x10	ca_gsp	severe	FRB CCAR 2017 - Severely Adverse : Gross State Product: Total, (Bil. $, SAAR) Nominal
# x11	ca_real_gsp	baseline	FRB CCAR 2017 - Baseline : Gross State Product: Total, (Bil. Chained 2009 $, SAAR) Real
# x12	ca_real_gsp	adverse	FRB CCAR 2017 - Adverse : Gross State Product: Total, (Bil. Chained 2009 $, SAAR) Real
# x13	ca_real_gsp	severe	FRB CCAR 2017 - Severely Adverse : Gross State Product: Total, (Bil. Chained 2009 $, SAAR) Real
# x14	ca_income	baseline	FRB CCAR 2017 - Baseline : Income: Disposable Personal, (Mil. $, SAAR) Nominal
# x15	ca_income	adverse	FRB CCAR 2017 - Adverse : Income: Disposable Personal, (Mil. $, SAAR) Nominal
# x16	ca_income	severe	FRB CCAR 2017 - Severely Adverse : Income: Disposable Personal, (Mil. $, SAAR) Nominal
# x17	ca_real_income	baseline	FRB CCAR 2017 - Baseline : Disposable Personal Income, (Mil. 09$, SAAR) Real
# x18	ca_real_income	adverse	FRB CCAR 2017 - Adverse : Disposable Personal Income, (Mil. 09$, SAAR) Real
# x19	ca_real_income	severe	FRB CCAR 2017 - Severely Adverse : Disposable Personal Income, (Mil. 09$, SAAR) Real

raw_region_data = read.xlsx(
    concat(pth_inputs, "/moodys/Regional Macrovariables Moodys.xlsx")
  , sheet="Sheet1"
  , colNames=FALSE
  , startRow=6
  , skipEmptyRows=FALSE
  , skipEmptyCols=FALSE
  , detectDates=TRUE
  , check.names=TRUE
  , na.strings="ND"
)

reg_baseline_cols = c("X1", "X2", "X5", "X8", "X11", "X14", "X17")
reg_adverse_cols = c("X1", "X3", "X6", "X9", "X12", "X15", "X18")
reg_severe_cols = c("X1", "X4", "X7", "X10", "X13", "X16", "X19")

reg_new_col_names = c(
    "qtr_dt"
  , "ca_unemp"
  , "ca_hpi"
  , "ca_gsp"
  , "ca_rgsp"
  , "ca_inc"
  , "ca_rinc"
)


reg_baseline = data.table(raw_region_data[, reg_baseline_cols])
reg_adverse = data.table(raw_region_data[, reg_adverse_cols])
reg_severe = data.table(raw_region_data[, reg_severe_cols])

setnames(reg_baseline, reg_baseline_cols, reg_new_col_names)
setnames(reg_adverse, reg_adverse_cols, reg_new_col_names)
setnames(reg_severe, reg_severe_cols, reg_new_col_names)

# Step 2 of 4
### Employment Data ############################################################
# ! Caution ! make sure the column order still matches the following
# r_name	var	scenario	region	description:
# X1	qtr_dt	none	none	Description:
# X2	empl	baseline	us	FRB CCAR 2017 - Baseline: Employment: Total Nonagricultural, (Mil. #, SA)
# X3	empl	adverse	us	FRB CCAR 2017 - Adverse: Employment: Total Nonagricultural, (Mil. #, SA)
# X4	empl	severe	us	FRB CCAR 2017 - Severely Adverse: Employment: Total Nonagricultural, (Mil. #, SA)
# X5	ca_empl	baseline	ca	FRB CCAR 2017 - Baseline : Employment: Total Nonagricultural, (Ths., SA)
# X6	ca_empl	adverse	ca	FRB CCAR 2017 - Adverse : Employment: Total Nonagricultural, (Ths., SA)
# X7	ca_empl	severe	ca	FRB CCAR 2017 - Severely Adverse : Employment: Total Nonagricultural, (Ths., SA)

raw_empl_data = read.xlsx(
    concat(pth_inputs, "/moodys/Non Farm Employment Moodys.xlsx")
  , sheet="Sheet1"
  , colNames=FALSE
  , startRow=6
  , skipEmptyRows=FALSE
  , skipEmptyCols=FALSE
  , detectDates=TRUE
  , check.names=TRUE
  , na.strings="ND"
)

empl_baseline_cols = c("X1", "X2", "X5")
empl_adverse_cols = c("X1", "X3", "X6")
empl_severe_cols = c("X1", "X4", "X7")

empl_new_col_names = c(
    "qtr_dt"
  , "empl"
  , "ca_empl"
)


empl_baseline = data.table(raw_empl_data[, empl_baseline_cols])
empl_adverse = data.table(raw_empl_data[, empl_adverse_cols])
empl_severe = data.table(raw_empl_data[, empl_severe_cols])

setnames(empl_baseline, empl_baseline_cols, empl_new_col_names)
setnames(empl_adverse, empl_adverse_cols, empl_new_col_names)
setnames(empl_severe, empl_severe_cols, empl_new_col_names)


# Step 3 of 4
# FRB Data #####################################################################
# Collect historical data

raw_historic = fread(concat(pth_inputs, "/frb/2017/Historic_Domestic.csv"))
raw_baseline = fread(concat(pth_inputs, "/frb/2017/Table_2A_Supervisory_Baseline_Domestic.csv"))
raw_adverse = fread(concat(pth_inputs, "/frb/2017/Table_3A_Supervisory_Adverse_Domestic.csv"))
raw_severe = fread(concat(pth_inputs, "/frb/2017/Table_4A_Supervisory_Severely_Adverse_Domestic.csv"))


# Step 4 of 4
# Transformations ##############################################################
get_frb_data = function(raw_frb_data) {

  tf_data = copy(raw_frb_data)
  orig_names = c(
      "Real GDP growth"
    , "Nominal GDP growth"
    , "Real disposable income growth"
    , "Nominal disposable income growth"
    , "Unemployment rate"
    , "CPI inflation rate"
    , "3-month Treasury rate"
    , "5-year Treasury yield"
    , "10-year Treasury yield"
    , "BBB corporate yield"
    , "Mortgage rate"
    , "Prime rate"
    , "Dow Jones Total Stock Market Index (Level)"
    , "House Price Index (Level)"
    , "Commercial Real Estate Price Index (Level)"
    , "Market Volatility Index (Level)"
    , "Date"
  )

  new_names = c(
      "rgdp_qg"
    , "gdp_qg"
    , "rinc_qg"
    , "inc_qg"
    , "unemp"
    , "cpi"
    , "yld_03m"
    , "yld_05y"
    , "yld_10y"
    , "yld_bbb"
    , "mort"
    , "prime"
    , "dow"
    , "hpi"
    , "crei"
    , "vix"
    , "qtr_date_string"
  )



  setnames(tf_data, orig_names, new_names)
  # also add yield spread
  tf_data[["bbb_spread"]]= tf_data[["yld_bbb"]] - tf_data[["yld_10y"]]
  tf_data[["yld_spread"]]= tf_data[["yld_10y"]] - tf_data[["yld_03m"]]


  # get date variable
  yr = substr(tf_data[["qtr_date_string"]], 1, 4)
  qtr = substr(tf_data[["qtr_date_string"]], 6, 7)
  qtr_yr = paste(qtr, yr)
  tf_data[["qtr_dt"]] = as.Date(as.yearqtr(qtr_yr, format = "Q%q %Y"), frac=1)

  tf_data

}


frb_historic = get_frb_data(raw_historic)
frb_baseline = get_frb_data(rbind(raw_historic, raw_baseline))
frb_adverse  = get_frb_data(rbind(raw_historic, raw_adverse))
frb_severe   = get_frb_data(rbind(raw_historic, raw_severe))

transform = function(raw_frb_data, reg_data, empl_data) {

  # Add regional and empl variables
  tf_data = empl_data[reg_data[raw_frb_data, on="qtr_dt"], on="qtr_dt"]

  # calc growth rates
  tf_data[["hpi_qg"]] = gr(tf_data[["hpi"]])
  tf_data[["ca_hpi_qg"]] = gr(tf_data[["ca_hpi"]])
  tf_data[["crei_qg"]] = gr(tf_data[["crei"]])
  tf_data[["dow_qg"]] = gr(tf_data[["dow"]])
  tf_data[["empl_qg"]] = gr(tf_data[["empl"]])
  tf_data[["ca_empl_qg"]] = gr(tf_data[["ca_empl"]])
  tf_data[["ca_gsp_qg"]] = gr(tf_data[["ca_gsp"]])
  tf_data[["ca_rgsp_qg"]] = gr(tf_data[["ca_rgsp"]])
  tf_data[["ca_inc_qg"]] = gr(tf_data[["ca_inc"]])
  tf_data[["ca_rinc_qg"]] = gr(tf_data[["ca_rinc"]])

  tf_data[["hpi_eg"]] = gr(tf_data[["hpi"]], lag=8)
  tf_data[["ca_hpi_eg"]] = gr(tf_data[["ca_hpi"]], lag=8)
  tf_data[["crei_eg"]] = gr(tf_data[["crei"]], lag=8)
  tf_data[["dow_eg"]] = gr(tf_data[["dow"]], lag=8)
  tf_data[["empl_eg"]] = gr(tf_data[["empl"]], lag=8)
  tf_data[["ca_empl_eg"]] = gr(tf_data[["ca_empl"]], lag=8)
  tf_data[["ca_gsp_eg"]] = gr(tf_data[["ca_gsp"]], lag=8)
  tf_data[["ca_rgsp_eg"]] = gr(tf_data[["ca_rgsp"]], lag=8)
  tf_data[["ca_inc_eg"]] = gr(tf_data[["ca_inc"]], lag=8)
  tf_data[["ca_rinc_eg"]] = gr(tf_data[["ca_rinc"]], lag=8)

  # keep relevant columns
  core_names = c(
      "dow"
    , "hpi"
    , "ca_hpi"
    , "crei"
    , "dow_qg"
    , "hpi_qg"
    , "ca_hpi_qg"
    , "crei_qg"
    , "ca_rgsp_qg"
    , "ca_gsp_qg"
    , "rgdp_qg"
    , "gdp_qg"
    , "ca_rinc_qg"
    , "ca_inc_qg"
    , "rinc_qg"
    , "inc_qg"
    , "ca_unemp"
    , "unemp"
    , "ca_empl_qg"
    , "empl_qg"
    , "yld_spread"
    , "bbb_spread"
    , "hpi_eg"
    , "ca_hpi_eg"
    , "crei_eg"
    , "dow_eg"
    , "empl_eg"
    , "ca_empl_eg"
    , "ca_gsp_eg"
    , "ca_rgsp_eg"
    , "ca_inc_eg"
    , "ca_rinc_eg"
  )

  tf_data = tf_data[, c("qtr_dt", "yld_03m", core_names), with=FALSE]

  for (name in c("yld_spread", "bbb_spread", "unemp", "ca_unemp", "yld_03m")) {
    dq_nm = concat(name, "_qd")
    dy_nm = concat(name, "_yd")

    tf_data[[dq_nm]] = delta(tf_data[[name]], lag=1)
    tf_data[[concat(dq_nm, "_lag", 1)]] = shift(tf_data[[dq_nm]], n=1)
    tf_data[[concat(dq_nm, "_lag", 2)]] = shift(tf_data[[dq_nm]], n=2)
    tf_data[[concat(dq_nm, "_lag", 3)]] = shift(tf_data[[dq_nm]], n=3)

    tf_data[[dy_nm]] = delta(tf_data[[name]], lag=4)
    tf_data[[concat(dy_nm, "_lag", 1)]] = shift(tf_data[[dy_nm]], n=1)
    tf_data[[concat(dy_nm, "_lag", 2)]] = shift(tf_data[[dy_nm]], n=2)
    tf_data[[concat(dy_nm, "_lag", 3)]] = shift(tf_data[[dy_nm]], n=3)

  }

  for (name in core_names) {
    # Transformations:

    if (name %in% c("hpi", "crei", "ca_hpi", "bbb_spread", "yld_spread")) {
      # Log-run ratio

      lf_nm = concat(name,"_lf")
      le_nm = concat(name,"_le")
      lt_nm = concat(name,"_lt")

      # Long-run ratio:
      tf_data[[lf_nm]] = 100 * ((tf_data[[name]]/ma(tf_data[[name]], n=4)) - 1)
      tf_data[[concat(lf_nm, "_lag", 1)]] = shift(tf_data[[lf_nm]], n=1)
      tf_data[[concat(lf_nm, "_lag", 2)]] = shift(tf_data[[lf_nm]], n=2)
      tf_data[[concat(lf_nm, "_lag", 3)]] = shift(tf_data[[lf_nm]], n=3)
      tf_data[[concat(lf_nm, "_lag", 4)]] = shift(tf_data[[lf_nm]], n=4)

      tf_data[[le_nm]] = 100 * ((tf_data[[name]]/ma(tf_data[[name]], n=8)) - 1)
      tf_data[[concat(le_nm, "_lag", 1)]] = shift(tf_data[[le_nm]], n=1)
      tf_data[[concat(le_nm, "_lag", 2)]] = shift(tf_data[[le_nm]], n=2)
      tf_data[[concat(le_nm, "_lag", 3)]] = shift(tf_data[[le_nm]], n=3)
      tf_data[[concat(le_nm, "_lag", 4)]] = shift(tf_data[[le_nm]], n=4)

      tf_data[[lt_nm]] = 100 * ((tf_data[[name]]/ma(tf_data[[name]], n=12)) - 1)
      tf_data[[concat(lt_nm, "_lag", 1)]] = shift(tf_data[[lt_nm]], n=1)
      tf_data[[concat(lt_nm, "_lag", 2)]] = shift(tf_data[[lt_nm]], n=2)
      tf_data[[concat(lt_nm, "_lag", 3)]] = shift(tf_data[[lt_nm]], n=3)
      tf_data[[concat(lt_nm, "_lag", 4)]] = shift(tf_data[[lt_nm]], n=4)

    }

    # Lag-1:
    tf_data[[concat(name, "_lag", 1)]] = shift(tf_data[[name]], n=1)
    tf_data[[concat(name, "_lag", 2)]] = shift(tf_data[[name]], n=2)
    tf_data[[concat(name, "_lag", 3)]] = shift(tf_data[[name]], n=3)
    tf_data[[concat(name, "_lag", 4)]] = shift(tf_data[[name]], n=4)

    # Annualized Growth Rates
    if (length(grep("_qg", name)) != 0) {
      # Annualized Rate
      ag_nm = gsub("_qg", "_ag", name)

      rate_vec = tf_data[[name]]/100
      n = length(rate_vec)
      tf_data[[ag_nm]] = sapply(1:n, function(t) {

          if (t < 4) { agr = NA }
          else {
            agr = 1
            for (j in 0:3) {
              agr = agr * (1 + rate_vec[t - j])
            }
          }
          agr = (agr^(1/4)) - 1
          agr = 100 * agr

          agr
        }
      )

      tf_data[[concat(ag_nm, "_lag", 1)]] = shift(tf_data[[ag_nm]], n=1)
      tf_data[[concat(ag_nm, "_lag", 2)]] = shift(tf_data[[ag_nm]], n=2)
      tf_data[[concat(ag_nm, "_lag", 3)]] = shift(tf_data[[ag_nm]], n=3)
      tf_data[[concat(ag_nm, "_lag", 4)]] = shift(tf_data[[ag_nm]], n=4)

      ya_nm = gsub("_qg", "_ya", name)
      tf_data[[ya_nm]] = ma(tf_data[[name]], n=4)
      tf_data[[concat(ya_nm, "_lag", 1)]] = shift(tf_data[[ya_nm]], n=1)
      tf_data[[concat(ya_nm, "_lag", 2)]] = shift(tf_data[[ya_nm]], n=2)
      tf_data[[concat(ya_nm, "_lag", 3)]] = shift(tf_data[[ya_nm]], n=3)
      tf_data[[concat(ya_nm, "_lag", 4)]] = shift(tf_data[[ya_nm]], n=4)

    }

  }


  RECESSION_START = "2007-12-31"
  RECESSION_END = "2009-06-30"
  START_9Q = "2017-03-31"
  END_9Q = "2019-03-31"
  tf_data[["is_recession"]] = ifelse(tf_data[["qtr_dt"]] >= as.Date(RECESSION_START) & tf_data[["qtr_dt"]] <= as.Date(RECESSION_END), TRUE, FALSE)
  tf_data[["is_9q_data"]] = ifelse(tf_data[["qtr_dt"]] >= as.Date(START_9Q) & tf_data[["qtr_dt"]] <= as.Date(END_9Q), TRUE, FALSE)

  tf_data

}

historic = transform(frb_historic, reg_baseline, empl_baseline)[qtr_dt <= as.Date("2019-03-31"),]
baseline = transform(frb_baseline, reg_baseline, empl_baseline)[qtr_dt <= as.Date("2019-03-31"),]
adverse = transform(frb_adverse, reg_adverse, empl_adverse)[qtr_dt <= as.Date("2019-03-31"),]
severe = transform(frb_severe, reg_severe, empl_severe)[qtr_dt <= as.Date("2019-03-31"),]

### Save Files for Later #######################################################
saveRDS(historic, concat(pth_out, "/econ-data-historic.RDS"))
saveRDS(baseline, concat(pth_out, "/econ-data-baseline.RDS"))
saveRDS(adverse, concat(pth_out, "/econ-data-adverse.RDS"))
saveRDS(severe, concat(pth_out, "/econ-data-severe.RDS"))
################################################################################
