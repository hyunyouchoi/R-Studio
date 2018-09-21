
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
# source has the following functions:
#  - stack()
#  - get_bal_forecast()
#  - concat()
#  - bin_interval_variable()
#  - calc_rsq(), calc_mape(), calc_mad(), calc_rmset()
#  - log_diff()
#  - cv_step(), cv_select()

library("openxlsx")
library("data.table")
library("lubridate")
library("ggplot2")
library("scales")
library("zoo")


### Collect SNL Data
# note: the ending balance units are thousands
c_i = read.xlsx(
    concat(pth_inputs, "/snl/Modified SNL_CI_EB.xlsx")
  , sheet="Sheet1"
  , colNames=FALSE
  , startRow=3
  , skipEmptyRows=FALSE
  , skipEmptyCols=FALSE
  , detectDates=TRUE
  , check.names=TRUE
  , na.strings="NA"
)


rename_list = list(
    "X1" = "quarter_date"
  , "X2" = "quarter_month"
  , "X3" = "quarter_year"
  , "X4" = "snl_field_key"
  , "X5" = "qtr_string"
  , "X6" = "bbcn_eb"
  , "X7" = "wilshire_eb"
  , "X8" = "saehan_eb"
  , "X9" = "bank_asiana_eb"
  , "X10" = "foster_eb"
  , "X11" = "pacific_eb"
  , "X12" = "nara_eb"
  , "X13" = "asiana_bank_eb"
  , "X14" = "liberty_eb"
  , "X15" = "mirae_eb"
  , "X16" = "innovative_eb"
)

c_i = c_i[, names(rename_list)]
new_names = sapply(names(rename_list), function(x) rename_list[[x]])
names(new_names) = NULL
names(c_i) = new_names
c_i = data.table(c_i)


# convert ending balances to numeric
# and replace NA values with 0
name_vec = names(c_i)
numeric_conv_vec = name_vec[grep("_eb", name_vec)]

for (name in numeric_conv_vec) {
  c_i[, name] = as.numeric(c_i[, ..name][[1]])
  col = as.numeric(c_i[, ..name][[1]])/1e6
  c_i[, name] = ifelse(is.na(col), 0, col)
}


# Add variables
# total ending balance
c_i[, ci :=
    bbcn_eb
  + wilshire_eb
  + saehan_eb
  + bank_asiana_eb
  + foster_eb
  + pacific_eb
  + nara_eb
  + asiana_bank_eb
  + liberty_eb
  + mirae_eb
  + innovative_eb
]

# Date Variable
c_i[, qtr_dt := as.Date(quarter_date, "%Y-%m-%d")]

c_i = c_i[order(qtr_dt)]

banks = c(
    "bbcn_eb"
  , "wilshire_eb"
  , "saehan_eb"
  , "bank_asiana_eb"
  , "foster_eb"
  , "pacific_eb"
  , "nara_eb"
  , "asiana_bank_eb"
  , "liberty_eb"
  , "mirae_eb"
  , "innovative_eb"
)

c_i_banks = melt(c_i, c("qtr_dt", "ci"), banks, variable.name="bank", value.name="ci_bank")
c_i_banks[, bank_pct_ci := ifelse(ci > 0, ci_bank/ci, 0)]
c_i = c_i[, c("qtr_dt", "ci")]
### Save Files for Later #######################################################
saveRDS(c_i, concat(pth_out, "/data-c_i.RDS"))
saveRDS(c_i_banks, concat(pth_out, "/data-c_i_banks.RDS"))
################################################################################





 
