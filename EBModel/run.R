

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

################################################################################

src_path = "/Users/jerrywatkins/Projects/Engagements/BOH/ending-balance/combined"
source(concat(src_path, "/1-ImportEconomicData.R"))
source(concat(src_path, "/2-ImportCREEndingBalances.R"))
source(concat(src_path, "/3-ImportCIEndingBalances.R"))
source(concat(src_path, "/4-CombineCREandCIEndingBalances.R"))
source(concat(src_path, "/5-ExploreData.R"))
source(concat(src_path, "/6-LeastSquaresModelSelection.R"))
source(concat(src_path, "/7-BackTesting.R"))
source(concat(src_path, "/8-Forecasts.R"))
