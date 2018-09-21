
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


### Collect SNL Data
# note: the ending balance units are thousands
pth_excel_data = concat(pth_inputs, "/snl/Modified SNL CRE ending balance.xlsx")

comm_re = get_excel(pth_excel_data, "206548 Comm RE")
multi_family = get_excel(pth_excel_data, "206545 Multifamly")
owner_occ = get_excel(pth_excel_data, "206546 Owner Occ ")
non_owner_occ = get_excel(pth_excel_data, "206547 Non Owner Occ")

snl_cre = stack(comm_re, multi_family, owner_occ, non_owner_occ
  , labels=c("oo_no", "mf", "oo", "no")
)

rename_list = list(
    "label"="segment"
  , "X1" = "quarter_date"
  , "X2" = "quarter_month"
  , "X3" = "quarter_year"
  , "X4" = "snl_field_key"
  , "X5" = "qtr_"
  , "BBCN" = "bbcn_eb"
  , "Wilshire.Bank" = "wilshire_eb"
  , "Saehan.Bancorp" = "saehan_eb"
  , "Bank.Asiana" = "bank_asiana_eb"
  , "Foster.Bankshares..Inc." = "foster_eb"
  , "Pacific.International.Bank" = "pacific_eb"
  , "Nara.Bank" = "nara_eb"
  , "Asiana.bank" = "asiana_bank_eb"
  , "Liberty.Bank.of.New.York" = "liberty_eb"
  , "Mirae.Bank" = "mirae_eb"
  , "Innovative.Bank" = "innovative_eb"
)

snl_cre = snl_cre[, names(rename_list)]
new_names = sapply(names(rename_list), function(x) rename_list[[x]])
names(new_names) = NULL
names(snl_cre) = new_names
snl_cre = data.table(snl_cre)
snl_cre = snl_cre[, is_data_row := !(snl_field_key == "SNL Field Key")]
snl_cre = snl_cre[is_data_row == TRUE, ]


# convert ending balances to numeric
# and replace NA values with 0
name_vec = names(snl_cre)
numeric_conv_vec = name_vec[grep("_eb", name_vec)]

### convert to numeric and make units billions.
### dividing by 1e6 because values are in thousands
for (name in numeric_conv_vec) {
  snl_cre[, name] = as.numeric(snl_cre[, ..name][[1]])
  col = as.numeric(snl_cre[, ..name][[1]])/1e6
  snl_cre[, name] = ifelse(is.na(col), 0, col)
}

# Add variables
# total ending balance
snl_cre[, end_bal :=
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
snl_cre[, qtr_dt := as.Date(quarter_date, "%Y-%m-%d")]
snl_cre_seg = snl_cre[, c("qtr_dt", "segment", "end_bal")]
cre = dcast(snl_cre_seg, qtr_dt ~ segment, value.var = "end_bal", fun.aggregate=sum)

cre_agg = cre[, c("qtr_dt", "oo", "mf", "no")]


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

j = 1
for (bank in banks){
  bank_data = dcast(snl_cre, qtr_dt ~ segment, value.var = bank, fun.aggregate=sum)
  bank_data[["bank"]] = bank
  if (j == 1) {
    cre_banks = bank_data
  } else {
    cre_banks = rbind(cre_banks, bank_data)
  }
  j = j + 1
}

cre_banks = cre_banks[, c("qtr_dt", "oo", "mf", "no", "bank")]
setnames(cre_banks, c("oo", "mf", "no"), c("oo_bank", "mf_bank", "no_bank"))
cre_banks = cre_banks[cre_agg, on="qtr_dt"]
cre_banks[,
  `:=`(
      bank_pct_mf = ifelse(mf > 0, mf_bank/mf, 0)
    , bank_pct_no = ifelse(no > 0, no_bank/no, 0)
    , bank_pct_oo = ifelse(oo >0 , oo_bank/oo, 0)
  )

]

rm("snl_cre_seg", "snl_cre", "cre_agg")

cre = cre[order(qtr_dt)]
cre[,
  `:=`(
      year=year(qtr_dt)
    , cre_snl = mf + oo_no
    , cre_boh = mf + oo + no
    , ip=no + mf
  )
]

### Save Files for Later #######################################################
saveRDS(cre, concat(pth_out, "/data-cre.RDS"))
saveRDS(cre_banks, concat(pth_out, "/data-cre_banks.RDS"))
################################################################################
