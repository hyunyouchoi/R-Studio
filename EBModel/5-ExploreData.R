
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
library("orcutt")

################################################################################
GOLDEN_RATIO = 1.61803398875
STACK_RATIO = 1.20

### Import Data ################################################################
boh = readRDS(concat(pth_out, "/data-boh.RDS"))
boh_train = function(seg) {
  train_ind = concat("is_train_", seg)
  boh_cp = copy(boh)
  setnames(boh_cp, train_ind, "is_train")
  boh_cp[is_train == TRUE,]
}
boh_train_ce = boh_train("ip")
#boh_train_ci = boh_train("ci")
################################################################################

cre_banks = readRDS(concat(pth_out, "/data-cre_banks.RDS"))
#c_i_banks = readRDS(concat(pth_out, "/data-c_i_banks.RDS"))
#boh_banks = c_i_banks[cre_banks, on=c("qtr_dt", "bank")]
info = data.table(get_excel(concat(pth_inputs, "/table-variable_information.xlsx"), "vars"))

#boh_banks_agg = boh_banks[, c(.N, lapply(.SD, sum)), by=c("qtr_dt", "bank")][, c("qtr_dt","bank", "bank_pct_mf", "bank_pct_no", "bank_pct_oo", "bank_pct_ci")]
#write.csv(boh_banks_agg, file=concat(pth_out, "/table-ExploreData-boh-banks.csv"))
#write.csv(boh[, c("qtr_dt", "ci", "mf", "no", "oo")], file=concat(pth_out, "/table-ExploreData-boh-balances.csv"))

boh_banks_agg = cre_banks[, c(.N, lapply(.SD, sum)), by=c("qtr_dt", "bank")][, c("qtr_dt","bank", "bank_pct_mf", "bank_pct_no", "bank_pct_oo")]
write.csv(boh_banks_agg, file=concat(pth_out, "/table-ExploreData-boh-banks.csv"))
write.csv(boh[, c("qtr_dt", "mf", "no", "oo")], file=concat(pth_out, "/table-ExploreData-boh-balances.csv"))

################################################################################

### Find correlations ##########################################################

#segments = c("ci", "no", "mf", "oo")

segments = c("no", "mf", "oo")

cor_table = info[, c("name")]
cor_table[, segments] = 0
n = dim(cor_table)[1]
names = cor_table[["name"]]

for (seg in segments) {
  ldiff = concat("ldiff_", seg)

  if (seg == "ci") {
    X = boh_train_ci
  } else {
    X = boh_train_ce
  }

  Y = X[[ldiff]]
  for (i in 1:n) {
      name = names[i]
      cor_table[i, seg] = cor(X[[name]], Y)
    }
}

### updat variable information
updated_info = info[cor_table, on="name"]
saveRDS(updated_info, concat(pth_inputs, "/table-variable_information_rho.RDS"))



### Percent of BoH Plots #######################################################
###  Also add PSI

#past_data = boh_train_ce[, c("qtr_dt", "ci", "mf", "no" , "oo")]
#active_data = boh[qtr_dt > as.Date("2016-12-31"), c("qtr_dt", "ci", "mf", "no" , "oo")]

past_data = boh_train_ce[, c("qtr_dt", "mf", "no" , "oo")]
active_data = boh[qtr_dt > as.Date("2016-12-31"), c("qtr_dt", "mf", "no" , "oo")]

past_data[["psi"]] = get_psi_from_vector_amts(active_data, past_data, cols=c( "mf", "no", "oo"))

avg_mf_pct = mean(boh_train_ce[["pct_mf"]])

p_pct = (
  ggplot()
  + geom_line(data=boh_train_ce, aes(x=qtr_dt, y=pct_mf, color="Multifamily"))
  + geom_line(data=boh_train_ce, aes(x=qtr_dt, y=pct_no, color="Non-Owner"))
  + geom_line(data=boh_train_ce, aes(x=qtr_dt, y=pct_ip, color="Income Producing"))
  + geom_line(data=boh_train_ce, aes(x=qtr_dt, y=pct_oo, color="Owner Occupied"))
  + theme_minimal()
  + labs(x=NULL, y="% of BOH CRE Balance")
  + theme(panel.grid.minor=element_blank())
  + theme(legend.title=element_blank(), legend.position="bottom")
  + scale_y_continuous(label=percent_format())
  + geom_hline(yintercept = avg_mf_pct, linetype=2, color=hxGray)
  + geom_text(aes(as.Date("2009-03-31"),avg_mf_pct,label = "3%", vjust = -1))
  + guides(col=guide_legend(nrow=1))

)

p_psi =(
      ggplot(data=past_data, mapping = aes(x = qtr_dt, y = psi))
    #+ geom_hline(aes(yintercept = 0.25), linetype=2, color="red")
    + geom_segment(mapping = aes(xend = qtr_dt, yend = 0))
    + theme_minimal()
    + theme(panel.grid.minor=element_blank())
    + labs(x=NULL, y="PSI")
)

pop_plot = grid.arrange(p_pct, p_psi, nrow=2, heights=c(9,3))
ggsave(concat(pth_out, "/image-ExploreData-boh-balance_pct_of_boh_chart.png"), plot=pop_plot, height=12*GOLDEN_RATIO, width=12*GOLDEN_RATIO, unit="cm")


### cumulative growth chart

#anchors = boh_train_ce[qtr_dt == as.Date("2007-06-30"), c("ci", "no", "mf", "ip", "oo")]

anchors = boh_train_ce[qtr_dt == as.Date("2007-06-30"), c("no", "mf", "ip", "oo")]

g_p = (
 ggplot()
 + geom_rect(aes(xmin=as.Date("2007-12-31"), xmax=as.Date("2009-06-30"), ymin=1, ymax=2.75), alpha=0.35)
 + geom_text(aes(x=as.Date("2008-09-30"), y=2.40, label="Recession"), size=4, color="white")
 + geom_line(data=boh_train_ce[qtr_dt >= as.Date("2007-12-31"),], aes(x=qtr_dt, y=ma(mf/anchors[["mf"]],n=2), color="Multifamily"))
 + geom_line(data=boh_train_ce[qtr_dt >= as.Date("2007-12-31"),], aes(x=qtr_dt, y=ma(no/anchors[["no"]],n=2), color="Non-Owner Occupied"))
 + geom_line(data=boh_train_ce[qtr_dt >= as.Date("2007-12-31"),], aes(x=qtr_dt, y=ma(oo/anchors[["oo"]],n=2), color="Owner Occupied"))
# + geom_line(data=boh_train_ce[qtr_dt >= as.Date("2007-12-31"),], aes(x=qtr_dt, y=ma(ci/anchors[["ci"]],n=2), color="C&I"))
 + geom_line(data=boh_train_ce[qtr_dt >= as.Date("2007-12-31"),], aes(x=qtr_dt, y=ma(ip/anchors[["ip"]],n=2), color="Income Producing"), linetype=2)

 + theme_minimal()
 + labs(x=NULL, y="Cumulative Growth %")
 + scale_y_continuous(label=percent_format())
 + theme(panel.grid.minor=element_blank(), plot.title = element_text(hjust = 0.50))
 + theme(legend.title=element_blank(), legend.position="bottom")
)
ggsave(concat(pth_out, "/image-ExploreData-boh-growth_patterns.png"), plot=g_p, height=12, width=12*GOLDEN_RATIO, unit="cm")
################################################################################


#### Any Outliers? #############################################################
box_data = data.table()

#labs = list(ci="C&I", mf="Multifamily", oo="Owner Occupied", no="Non-Owner Occupied", ip="Income Producing")
labs = list(mf="Multifamily", oo="Owner Occupied", no="Non-Owner Occupied", ip="Income Producing")

#for (seg in c("ci", "mf", "no", "oo", "ip")) {

for (seg in c("mf", "no", "oo", "ip")) {
  train = boh_train_ce
  if (seg == "ci") { train = boh_train_ci }

  resp = concat("ldiff_", seg)
  resp_data = train[, resp, with=FALSE]
  setnames(resp_data, resp, "resp")
  resp_data[["seg"]] = labs[[seg]]

  box_data = rbind(box_data, resp_data)

}

p_box = (
    ggplot()
  + geom_boxplot(data=box_data[seg %in% c(labs[["ip"]],labs[["oo"]], labs[["mf"]], labs[["no"]]), ], mapping=aes(x=seg, y=resp))
  + coord_flip()
  + labs(y="Log-difference", x=NULL)
  + theme_minimal()
  + theme(panel.grid.minor=element_blank())
)

ggsave(concat(pth_out, "/image-ExloreData-boh-distributions.png"), plot=p_box, height=5, width=15, unit="cm")

### Balance and Log-diff Plots and ACF #########################################
### C-I

#for (seg in c("boh","ci", "mf", "no", "oo")) {


for (seg in c("cre_boh","mf", "no", "oo", "boh")) {
  
  if(seg == "mf"){
    oldCopy <- boh
    boh <- boh[qtr_dt > "2007-03-31"]
  }
  
  bal = boh_train(seg)[[seg]]
  ldiff = boh_train(seg)[[concat("ldiff_", seg)]]
  dates = boh_train(seg)[["qtr_dt"]]

  acf_obj = acf(bal, plot=FALSE)
  acf_df = with(acf_obj, data.frame(lag, acf))
  acf_hi = 1.96/sqrt(length(bal))
  acf_lo = -acf_hi

  b_acf = (
        ggplot(data = acf_df, mapping = aes(x = lag, y = acf))
      + geom_hline(aes(yintercept = 0))
      + geom_hline(aes(yintercept = acf_hi), linetype=2, color=hxBlue)
      + geom_hline(aes(yintercept = acf_lo), linetype=2, color=hxBlue)
      + geom_segment(mapping = aes(xend = lag, yend = 0))
      + theme_minimal()
      + theme(panel.grid.minor=element_blank(), plot.title = element_text(size=14, hjust = 0.50))
      + labs(x="Lag", y="ACF",title="Balance ACF")
  )

  acf_obj = acf(ldiff, plot=FALSE)
  acf_df = with(acf_obj, data.frame(lag, acf))
  acf_hi = 1.96/sqrt(length(ldiff))
  acf_lo = -acf_hi

  l_acf = (
        ggplot(data = acf_df, mapping = aes(x = lag, y = acf))
      + geom_hline(aes(yintercept = 0))
      + geom_hline(aes(yintercept = acf_hi), linetype=2, color=hxBlue)
      + geom_hline(aes(yintercept = acf_lo), linetype=2, color=hxBlue)
      + geom_segment(mapping = aes(xend = lag, yend = 0))
      + theme_minimal()
      + theme(panel.grid.minor=element_blank(), plot.title = element_text(size=14, hjust = 0.50))
      + labs(x="Lag", y="ACF",title="Transformation ACF")
  )


  b_series = (
        ggplot()
      + geom_line(aes(x=dates, y=bal), color=hxBlue)
      + geom_point(aes(x=dates, y=bal), color=hxBlue)
      + theme_minimal()
      + theme(panel.grid.minor=element_blank(), plot.title = element_text(size=14, hjust = 0.50))
      + labs(x=NULL, y="Balance",title="Balance")
  )


  l_series = (
        ggplot()
      + geom_line(aes(x=dates, y=ldiff), color=hxBlue)
      + geom_point(aes(x=dates, y=ldiff), color=hxBlue)
      + theme_minimal()
      + theme(panel.grid.minor=element_blank(), plot.title = element_text(size=14, hjust = 0.50))
      + labs(x=NULL, y="Log-difference",title="Transformed Balance")
  )


  t_top = grid.arrange(b_series, b_acf,ncol=2)
  t_bot = grid.arrange(l_series, l_acf,ncol=2)
  t_plot = grid.arrange(t_top, t_bot, nrow=2)
  ggsave(concat(pth_out, "/image-ExploreData-", seg, "-targets.png"), plot=t_plot, height=10*GOLDEN_RATIO, width=12*GOLDEN_RATIO, unit="cm")
  
  if(seg == "mf"){
    boh <- oldCopy
  }

}


bal = boh_train(seg)[[seg]]

b_series = (
  ggplot()
  + geom_line(aes(x=dates, y=bal), color=hxBlue)
  + geom_point(aes(x=dates, y=bal), color=hxBlue)
  + theme_minimal()
  + theme(panel.grid.minor=element_blank(), plot.title = element_text(size=14, hjust = 0.50))
  + labs(x=NULL, y="Balance",title="Balance")
)

b_series 


