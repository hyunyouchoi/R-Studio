###################################################################
# Project: Bank of Hope
# LGD C&I Model
###################################################################  

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(DataAnalytics)
library(tseries)
library(urca)
library (fUnitRoots)
library(lubridate)
library(forecast)
library(tseries)
library(CADFtest)
library (leaps)
library(data.table)
library(openxlsx)
library(car)

####################

##save image
# save.image("lgd-ci-v2.RData")

##load image
#load("lgd-ci-v2.RData")
##################################

# StepFun= dget("StepFun.R")

#####################
#read in the raw data
#####################
df=read.csv("input_lgd_ci.csv",header = TRUE)
names(df)= c("date", "wa_lgd","year", "q")

plot(df$wa_lgd)
pp.test(na.remove(df$wa_lgd), lshort = F)

###################################
#transformations-- Q-o-Q and Y-o-Y
##################################
#q-o-q
df$wa_lgd_qd=c(NA, diff(df$wa_lgd))
#y-o-y
df$wa_lgd_ad= df$wa_lgd- back(df$wa_lgd, noperiods = 4)

#######################
#read in the macro vars 
#######################

######
#base
######
base=read.csv("base.csv", header=T)
aaa=which(base$year==2007 & base$quarter==1)
bbb=which(base$year==2018 & base$quarter==4)
base=base[aaa:bbb,]

#########
#adverse
#########

adverse=read.csv("adverse.csv", header=T)
aaa=which(adverse$year==2007 & adverse$quarter==1)
bbb=which(adverse$year==2018 & adverse$quarter==4)
adverse=adverse[aaa:bbb,]

########
#severe
########
severe=read.csv("severe.csv", header=T)
aaa=which(severe$year==2007 & severe$quarter==1)
bbb=which(severe$year==2018 & severe$quarter==4)
severe=severe[aaa:bbb,]

##################
#development macro
##################
D1=which(base$year==2007 & base$q==1)
D2=which(base$year==2015 & base$q==4)
macro_dev=base[c(D1:D2), ]


######################################
# Create the dep_var matrix
######################################

#create var info
var.names=colnames(macro_dev[,-c(1,2,3)])
var_info=as.data.frame(matrix(0, length(var.names), 6 ))
names(var_info) = c("var", "tier", "base", "lag", "diff", "sign")
var_info[,1]=var.names
var_info[,5]=0


#diff
var_info[grepl("_qd", var_info$var),5] = TRUE
var_info[grepl("_yd", var_info$var),5] = TRUE
var_info[grepl("_ad", var_info$var),5] = TRUE
var_info[grepl("_ag", var_info$var),5] = TRUE
var_info[grepl("_qg", var_info$var),5] = TRUE


#lag
var_info[grepl("_lag_1", var_info$var),4] = 1
var_info[grepl("_lag_2", var_info$var),4] = 2
var_info[grepl("_lag_3", var_info$var),4] = 3
var_info[grepl("_lag_4", var_info$var),4] = 4

#var.base

var_info[grepl("ngdp", var_info$var),3] = "gdp"
var_info[grepl("rgdp", var_info$var),3] = "gdp"
var_info[grepl("rdi", var_info$var),3] = "dpi"
var_info[grepl("ndi", var_info$var),3] = "dpi"
var_info[grepl("ur_", var_info$var),3] = "ur_diff"
var_info[grepl("UR_", var_info$var),3] = "ur_diff"
var_info[grepl("cpi_", var_info$var),3] = "cpi"
var_info[grepl("i3m", var_info$var),3] = "i"
var_info[grepl("i5y", var_info$var),3] = "i"
var_info[grepl("i10y", var_info$var),3] = "i"
var_info[grepl("bbb", var_info$var),3] = "spr10"
var_info[grepl("imort", var_info$var),3] = "i"
var_info[grepl("iprim", var_info$var),3] = "i"
var_info[grepl("cppi", var_info$var),3] = "cppi"
var_info[grepl("dji", var_info$var),3] = "dji"
var_info[grepl("VIX", var_info$var),3] = "vix"
var_info[grepl("vix", var_info$var),3] = "vix"
var_info[grepl("hpi_q", var_info$var),3] = "hpi_diff"
var_info[grepl("HPI_q", var_info$var),3] = "hpi_diff"
var_info[grepl("hpi_a", var_info$var),3] = "hpi_diff"
var_info[grepl("HPI_a", var_info$var),3] = "hpi_diff"
var_info[grepl("hpi_g", var_info$var),3] = "hpi_diff"
var_info[grepl("spr10", var_info$var),3] = "spr10"
var_info[grepl("spr10_q", var_info$var),3] = "spr10"
var_info[grepl("spr10_a", var_info$var),3] = "spr10"
var_info[grepl("equipment", var_info$var), 3]= "equipment"
var_info[grepl("pfi_nonres", var_info$var), 3]= "pfi_nonres"
var_info[grepl("willreit", var_info$var), 3]= "willreit"
var_info[grepl("KOGDP", var_info$var), 3]= "KOGDP"
var_info[grepl("KOCPI", var_info$var), 3]= "KOCPI"
var_info[grepl("CCI", var_info$var),3] = "CCI_g"
var_info[grepl("NCREIF", var_info$var),3] = "NCREIF"

var_info[var_info$base==0,]

#sign
var_info[grepl("ngdp", var_info$var),6] = -1
var_info[grepl("rgdp", var_info$var),6] = -1
var_info[grepl("rdi", var_info$var),6] = -1
var_info[grepl("ndi", var_info$var),6] = -1
var_info[grepl("ur_", var_info$var),6] = 1
var_info[grepl("UR_", var_info$var),6] = 1
var_info[grepl("cpi_", var_info$var),6] = 0
var_info[grepl("i3m", var_info$var),6] = 0
var_info[grepl("i5y", var_info$var),6] = 0
var_info[grepl("i10y", var_info$var),6] = 0
var_info[grepl("bbb", var_info$var),6] = 1
var_info[grepl("imort", var_info$var),6] = 0
var_info[grepl("iprim", var_info$var),6] = 0
var_info[grepl("cppi", var_info$var),6] = -1
var_info[grepl("dji", var_info$var),6] = -1
var_info[grepl("VIX", var_info$var),6] = 0
var_info[grepl("vix", var_info$var),6] = 0
var_info[grepl("hpi_q", var_info$var),6] = -1
var_info[grepl("HPI_q", var_info$var),6] = -1
var_info[grepl("hpi_a", var_info$var),6] = -1
var_info[grepl("HPI_a", var_info$var),6] = -1
var_info[grepl("hpi_g", var_info$var),6] = -1
var_info[grepl("spr10", var_info$var),6] = 1
var_info[grepl("spr10_q", var_info$var),6] = 1
var_info[grepl("spr10_a", var_info$var),6] = 1
var_info[grepl("equipment", var_info$var), 6]= -1
var_info[grepl("pfi_nonres", var_info$var), 6]= -1
var_info[grepl("willreit", var_info$var), 6]= -1
var_info[grepl("KOGDP", var_info$var), 6]= -1
var_info[grepl("KOCPI", var_info$var), 6]= 0
var_info[grepl("CCI", var_info$var),6] = -1
var_info[grepl("NCREIF", var_info$var),6] = -1

var_info[var_info$sign==0,]

#Tier
var_info[grepl("ngdp", var_info$var),2] = 1
var_info[grepl("rgdp", var_info$var),2] = 1
var_info[grepl("rdi", var_info$var),2] = 1
var_info[grepl("ndi", var_info$var),2] = 1
var_info[grepl("ur_", var_info$var),2] = 1
var_info[grepl("UR_", var_info$var),2] = 1
var_info[grepl("cpi_", var_info$var),2] = 3
var_info[grepl("i3m", var_info$var),2] = 3
var_info[grepl("i5y", var_info$var),2] = 3
var_info[grepl("i10y", var_info$var),2] = 3
var_info[grepl("bbb", var_info$var),2] = 3
var_info[grepl("imort", var_info$var),2] = 3
var_info[grepl("iprim", var_info$var),2] = 3
var_info[grepl("cppi", var_info$var),2] = 2
var_info[grepl("dji", var_info$var),2] = 1
var_info[grepl("VIX", var_info$var),2] = 2
var_info[grepl("vix", var_info$var),2] = 2
var_info[grepl("hpi_q", var_info$var),2] = 2
var_info[grepl("HPI_q", var_info$var),2] = 2
var_info[grepl("hpi_a", var_info$var),2] = 2
var_info[grepl("HPI_a", var_info$var),2] = 2
var_info[grepl("hpi_g", var_info$var),2] = 2
var_info[grepl("spr10", var_info$var),2] = 3
var_info[grepl("spr10_q", var_info$var),2] = 3
var_info[grepl("spr10_a", var_info$var),2] = 3
var_info[grepl("equipment", var_info$var), 2]= 1
var_info[grepl("pfi_nonres", var_info$var), 2]= 1
var_info[grepl("willreit", var_info$var), 2]= 2
var_info[grepl("KOGDP", var_info$var), 2]= 3
var_info[grepl("KOCPI", var_info$var), 2]= 3
var_info[grepl("CCI", var_info$var),2] = 3
var_info[grepl("NCREIF", var_info$var),2] = 2

#####################
#Variable Selection 
#####################
df2=df[c(1:36),]
date_col= as.data.frame(df2$date)
names(date_col)="Date"
lgd_input=as.data.frame(cbind(df2$wa_lgd, df2$wa_lgd_qd, df2$wa_lgd_ad))
names(lgd_input)=c("wa_lgd", "wa_lgd_qd", "wa_lgd_ad")
b1=cbind(date_col, lgd_input)
names(b1)=c("Date", names(lgd_input))
b=data.table(b1)

c1=cbind(date_col, macro_dev)
names(c1)=c("Date", names(macro_dev))
c=data.table(c1)
a=data.table(var_info)

df_total_dev= as.data.frame(cbind(date_col, lgd_input, macro_dev))

source("StepFun.R")

# # 
# fix_vars0=c("1")
# model1_ci_lgd_sep=StepFun(a,b,c, tier=1,         #indicate which tier of variables to consider
#                           y='wa_lgd~',       #indicate response variable
#                           thresh=c(0.05, 0.01, 0.001),    #significance level for SE based p-value and LR test based p-value for each tier
#                           criteria='SE.p', #variable selection criteria; other values='bic', 'LR.p', 'SE.p', 'rsq'
#                           vars0 = c("1"),          #model 0 variables
#                           fix_vars0,    #indicate which variables are fixed
#                           out.print=T         #indicate wheter intermediate output will be printed 
# )
# 
# fix_vars0=c("1", "NJUR_qd", "real_gross_pdi_equipment_ag_lag_4")
# model1_ci_lgd_sep=StepFun(a,b,c, tier=2,         #indicate which tier of variables to consider
#                           y='wa_lgd~',       #indicate response variable
#                           thresh=c(0.05, 0.01, 0.001),    #significance level for SE based p-value and LR test based p-value for each tier
#                           criteria='SE.p', #variable selection criteria; other values='bic', 'LR.p', 'SE.p', 'rsq'
#                           vars0 = c("1", "NJUR_qd", "real_gross_pdi_equipment_ag_lag_4"),          #model 0 variables
#                           fix_vars0,    #indicate which variables are fixed
#                           out.print=T         #indicate wheter intermediate output will be printed 
# )
# 
# fix_vars0=c("1", "NJUR_qd", "real_gross_pdi_equipment_ag_lag_4")
# model1_ci_lgd_sep=StepFun(a,b,c, tier=3,         #indicate which tier of variables to consider
#                           y='wa_lgd~',       #indicate response variable
#                           thresh=c(0.05, 0.01, 0.001),    #significance level for SE based p-value and LR test based p-value for each tier
#                           criteria='SE.p', #variable selection criteria; other values='bic', 'LR.p', 'SE.p', 'rsq'
#                           vars0 = c("1", "NJUR_qd", "real_gross_pdi_equipment_ag_lag_4"),          #model 0 variables
#                           fix_vars0,    #indicate which variables are fixed
#                           out.print=T         #indicate wheter intermediate output will be printed 
# )
# 
# out=lm(b1$wa_lgd~c1$NJUR_qd+c1$real_gross_pdi_equipment_ag_lag_4)
# summary(out)
# acf(out$residuals)
# Box.test(out$residuals, type = "Ljung-Box", lag = 3)
# vif(out)
# 
# x1=c1$NJUR_qd
# sd1= stdev(c1$NJUR_qd)
# x2=c1$real_gross_pdi_equipment_ag_lag_4
# sd2=stdev(c1$real_gross_pdi_equipment_ag_lag_4)
# 
# out=lm(b1$wa_lgd~x1+x2)
# summary(out)
# acf(out$residuals)
# durbinWatsonTest(out)
# Box.test(out$residuals, type = "Ljung-Box", lag = 3) #null: independence ==> accept

# 
# #
# 
# # #implement the model
# #
# ndata=nrow(b1)
# npred=9
# output=as.data.frame(matrix(0, ndata+npred,5))
# 
# D1=which(df$year==2007 & df$q==1)
# D2=which(df$year==2015 & df$q==4)
# 
# output[1:ndata, 1]=df$wa_lgd[D1:D2]
# output[1:ndata, 2]=out$fitted.values
# 
# plot(output[2:ndata, 2])
# lines(output[2:ndata, 1], col='red')
# 
# #Predict
# aaaa=which(base$year==2016 & base$quarter==1)
# bbbb=which(base$year==2018 & base$quarter==1)
# 
# indx=cbind(base$NJUR_qd, base$real_gross_pdi_equipment_ag_lag_4)
# xreg_base=as.data.frame(indx[aaaa:bbbb,])
# names(xreg_base)=c("x1", "x2")
# fitted.base=as.data.frame(predict(out, xreg_base))
# 
# indx=cbind(adverse$NJUR_qd, adverse$real_gross_pdi_equipment_ag_lag_4)
# xreg_adverse=as.data.frame(indx[aaaa:bbbb,])
# names(xreg_adverse)=c("x1", "x2")
# fitted.adverse=as.data.frame(predict(out, xreg_adverse))
# 
# indx=cbind(severe$NJUR_qd, severe$real_gross_pdi_equipment_ag_lag_4)
# xreg_severe=as.data.frame(indx[aaaa:bbbb,])
# names(xreg_severe)=c("x1", "x2")
# fitted.severe=as.data.frame(predict(out, xreg_severe))
# 
# abb=ndata+1
# abc=nrow(output)
# output[abb:abc, 3]=fitted.base
# output[abb:abc, 4]=fitted.adverse
# output[abb:abc, 5]=fitted.severe
# 
# output[1:ndata,c(3,4,5)]=output[1:ndata, 2]
# 
# 
# output[abb:abc,1]=NA
# 
# date1 = seq(ISOdate(2007,1,1), by = "quarter", length.out = 45)
# plot(date1,output[,5], type='l', ylab="C&I LGD", col='red', ylim=c(0,1))
# lines(date1,output[,4], col='magenta')
# lines(date1,output[,3], col='black')
# lines(date1,output[,1], col='green')
# legend("topleft", legend= c("base", "adverse", "severe", "Historical"), fill=c("black", "blue", "red", "green"))

# colnames(output)=c("Historical", "estimated", "estimated_base", "estimated_adverse", "estimated_severe")
# output_wa_lgd_ci=output
# write.csv(as.data.frame(cbind(date1,output)), "wa_lgd_ci-v2.csv", col.names = T, row.names = F)
#
# # sensitivity
# #NJUR
#
# indx=cbind(base$NJUR_qd, base$real_gross_pdi_equipment_ag_lag_4)
# xreg_base=as.data.frame(indx[aaaa:bbbb,])
# names(xreg_base)=c("x1", "x2")
# fitted.base=as.data.frame(predict(out, xreg_base))
#
# indx_1sd=cbind(base$NJUR_qd+sd1, base$real_gross_pdi_equipment_ag_lag_4)
# xreg_base_1sd=as.data.frame(indx_1sd[aaaa:bbbb,])
# names(xreg_base_1sd)=c("x1", "x2")
# fitted.base_1sd=as.data.frame(predict(out, xreg_base_1sd))
#
# indx_2sd=cbind(base$NJUR_qd+2*sd1, base$real_gross_pdi_equipment_ag_lag_4)
# xreg_base_2sd=as.data.frame(indx_2sd[aaaa:bbbb,])
# names(xreg_base_2sd)=c("x1", "x2")
# fitted.base_2sd=as.data.frame(predict(out, xreg_base_2sd))
#
# abb=ndata+1
# abc=nrow(output)
# output[abb:abc, 3]=fitted.base
# output[abb:abc, 4]=fitted.base_1sd
# output[abb:abc, 5]=fitted.base_2sd
#
# output[1:ndata,c(3,4,5)]=output[1:ndata, 2]
#
#
# output[abb:abc,1]=NA
#
# date1 = seq(ISOdate(2007,1,1), by = "quarter", length.out = 45)
# plot(date1,output[,5], type='l', ylab="C&I LGD", col='red', ylim=c(0,1))
# lines(date1,output[,4], col='magenta')
# lines(date1,output[,3], col='black')
# lines(date1,output[,1], col='green')
# legend("topleft", legend= c("base", "adverse", "severe", "Historical"), fill=c("black", "blue", "red", "green"))
#
# colnames(output)=c("Historical", "estimated", "estimated_base", "estimated_adverse", "estimated_severe")
# output_wa_lgd_ci_sensitivity_NJUR=output
# write.csv(as.data.frame(cbind(date1,output)), "wa_lgd_ci_sensitivity_NJUR.csv", col.names = T, row.names = F)
#
# #gross pdi
# indx=cbind(base$NJUR_qd, base$real_gross_pdi_equipment_ag_lag_4)
# xreg_base=as.data.frame(indx[aaaa:bbbb,])
# names(xreg_base)=c("x1", "x2")
# fitted.base=as.data.frame(predict(out, xreg_base))
#
# indx_1sd=cbind(base$NJUR_qd, base$real_gross_pdi_equipment_ag_lag_4+sd2)
# xreg_base_1sd=as.data.frame(indx_1sd[aaaa:bbbb,])
# names(xreg_base_1sd)=c("x1", "x2")
# fitted.base_1sd=as.data.frame(predict(out, xreg_base_1sd))
#
# indx_2sd=cbind(base$NJUR_qd, base$real_gross_pdi_equipment_ag_lag_4+2*sd2)
# xreg_base_2sd=as.data.frame(indx_2sd[aaaa:bbbb,])
# names(xreg_base_2sd)=c("x1", "x2")
# fitted.base_2sd=as.data.frame(predict(out, xreg_base_2sd))
#
# abb=ndata+1
# abc=nrow(output)
# output[abb:abc, 3]=fitted.base
# output[abb:abc, 4]=fitted.base_1sd
# output[abb:abc, 5]=fitted.base_2sd
#
# output[1:ndata,c(3,4,5)]=output[1:ndata, 2]
#
#
# output[abb:abc,1]=NA
#
# date1 = seq(ISOdate(2007,1,1), by = "quarter", length.out = 45)
# plot(date1,output[,5], type='l', ylab="C&I LGD", col='red', ylim=c(0,1))
# lines(date1,output[,4], col='magenta')
# lines(date1,output[,3], col='black')
# lines(date1,output[,1], col='green')
# legend("topleft", legend= c("base", "adverse", "severe", "Historical"), fill=c("black", "blue", "red", "green"))
#
# colnames(output)=c("Historical", "estimated", "estimated_base", "estimated_adverse", "estimated_severe")
# output_wa_lgd_ci_sensitivity_PDI=output
# write.csv(as.data.frame(cbind(date1,output)), "wa_lgd_ci_sensitivity_PDI.csv", col.names = T, row.names = F)
#
# 

####################################
#final Model
####################################
fix_vars0=c("1")
model1_ci_lgd_sep=StepFun(a,b,c, tier=1,         #indicate which tier of variables to consider
                           y='wa_lgd~',       #indicate response variable
                           thresh=c(0.05, 0.01, 0.001),    #significance level for SE based p-value and LR test based p-value for each tier
                           criteria='SE.p', #variable selection criteria; other values='bic', 'LR.p', 'SE.p', 'rsq'
                           vars0 = c("1"),          #model 0 variables
                           fix_vars0,    #indicate which variables are fixed
                           out.print=T         #indicate wheter intermediate output will be printed 
)
# Select CAUR_yd_lag_3

fix_vars0=c("1")
model1_ci_lgd_sep=StepFun(a,b,c, tier=1,         #indicate which tier of variables to consider
                          y='wa_lgd~',       #indicate response variable
                          thresh=c(0.05, 0.01, 0.001),    #significance level for SE based p-value and LR test based p-value for each tier
                          criteria='rsq', #variable selection criteria; other values='bic', 'LR.p', 'SE.p', 'rsq'
                          vars0 = c("1","CAUR_yd_lag_3"),          #model 0 variables
                          fix_vars0,    #indicate which variables are fixed
                          out.print=T         #indicate wheter intermediate output will be printed 
)
# Select rgdp_qg

fix_vars0=c("1", "CAUR_yd_lag_3", "rgdp_qg")
model1_ci_lgd_sep=StepFun(a,b,c, tier=2,         #indicate which tier of variables to consider
                          y='wa_lgd~',       #indicate response variable
                          thresh=c(0.05, 0.01, 0.001),    #significance level for SE based p-value and LR test based p-value for each tier
                          criteria='rsq', #variable selection criteria; other values='bic', 'LR.p', 'SE.p', 'rsq'
                          vars0 = c("1","CAUR_yd_lag_3", "rgdp_qg"),          #model 0 variables
                          fix_vars0,    #indicate which variables are fixed
                          out.print=T         #indicate wheter intermediate output will be printed 
)
# no added variable
fix_vars0=c("1", "CAUR_yd_lag_3", "rgdp_qg")
model1_ci_lgd_sep=StepFun(a,b,c, tier=3,         #indicate which tier of variables to consider
                          y='wa_lgd~',       #indicate response variable
                          thresh=c(0.05, 0.01, 0.001),    #significance level for SE based p-value and LR test based p-value for each tier
                          criteria='rsq', #variable selection criteria; other values='bic', 'LR.p', 'SE.p', 'rsq'
                          vars0 = c("1","CAUR_yd_lag_3", "rgdp_qg"),          #model 0 variables
                          fix_vars0,    #indicate which variables are fixed
                          out.print=T         #indicate wheter intermediate output will be printed 
)
# no added variable


x1=c1$CAUR_yd_lag_3
sd1=stdev(c1$CAUR_yd_lag_3)
x2=c1$rgdp_qg
sd2= stdev(c1$rgdp_qg)

out=lm(b1$wa_lgd~x1+x2)
summary(out)

#Multicolinearity
vif(out)


#####################
# Residual tests 
#####################
out_res=out$residuals
fitted=out$fitted.values

# Autocorrelations 
par(mfrow=c(1,2))
acf(out_res, main="")
pacf(out_res, main="")

#white noise tests
Box.test(out_res, type = "Ljung-Box") #null: independence ==> accept
durbinWatsonTest(model = out)

#Q-Q Plot
par(mfrow=c(1,1))
qqnorm(out_res, ylab="Residuals", xlab="Quantiles of Standard Normal", main="CI LGD Model") 
qqline(out_res)

# Residual vs predicted
plot(fitted,out_res, ylab="Residuals", xlab="predicted values", main="CI LGD Model", ylim=c(-0.5, 0.5)) 
abline(0, 0)



# plot(b1$wa_lgd)
# lines(out$fitted.values)


##############
#stationarity
#############
summary(ur.df(na.remove(base$CAUR_yd_lag_3)))
summary(ur.df(na.remove(base$rgdp_qg)))


#####################
#implement the model
#####################
ndata=nrow(b1)
npred=9
output=as.data.frame(matrix(0, ndata+npred,5))

D1=which(df$year==2007 & df$q==1)
D2=which(df$year==2015 & df$q==4)

output[1:ndata, 1]=df$wa_lgd[D1:D2]
output[1:ndata, 2]=out$fitted.values

plot(output[2:ndata, 2])
lines(output[2:ndata, 1], col='red')

#################################
#Predict
#################################
aaaa=which(base$year==2016 & base$quarter==1)
bbbb=which(base$year==2018 & base$quarter==1)

indx=cbind(base$CAUR_yd_lag_3, base$rgdp_qg)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2")
fitted.base=as.data.frame(predict(out, xreg_base))

indx=cbind( adverse$CAUR_yd_lag_3,  adverse$rgdp_qg)
xreg_adverse=as.data.frame(indx[aaaa:bbbb,])
names(xreg_adverse)=c("x1", "x2")
fitted.adverse=as.data.frame(predict(out, xreg_adverse))

indx=cbind( severe$CAUR_yd_lag_3,  severe$rgdp_qg)
xreg_severe=as.data.frame(indx[aaaa:bbbb,])
names(xreg_severe)=c("x1", "x2")
fitted.severe=as.data.frame(predict(out, xreg_severe))

abb=ndata+1
abc=nrow(output)
output[abb:abc, 3]=fitted.base
output[abb:abc, 4]=fitted.adverse
output[abb:abc, 5]=fitted.severe

output[1:ndata,c(3,4,5)]=output[1:ndata, 2]


output[abb:abc,1]=NA

date1 = seq(ISOdate(2007,1,1), by = "quarter", length.out = 45)
plot(date1,output[,5], type='l', ylab="C&I LGD", col='red', ylim=c(0,1))
lines(date1,output[,4], col='magenta')
lines(date1,output[,3], col='black')
lines(date1,output[,1], col='green')
legend("topleft", legend= c("base", "adverse", "severe", "Historical"), fill=c("black", "blue", "red", "green"))

colnames(output)=c("Historical", "estimated", "estimated_base", "estimated_adverse", "estimated_severe")
output_wa_lgd_ci=output
fcompare=output$estimated[1:36]
write.csv(as.data.frame(cbind(date1,output)), "ci_lgd_output.csv", col.names = T, row.names = F)

###############################################
#sensitivity
###############################################

#####
#CAur
#####
indx=cbind(base$CAUR_yd_lag_3, base$rgdp_qg)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2")
fitted.base=as.data.frame(predict(out, xreg_base))

indx_1sd=cbind(base$CAUR_yd_lag_3+sd1, base$rgdp_qg)
xreg_base_1sd=as.data.frame(indx_1sd[aaaa:bbbb,])
names(xreg_base_1sd)=c("x1", "x2")
fitted.base_1sd=as.data.frame(predict(out, xreg_base_1sd))

indx_2sd=cbind(base$CAUR_yd_lag_3+2*sd1, base$rgdp_qg)
xreg_base_2sd=as.data.frame(indx_2sd[aaaa:bbbb,])
names(xreg_base_2sd)=c("x1", "x2")
fitted.base_2sd=as.data.frame(predict(out, xreg_base_2sd))

abb=ndata+1
abc=nrow(output)
output[abb:abc, 3]=fitted.base
output[abb:abc, 4]=fitted.base_1sd
output[abb:abc, 5]=fitted.base_2sd


output[1:ndata,c(3,4,5)]=output[1:ndata, 2]


output[abb:abc,1]=NA

date1 = seq(ISOdate(2007,1,1), by = "quarter", length.out = 45)
plot(date1,output[,5], type='l', ylab="C&I LGD", col='red', ylim=c(0,1))
lines(date1,output[,4], col='magenta')
lines(date1,output[,3], col='black')
lines(date1,output[,1], col='green')
legend("topleft", legend= c("base", "adverse", "severe", "Historical"), fill=c("black", "blue", "red", "green"))

colnames(output)=c("Historical", "estimated", "estimated_base", "estimated_adverse", "estimated_severe")
output_wa_lgd_ci_sensitivity_caur=output
write.csv(as.data.frame(cbind(date1,output)), "lgd_ci_sensitivity_caur-v3.csv", col.names = T, row.names = F)

#####
#rgdp
#####
indx=cbind(base$CAUR_yd_lag_3, base$rgdp_qg)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2")
fitted.base=as.data.frame(predict(out, xreg_base))

indx_1sd=cbind(base$CAUR_yd_lag_3, base$rgdp_qg+sd2)
xreg_base_1sd=as.data.frame(indx_1sd[aaaa:bbbb,])
names(xreg_base_1sd)=c("x1", "x2")
fitted.base_1sd=as.data.frame(predict(out, xreg_base_1sd))

indx_2sd=cbind(base$CAUR_yd_lag_3, base$rgdp_qg+2*sd2)
xreg_base_2sd=as.data.frame(indx_2sd[aaaa:bbbb,])
names(xreg_base_2sd)=c("x1", "x2")
fitted.base_2sd=as.data.frame(predict(out, xreg_base_2sd))

abb=ndata+1
abc=nrow(output)
output[abb:abc, 3]=fitted.base
output[abb:abc, 4]=fitted.base_1sd
output[abb:abc, 5]=fitted.base_2sd


output[1:ndata,c(3,4,5)]=output[1:ndata, 2]


output[abb:abc,1]=NA

date1 = seq(ISOdate(2007,1,1), by = "quarter", length.out = 45)
plot(date1,output[,5], type='l', ylab="C&I LGD", col='red', ylim=c(0,1))
lines(date1,output[,4], col='magenta')
lines(date1,output[,3], col='black')
lines(date1,output[,1], col='green')
legend("topleft", legend= c("base", "adverse", "severe", "Historical"), fill=c("black", "blue", "red", "green"))

colnames(output)=c("Historical", "estimated", "estimated_base", "estimated_adverse", "estimated_severe")
output_wa_lgd_ci_sensitivity_rgdp=output
write.csv(as.data.frame(cbind(date1,output)), "wa_lgd_ci_sensitivity_rgdp-v3.csv", col.names = T, row.names = F)




#####################################
# OOS testing
####################################
n=1
oos<-function(n){
  #n defines how many quarters before 2015Q4  
  ind=nrow(df_total_dev)-n
  df_oos=df_total_dev[1:ind,]
  ind0=nrow(df_oos)
  ind1=nrow(df_oos)+1
  ind2=nrow(df_oos)+npred
  ind4=nrow(df_total_dev)
  ind5=nrow(df_total_dev)-n+1
  
  
  x1=df_oos$CAUR_yd_lag_3
  x2=df_oos$rgdp_qg

  out_oos=lm(df_oos$wa_lgd~x1+x2)
  summary(out_oos)
  
  ####################################
  #implement the model - out of sample
  ####################################
  
  ndata=nrow(df_oos)
  npred=1
  ind3=ndata+npred
  output_oos=as.data.frame(matrix(0, ind3,7))
  input=fcompare[1:ind3]
  output_oos[, 1]=input
  output_oos[1:ndata, 2]=out_oos$fitted.values
  
  #PREDICT
  indx=cbind(df_total_dev$CAUR_yd_lag_3, df_total_dev$rgdp_qg)
  xreg_base=as.data.frame(t(indx[ind5,]))
  names(xreg_base)=c("x1", "x2")
  fitted.base=as.data.frame(predict(out_oos, xreg_base))
  
  
  # get the values
  output_oos[ind5, 2]=fitted.base
  in_out_diff= output_oos[ind5,2]-output_oos[ind5,1]
  result_oos=as.data.frame(cbind(n, output_oos[ind1,1], output_oos[ind1,2],in_out_diff))
  return(result_oos)
}
oos(1)
oos(2)
oos(3)
oos(4)

#####################################
# Prediction CI
#####################################

ndata=nrow(b1)
npred=11
output_ci=as.data.frame(matrix(0, ndata+npred,10))

D1=which(df$year==2007 & df$q==1)
D2=which(df$year==2015 & df$q==4)

output_ci[1:ndata, 1]=df$wa_lgd[D1:D2]
output_ci[1:ndata, 2]=out$fitted.values

#Predict
aaaa=which(base$year==2016 & base$quarter==1)
bbbb=which(base$year==2018 & base$quarter==3)

x1=c1$CAUR_yd_lag_3
sd1=stdev(c1$CAUR_yd_lag_3)
x2=c1$rgdp_qg
sd2= stdev(c1$rgdp_qg)

indx=cbind(base$CAUR_yd_lag_3, base$rgdp_qg)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2")
fitted.base=as.data.frame(predict(out, xreg_base, interval = "predict", level = 0.95))

indx=cbind( adverse$CAUR_yd_lag_3,  adverse$rgdp_qg)
xreg_adverse=as.data.frame(indx[aaaa:bbbb,])
names(xreg_adverse)=c("x1", "x2")
fitted.adverse=as.data.frame(predict(out, xreg_adverse, interval = "predict", level = 0.95))

indx=cbind( severe$CAUR_yd_lag_3,  severe$rgdp_qg)
xreg_severe=as.data.frame(indx[aaaa:bbbb,])
names(xreg_severe)=c("x1", "x2")
fitted.severe=as.data.frame(predict(out, xreg_severe, interval = "predict", level = 0.95))

abb=ndata+1
abc=nrow(output_ci)
output_ci[abb:abc, 2]=fitted.base[,1]
output_ci[abb:abc, 3]=fitted.base[,2]
output_ci[abb:abc, 4]=fitted.base[,3]
output_ci[abb:abc, 5]=fitted.adverse[,1]
output_ci[abb:abc, 6]=fitted.adverse[,2]
output_ci[abb:abc, 7]=fitted.adverse[,3]
output_ci[abb:abc, 8]=fitted.severe[,1]
output_ci[abb:abc, 9]=fitted.severe[,2]
output_ci[abb:abc, 10]=fitted.severe[,3]

colnames(output_ci)=c("Historical", "estimated_base_fit", "estimated_base_lwr", 
                      "estimated_base_upr", "estimated_adverse_fit", 
                      "estimated_adverse_lwr", "estimated_adverse_upr", 
                      "estimated_severe_fit", "estimated_severe_lwr",
                      "estimated_severe_upr")

date1 = seq(ISOdate(2007,1,1), by = "quarter", length.out = 47)
write.csv(as.data.frame(cbind(date1,output_ci)), "C&I LGD prediction ci.csv", col.names = T, row.names = F)

