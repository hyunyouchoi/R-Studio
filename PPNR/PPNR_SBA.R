###################################################################
# Project: Bank of Hope
# PPNR - Gain on Sale of Loans (SBA)
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
# save.image("sba-v1.RData")

##load image
#load("sba-v1.RData")
##################################


#####################
#read in the raw data
#####################
df=read.csv("sba.csv",header = TRUE)
names(df)= c("date", "year", "q", "sba")

###############
# Plot the data
###############
df = df[-1,] #2003 Q1 starting point
plot(df$sba)
acf(df$sba)
pacf(df$sba)

###################################
#transformations-- Q-o-Q and Y-o-Y
##################################
df$sba_qd=c(NA, diff(df$sba))
df$sba_ad= df$sba- back(df$sba, noperiods = 4)
df$sba_qg=c(NA, diff(log(df$sba)))
df$sba_ag=log(df$sba)-back(log(df$sba), noperiods = 4)


###########################################
#Stationarity tests for the input variables
###########################################
pp.test(na.remove(df$sba), lshort = F) # Stationary!
pp.test(na.remove(df$sba_qd), lshort = F)  # Stationary!
pp.test(na.remove(df$sba_ad), lshort = F) # Stationary!

#boxplot & auto correlations
boxplot(df$sba_qd~df$q, main="QoQ SBA", 
        xlab="season/quarter", ylab="SBA")
acf(na.remove(df$sba_qd), lag.max = 25)

boxplot(df$sba_ad~df$q, main="YoY SBA", 
        xlab="season/quarter", ylab="SBA")
acf(na.remove(df$sba_ad), lag.max = 25)

#######################
#read in the macro vars 
#######################

######
#base
######
base=read.csv("base_sba2.csv", header=T)
aaa=which(base$year==2003 & base$quarter==1)
bbb=which(base$year==2018 & base$quarter==4)
base=base[aaa:bbb,]

#########
#adverse
#########

adverse=read.csv("adverse_sba2.csv", header=T)
aaa=which(adverse$year==2003 & adverse$quarter==1)
bbb=which(adverse$year==2018 & adverse$quarter==4)
adverse=adverse[aaa:bbb,]

########
#severe
########
severe=read.csv("severe_sba2.csv", header=T)
aaa=which(severe$year==2003 & severe$quarter==1)
bbb=which(severe$year==2018 & severe$quarter==4)
severe=severe[aaa:bbb,]

##################
#development macro
##################
D1=which(base$year==2003 & base$q==1)
D2=which(base$year==2016 & base$q==3)
macro_dev=base[c(D1:D2), ]

########################################
# Create the dep_var matrix
########################################

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
var_info[grepl("fall", var_info$var),3] = "fall"
var_info[grepl("winter", var_info$var),3] = "winter"
var_info[grepl("spring", var_info$var),3] = "spring"
var_info[grepl("summer", var_info$var),3] = "summer"
#var_info[var_info$base==0,]

#sign
var_info[grepl("ngdp", var_info$var),6] = 1
var_info[grepl("rgdp", var_info$var),6] = 1
var_info[grepl("rdi", var_info$var),6] = 1
var_info[grepl("ndi", var_info$var),6] = 1
var_info[grepl("ur_", var_info$var),6] = -1
var_info[grepl("UR_", var_info$var),6] = -1
var_info[grepl("cpi_", var_info$var),6] = 0
var_info[grepl("i3m", var_info$var),6] = 0
var_info[grepl("i5y", var_info$var),6] = 0
var_info[grepl("i10y", var_info$var),6] = 0
var_info[grepl("bbb", var_info$var),6] = -1
var_info[grepl("imort", var_info$var),6] = 0
var_info[grepl("iprim", var_info$var),6] = 0
var_info[grepl("cppi", var_info$var),6] = 1
var_info[grepl("dji", var_info$var),6] = 1
var_info[grepl("VIX", var_info$var),6] = 0
var_info[grepl("vix", var_info$var),6] = 0
var_info[grepl("hpi_q", var_info$var),6] = 1
var_info[grepl("HPI_q", var_info$var),6] = 1
var_info[grepl("hpi_a", var_info$var),6] = 1
var_info[grepl("HPI_a", var_info$var),6] = 1
var_info[grepl("hpi_g", var_info$var),6] = 1
var_info[grepl("spr10", var_info$var),6] = -1
var_info[grepl("spr10_q", var_info$var),6] = -1
var_info[grepl("spr10_a", var_info$var),6] = -1
var_info[grepl("equipment", var_info$var), 6]= 1
var_info[grepl("pfi_nonres", var_info$var), 6]= 1
var_info[grepl("willreit", var_info$var), 6]= 1
var_info[grepl("KOGDP", var_info$var), 6]= 1
var_info[grepl("KOCPI", var_info$var), 6]= 0
var_info[grepl("CCI", var_info$var),6] = 1
var_info[grepl("NCREIF", var_info$var),6] = 1
var_info[grepl("fall", var_info$var),6] = 0
var_info[grepl("winter", var_info$var),6] = 0
var_info[grepl("spring", var_info$var),6] = 0
var_info[grepl("summer", var_info$var),6] = 0

#var_info[var_info$sign==0,]

#Tier
var_info[grepl("fall", var_info$var),2] = 1
var_info[grepl("spring", var_info$var),2] = 1
var_info[grepl("summer", var_info$var),2] = 1
var_info[grepl("winter", var_info$var),2] = 1
var_info[grepl("ngdp", var_info$var),2] = 1
var_info[grepl("rgdp", var_info$var),2] = 1
var_info[grepl("rdi", var_info$var),2] = 1
var_info[grepl("ndi", var_info$var),2] = 1
var_info[grepl("ur_", var_info$var),2] = 1
var_info[grepl("UR_", var_info$var),2] = 1
var_info[grepl("cpi_", var_info$var),2] = 3
var_info[grepl("i3m", var_info$var),2] = 1
var_info[grepl("i5y", var_info$var),2] = 1
var_info[grepl("i10y", var_info$var),2] = 1
var_info[grepl("bbb", var_info$var),2] = 1
var_info[grepl("imort", var_info$var),2] = 3
var_info[grepl("iprim", var_info$var),2] = 2
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
var_info[grepl("equipment", var_info$var), 2]= 2
var_info[grepl("pfi_nonres", var_info$var), 2]= 2
var_info[grepl("willreit", var_info$var), 2]= 2
var_info[grepl("KOGDP", var_info$var), 2]= 3
var_info[grepl("KOCPI", var_info$var), 2]= 3
var_info[grepl("CCI", var_info$var),2] = 4
var_info[grepl("NCREIF", var_info$var),2] = 2

# var_info[var_info$tier==0,]

#####################
#Variable Selection 
#####################

D1=which(df$year==2003 & df$q==1)
D2=which(df$year==2016 & df$q==3)
df_dev=df[c(D1:D2), ]

date_col= as.data.frame(df$date)
names(date_col)="Date"
sba_input=as.data.frame(cbind(df$sba, df$sba_qd, df$sba_ad, df$sba_qg, df$sba_ag))
names(sba_input)=c("sba", "sba_qd", "sba_ad", "sba_qg", "sba_ag")
b1=cbind(date_col, sba_input)
names(b1)=c("Date", names(sba_input))
b=data.table(b1)

c1=cbind(date_col, macro_dev)
names(c1)=c("Date", names(macro_dev))
c=data.table(c1)
a=data.table(var_info)

df_total_dev= as.data.frame(cbind(date_col, sba_input, macro_dev))

source("StepFun.R")
########################################################
# Model Selection
########################################################

# fix_vars0=c("1")
# model3_sba_sep=StepFun(a,b,c, tier=1,         #indicate which tier of variables to consider
#                        y='sba~',       #indicate response variable
#                        thresh=c(0.05, 0.01, 0.001),    #significance level for SE based p-value and LR test based p-value for each tier
#                        criteria='SE.p', #variable selection criteria; other values='bic', 'LR.p', 'SE.p', 'rsq'
#                        vars0 = c("1"),          #model 0 variables
#                        fix_vars0,    #indicate which variables are fixed
#                        out.print=T         #indicate wheter intermediate output will be printed
# )
# # Add CAUR_qd_lag_1, djia_ag_lag_4, i10y_qd_lag_1
# 
# fix_vars0=c("1", "CAUR_qd_lag_1", "i10y_qd_lag_1", "djia_ag_lag_4")
# model3_sba_sep=StepFun(a,b,c, tier=2,         #indicate which tier of variables to consider
#                        y='sba~',       #indicate response variable
#                        thresh=c(0.05, 0.01, 0.001),    #significance level for SE based p-value and LR test based p-value for each tier
#                        criteria='SE.p', #variable selection criteria; other values='bic', 'LR.p', 'SE.p', 'rsq'
#                        vars0 = c("1", "CAUR_qd_lag_1", "i10y_qd_lag_1", "djia_ag_lag_4"),          #model 0 variables
#                        fix_vars0,    #indicate which variables are fixed
#                        out.print=T         #indicate wheter intermediate output will be printed
# )
# # no added variable 
# 
# fix_vars0=c("1", "CAUR_qd_lag_1", "i10y_qd_lag_1", "djia_ag_lag_4")
# model3_sba_sep=StepFun(a,b,c, tier=3,         #indicate which tier of variables to consider
#                        y='sba~',       #indicate response variable
#                        thresh=c(0.05, 0.01, 0.001),    #significance level for SE based p-value and LR test based p-value for each tier
#                        criteria='SE.p', #variable selection criteria; other values='bic', 'LR.p', 'SE.p', 'rsq'
#                        vars0 = c("1", "CAUR_qd_lag_1", "i10y_qd_lag_1", "djia_ag_lag_4"),          #model 0 variables
#                        fix_vars0,    #indicate which variables are fixed
#                        out.print=T         #indicate wheter intermediate output will be printed
# )
# 
# # no added variable


out=lm(b1$sba~c1$CAUR_qd_lag_1+c1$i10y_qd_lag_1+c1$djia_ag_lag_4)

summary(out)
vif(out)
acf(out$residuals)
Box.test(out$residuals, type = "Ljung-Box", lag = 3)
durbinWatsonTest(out)

#####################
#Independent Variables 
#####################
x1=c1$CAUR_qd_lag_1
mu1=mean(c1$CAUR_qd_lag_1)
sd1= stdev(c1$CAUR_qd_lag_1)

x2= c1$i10y_qd_lag_1
mu2=mean(c1$i10y_qd_lag_1)
sd2= stdev(c1$i10y_qd_lag_1)

x3= c1$djia_ag_lag_4
mu3=mean(c1$djia_ag_lag_4)
sd3= stdev(c1$djia_ag_lag_4)

#####################
# Model Estimation
#####################
out=lm(b1$sba~x1+x2+x3)
summary(out)
lmSumm(out, HAC = T)

#Multicolinearity
vif(out)

#Stationarity
summary(ur.df(na.remove(base$CAUR_qd_lag_1), selectlags = c("BIC")))
summary(ur.df(na.remove(base$i10y_qd_lag_1), selectlags = c("BIC")))
summary(ur.df(na.remove(base$djia_ag_lag_4), selectlags = c("BIC")))

#####################
# Residual tests 
#####################
out_res=out$residuals
out_res2=rstandard(out)
# Autocorrelations 
par(mfrow=c(1,2))
acf(out$residuals, main="")
pacf(out$residuals, main="")

#white noise tests
Box.test(out$residuals, type = "Ljung-Box", lag = 3) #null: independence ==> accept
durbinWatsonTest(out)

#Q-Q Plot
par(mfrow=c(1,1))
qqnorm(out_res2, ylab="Residuals", xlab="Quantiles of Standard Normal", main="SBA Model") 
qqline(out_res2)

# Residual vs predicted
plot(out$fitted.values,out_res2, ylab="Residuals", xlab="Fitted Values", main="SBA Model") 
abline(0, 0)


#####################
#implement the model
#####################

ndata=nrow(b1)
npred=9
output=as.data.frame(matrix(0, ndata+npred,7))

D1=which(df$year==2003 & df$q==1)
D2=which(df$year==2016 & df$q==3)

output[1:ndata, 1]=df$sba[D1:D2]
output[1:ndata, 2]=out$fitted.values

plot(output[1:ndata, 2], ylim=c(-1200000,7000000))
lines(output[1:ndata, 1], col='red')

#####################
#Scenario Forecasts
#####################
aaaa=which(base$year==2016 & base$quarter==1)
bbbb=which(base$year==2018 & base$quarter==1)

indx=cbind(base$CAUR_qd_lag_1,base$i10y_qd_lag_1,base$djia_ag_lag_4)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2", "x3")
fitted.base=as.data.frame(predict(out, xreg_base))

indx=cbind(adverse$CAUR_qd_lag_1,adverse$i10y_qd_lag_1,adverse$djia_ag_lag_4)
xreg_adverse=as.data.frame(indx[aaaa:bbbb,])
names(xreg_adverse)=c("x1", "x2", "x3")
fitted.adverse=as.data.frame(predict(out, xreg_adverse))

indx=cbind(severe$CAUR_qd_lag_1,severe$i10y_qd_lag_1,severe$djia_ag_lag_4)
xreg_severe=as.data.frame(indx[aaaa:bbbb,])
names(xreg_severe)=c("x1", "x2", "x3")
fitted.severe=as.data.frame(predict(out, xreg_severe))

abb=ndata+1
abc=nrow(output)
output[abb:abc, 2]=fitted.base
output[abb:abc, 3]=fitted.adverse
output[abb:abc, 4]=fitted.severe

plot(output[1:ndata, 4], ylim=c(-1200000,7000000))
lines(output[1:abc, 3], col='magenta')
lines(output[1:abc, 2], col='green')
lines(output[1:abc, 1], col='black')

write.csv(as.data.frame(cbind(output)), "outpput_sba_final_model.csv", col.names = T, row.names = F)

#####################
#sensitivity Analysis 
#####################
# CAUR_qd_lag_1
indx=cbind(base$CAUR_qd_lag_1,base$i10y_qd_lag_1,base$djia_ag_lag_4)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2", "x3")
fitted.base=as.data.frame(predict(out, xreg_base))

indx=cbind(base$CAUR_qd_lag_1+sd1,base$i10y_qd_lag_1,base$djia_ag_lag_4)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2", "x3")
fitted.base1=as.data.frame(predict(out, xreg_base))

indx=cbind(base$CAUR_qd_lag_1+2*sd1,base$i10y_qd_lag_1,base$djia_ag_lag_4)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2", "x3")
fitted.base2=as.data.frame(predict(out, xreg_base))

abb=ndata+1
abc=nrow(output)
output[abb:abc, 2]=fitted.base
output[abb:abc, 3]=fitted.base1
output[abb:abc, 4]=fitted.base2
write.csv(as.data.frame(cbind(output)), "outpput_sba_final_sensitivity_caur.csv", col.names = T, row.names = F)

#i10y_qd_lag_1
indx=cbind(base$CAUR_qd_lag_1,base$i10y_qd_lag_1,base$djia_ag_lag_4)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2", "x3")
fitted.base=as.data.frame(predict(out, xreg_base))

indx=cbind(base$CAUR_qd_lag_1,base$i10y_qd_lag_1+sd2,base$djia_ag_lag_4)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2", "x3")
fitted.base1=as.data.frame(predict(out, xreg_base))

indx=cbind(base$CAUR_qd_lag_1,base$i10y_qd_lag_1+2*sd2,base$djia_ag_lag_4)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2", "x3")
fitted.base2=as.data.frame(predict(out, xreg_base))

abb=ndata+1
abc=nrow(output)
output[abb:abc, 2]=fitted.base
output[abb:abc, 3]=fitted.base1
output[abb:abc, 4]=fitted.base2
write.csv(as.data.frame(cbind(output)), "outpput_sba_final_sensitivity_i10.csv", col.names = T, row.names = F)


#djia_ag_lag_4
indx=cbind(base$CAUR_qd_lag_1,base$i10y_qd_lag_1,base$djia_ag_lag_4)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2", "x3")
fitted.base=as.data.frame(predict(out, xreg_base))

indx=cbind(base$CAUR_qd_lag_1,base$i10y_qd_lag_1,base$djia_ag_lag_4+sd3)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2", "x3")
fitted.base1=as.data.frame(predict(out, xreg_base))

indx=cbind(base$CAUR_qd_lag_1,base$i10y_qd_lag_1,base$djia_ag_lag_4+2*sd3)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2", "x3")
fitted.base2=as.data.frame(predict(out, xreg_base))

abb=ndata+1
abc=nrow(output)
output[abb:abc, 2]=fitted.base
output[abb:abc, 3]=fitted.base1
output[abb:abc, 4]=fitted.base2
write.csv(as.data.frame(cbind(output)), "outpput_sba-final_sensitivity_djia.csv", col.names = T, row.names = F)



#####################################
# OOS testing
####################################
oos<-function(n){
  #n defines how many quarters before 2015Q4  
  ind=nrow(df_total_dev)-n
  df_oos=df_total_dev[1:ind,]
  ind0=nrow(df_oos)
  ind1=nrow(df_oos)+1
  ind2=nrow(df_oos)+npred
  ind4=nrow(df_total_dev)
  ind5=nrow(df_total_dev)-n+1
  
  x1=df_oos$CAUR_qd_lag_1
  mu1=mean(df_oos$CAUR_qd_lag_1)
  sd1= stdev(df_oos$CAUR_qd_lag_1)
  
  x2= df_oos$i10y_qd_lag_1
  mu2=mean(df_oos$i10y_qd_lag_1)
  sd2= stdev(df_oos$i10y_qd_lag_1)
  
  x3= df_oos$djia_ag_lag_4
  mu3=mean(df_oos$djia_ag_lag_4)
  sd3= stdev(df_oos$djia_ag_lag_4)
  
  out_oos=lm(df_oos$sba~x1+x2+x3)
  summary(out_oos)
  ####################################
  #implement the model - out of sample
  ####################################
  
  ndata=nrow(df_oos)
  npred=1
  ind3=ndata+npred
  output_oos=as.data.frame(matrix(0, ind3,7))
  input=sba_input[1:ind3,]
  output_oos[, 1]=input$sba
  output_oos[1:ndata, 2]=out_oos$fitted.values
  
  #PREDICT
  
  indx=cbind(df_total_dev$CAUR_qd_lag_1,df_total_dev$i10y_qd_lag_1,df_total_dev$djia_ag_lag_4)
  xreg_base=as.data.frame(t(indx[ind5,]))
  names(xreg_base)=c("x1", "x2", "x3")
  fitted.base=as.data.frame(predict(out_oos, xreg_base))
  
  # get the values
  output_oos[ind5, 2]=fitted.base
  pct_error= 100*(output_oos[ind5,2]-output_oos[ind5,1])/output_oos[ind5,2]
  result_oos=as.data.frame(cbind(n, output_oos[ind1,1], output_oos[ind1,2],pct_error))
  return(result_oos)
}
oos(2)
oos(3)
oos(4)
oos(5)
indx101=rbind(oos(2),oos(3),oos(4),oos(5))
#####################################
# Prediction CI
#####################################
ndata=nrow(b1)
npred=9
output_ci=as.data.frame(matrix(0, ndata+npred,10))

D1=which(df$year==2003 & df$q==1)
D2=which(df$year==2016 & df$q==3)

output_ci[1:ndata, 1]=df$sba[D1:D2]
output_ci[1:ndata, 2]=out$fitted.values

#####################
#Scenario Forecasts
#####################
aaaa=which(base$year==2016 & base$quarter==1)
bbbb=which(base$year==2018 & base$quarter==1)

indx=cbind(base$CAUR_qd_lag_1,base$i10y_qd_lag_1,base$djia_ag_lag_4)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2", "x3")
fitted.base=as.data.frame(predict(out, xreg_base, interval = "predict", level = 0.95))

indx=cbind(adverse$CAUR_qd_lag_1,adverse$i10y_qd_lag_1,adverse$djia_ag_lag_4)
xreg_adverse=as.data.frame(indx[aaaa:bbbb,])
names(xreg_adverse)=c("x1", "x2", "x3")
fitted.adverse=as.data.frame(predict(out, xreg_adverse, interval = "predict", level = 0.95))

indx=cbind(severe$CAUR_qd_lag_1,severe$i10y_qd_lag_1,severe$djia_ag_lag_4)
xreg_severe=as.data.frame(indx[aaaa:bbbb,])
names(xreg_severe)=c("x1", "x2", "x3")
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

date1 = seq(ISOdate(2003,1,1), by = "quarter", length.out = 64)
write.csv(as.data.frame(cbind(date1,output_ci)), "SBA prediction ci.csv", col.names = T, row.names = F)
