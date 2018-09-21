###################################################################
# Project: Bank of Hope
# Finding Ending Balances
###################################################################  
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(DataAnalytics)
library(tseries)
library(urca)
library (fUnitRoots)
library(lubridate)
library(forecast)

library(CADFtest)
library (leaps)
library(data.table)
library(openxlsx)
library(car)
library(lmtest)
library(orcutt)
library(plot.lm)
library(lmtest)
####################

##save image
# save.image("endbalmodels.RData")

##load image
#load("endbalmodels.RData")
##################################


#####################
#read in the raw data
#####################

#read in the raw data
endbal=read.csv("Ending Balances.csv",header = TRUE)
endbal=endbal[,c(4,2,3)]
names(endbal)= c("date", "ci_bal", "cre_bal")

# #Difference transformation 
endbal$cre_qd=c(NA, diff(endbal$cre_bal))
endbal$cre_yd=endbal$cre_bal-back(endbal$cre_bal, noperiods = 4)
endbal$cre_qg=c(NA, diff(log(endbal$cre_bal)))
endbal$cre_yg=log(endbal$cre_bal)-back(log(endbal$cre_bal), noperiods = 4)
endbal$ci_qd=c(NA, diff(endbal$ci_bal))
endbal$ci_yd=endbal$ci_bal-back(endbal$ci_bal, noperiods = 4)
endbal$ci_qg=c(NA, diff(log(endbal$ci_bal)))
endbal$ci_yg=log(endbal$ci_bal)-back(log(endbal$ci_bal), noperiods = 4)

#making dataset ready for merge
endbal$year=year(mdy(endbal$date))
endbal$month=month(mdy(endbal$date))
endbal$q[endbal$month %in% c(1,2,3)]=1
endbal$q[endbal$month %in% c(4,5,6)]=2
endbal$q[endbal$month %in% c(7,8,9)]=3
endbal$q[endbal$month %in% c(10,11,12)]=4
endbal$month=NULL


#max_lag
max_lag= floor(12*(nrow(endbal)/100)^(1/4))


#######################
#read in the macro vars 
#######################

######
#base
######
base=read.csv("macro_base.csv", header=T)
aaa=which(base$year==2003 & base$q==1)
bbb=which(base$year==2018 & base$q==4)
base=base[aaa:bbb,]

#########
#adverse
#########

adverse=read.csv("macro_adverse.csv", header=T)
aaa=which(adverse$year==2003 & adverse$q==1)
bbb=which(adverse$year==2018 & adverse$q==4)
adverse=adverse[aaa:bbb,]

########
#severe
########
severe=read.csv("macro_severe.csv", header=T)
aaa=which(severe$year==2003 & severe$q==1)
bbb=which(severe$year==2018 & severe$q==4)
severe=severe[aaa:bbb,]

##################
#development macro
##################
D1=which(base$year==2003 & base$q==1)
D2=which(base$year==2015 & base$q==4)
macro_dev=base[c(D1:D2), ]
macro_input=macro_dev[,-c(1,2,3)]


dep_var= as.data.frame(cbind(endbal))


########################################
# Create the dep_var matrix
########################################

var.names=colnames(macro_input)
var_info=as.data.frame(matrix(0, length(var.names), 6 ))
names(var_info) = c("var", "tier", "base", "lag", "diff", "sign")
var_info[,1]=var.names
var_info[,5]=0

#diff
var_info[grepl("_qd", var_info$var),5] = TRUE
var_info[grepl("_yd", var_info$var),5] = TRUE
var_info[grepl("_qg", var_info$var),5] = TRUE
var_info[grepl("_yg", var_info$var),5] = TRUE

#lag
var_info[grepl("_lag_1", var_info$var),4] = 1
var_info[grepl("_lag_2", var_info$var),4] = 2
var_info[grepl("_lag_3", var_info$var),4] = 3
var_info[grepl("_lag_4", var_info$var),4] = 4

#var.base
var_info[grepl("ngdp", var_info$var),3] = "ngdp_g"
var_info[grepl("rgdp", var_info$var),3] = "rgdp_g"
var_info[grepl("rdi", var_info$var),3] = "rdi_g"
var_info[grepl("ndi", var_info$var),3] = "ndi_g"
var_info[grepl("ur", var_info$var),3] = "ur_diff"
var_info[grepl("cpi", var_info$var),3] = "cpi"
var_info[grepl("i3m", var_info$var),3] = "i3m_diff"
var_info[grepl("i5y", var_info$var),3] = "i5yr_diff"
var_info[grepl("i10y", var_info$var),3] = "i10yr_diff"
var_info[grepl("bbb", var_info$var),3] = "bbb_diff"
var_info[grepl("imort", var_info$var),3] = "imort_diff"
var_info[grepl("iprim", var_info$var),3] = "iprim_diff"
var_info[grepl("cppi", var_info$var),3] = "cppi_diff"
var_info[grepl("dji", var_info$var),3] = "dji_diff"
var_info[grepl("vix", var_info$var),3] = "vix"
var_info[grepl("hpi", var_info$var),3] = "hpi_g"
var_info[grepl("spr10", var_info$var),3] = "spread"
var_info[grepl("spr10_q", var_info$var),3] = "spread_diff"
var_info[grepl("spr10_y", var_info$var),3] = "spread_diff"
var_info[grepl("rgpdi_eqp", var_info$var),3] = "rgpdi_eqp"
var_info[grepl("gpdi_eqp", var_info$var),3] = "gpdi_eqp"
var_info[grepl("pfi_nonres", var_info$var),3] = "pfi_nonres"
var_info[grepl("willreit", var_info$var),3] = "willreit"

#var_info[var_info$base==0,]

#sign
var_info[grepl("gdp", var_info$var),6] = 1
var_info[grepl("rdi", var_info$var),6] = 1
var_info[grepl("ndi", var_info$var),6] = 1
var_info[grepl("ur", var_info$var),6] = -1
var_info[grepl("cpi", var_info$var),6] = -1
var_info[grepl("i3m", var_info$var),6] = 0
var_info[grepl("i5y", var_info$var),6] = 0
var_info[grepl("i10y", var_info$var),6] = 0
var_info[grepl("imor", var_info$var),6] = 0
var_info[grepl("ipri", var_info$var),6] = 0
var_info[grepl("bbb", var_info$var),6] = 0
var_info[grepl("dji", var_info$var),6] = 1
var_info[grepl("hpi", var_info$var),6] = 1
var_info[grepl("vix", var_info$var),6] = -1
var_info[grepl("cppi", var_info$var),6] = 1
var_info[grepl("spr10", var_info$var),6] = -1
var_info[grepl("rgpdi_eqp", var_info$var),6] = 1
var_info[grepl("gpdi_eqp", var_info$var),6] = 1
var_info[grepl("pfi_nonres", var_info$var),6] = 1 
var_info[grepl("willreit", var_info$var),6] = 1

# var_info[var_info$sign==0,]

# Tier
var_info[grepl("gdp", var_info$var),2] = 1
var_info[grepl("rdi", var_info$var),2] = 1
var_info[grepl("ndi", var_info$var),2] = 1
var_info[grepl("ur", var_info$var),2] = 1
var_info[grepl("cpi", var_info$var),2] = 3
var_info[grepl("i3m", var_info$var),2] = 2
var_info[grepl("i5y", var_info$var),2] = 2
var_info[grepl("i10y", var_info$var),2] = 2
var_info[grepl("imor", var_info$var),2] = 3
var_info[grepl("ipri", var_info$var),2] = 3
var_info[grepl("bbb", var_info$var),2] = 2
var_info[grepl("dji", var_info$var),2] = 2
var_info[grepl("hpi", var_info$var),2] = 1
var_info[grepl("vix", var_info$var),2] = 3
var_info[grepl("cppi", var_info$var),2] = 1
var_info[grepl("spr10", var_info$var),2] = 1
var_info[grepl("rgpdi_eqp", var_info$var),2] = 2
var_info[grepl("gpdi_eqp", var_info$var),2] = 2
var_info[grepl("pfi_nonres", var_info$var),2] = 2 
var_info[grepl("willreit", var_info$var),2] = 2

# var_info[var_info$tier==0,]
var_info_cre=var_info
var_info_ci=var_info

#####################
#Variable Selection 
#####################
first.obs=which(base$year==2003 & base$q==1)
ndata=which(base$year==2015 & base$q==4)

date_col=as.data.frame(base$Date[first.obs:ndata])
colnames(date_col)="Date"


aaa=which(endbal$year==2003 & endbal$q==1)
bbb=which(endbal$year==2015 & endbal$q==4)
loan_input=endbal[aaa:bbb,-c(1,12,13)]

b1=cbind(date_col, loan_input)
names(b1)=c("Date", names(loan_input))
b=data.table(b1)


c1=cbind(date_col, macro_input)
names(c1)=c("Date", names(macro_input))
c=data.table(c1)
a=data.table(var_info_cre)

df_total_dev= as.data.frame(cbind(date_col, loan_input,  macro_input))

###############
# Models
###############

#################################
# CRE Model
#################################
# source("StepFun.R")
# fix_vars0=c("1")
# v110_model2_cre_qg_sep=StepFun(a,b,c, tier=1,         #indicate which tier of variables to consider
#                                y='cre_qg~',       #indicate response variable
#                                thresh=c(0.05, 0.01, 0.001),    #significance level for SE based p-value and LR test based p-value for each tier
#                                criteria='SE.p', #variable selection criteria; other values='bic', 'LR.p', 'SE.p', 'rsq'
#                                vars0 = c("1"),          #model 0 variables
#                                fix_vars0,    #indicate which variables are fixed
#                                out.print=T         #indicate wheter intermediate output will be printed
# )

# fix_vars0=c("1", "hpi_ag_lag_4", "ndi_ag_lag_4", "spr10_yd")
# v110_model2_cre_qg_sep=StepFun(a,b,c, tier=2,         #indicate which tier of variables to consider
#                                y='cre_qg~',       #indicate response variable
#                                thresh=c(0.05, 0.01, 0.001),    #significance level for SE based p-value and LR test based p-value for each tier
#                                criteria='SE.p', #variable selection criteria; other values='bic', 'LR.p', 'SE.p', 'rsq'
#                                vars0 = c("1", "hpi_ag_lag_4", "ndi_ag_lag_4", "spr10_yd"),          #model 0 variables
#                                fix_vars0,    #indicate which variables are fixed
#                                out.print=T         #indicate wheter intermediate output will be printed
# )
# fix_vars0=c("1", "hpi_ag_lag_4", "ndi_ag_lag_4", "spr10_yd", "i3m_yd_lag_3")
# v110_model2_cre_qg_sep=StepFun(a,b,c, tier=3,         #indicate which tier of variables to consider
#                                y='cre_qg~',       #indicate response variable
#                                thresh=c(0.05, 0.01, 0.001),    #significance level for SE based p-value and LR test based p-value for each tier
#                                criteria='SE.p', #variable selection criteria; other values='bic', 'LR.p', 'SE.p', 'rsq'
#                                vars0 = c("1", "hpi_ag_lag_4", "ndi_ag_lag_4", "spr10_yd", "i3m_yd_lag_3"),          #model 0 variables
#                                fix_vars0,    #indicate which variables are fixed
#                                out.print=T         #indicate wheter intermediate output will be printed
# )

out=lm(b$cre_qg~c$hpi_ag_lag_4+c$ndi_ag_lag_4+c$spr10_yd+c$i3m_yd_lag_3)
summary(out)
Box.test(out$residuals, type = "Ljung-Box", lag = 3)
vif(out)
durbinWatsonTest(out,3)
acf(out$residuals)
pacf(out$residuals)
out_res=rstandard(out)
bgtest(out,3)


#demean it
x1= c$hpi_ag_lag_4- mean(c$hpi_ag_lag_4)
x2= c$ndi_ag_lag_4- mean(c$ndi_ag_lag_4)
x3= c$spr10_yd- mean(c$spr10_yd)
x4= c$i3m_yd_lag_3- mean(c$i3m_yd_lag_3)

mu1=mean(c$hpi_ag_lag_4)
mu2=mean(c$ndi_ag_lag_4)
mu3=mean(c$spr10_yd)
mu4=mean(c$i3m_yd_lag_3)

sd1=stdev(c$hpi_ag_lag_4)
sd2=stdev(c$ndi_ag_lag_4)
sd3=stdev(c$spr10_yd)
sd4=stdev(c$i3m_yd_lag_3)
  
out=lm(b$cre_qg~x1+x2+x3+x4)
summary(out)

#multicolinearity
vif(out)

# Autocorrelation
par(mfrow=c(1,2))
acf(out$residuals, main="")
pacf(out$residuals, main="")
Box.test(out$residuals, type = "Ljung-Box", lag = max_lag)
bgtest(out, order = 3)
durbinWatsonTest(out, max.lag = 3)
durbinWatsonTest(out)

#normality test
#QQ-plot
par(mfrow=c(1,1))
qqnorm(out_res, ylab="Residuals", xlab="Quantiles of Standard Normal", main="CRE Ending Balance") 
qqline(out_res)

# Residual histogram
hist(out_res, breaks="FD", xlab="Residuals", main="Histogram of residuals", ylim=c(0,25))
x<- -3:3
lines(x, 52*dnorm(x,0,sd(out_res)),col=2)

# Residual vs predicted
plot(b$cre_qg,out_res, ylab="Residuals", xlab="Q-o-Q Growth Rate", main="CRE Ending Balance") 
abline(0, 0)


#################################
# Stationarity
#################################
summary(ur.df(na.remove(c$hpi_ag_lag_4)))
summary(ur.df(na.remove(c$ndi_ag_lag_4)))
summary(ur.df(na.remove(c$spr10_yd)))
summary(ur.df(na.remove(c$i3m_yd_lag_3)))

pp.test(na.remove(c$ndi_ag_lag_4))

summary(ur.df(na.remove(x1)))
summary(ur.df(na.remove(x2)))
summary(ur.df(na.remove(x3)))
summary(ur.df(na.remove(x4)))

#################################
#implement the model
#################################

ndata=nrow(b1)
npred=9
output=as.data.frame(matrix(0, ndata+npred,7))

D1=which(endbal$year==2003 & endbal$q==1)
D2=which(endbal$year==2015 & endbal$q==4)
input=endbal[D1:D2,]
output[1:ndata, 1]=input$cre_bal
output[1:ndata, 2]=out$fitted.values
dummy1=ndata-1
output[2:ndata, 3]= exp(log(output[1:dummy1, 1]) + output[2:ndata, 2])
# plot(output[2:ndata, 3])
# lines(output[2:ndata, 1], col='red')

#PREDICT
aaaa=which(base$year==2016 & base$q==1)
bbbb=which(base$year==2018 & base$q==1)

indx=cbind(base$hpi_ag_lag_4-mu1,base$ndi_ag_lag_4-mu2,base$spr10_yd-mu3, base$i3m_yd_lag_3-mu4)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2", "x3", "x4")
fitted.base=as.data.frame(predict(out, xreg_base))

indx=cbind(adverse$hpi_ag_lag_4-mu1,adverse$ndi_ag_lag_4-mu2,adverse$spr10_yd-mu3, adverse$i3m_yd_lag_3-mu4)
xreg_adverse=as.data.frame(indx[aaaa:bbbb,])
names(xreg_adverse)=c("x1", "x2", "x3", "x4")
fitted.adverse=predict(out, xreg_adverse)

indx=cbind(severe$hpi_ag_lag_4-mu1,severe$ndi_ag_lag_4-mu2,severe$spr10_yd-mu3, severe$i3m_yd_lag_3-mu4)
xreg_severe=as.data.frame(indx[aaaa:bbbb,])
names(xreg_severe)=c("x1", "x2", "x3", "x4")
fitted.severe=predict(out, xreg_severe)

#output[1:ndata, 1]=ppnr$Int_bearing[D1:D2]
output[53:61, 2]=fitted.base
output[53, 3]= exp(log(output[ndata, 1]) + output[53, 2])

for (i in 2:npred){
  ab=52+i
  ac=52+i-1
  output[ab, 3]= exp(log(output[ac, 3]) + output[ab, 2])
}

output[1:ndata,c(4,5)]=output[1:ndata, c(2,3)]
output[1:ndata,c(6,7)]=output[1:ndata, c(2,3)]

output[53:61, 4]=fitted.adverse
output[53,5]= exp(log(output[ndata, 1]) + output[53, 4])

for (i in 2:npred){
  ab=52+i
  ac=52+i-1
  output[ab, 5]= exp(log(output[ac, 5]) + output[ab, 4])
}

output[53:61, 6]=fitted.severe
output[53,7]= exp(log(output[ndata, 1]) + output[53, 6])

for (i in 2:npred){
  ab=52+i
  ac=52+i-1
  output[ab, 7]= exp(log(output[ac, 7]) + output[ab, 6])
}
output[1, c(3,5,7)]=output[1,1]
output[which(output[,1]==0),1]=NA
#plot together
date1 = seq(ISOdate(2003,1,1), by = "quarter", length.out = 61)

plot(date1, output[,3], type='l', lty='dotted', ylab='CRE EB')
lines(date1, output[,5], col='blue')
lines(date1,output[,7], col='red')
lines(date1, output[,1], col='green')
legend("topleft", legend= c("base", "adverse", "severe", "Historical"), fill=c("black", "blue", "red", "green"))
colnames(output)=c("Historical", "estimated_base", "estimated_base_bal", "estimated_adverse", "adverse_bal", "estimated_severe", "severe_bal")
#output the Cre results 
write.csv(as.data.frame(cbind(date1,output)), "cre EB Projections.csv", col.names = T, row.names = F)

#######################
# Sensitivity Analysis
#######################
##HPI 
ndata=nrow(b1)
npred=9
output=as.data.frame(matrix(0, ndata+npred,7))

D1=which(endbal$year==2003 & endbal$q==1)
D2=which(endbal$year==2015 & endbal$q==4)
input=endbal[D1:D2,]
output[1:ndata, 1]=input$cre_bal
output[1:ndata, 2]=out$fitted.values
dummy1=ndata-1
output[2:ndata, 3]= exp(log(output[1:dummy1, 1]) + output[2:ndata, 2])
# plot(output[2:ndata, 3])
# lines(output[2:ndata, 1], col='red')

#PREDICT
aaaa=which(base$year==2016 & base$q==1)
bbbb=which(base$year==2018 & base$q==1)

indx=cbind(base$hpi_ag_lag_4-mu1,base$ndi_ag_lag_4-mu2,base$spr10_yd-mu3, base$i3m_yd_lag_3-mu4)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2", "x3", "x4")
fitted.base=as.data.frame(predict(out, xreg_base))

indx_1sd=cbind(base$hpi_ag_lag_4-mu1+sd1,base$ndi_ag_lag_4-mu2,base$spr10_yd-mu3, base$i3m_yd_lag_3-mu4)
xreg_1sd=as.data.frame(indx_1sd[aaaa:bbbb,])
names(xreg_1sd)=c("x1", "x2", "x3", "x4")
fitted.1sd=as.data.frame(predict(out, xreg_1sd))

indx_2sd=cbind(base$hpi_ag_lag_4-mu1+2*sd1,base$ndi_ag_lag_4-mu2,base$spr10_yd-mu3, base$i3m_yd_lag_3-mu4)
xreg_2sd=as.data.frame(indx_2sd[aaaa:bbbb,])
names(xreg_2sd)=c("x1", "x2", "x3", "x4")
fitted.2sd=as.data.frame(predict(out, xreg_2sd))

#output[1:ndata, 1]=ppnr$Int_bearing[D1:D2]
output[53:61, 2]=fitted.base
output[53, 3]= exp(log(output[ndata, 1]) + output[53, 2])

for (i in 2:npred){
  ab=52+i
  ac=52+i-1
  output[ab, 3]= exp(log(output[ac, 3]) + output[ab, 2])
}

output[1:ndata,c(4,5)]=output[1:ndata, c(2,3)]
output[1:ndata,c(6,7)]=output[1:ndata, c(2,3)]

output[53:61, 4]=fitted.1sd
output[53,5]= exp(log(output[ndata, 1]) + output[53, 4])

for (i in 2:npred){
  ab=52+i
  ac=52+i-1
  output[ab, 5]= exp(log(output[ac, 5]) + output[ab, 4])
}

output[53:61, 6]=fitted.2sd
output[53,7]= exp(log(output[ndata, 1]) + output[53, 6])

for (i in 2:npred){
  ab=52+i
  ac=52+i-1
  output[ab, 7]= exp(log(output[ac, 7]) + output[ab, 6])
}
output[1, c(3,5,7)]=output[1,1]
output[which(output[,1]==0),1]=NA
#plot together
date1 = seq(ISOdate(2003,1,1), by = "quarter", length.out = 61)

plot(date1, output[,3], type='l', lty='dotted', ylab='CRE EB')
lines(date1, output[,5], col='blue')
lines(date1,output[,7], col='red')
lines(date1, output[,1], col='green')
legend("topleft", legend= c("base", "adverse", "severe", "Historical"), fill=c("black", "blue", "red", "green"))
colnames(output)=c("Historical", "estimated_base", "estimated_base_bal", "estimated_adverse", "adverse_bal", "estimated_severe", "severe_bal")
#output the Cre results 

#clean
output2=output[c(53:61), c(3,5,7)]
write.csv(as.data.frame(cbind(date1[53:61],output2)), "cre EB sensitivity hpi.csv", col.names = T, row.names = F)

##NDI
ndata=nrow(b1)
npred=9
output=as.data.frame(matrix(0, ndata+npred,7))

D1=which(endbal$year==2003 & endbal$q==1)
D2=which(endbal$year==2015 & endbal$q==4)
input=endbal[D1:D2,]
output[1:ndata, 1]=input$cre_bal
output[1:ndata, 2]=out$fitted.values
dummy1=ndata-1
output[2:ndata, 3]= exp(log(output[1:dummy1, 1]) + output[2:ndata, 2])
# plot(output[2:ndata, 3])
# lines(output[2:ndata, 1], col='red')

#PREDICT
aaaa=which(base$year==2016 & base$q==1)
bbbb=which(base$year==2018 & base$q==1)

indx=cbind(base$hpi_ag_lag_4-mu1,base$ndi_ag_lag_4-mu2,base$spr10_yd-mu3, base$i3m_yd_lag_3-mu4)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2", "x3", "x4")
fitted.base=as.data.frame(predict(out, xreg_base))

indx_1sd=cbind(base$hpi_ag_lag_4-mu1,base$ndi_ag_lag_4-mu2+sd2,base$spr10_yd-mu3, base$i3m_yd_lag_3-mu4)
xreg_1sd=as.data.frame(indx_1sd[aaaa:bbbb,])
names(xreg_1sd)=c("x1", "x2", "x3", "x4")
fitted.1sd=as.data.frame(predict(out, xreg_1sd))

indx_2sd=cbind(base$hpi_ag_lag_4-mu1,base$ndi_ag_lag_4+2*sd2-mu2,base$spr10_yd-mu3, base$i3m_yd_lag_3-mu4)
xreg_2sd=as.data.frame(indx_2sd[aaaa:bbbb,])
names(xreg_2sd)=c("x1", "x2", "x3", "x4")
fitted.2sd=as.data.frame(predict(out, xreg_2sd))

output[53:61, 2]=fitted.base
output[53, 3]= exp(log(output[ndata, 1]) + output[53, 2])

for (i in 2:npred){
  ab=52+i
  ac=52+i-1
  output[ab, 3]= exp(log(output[ac, 3]) + output[ab, 2])
}

output[1:ndata,c(4,5)]=output[1:ndata, c(2,3)]
output[1:ndata,c(6,7)]=output[1:ndata, c(2,3)]

output[53:61, 4]=fitted.1sd
output[53,5]= exp(log(output[ndata, 1]) + output[53, 4])

for (i in 2:npred){
  ab=52+i
  ac=52+i-1
  output[ab, 5]= exp(log(output[ac, 5]) + output[ab, 4])
}

output[53:61, 6]=fitted.2sd
output[53,7]= exp(log(output[ndata, 1]) + output[53, 6])

for (i in 2:npred){
  ab=52+i
  ac=52+i-1
  output[ab, 7]= exp(log(output[ac, 7]) + output[ab, 6])
}
output[1, c(3,5,7)]=output[1,1]
output[which(output[,1]==0),1]=NA
#plot together
date1 = seq(ISOdate(2003,1,1), by = "quarter", length.out = 61)

plot(date1, output[,3], type='l', lty='dotted', ylab='CRE EB')
lines(date1, output[,5], col='blue')
lines(date1,output[,7], col='red')
lines(date1, output[,1], col='green')
legend("topleft", legend= c("base", "adverse", "severe", "Historical"), fill=c("black", "blue", "red", "green"))
colnames(output)=c("Historical", "estimated_base", "estimated_base_bal", "estimated_adverse", "adverse_bal", "estimated_severe", "severe_bal")
#output the Cre results 

#clean
output2=output[c(53:61), c(3,5,7)]
write.csv(as.data.frame(cbind(date1[53:61],output2)), "cre EB sensitivity ndi.csv", col.names = T, row.names = F)


# SPR10 
ndata=nrow(b1)
npred=9
output=as.data.frame(matrix(0, ndata+npred,7))

D1=which(endbal$year==2003 & endbal$q==1)
D2=which(endbal$year==2015 & endbal$q==4)
input=endbal[D1:D2,]
output[1:ndata, 1]=input$cre_bal
output[1:ndata, 2]=out$fitted.values
dummy1=ndata-1
output[2:ndata, 3]= exp(log(output[1:dummy1, 1]) + output[2:ndata, 2])
# plot(output[2:ndata, 3])
# lines(output[2:ndata, 1], col='red')

#PREDICT
aaaa=which(base$year==2016 & base$q==1)
bbbb=which(base$year==2018 & base$q==1)

indx=cbind(base$hpi_ag_lag_4-mu1,base$ndi_ag_lag_4-mu2,base$spr10_yd-mu3, base$i3m_yd_lag_3-mu4)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2", "x3", "x4")
fitted.base=as.data.frame(predict(out, xreg_base))

indx_1sd=cbind(base$hpi_ag_lag_4-mu1,base$ndi_ag_lag_4-mu2,base$spr10_yd-mu3+sd3, base$i3m_yd_lag_3-mu4)
xreg_1sd=as.data.frame(indx_1sd[aaaa:bbbb,])
names(xreg_1sd)=c("x1", "x2", "x3", "x4")
fitted.1sd=as.data.frame(predict(out, xreg_1sd))

indx_2sd=cbind(base$hpi_ag_lag_4-mu1,base$ndi_ag_lag_4-mu2,base$spr10_yd-mu3+2*sd3, base$i3m_yd_lag_3-mu4)
xreg_2sd=as.data.frame(indx_2sd[aaaa:bbbb,])
names(xreg_2sd)=c("x1", "x2", "x3", "x4")
fitted.2sd=as.data.frame(predict(out, xreg_2sd))

#output[1:ndata, 1]=ppnr$Int_bearing[D1:D2]
output[53:61, 2]=fitted.base
output[53, 3]= exp(log(output[ndata, 1]) + output[53, 2])

for (i in 2:npred){
  ab=52+i
  ac=52+i-1
  output[ab, 3]= exp(log(output[ac, 3]) + output[ab, 2])
}

output[1:ndata,c(4,5)]=output[1:ndata, c(2,3)]
output[1:ndata,c(6,7)]=output[1:ndata, c(2,3)]

output[53:61, 4]=fitted.1sd
output[53,5]= exp(log(output[ndata, 1]) + output[53, 4])

for (i in 2:npred){
  ab=52+i
  ac=52+i-1
  output[ab, 5]= exp(log(output[ac, 5]) + output[ab, 4])
}

output[53:61, 6]=fitted.2sd
output[53,7]= exp(log(output[ndata, 1]) + output[53, 6])

for (i in 2:npred){
  ab=52+i
  ac=52+i-1
  output[ab, 7]= exp(log(output[ac, 7]) + output[ab, 6])
}
output[1, c(3,5,7)]=output[1,1]
output[which(output[,1]==0),1]=NA
#plot together
date1 = seq(ISOdate(2003,1,1), by = "quarter", length.out = 61)

plot(date1, output[,3], type='l', lty='dotted', ylab='CRE EB')
lines(date1, output[,5], col='blue')
lines(date1,output[,7], col='red')
lines(date1, output[,1], col='green')
legend("topleft", legend= c("base", "adverse", "severe", "Historical"), fill=c("black", "blue", "red", "green"))
colnames(output)=c("Historical", "estimated_base", "estimated_base_bal", "estimated_adverse", "adverse_bal", "estimated_severe", "severe_bal")
#output the Cre results 

#clean
output2=output[c(53:61), c(3,5,7)]
write.csv(as.data.frame(cbind(date1[53:61],output2)), "cre EB sensitivity spr10.csv", col.names = T, row.names = F)


# i3m
ndata=nrow(b1)
npred=9
output=as.data.frame(matrix(0, ndata+npred,7))

D1=which(endbal$year==2003 & endbal$q==1)
D2=which(endbal$year==2015 & endbal$q==4)
input=endbal[D1:D2,]
output[1:ndata, 1]=input$cre_bal
output[1:ndata, 2]=out$fitted.values
dummy1=ndata-1
output[2:ndata, 3]= exp(log(output[1:dummy1, 1]) + output[2:ndata, 2])
# plot(output[2:ndata, 3])
# lines(output[2:ndata, 1], col='red')

#PREDICT
aaaa=which(base$year==2016 & base$q==1)
bbbb=which(base$year==2018 & base$q==1)

indx=cbind(base$hpi_ag_lag_4-mu1,base$ndi_ag_lag_4-mu2,base$spr10_yd-mu3, base$i3m_yd_lag_3-mu4)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2", "x3", "x4")
fitted.base=as.data.frame(predict(out, xreg_base))

indx_1sd=cbind(base$hpi_ag_lag_4-mu1,base$ndi_ag_lag_4-mu2,base$spr10_yd-mu3, base$i3m_yd_lag_3-mu4+sd4)
xreg_1sd=as.data.frame(indx_1sd[aaaa:bbbb,])
names(xreg_1sd)=c("x1", "x2", "x3", "x4")
fitted.1sd=as.data.frame(predict(out, xreg_1sd))

indx_2sd=cbind(base$hpi_ag_lag_4-mu1,base$ndi_ag_lag_4-mu2,base$spr10_yd-mu3, base$i3m_yd_lag_3-mu4+2*sd4)
xreg_2sd=as.data.frame(indx_2sd[aaaa:bbbb,])
names(xreg_2sd)=c("x1", "x2", "x3", "x4")
fitted.2sd=as.data.frame(predict(out, xreg_2sd))

#output[1:ndata, 1]=ppnr$Int_bearing[D1:D2]
output[53:61, 2]=fitted.base
output[53, 3]= exp(log(output[ndata, 1]) + output[53, 2])

for (i in 2:npred){
  ab=52+i
  ac=52+i-1
  output[ab, 3]= exp(log(output[ac, 3]) + output[ab, 2])
}

output[1:ndata,c(4,5)]=output[1:ndata, c(2,3)]
output[1:ndata,c(6,7)]=output[1:ndata, c(2,3)]

output[53:61, 4]=fitted.1sd
output[53,5]= exp(log(output[ndata, 1]) + output[53, 4])

for (i in 2:npred){
  ab=52+i
  ac=52+i-1
  output[ab, 5]= exp(log(output[ac, 5]) + output[ab, 4])
}

output[53:61, 6]=fitted.2sd
output[53,7]= exp(log(output[ndata, 1]) + output[53, 6])

for (i in 2:npred){
  ab=52+i
  ac=52+i-1
  output[ab, 7]= exp(log(output[ac, 7]) + output[ab, 6])
}
output[1, c(3,5,7)]=output[1,1]
output[which(output[,1]==0),1]=NA
#plot together
date1 = seq(ISOdate(2003,1,1), by = "quarter", length.out = 61)

plot(date1, output[,3], type='l', lty='dotted', ylab='CRE EB')
lines(date1, output[,5], col='blue')
lines(date1,output[,7], col='red')
lines(date1, output[,1], col='green')
legend("topleft", legend= c("base", "adverse", "severe", "Historical"), fill=c("black", "blue", "red", "green"))
colnames(output)=c("Historical", "estimated_base", "estimated_base_bal", "estimated_adverse", "adverse_bal", "estimated_severe", "severe_bal")
#output the Cre results 

#clean
output2=output[c(53:61), c(3,5,7)]
write.csv(as.data.frame(cbind(date1[53:61],output2)), "cre EB sensitivity i3m.csv", col.names = T, row.names = F)

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
          
          #demean it
          x1= df_oos$hpi_ag_lag_4- mean(df_oos$hpi_ag_lag_4)
          x2= df_oos$ndi_ag_lag_4- mean(df_oos$ndi_ag_lag_4)
          x3= df_oos$spr10_yd- mean(df_oos$spr10_yd)
          x4= df_oos$i3m_yd_lag_3- mean(df_oos$i3m_yd_lag_3)
          
          mu1=mean(df_oos$hpi_ag_lag_4)
          mu2=mean(df_oos$ndi_ag_lag_4)
          mu3=mean(df_oos$spr10_yd)
          mu4=mean(df_oos$i3m_yd_lag_3)
          
          out_oos=lm(df_oos$cre_qg~x1+x2+x3+x4)
          summary(out_oos)
          
          ####################################
          #implement the model - out of sample
          ####################################
          
          ndata=nrow(df_oos)
          npred=1
          ind3=ndata+npred
          output_oos=as.data.frame(matrix(0, ndata+npred,7))
          input=endbal[1:ind5,]
          output_oos[, 1]=input$cre_bal
          output_oos[1:ndata, 2]=out_oos$fitted.values
          dummy1=ndata-1
          output_oos[2:ndata, 3]= exp(log(output_oos[1:dummy1, 1]) + output_oos[2:ndata, 2])
          
          #PREDICT
          indx=cbind(df_total_dev$hpi_ag_lag_4-mu1,df_total_dev$ndi_ag_lag_4-mu2,df_total_dev$spr10_yd-mu3, df_total_dev$i3m_yd_lag_3-mu4)
          xreg_base=as.data.frame(t(indx[ind5,]))
          names(xreg_base)=c("x1", "x2", "x3", "x4")
          fitted.base=as.data.frame(predict(out_oos, xreg_base))
          
          # get the values
          output_oos[ind5, 2]=fitted.base
          output_oos[ind5, 3]= exp(log(output_oos[ndata, 1]) + output_oos[ind1, 2])
          
          pct_error= 100*(output_oos[ind5,3]-output_oos[ind5,1])/output_oos[ind5,3]
          result_oos=as.data.frame(cbind(n, output_oos[ind1,1], output_oos[ind1,3],pct_error))
          return(result_oos)
}

oos(1)
oos(2)
oos(3)
oos(4)

#####################################
# Prediction CI
####################################
ndata=nrow(b1)
npred=9
output_ci=as.data.frame(matrix(0, ndata+npred,10))

D1=which(endbal$year==2003 & endbal$q==1)
D2=which(endbal$year==2015 & endbal$q==4)
input=endbal[D1:D2,]
output_ci[1:ndata, 1]=input$cre_qg
output_ci[1:ndata, 2]=out$fitted.values

#PREDICT
aaaa=which(base$year==2016 & base$q==1)
bbbb=which(base$year==2018 & base$q==1)

indx=cbind(base$hpi_ag_lag_4-mu1,base$ndi_ag_lag_4-mu2,base$spr10_yd-mu3, base$i3m_yd_lag_3-mu4)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2", "x3", "x4")
fitted.base=as.data.frame(predict(out, xreg_base,interval = "predict", level = 0.95))


indx=cbind(adverse$hpi_ag_lag_4-mu1,adverse$ndi_ag_lag_4-mu2,adverse$spr10_yd-mu3, adverse$i3m_yd_lag_3-mu4)
xreg_adverse=as.data.frame(indx[aaaa:bbbb,])
names(xreg_adverse)=c("x1", "x2", "x3", "x4")
fitted.adverse=predict(out, xreg_adverse,interval = "predict",level = 0.95)

indx=cbind(severe$hpi_ag_lag_4-mu1,severe$ndi_ag_lag_4-mu2,severe$spr10_yd-mu3, severe$i3m_yd_lag_3-mu4)
xreg_severe=as.data.frame(indx[aaaa:bbbb,])
names(xreg_severe)=c("x1", "x2", "x3", "x4")
fitted.severe=predict(out, xreg_severe ,interval = "predict",level = 0.95)


output_ci[53:61, 2]=fitted.base[,1] #fit base
output_ci[53:61, 3]=fitted.base[,2] #lwr base
output_ci[53:61, 4]=fitted.base[,3] #upr base
output_ci[53:61, 5]=fitted.adverse[,1] #fit adverse
output_ci[53:61, 6]=fitted.adverse[,2] #lwr adverse
output_ci[53:61, 7]=fitted.adverse[,3] #upr adverse
output_ci[53:61,8]=fitted.severe[,1]#fit severe
output_ci[53:61,9]=fitted.severe[,2] #lwr severe
output_ci[53:61,10]=fitted.severe[,3] # upr Severe
colnames(output_ci)=c("Historical", "estimated_base_fit", "estimated_base_lwr", 
                      "estimated_base_upr", "estimated_adverse_fit", 
                      "estimated_adverse_lwr", "estimated_adverse_upr", 
                      "estimated_severe_fit", "estimated_severe_lwr",
                      "estimated_severe_upr")

write.csv(as.data.frame(cbind(date1,output_ci)), "cre_prediction_ci.csv", col.names = T, row.names = F)

#####################################################################
# CI Model
#####################################################################

# source("StepFun.R")
# fix_vars0=c("1", "rgdp_ag_lag_1")
# v110_model2_ci_qd_sep=StepFun(a,b,c, tier=1,         #indicate which tier of variables to consider
#                               y='ci_qd~',       #indicate response variable
#                               thresh=c(0.05, 0.01, 0.001),    #significance level for SE based p-value and LR test based p-value for each tier
#                               criteria='SE.p', #variable selection criteria; other values='bic', 'LR.p', 'SE.p', 'rsq'
#                               vars0 = c("1", "rgdp_ag_lag_1"),          #model 0 variables
#                               fix_vars0,    #indicate which variables are fixed
#                               out.print=T         #indicate wheter intermediate output will be printed
# )
# 
# fix_vars0=c("1", "rgdp_ag_lag_1", "rdi_ag_lag_4")
# v110_model2_ci_qd_sep=StepFun(a,b,c, tier=2,         #indicate which tier of variables to consider
#                               y='ci_qd~',       #indicate response variable
#                               thresh=c(0.05, 0.01, 0.001),    #significance level for SE based p-value and LR test based p-value for each tier
#                               criteria='SE.p', #variable selection criteria; other values='bic', 'LR.p', 'SE.p', 'rsq'
#                               vars0 = c("1", "rgdp_ag_lag_1", "rdi_ag_lag_4"),          #model 0 variables
#                               fix_vars0,    #indicate which variables are fixed
#                               out.print=T         #indicate wheter intermediate output will be printed
# )
# 
# fix_vars0=c("1", "rgdp_ag_lag_1", "rdi_ag_lag_4")
# v110_model2_ci_qd_sep=StepFun(a,b,c, tier=3,         #indicate which tier of variables to consider
#                               y='ci_qd~',       #indicate response variable
#                               thresh=c(0.05, 0.01, 0.001),    #significance level for SE based p-value and LR test based p-value for each tier
#                               criteria='SE.p', #variable selection criteria; other values='bic', 'LR.p', 'SE.p', 'rsq'
#                               vars0 = c("1", "rgdp_ag_lag_1", "rdi_ag_lag_4"),          #model 0 variables
#                               fix_vars0,    #indicate which variables are fixed
#                               out.print=T         #indicate wheter intermediate output will be printed
# )


x1=c$rgdp_ag_lag_1-mean(c$rgdp_ag_lag_1)
x2=c$rdi_ag_lag_4-mean(c$rdi_ag_lag_4)
mu1=mean(c$rgdp_ag_lag_1)
mu2= mean(c$rdi_ag_lag_4)
sd1=stdev(c$rgdp_ag_lag_1)
sd2= stdev(c$rdi_ag_lag_4)

out=lm(b$ci_qd~x1+x2)
summary(out)
Box.test(out$residuals, type = "Ljung-Box", lag = 1)


#Multicolinearity
vif(out)

par(mfrow=c(1,2))
acf(out$residuals, main="")
pacf(out$residuals, main="")
Box.test(out$residuals, type = "Ljung-Box", lag = max_lag)
bgtest(out, order = 3)
durbinWatsonTest(out, 3)

# Stationarity 
summary(ur.df(na.remove(x1)))
summary(ur.df(na.remove(x2)))



#normality test
out_res=rstandard(out)
#QQ-plot
qqnorm(out_res, ylab="Residuals", xlab="Quantiles of Standard Normal", main="CI Ending Balance") 
qqline(out_res)

max(out_res)
min(out_res)
# Residual histogram
hist(out_res, breaks="FD", xlab="Residuals", main="Histogram of residuals", ylim=c(0,25))
x<- -3:4
lines(x, 52*dnorm(x,0,sd(out_res)),col=2)


# homoskedasticity: Residual vs predicted
plot(out$fitted.values,out_res, ylab="Residuals", xlab="Predicted Values", main="CI Ending Balance") 
abline(0, 0)



#################################
#implement the model
#################################

ndata=nrow(b1)
npred=9
output=as.data.frame(matrix(0, ndata+npred,7))

D1=which(endbal$year==2003 & endbal$q==1)
D2=which(endbal$year==2015 & endbal$q==4)
input=endbal[D1:D2,]
output[1:ndata, 1]=input$ci_bal
output[1:ndata, 2]=out$fitted.values
dummy1=ndata-1
output[2:ndata, 3]= output[1:dummy1, 1] + output[2:ndata, 2]
# plot(output[2:ndata, 3])
# lines(output[2:ndata, 1], col='red')

#PREDICT
aaaa=which(base$year==2016 & base$q==1)
bbbb=which(base$year==2018 & base$q==1)

indx=cbind(base$rgdp_ag_lag_1-mu1,base$rdi_ag_lag_4-mu2)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2")
fitted.base=as.data.frame(predict(out, xreg_base))

indx=cbind(adverse$rgdp_ag_lag_1-mu1,adverse$rdi_ag_lag_4-mu2)
xreg_adverse=as.data.frame(indx[aaaa:bbbb,])
names(xreg_adverse)=c("x1", "x2")
fitted.adverse=predict(out, xreg_adverse)

indx=cbind(severe$rgdp_ag_lag_1-mu1,severe$rdi_ag_lag_4-mu2)
xreg_severe=as.data.frame(indx[aaaa:bbbb,])
names(xreg_severe)=c("x1", "x2")
fitted.severe=predict(out, xreg_severe)

#output[1:ndata, 1]=ppnr$Int_bearing[D1:D2]
output[53:61, 2]=fitted.base
output[53, 3]= output[ndata, 1] + output[53, 2]

for (i in 2:npred){
  ab=52+i
  ac=52+i-1
  output[ab, 3]= output[ac, 3]+ output[ab, 2]
}

output[1:ndata,c(4,5)]=output[1:ndata, c(2,3)]
output[1:ndata,c(6,7)]=output[1:ndata, c(2,3)]

output[53:61, 4]=fitted.adverse
output[53,5]= output[ndata, 1] + output[53, 4]

for (i in 2:npred){
  ab=52+i
  ac=52+i-1
  output[ab, 5]= output[ac, 5] + output[ab, 4]
}

output[53:61, 6]=fitted.severe
output[53,7]= output[ndata, 1] + output[53, 6]

for (i in 2:npred){
  ab=52+i
  ac=52+i-1
  output[ab, 7]= output[ac, 7] + output[ab, 6]
}
output[1, c(3,5,7)]=output[1,1]
output[which(output[,1]==0),1]=NA
#plot together
par(mfrow=c(1,1))
date1 = seq(ISOdate(2003,1,1), by = "quarter", length.out = 61)

plot(date1, output[,3], type='l', lty='dotted', ylab='CI EB')
lines(date1, output[,5], col='blue')
lines(date1,output[,7], col='red')
lines(date1, output[,1], col='green')
legend("topleft", legend= c("base", "adverse", "severe", "Historical"), fill=c("black", "blue", "red", "green"))
colnames(output)=c("Historical", "estimated_base", "estimated_base_bal", "estimated_adverse", "adverse_bal", "estimated_severe", "severe_bal")
#output the ci results 
write.csv(as.data.frame(cbind(date1,output)), "CI EB Projections.csv", col.names = T, row.names = F)



#################################
#Sensitivity Analysis
#################################
#rgdp
ndata=nrow(b1)
npred=9
output=as.data.frame(matrix(0, ndata+npred,7))

D1=which(endbal$year==2003 & endbal$q==1)
D2=which(endbal$year==2015 & endbal$q==4)
input=endbal[D1:D2,]
output[1:ndata, 1]=input$ci_bal
output[1:ndata, 2]=out$fitted.values
dummy1=ndata-1
output[2:ndata, 3]= output[1:dummy1, 1] + output[2:ndata, 2]
# plot(output[2:ndata, 3])
# lines(output[2:ndata, 1], col='red')

#PREDICT
aaaa=which(base$year==2016 & base$q==1)
bbbb=which(base$year==2018 & base$q==1)

indx=cbind(base$rgdp_ag_lag_1-mu1,base$rdi_ag_lag_4-mu2)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2")
fitted.base=as.data.frame(predict(out, xreg_base))

indx=cbind(base$rgdp_ag_lag_1-mu1+sd1,base$rdi_ag_lag_4-mu2)
xreg_1sd=as.data.frame(indx[aaaa:bbbb,])
names(xreg_1sd)=c("x1", "x2")
fitted.1sd=as.data.frame(predict(out, xreg_1sd))

indx=cbind(base$rgdp_ag_lag_1-mu1+2*sd1,base$rdi_ag_lag_4-mu2)
xreg_2sd=as.data.frame(indx[aaaa:bbbb,])
names(xreg_2sd)=c("x1", "x2")
fitted.2sd=as.data.frame(predict(out, xreg_2sd))

#output[1:ndata, 1]=ppnr$Int_bearing[D1:D2]
output[53:61, 2]=fitted.base
output[53, 3]= output[ndata, 1] + output[53, 2]

for (i in 2:npred){
  ab=52+i
  ac=52+i-1
  output[ab, 3]= output[ac, 3]+ output[ab, 2]
}

output[1:ndata,c(4,5)]=output[1:ndata, c(2,3)]
output[1:ndata,c(6,7)]=output[1:ndata, c(2,3)]

output[53:61, 4]=fitted.1sd
output[53,5]= output[ndata, 1] + output[53, 4]

for (i in 2:npred){
  ab=52+i
  ac=52+i-1
  output[ab, 5]= output[ac, 5] + output[ab, 4]
}

output[53:61, 6]=fitted.2sd
output[53,7]= output[ndata, 1] + output[53, 6]

for (i in 2:npred){
  ab=52+i
  ac=52+i-1
  output[ab, 7]= output[ac, 7] + output[ab, 6]
}
output[1, c(3,5,7)]=output[1,1]
output[which(output[,1]==0),1]=NA
#plot together
date1 = seq(ISOdate(2003,1,1), by = "quarter", length.out = 61)

plot(date1, output[,3], type='l', lty='dotted', ylab='CI EB')
lines(date1, output[,5], col='blue')
lines(date1,output[,7], col='red')
lines(date1, output[,1], col='green')
legend("topleft", legend= c("base", "adverse", "severe", "Historical"), fill=c("black", "blue", "red", "green"))
colnames(output)=c("Historical", "estimated_base", "estimated_base_bal", "estimated_adverse", "adverse_bal", "estimated_severe", "severe_bal")
#output the ci results 
output2=output[c(53:61), c(3,5,7)]
write.csv(as.data.frame(cbind(date1[53:61],output2)), "CI EB sensitivity rgdp.csv", col.names = T, row.names = F)

#rdi
ndata=nrow(b1)
npred=9
output=as.data.frame(matrix(0, ndata+npred,7))

D1=which(endbal$year==2003 & endbal$q==1)
D2=which(endbal$year==2015 & endbal$q==4)
input=endbal[D1:D2,]
output[1:ndata, 1]=input$ci_bal
output[1:ndata, 2]=out$fitted.values
dummy1=ndata-1
output[2:ndata, 3]= output[1:dummy1, 1] + output[2:ndata, 2]
# plot(output[2:ndata, 3])
# lines(output[2:ndata, 1], col='red')

#PREDICT
aaaa=which(base$year==2016 & base$q==1)
bbbb=which(base$year==2018 & base$q==1)

indx=cbind(base$rgdp_ag_lag_1-mu1,base$rdi_ag_lag_4-mu2)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2")
fitted.base=as.data.frame(predict(out, xreg_base))

indx=cbind(base$rgdp_ag_lag_1-mu1,base$rdi_ag_lag_4-mu2+sd2)
xreg_1sd=as.data.frame(indx[aaaa:bbbb,])
names(xreg_1sd)=c("x1", "x2")
fitted.1sd=as.data.frame(predict(out, xreg_1sd))

indx=cbind(base$rgdp_ag_lag_1-mu1,base$rdi_ag_lag_4-mu2+2*sd2)
xreg_2sd=as.data.frame(indx[aaaa:bbbb,])
names(xreg_2sd)=c("x1", "x2")
fitted.2sd=as.data.frame(predict(out, xreg_2sd))

#output[1:ndata, 1]=ppnr$Int_bearing[D1:D2]
output[53:61, 2]=fitted.base
output[53, 3]= output[ndata, 1] + output[53, 2]

for (i in 2:npred){
  ab=52+i
  ac=52+i-1
  output[ab, 3]= output[ac, 3]+ output[ab, 2]
}

output[1:ndata,c(4,5)]=output[1:ndata, c(2,3)]
output[1:ndata,c(6,7)]=output[1:ndata, c(2,3)]

output[53:61, 4]=fitted.1sd
output[53,5]= output[ndata, 1] + output[53, 4]

for (i in 2:npred){
  ab=52+i
  ac=52+i-1
  output[ab, 5]= output[ac, 5] + output[ab, 4]
}

output[53:61, 6]=fitted.2sd
output[53,7]= output[ndata, 1] + output[53, 6]

for (i in 2:npred){
  ab=52+i
  ac=52+i-1
  output[ab, 7]= output[ac, 7] + output[ab, 6]
}
output[1, c(3,5,7)]=output[1,1]
output[which(output[,1]==0),1]=NA
#plot together
date1 = seq(ISOdate(2003,1,1), by = "quarter", length.out = 61)

plot(date1, output[,3], type='l', lty='dotted', ylab='CI EB')
lines(date1, output[,5], col='blue')
lines(date1,output[,7], col='red')
lines(date1, output[,1], col='green')
legend("topleft", legend= c("base", "adverse", "severe", "Historical"), fill=c("black", "blue", "red", "green"))
colnames(output)=c("Historical", "estimated_base", "estimated_base_bal", "estimated_adverse", "adverse_bal", "estimated_severe", "severe_bal")
#output the ci results 
output2=output[c(53:61), c(3,5,7)]
write.csv(as.data.frame(cbind(date1[53:61],output2)), "CI EB sensitivity rdi.csv", col.names = T, row.names = F)


#####################################
# OOS testing
####################################


oos_ci<-function(n){
  
  #n defines how many quarters before 2015Q4  
  ind=nrow(df_total_dev)-n
  df_oos=df_total_dev[1:ind,]
  ind0=nrow(df_oos)
  ind1=nrow(df_oos)+1
  ind2=nrow(df_oos)+npred
  ind4=nrow(df_total_dev)
  ind5=nrow(df_total_dev)-n+1
  
  #demean it
  x1=df_oos$rgdp_ag_lag_1-mean(df_oos$rgdp_ag_lag_1)
  x2=df_oos$rdi_ag_lag_4-mean(df_oos$rdi_ag_lag_4)
  mu1=mean(df_oos$rgdp_ag_lag_1)
  mu2= mean(df_oos$rdi_ag_lag_4)
  out_oos=lm(df_oos$ci_qd~x1+x2)
  
  ####################################
  #implement the model - out of sample
  ####################################
  
  ndata=nrow(df_oos)
  npred=1
  ind3=ndata+npred
  output_oos=as.data.frame(matrix(0, ndata+npred,7))
  input=endbal[1:ind5,]
  output_oos[, 1]=input$ci_bal
  output_oos[1:ndata, 2]=out_oos$fitted.values
  dummy1=ndata-1
  output_oos[2:ndata, 3]= output_oos[1:dummy1, 1] + output_oos[2:ndata, 2]
  
  #PREDICT
  indx=cbind(df_total_dev$rgdp_ag_lag_1-mu1,df_total_dev$rdi_ag_lag_4-mu2)
  xreg_base=as.data.frame(t(indx[ind5,]))
  names(xreg_base)=c("x1", "x2")
  fitted.base=as.data.frame(predict(out_oos, xreg_base))

  # get the values
  output_oos[ind5, 2]=fitted.base
  output_oos[ind5, 3]= output_oos[ndata, 1] + output_oos[ind1, 2]
  
  pct_error= 100*(output_oos[ind5,3]-output_oos[ind5,1])/output_oos[ind5,3]
  result_oos=as.data.frame(cbind(n, output_oos[ind1,1], output_oos[ind1,3],pct_error))
  return(result_oos)
}

oos_ci(1)
oos_ci(2)
oos_ci(3)
oos_ci(4)

#####################################
# Prediction CI
####################################
ndata=nrow(b1)
npred=9
output_ci=as.data.frame(matrix(0, ndata+npred,10))

D1=which(endbal$year==2003 & endbal$q==1)
D2=which(endbal$year==2015 & endbal$q==4)
input=endbal[D1:D2,]
output_ci[1:ndata, 1]=input$ci_qd
output_ci[1:ndata, 2]=out$fitted.values

#PREDICT
aaaa=which(base$year==2016 & base$q==1)
bbbb=which(base$year==2018 & base$q==1)

indx=cbind(base$rgdp_ag_lag_1-mu1,base$rdi_ag_lag_4-mu2)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2")
fitted.base=as.data.frame(predict(out, xreg_base, interval = "predict",level = 0.95))

indx=cbind(adverse$rgdp_ag_lag_1-mu1,adverse$rdi_ag_lag_4-mu2)
xreg_adverse=as.data.frame(indx[aaaa:bbbb,])
names(xreg_adverse)=c("x1", "x2")
fitted.adverse=predict(out, xreg_adverse, interval = "predict",level = 0.95)

indx=cbind(severe$rgdp_ag_lag_1-mu1,severe$rdi_ag_lag_4-mu2)
xreg_severe=as.data.frame(indx[aaaa:bbbb,])
names(xreg_severe)=c("x1", "x2")
fitted.severe=predict(out, xreg_severe, interval = "predict",level = 0.95)

output_ci[53:61, 2]=fitted.base[,1] #fit base
output_ci[53:61, 3]=fitted.base[,2] #lwr base
output_ci[53:61, 4]=fitted.base[,3] #upr base
output_ci[53:61, 5]=fitted.adverse[,1] #fit adverse
output_ci[53:61, 6]=fitted.adverse[,2] #lwr adverse
output_ci[53:61, 7]=fitted.adverse[,3] #upr adverse
output_ci[53:61,8]=fitted.severe[,1]#fit severe
output_ci[53:61,9]=fitted.severe[,2] #lwr severe
output_ci[53:61,10]=fitted.severe[,3] # upr Severe
colnames(output_ci)=c("Historical", "estimated_base_fit", "estimated_base_lwr", 
                      "estimated_base_upr", "estimated_adverse_fit", 
                      "estimated_adverse_lwr", "estimated_adverse_upr", 
                      "estimated_severe_fit", "estimated_severe_lwr",
                      "estimated_severe_upr")

write.csv(as.data.frame(cbind(date1,output_ci)), "ci_prediction_ci.csv", col.names = T, row.names = F)

