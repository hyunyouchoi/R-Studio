###################################################################
# Project: Bank of Hope
# PPNR Models - NIE Salary and Benefits
###################################################################  
#setwd("C:/Users/doxborrow/Desktop/BoH/Modeling/PPNR/NIE_ Salary and Benefits")

setwd("//useomvfs77/MCLP/Common/Clients/Bank of Hope/Model Development/Code/PPNR/NIE_ Salary and Benefits")

#library(DataAnalytics)
library(tseries)
library(urca)
#library (fUnitRoots)
library(lubridate)
library(forecast)
library(tseries)
library(CADFtest)
library (leaps)
library(data.table)
library(openxlsx)
library(car)
library(dplyr)

#read in the raw data
ppnr=read.csv("NIE salary and benefits.csv",header = TRUE)
names(ppnr)= c("Date", "Year", "q", "NIE_SB")
ind1=which(ppnr$Year==2005 & ppnr$q==1)
ind2=which(ppnr$Year==2015 & ppnr$q==4)
ppnr=ppnr[c(ind1:ind2),]
ppnr$date = as.Date(as.yearqtr(ppnr$Date, format = "%YQ%q"))

#plot the data
#jpeg(filename= "NIE_SB.jpg", width=720, height=480, bg="white" )
plot(c(1:nrow(ppnr)), ppnr$NIE_SB, type="l", col="red",lty="dotted",ylab="non_int")

# Plot
sb_area_df <- melt(ppnr[,c("date","NIE_SB")], id = "date")

sba_area_p <- ggplot(sb_area_df, aes(x=date, y=value/1000, fill=variable, group=variable)) + geom_area() + xlab("Date") + ylab("Salary and Benefits ($ Mill)") + ggtitle("Non-Interest Expense: Salary and Benefits") + theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=15)) + theme(legend.title=element_blank()) + theme(legend.position="bottom") + theme(legend.position="none")
sba_area_p


#make the transformations for the data
ppnr$NIE_SB_qd=ppnr$NIE_SB-lag(ppnr$NIE_SB)
ppnr$NIE_SB_ad=ppnr$NIE_SB-lag(ppnr$NIE_SB, 4)
ppnr$NIE_SB_qg=c(NA, diff(log(ppnr$NIE_SB)))
ppnr$NIE_SB_ag= log(ppnr$NIE_SB)-log(lag(ppnr$NIE_SB, 4))

#Stationarity test
max_lag= floor(12*(nrow(ppnr)/100)^(1/4))
pp.test(na.remove(ppnr$NIE_SB_qd), lshort = T) #qd is stationary
pp.test(na.remove(ppnr$NIE_SB_ad), lshort = T) #ad fails 
pp.test(na.remove(ppnr$NIE_SB_ag), lshort = T) #ag fails
pp.test(na.remove(ppnr$NIE_SB_qg), lshort = T) #qg is stationary

# Boxplot of PPNR variables  
boxplot(ppnr$NIE_SB_qg~ppnr$q, main="Salary and benefits", 
        xlab="season/quarter", ylab="NIE_SB QG")
acf(na.remove(ppnr$NIE_SB_qg), lag.max = 25)


boxplot(ppnr$NIE_SB_qd~ppnr$q, main="Salary and benefits", 
        xlab="season/quarter", ylab="NIE_SB QoQ Diff")
acf(na.remove(ppnr$NIE_SB_qd), lag.max = 25)

#######################
#read in the macro vars 
#######################

######
#base
######
base=read.csv("base.csv", header=T)
aaa=which(base$year==2005 & base$quarter==1)
bbb=which(base$year==2018 & base$quarter==1)
base=base[aaa:bbb,]

#########
#adverse
#########

adverse=read.csv("adverse.csv", header=T)
aaa=which(adverse$year==2005 & adverse$quarter==1)
bbb=which(adverse$year==2018 & adverse$quarter==1)
adverse=adverse[aaa:bbb,]

########
#severe
########
severe=read.csv("severe.csv", header=T)
aaa=which(severe$year==2005 & severe$quarter==1)
bbb=which(severe$year==2018 & severe$quarter==1)
severe=severe[aaa:bbb,]

##################
#development macro
##################
D1=which(base$year==2005 & base$q==1)
D2=which(base$year==2015 & base$q==4)
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
var_info[grepl("i3m", var_info$var),3] = "i3m_diff"
var_info[grepl("i5y", var_info$var),3] = "i5yr_diff"
var_info[grepl("i10y", var_info$var),3] = "i10yr_diff"
var_info[grepl("bbb", var_info$var),3] = "bbb_diff"
var_info[grepl("imort", var_info$var),3] = "imort_diff"
var_info[grepl("iprim", var_info$var),3] = "iprim_diff"
var_info[grepl("cppi", var_info$var),3] = "cppi_diff"
var_info[grepl("dji", var_info$var),3] = "dji_diff"
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
var_info[grepl("imort", var_info$var),6] =0
var_info[grepl("iprim", var_info$var),6] = 0
var_info[grepl("cppi", var_info$var),6] = 1
var_info[grepl("dji", var_info$var),6] = 1
var_info[grepl("VIX", var_info$var),6] = -1
var_info[grepl("vix", var_info$var),6] = -1
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
var_info[grepl("dji", var_info$var),2] = 2
var_info[grepl("VIX", var_info$var),2] = 3
var_info[grepl("vix", var_info$var),2] = 3
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
var_info[grepl("CCI", var_info$var),2] = 3
var_info[grepl("NCREIF", var_info$var),2] = 3

#####################
#Variable Selection 
#####################
#dependent var
D1=which(ppnr$Year==2005 & ppnr$q==1)
D2=which(ppnr$Year==2015 & ppnr$q==4)
dependent= as.data.frame(cbind(ppnr$NIE_SB_qd, ppnr$NIE_SB_qg))[c(D1:D2),]
names(dependent)= c("NIE_SB_qd","NIE_SB_qg")
date_col=as.data.frame(ppnr$Date)[c(D1:D2),]
b1=cbind(date_col, dependent)
names(b1)=c("Date", names(dependent))
b=data.table(b1)

c1=cbind(date_col, macro_dev)
names(c1)=c("Date", names(macro_dev))
c=data.table(c1)
a=data.table(var_info)

df_total_dev <- as.data.frame(cbind(b1, c1))

#######
#model
#######

source("StepFun.R")

fix_vars0=c("1")
mod1_NIE_SB_qd_sep=StepFun(a,b,c, tier=1,         #indicate which tier of variables to consider
                           y='NIE_SB_qd~',       #indicate response variable
                           thresh=c(0.05, 0.01, 0.001),    #significance level for SE based p-value and LR test based p-value for each tier
                           criteria='SE.p', #variable selection criteria; other values='bic', 'LR.p', 'SE.p', 'rsq'
                           vars0 = c("1"),          #model 0 variables
                           fix_vars0,    #indicate which variables are fixed
                           out.print=T         #indicate wheter intermediate output will be printed
)
# add ngdp_qg_lag_3
fix_vars0=c("1","ngdp_qg_lag_3")
mod1_NIE_SB_qd_sep=StepFun(a,b,c, tier=2,         #indicate which tier of variables to consider
                           y='NIE_SB_qd~',       #indicate response variable
                           thresh=c(0.05, 0.01, 0.001),    #significance level for SE based p-value and LR test based p-value for each tier
                           criteria='SE.p', #variable selection criteria; other values='bic', 'LR.p', 'SE.p', 'rsq'
                           vars0 = c("1","ngdp_qg_lag_3"),          #model 0 variables
                           fix_vars0,    #indicate which variables are fixed
                           out.print=T         #indicate wheter intermediate output will be printed
)
# add djia_qg_lag_1
fix_vars0=c("1","ngdp_qg_lag_3","djia_qg_lag_1")
mod1_NIE_SB_qd_sep=StepFun(a,b,c, tier=3,         #indicate which tier of variables to consider
                           y='NIE_SB_qd~',       #indicate response variable
                           thresh=c(0.05, 0.01, 0.001),    #significance level for SE based p-value and LR test based p-value for each tier
                           criteria='SE.p', #variable selection criteria; other values='bic', 'LR.p', 'SE.p', 'rsq'
                           vars0 = c("1","ngdp_qg_lag_3","djia_qg_lag_1"),          #model 0 variables
                           fix_vars0,    #indicate which variables are fixed
                           out.print=T         #indicate wheter intermediate output will be printed
)
# no variable added

# Save the alg model coefficients
coef_sb_alg <- mod1_NIE_SB_qd_sep[[1]]$final_model$coefficients
write.csv(coef_sb_alg, "coef_sb_alg.csv")


#####################
#Independent Variables - Demeaned
#####################
x1=c1$ngdp_qg_lag_3-mean(c1$ngdp_qg_lag_3)
mu1=mean(c1$ngdp_qg_lag_3)
sd1=sd(c1$ngdp_qg_lag_3)

x2=c1$djia_qg_lag_1-mean(c1$djia_qg_lag_1)
mu2=mean(c1$djia_qg_lag_1)
sd2=sd(c1$djia_qg_lag_1)

#################################
# Stationarity
#################################
summary(ur.df(na.remove(x1), lags=6, selectlags = 'BIC'))
summary(ur.df(na.remove(x2), lags=6, selectlags = 'BIC'))

#####################
# Model Estimation
#####################
out=lm(b1$NIE_SB_qd~x1+x2)
summary(out)

# Save the model coefficients
coef_sb <- summary(out)$coefficients
write.csv(coef_sb, "coef_sb.csv")

# Durbin Watson
durbinWatsonTest(out, 3)

#Multicolinearity
vif(out)

#Stationarity
pp.test(na.remove(base$ngdp_qg))
pp.test(na.remove(base$djia_qg))

#####################
# Residual tests 
#####################
out_res=out$residuals
out_res2=rstandard(out)

# Autocorrelations 
par(mfrow=c(1,2))
acf(out$residuals, main="")
pacf(out$residuals, main="")
par(mfrow=c(1,1))

#white noise tests
Box.test(out$residuals, type = "Ljung-Box", lag = 3) #null: independence ==> accept
durbinWatsonTest(out)

#Q-Q Plot
par(mfrow=c(1,1))
qqnorm(out_res2, ylab="Residuals", xlab="Quantiles of Standard Normal", main="NIE-Salary and Benefits") 
qqline(out_res2)

# Residual vs predicted
plot(out$fitted.values,out_res2, ylab="Residuals", xlab="Predicted Values", main="NIE_SB Model", ylim=c(-5, 5)) 
abline(0, 0)


#implement the model 
ndata=nrow(b1)
npred=9
output=as.data.frame(matrix(0, ndata+npred,7))

fitted.values=as.data.frame(c(NA, out$fitted.values))
output[1:ndata, 1]=ppnr$NIE_SB
output[1:ndata, 2]=fitted.values
dummy1=ndata-1
output[2:ndata, 3]= output[1:dummy1, 1] + output[2:ndata, 2]

plot(output[2:ndata, 3])
lines(output[2:ndata, 1], col='red')

#####################
#Scenario Forecasts
#####################
aaaa=which(base$year==2016 & base$quarter==1)
bbbb=which(base$year==2018 & base$quarter==1)

indx=cbind(base$ngdp_qg_lag_3-mu1, base$djia_qg_lag_1-mu2)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2")
fitted.base=as.data.frame(predict(out, xreg_base))

indx=cbind(adverse$ngdp_qg_lag_3-mu1,adverse$djia_qg_lag_1-mu2)
xreg_adverse=as.data.frame(indx[aaaa:bbbb,])
names(xreg_adverse)=c("x1", "x2")
fitted.adverse=as.data.frame(predict(out, xreg_adverse))


indx=cbind(severe$ngdp_qg_lag_3-mu1,severe$djia_qg_lag_1-mu2)
xreg_severe=as.data.frame(indx[aaaa:bbbb,])
names(xreg_severe)=c("x1", "x2")
fitted.severe=as.data.frame(predict(out, xreg_severe))

output[45:53, 2]=fitted.base
output[45, 3]=output[ndata, 1] + output[45, 2]

for (i in 2:npred){
  ab=44+i
  ac=44+i-1
  output[ab, 3]= output[ac, 3] + output[ab, 2]
}

output[1:ndata,c(4,5)]=output[1:ndata, c(2,3)]
output[1:ndata,c(6,7)]=output[1:ndata, c(2,3)]

output[45:53, 4]=fitted.adverse
output[45,5]= output[ndata, 1] + output[45, 4]

for (i in 2:npred){
  ab=44+i
  ac=44+i-1
  output[ab, 5]= output[ac, 5] + output[ab, 4]
}

output[45:53, 6]=fitted.severe
output[45,7]=output[ndata, 1] + output[45, 6]

for (i in 2:npred){
  ab=44+i
  ac=44+i-1
  output[ab, 7]= output[ac, 7] + output[ab, 6]
}
output[1, c(3,5,7)]=output[1,1]

output[which(output[,1]==0),1]=NA


#plot together
date1 = seq(ISOdate(2005,1,1), by = "quarter", length.out = 53)
plot(date1, output[,3], type='l', ylab="NIE Salary & Benefits")
lines(date1, output[,5], col='blue')
lines(date1, output[,7], col='red')
lines(date1, output[,1], col='green')
legend("topleft", legend= c("base", "adverse", "severe", "Historical"), fill=c("black", "blue", "red", "green"))
colnames(output)=c("Historical", "estimated_base", "estimated_base_bal", "estimated_adverse", "adverse_bal", "estimated_severe", "severe_bal")
output_NIE_SB=output
write.csv(as.data.frame(cbind(date1,output)), "NIE_SB_Projections.csv", col.names = T, row.names = F)

sb_gg_df <- data.frame(Date = date1, Actual = output$Historical, Base = output$estimated_base_bal, Adverse = output$adverse_bal, Severe = output$severe_bal, Fitted = append(output$estimated_base_bal[1:(length(output$estimated_base_bal)-9)],rep(NA,9)))


sb_gg_df_p <- melt(sb_gg_df, id="Date")

# Plot of forecasts
sb_fcst_plot <- ggplot(sb_gg_df_p, aes(x = Date, y = value/1000, color = variable, group = variable)) + 
  geom_line() + 
  xlab("Date") + ylab("Salary and Benefits (Mill $)") + ggtitle("Scenario Forecasts") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=15)) + theme(legend.title=element_blank())
sb_fcst_plot

########################
#95% Confidence Interval
########################

#Generate the 95% confidence interval for the base case. Default in R is 95%. 
Date_ci = as.Date(as.yearqtr(paste(base$year[aaaa:bbbb],base$q[aaaa:bbbb],sep="-")))
indx=cbind(base$ngdp_qg_lag_3-mu1, base$djia_qg_lag_1-mu2)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2")
fitted.base_ci=as.data.frame(predict(out, xreg_base, interval = "confidence"))
base_ci_df <- as.data.frame(cbind(Date_ci,fitted.base_ci))
colnames(base_ci_df) <- c("Date", "Fcst","Lower","Upper")
write.csv(base_ci_df, "CB Confidence Interval Data.csv", row.names = F)

base_ci_df <- melt(base_ci_df, id="Date")

gg_in_df <- data.frame(Date = as.Date(as.yearqtr(b1$Date, format = "%YQ%q")), Actual = b1$NIE_SB_qd, Fitted = output$estimated_base[1:length(b1$NIE_SB_qd)])
gg_in_df <- melt(gg_in_df, id="Date")

gg_fcst_df_ci <- rbind(gg_in_df,base_ci_df)

# Plot the historical actual and fitted with base 95% forecast
sb_fcst_plot_ci <- ggplot(gg_fcst_df_ci, aes(x = Date, y = value/1000, color = variable, group = variable)) + 
  geom_line() + 
  xlab("Date") + ylab("Salary and Benefits (Mill $)") + ggtitle("NIE Salary and Benefits Baseline Forecast and 95% CI") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=15)) + theme(legend.title=element_blank())
sb_fcst_plot_ci

#####################
#sensitivity Analysis 
#####################
#GDP
ndata=nrow(b1)
npred=9
output=as.data.frame(matrix(0, ndata+npred,7))

fitted.values=as.data.frame(c(NA, out$fitted.values))
output[1:ndata, 1]=ppnr$NIE_SB
output[1:ndata, 2]=fitted.values
dummy1=ndata-1
output[2:ndata, 3]= output[1:dummy1, 1] + output[2:ndata, 2]
aaaa=which(base$year==2016 & base$quarter==1)
bbbb=which(base$year==2018 & base$quarter==1)

indx=cbind(base$ngdp_qg_lag_3-mu1, base$djia_qg_lag_1-mu2)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2")
fitted.base=as.data.frame(predict(out, xreg_base))

indx=cbind(base$ngdp_qg_lag_3-mu1+sd1, base$djia_qg_lag_1-mu2)
xreg_1sd=as.data.frame(indx[aaaa:bbbb,])
names(xreg_1sd)=c("x1", "x2")
fitted.1sd=as.data.frame(predict(out, xreg_1sd))

indx=cbind(base$ngdp_qg_lag_3-mu1+2*sd1, base$djia_qg_lag_1-mu2)
xreg_2sd=as.data.frame(indx[aaaa:bbbb,])
names(xreg_2sd)=c("x1", "x2")
fitted.2sd=as.data.frame(predict(out, xreg_2sd))

output[45:53, 2]=fitted.base
output[45, 3]=output[ndata, 1] + output[45, 2]

for (i in 2:npred){
  ab=44+i
  ac=44+i-1
  output[ab, 3]= output[ac, 3] + output[ab, 2]
}

output[1:ndata,c(4,5)]=output[1:ndata, c(2,3)]
output[1:ndata,c(6,7)]=output[1:ndata, c(2,3)]

output[45:53, 4]=fitted.1sd
output[45,5]= output[ndata, 1] + output[45, 4]

for (i in 2:npred){
  ab=44+i
  ac=44+i-1
  output[ab, 5]= output[ac, 5] + output[ab, 4]
}

output[45:53, 6]=fitted.2sd
output[45,7]=output[ndata, 1] + output[45, 6]

for (i in 2:npred){
  ab=44+i
  ac=44+i-1
  output[ab, 7]= output[ac, 7] + output[ab, 6]
}
output[1, c(3,5,7)]=output[1,1]
output[which(output[,1]==0),1]=NA
date2=base$date.x[45:53]
sens_out=as.data.frame(cbind(date2, output[c(43:51),c(3,5,7)]))
colnames(sens_out) <- c("Date","Baseline","1_std","2_std")
gdp_sens <- sens_out
gdp_sens$Date <-  as.Date(as.yearqtr(gdp_sens$Date, format = "Q%q %Y"))
write.csv(gdp_sens, "sensitivity NIE_SB GDP.csv", row.names = F)

# Plot
gdp_df_gg <- melt(gdp_sens, id = "Date")
gdp_df_gg_p <- ggplot(data = gdp_df_gg, mapping = aes(x = Date, y = value/1000, group = variable, color = variable)) + geom_line() + theme(legend.position = 'bottom') + theme(legend.title=element_blank()) + xlab("Date") + ylab("Salary and Benefits ($ Mill)") + ggtitle("Nominal GDP Quarterly Growth Rate") + theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=15)) + theme(legend.position="bottom")
gdp_df_gg_p


#DJIA
ndata=nrow(b1)
npred=9
output=as.data.frame(matrix(0, ndata+npred,7))

fitted.values=as.data.frame(c(NA, out$fitted.values))
output[1:ndata, 1]=ppnr$NIE_SB
output[1:ndata, 2]=fitted.values
dummy1=ndata-1
output[2:ndata, 3]= output[1:dummy1, 1] + output[2:ndata, 2]
aaaa=which(base$year==2016 & base$quarter==1)
bbbb=which(base$year==2018 & base$quarter==1)

indx=cbind(base$ngdp_qg_lag_3-mu1, base$djia_qg_lag_1-mu2)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2")
fitted.base=as.data.frame(predict(out, xreg_base))

indx=cbind(base$ngdp_qg_lag_3-mu1, base$djia_qg_lag_1-mu2+sd2)
xreg_1sd=as.data.frame(indx[aaaa:bbbb,])
names(xreg_1sd)=c("x1", "x2")
fitted.1sd=as.data.frame(predict(out, xreg_1sd))

indx=cbind(base$ngdp_qg_lag_3-mu1, base$djia_qg_lag_1-mu2+2*sd2)
xreg_2sd=as.data.frame(indx[aaaa:bbbb,])
names(xreg_2sd)=c("x1", "x2")
fitted.2sd=as.data.frame(predict(out, xreg_2sd))

output[45:53, 2]=fitted.base
output[45, 3]=output[ndata, 1] + output[45, 2]

for (i in 2:npred){
  ab=44+i
  ac=44+i-1
  output[ab, 3]= output[ac, 3] + output[ab, 2]
}

output[1:ndata,c(4,5)]=output[1:ndata, c(2,3)]
output[1:ndata,c(6,7)]=output[1:ndata, c(2,3)]

output[45:53, 4]=fitted.1sd
output[45,5]= output[ndata, 1] + output[45, 4]

for (i in 2:npred){
  ab=44+i
  ac=44+i-1
  output[ab, 5]= output[ac, 5] + output[ab, 4]
}

output[45:53, 6]=fitted.2sd
output[45,7]=output[ndata, 1] + output[45, 6]

for (i in 2:npred){
  ab=44+i
  ac=44+i-1
  output[ab, 7]= output[ac, 7] + output[ab, 6]
}
output[1, c(3,5,7)]=output[1,1]
output[which(output[,1]==0),1]=NA
date2=base$date.x[45:53]
sens_out=as.data.frame(cbind(date2, output[c(43:51),c(3,5,7)]))
colnames(sens_out) <- c("Date","Baseline","1_std","2_std")
djia_sens <- sens_out
djia_sens$Date <-  as.Date(as.yearqtr(djia_sens$Date, format = "Q%q %Y"))
write.csv(djia_sens, "sensitivity NIE_SB DJIA.csv", row.names = F)

# Plot
djia_df_gg <- melt(djia_sens, id = "Date")
djia_df_gg_p <- ggplot(data = djia_df_gg, mapping = aes(x = Date, y = value/1000, group = variable, color = variable)) + geom_line() + theme(legend.position = 'bottom') + theme(legend.title=element_blank()) + xlab("Date") + ylab("Salary and Benefits ($ Mill)") + ggtitle("DJIA Quarterly Growth Rate") + theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=15)) + theme(legend.position="bottom")
djia_df_gg_p



# Boot strap the regression coefficients
#rownames(mod1_NIE_SB_qd_sep[[1]]$final_model$coefficients)

model <- "NIE_SB_qd ~ ngdp_qg_lag_3 + djia_qg_lag_1"
summary(lm(model, data = df_total_dev))

set.seed(2)
# Bootstrap 95% CI for regression coefficients 
library(boot)
# function to obtain regression weights 
bs = function(data, indices, formula) {
  d = data[indices,] # allows boot to select sample 
  fit = lm(formula, data=d)
  return(coef(fit))
}
# bootstrapping with 100 replications 
results = boot(
  data=df_total_dev, 
  statistic=bs, 
  R=5000, 
  formula=model)


Names = names(results$t0)
SEs = sapply(data.frame(results$t), sd)
Coefs = as.numeric(results$t0)
zVals = Coefs / SEs
Pvals = 2*pnorm(-abs(zVals))

Formatted_Results = cbind(Names, Coefs, SEs, zVals, Pvals)

# Pot coefficient density

for (i in 1:length(names(results$t0))){
  plot(density(results$t[,i]), main = paste(names(results$t0)[i],"Density",sep=" - "))
}


# Back-testing Analysis

bt_ppnr_df <- ppnr
bt_ppnr_df$date <- as.Date(as.yearqtr(bt_ppnr_df$Date, format = "%YQ%q"))

# Macro Variables
bt_macro_df <- as.data.frame(c1)
bt_macro_df$date <- as.Date(as.yearqtr(bt_macro_df$Date, format = "%YQ%q"))

bt_macro_df$ngdp_qg_lag_3_dm <- bt_macro_df$ngdp_qg_lag_3 - mean(bt_macro_df$ngdp_qg_lag_3)
bt_macro_df$djia_qg_lag_1_dm <- bt_macro_df$djia_qg_lag_1 - mean(bt_macro_df$djia_qg_lag_1)

# Merge the end bal and macro data
bt_df <- merge(x = bt_ppnr_df, y = bt_macro_df, by.x = "date", by.y = "date")
#names(bt_df)[names(bt_df) == 'quarter'] <- 'q'

# Partition the data
in1 <- which(bt_df$year==2005 & bt_df$q ==1)
in2 <- which(bt_df$year ==2014 & bt_df$q ==4)
out1 <- which(bt_df$year ==2015 & bt_df$q ==1)
out2 <- which(bt_df$year ==2015 & bt_df$q ==4)
insample <- bt_df[in1:in2,]
outsample <- bt_df[out1:out2,]

# Estimate the model on the insample portion
out_bt <- lm(NIE_SB_qd ~ ngdp_qg_lag_3_dm + djia_qg_lag_1_dm, data = insample)
summary(out_bt)

# Add the fitted values to the insample data
insample$fitted <- append(NA,out_bt$fitted.values)

# Forecast added to the out of sample data
outsample$fitted <- predict(out_bt, outsample)

# Append the insample and out of sample data and select the columns
bt_df_final <- rbind(insample, outsample)
bt_df_final <- bt_df_final[,c("date","NIE_SB","fitted")]
fitted_bal <- bt_df_final[1:(nrow(bt_df_final)-1), 2] + bt_df_final[2:nrow(bt_df_final), 3]
bt_df_final$fitted_bal <- append(NA, fitted_bal)

# Plot
bt_df_final_p <- melt(bt_df_final[,c("date","NIE_SB","fitted_bal")], id = "date")
bt_df_final_plot <- ggplot(data = bt_df_final_p, mapping = aes(x = date, y = value, group = variable, color = variable)) + geom_line() + theme(legend.position = 'bottom') + theme(legend.title=element_blank()) + xlab("Date") + ylab("($)") + ggtitle("Out-of-Sample Forecast") + theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=15)) + theme(legend.position="bottom")
bt_df_final_plot

# Table 
fcst_table <- bt_df_final[out1:out2,c("date","NIE_SB","fitted_bal")]
fcst_table$p_error <- round(100*(fcst_table$fitted_bal-fcst_table$NIE_SB)/fcst_table$NIE_SB,2)
row.names(fcst_table) <- NULL

fcst_table



