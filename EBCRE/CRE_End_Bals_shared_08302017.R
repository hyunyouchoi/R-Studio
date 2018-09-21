###################################################################
# Project: Bank of Hope
# CRE Ending Balances
###################################################################  
#setwd("C:/Users/doxborrow/Desktop/BoH/Modeling/Ending Balance")
setwd("//useomvfs77/MCLP/Common/Clients/Bank of Hope/Model Development/Code/Ending Balance")

library(tseries)
library(urca)
library(lubridate)
library(forecast)
library(tseries)
library(CADFtest)
library (leaps)
library(openxlsx)
library(car)
library(lmtest)
library(orcutt)
library(lmtest)
library(dplyr)
library(ggplot2)
library(data.table)
####################

# Historical CRE loan balances

#####################
#read in the raw data
#####################

#read in the raw data
endbal <- read.csv("Ending Balances.csv",header = TRUE, stringsAsFactors = F)
endbal <- endbal[,c(4,2,3)]
names(endbal) <- c("date", "ci_bal", "cre_bal")
endbal <- as.data.frame(endbal)

# #Difference transformation 
endbal$cre_qd <- c(NA, diff(endbal$cre_bal))
endbal$cre_yd <- endbal$cre_bal-lag(endbal$cre_bal, 4)
endbal$cre_qg <- c(NA, diff(log(endbal$cre_bal)))
endbal$cre_yg <- log(endbal$cre_bal)-lag(log(endbal$cre_bal), 4)
endbal$ci_qd <- c(NA, diff(endbal$ci_bal))
endbal$ci_yd <- endbal$ci_bal-lag(endbal$ci_bal, 4)
endbal$ci_qg <- c(NA, diff(log(endbal$ci_bal)))
endbal$ci_yg <- log(endbal$ci_bal)-lag(log(endbal$ci_bal), 4)

#making dataset ready for merge
endbal$year <- year(mdy(endbal$date))
endbal$month <- month(mdy(endbal$date))
endbal$q[endbal$month %in% c(1,2,3)] <- 1
endbal$q[endbal$month %in% c(4,5,6)] <- 2
endbal$q[endbal$month %in% c(7,8,9)] <- 3
endbal$q[endbal$month %in% c(10,11,12)] <- 4
endbal$month <- NULL


#max_lag
max_lag <- floor(12*(nrow(endbal)/100)^(1/4))


#######################
#read in the macro vars 
#######################

######
#base
######
base <- read.csv("macro_base.csv", header = T)
aaa <- which(base$year==2003 & base$q==1)
bbb <- which(base$year==2018 & base$q==4)
base <- base[aaa:bbb,]

#########
#adverse
#########

adverse <- read.csv("macro_adverse.csv", header=T)
aaa <- which(adverse$year==2003 & adverse$q==1)
bbb <- which(adverse$year==2018 & adverse$q==4)
adverse <- adverse[aaa:bbb,]

########
#severe
########
severe <- read.csv("macro_severe.csv", header=T)
aaa <- which(severe$year==2003 & severe$q==1)
bbb <- which(severe$year==2018 & severe$q==4)
severe <- severe[aaa:bbb,]

##################
#development macro
##################
D1 <- which(base$year==2003 & base$q==1)
D2 <- which(base$year==2015 & base$q==4)
macro_dev <- base[c(D1:D2), ]
macro_input <- macro_dev[,-c(1,2,3)]

########################################
# Create the dep_var matrix
########################################

var.names <- colnames(macro_input)
var_info <- as.data.frame(matrix(0, length(var.names), 6 ))
names(var_info) <- c("var", "tier", "base", "lag", "diff", "sign")
var_info[,1] <- var.names
var_info[,5] <- 0

#diff
var_info[grepl("_qd", var_info$var),5] <- TRUE
var_info[grepl("_yd", var_info$var),5] <- TRUE
var_info[grepl("_qg", var_info$var),5] <- TRUE
var_info[grepl("_yg", var_info$var),5] <- TRUE

#lag
var_info[grepl("_lag1", var_info$var),4] <- 1
var_info[grepl("_lag2", var_info$var),4] <- 2
var_info[grepl("_lag3", var_info$var),4] <- 3
var_info[grepl("_lag4", var_info$var),4] <- 4

#var.base
var_info[grepl("ngdp", var_info$var),3] <- "ngdp_g"
var_info[grepl("rgdp", var_info$var),3] <- "rgdp_g"
var_info[grepl("rdpi", var_info$var),3] <- "rdpi_g"
var_info[grepl("ndpi", var_info$var),3] <- "ndpi_g"
var_info[grepl("ur", var_info$var),3] <- "ur_diff"
var_info[grepl("cpi", var_info$var),3] <- "cpi"
var_info[grepl("tr3m", var_info$var),3] <- "tr3m_diff"
var_info[grepl("tr5y", var_info$var),3] <- "tr5yr_diff"
var_info[grepl("tr10y", var_info$var),3] <- "tr10yr_diff"
var_info[grepl("bbb", var_info$var),3] <- "bbb_diff"
var_info[grepl("mort", var_info$var),3] <- "mort_diff"
var_info[grepl("prime", var_info$var),3] <- "prime_diff"
var_info[grepl("cppi", var_info$var),3] <- "cppi_diff"
var_info[grepl("djtsmx", var_info$var),3] <- "djtsmx_diff"
var_info[grepl("vix", var_info$var),3] <- "vix"
var_info[grepl("hpi", var_info$var),3] <- "hpi_g"
var_info[grepl("spr10", var_info$var),3] <- "spread"
var_info[grepl("spr10_q", var_info$var),3] <- "spread_diff"
var_info[grepl("spr10_y", var_info$var),3] <- "spread_diff"
var_info[grepl("rgpdi_eq", var_info$var),3] <- "rgpdi_eq"
var_info[grepl("gpdi_eq", var_info$var),3] <- "gpdi_eq"
var_info[grepl("pfi_nonres", var_info$var),3] <- "pfi_nonres"
var_info[grepl("willreit", var_info$var),3] <- "willreit"


#sign
var_info[grepl("gdp", var_info$var),6] <- 1
var_info[grepl("rdpi", var_info$var),6] <- 1
var_info[grepl("ndpi", var_info$var),6] <- 1
var_info[grepl("ur", var_info$var),6] <- -1
var_info[grepl("cpi", var_info$var),6] <- -1
var_info[grepl("tr3m", var_info$var),6] <- 0
var_info[grepl("tr5y", var_info$var),6] <- 0
var_info[grepl("tr10y", var_info$var),6] <- 0
var_info[grepl("mort", var_info$var),6] <- 0
var_info[grepl("prime", var_info$var),6] <- 0
var_info[grepl("bbb", var_info$var),6] <- 0
var_info[grepl("djtsmx", var_info$var),6] <- 1
var_info[grepl("hpi", var_info$var),6] <- 1
var_info[grepl("vix", var_info$var),6] <- -1
var_info[grepl("cppi", var_info$var),6] <- 1
var_info[grepl("spr10", var_info$var),6] <- -1
var_info[grepl("rgpdi_eqp", var_info$var),6] <- 1
var_info[grepl("gpdi_eqp", var_info$var),6] <- 1
var_info[grepl("pfi_nonres", var_info$var),6] <- 1 
var_info[grepl("willreit", var_info$var),6] <- 1


# Tier
var_info[grepl("rgdp", var_info$var),2] <- 1
var_info[grepl("ngdp", var_info$var),2] <- 1
var_info[grepl("rdpi", var_info$var),2] <- 1
var_info[grepl("ndpi", var_info$var),2] <- 1
var_info[grepl("ur", var_info$var),2] <- 1
var_info[grepl("cpi", var_info$var),2] <- 3
var_info[grepl("tr3m", var_info$var),2] <- 2
var_info[grepl("tr5y", var_info$var),2] <- 2
var_info[grepl("tr10y", var_info$var),2] <- 2
var_info[grepl("mort", var_info$var),2] <- 3
var_info[grepl("prime", var_info$var),2] <- 3
var_info[grepl("bbb", var_info$var),2] <- 2
var_info[grepl("djtsmx", var_info$var),2] <- 2
var_info[grepl("hpi", var_info$var),2] <- 1
var_info[grepl("vix", var_info$var),2] <- 3
var_info[grepl("cppi", var_info$var),2] <- 1
var_info[grepl("spr10", var_info$var),2] <- 1
var_info[grepl("rgpdi_eq", var_info$var),2] <- 2
var_info[grepl("gpdi_eq", var_info$var),2] <- 2
var_info[grepl("pfi_nonres", var_info$var),2] <- 2 
var_info[grepl("willreit", var_info$var),2] <- 2


#####################
#Variable Selection 
#####################
first.obs <- which(base$year==2003 & base$q==1)
ndata <- which(base$year==2015 & base$q==4)

date_col <- as.data.frame(base$Date[first.obs:ndata])
colnames(date_col) <- "Date"

aaa <- which(endbal$year==2003 & endbal$q==1)
bbb <- which(endbal$year==2015 & endbal$q==4)
loan_input <- endbal[aaa:bbb,-c(1,12,13)]

b1 <- cbind(date_col, loan_input)
names(b1) <- c("Date", names(loan_input))
b <- data.table(b1)

c1 <- cbind(date_col, macro_input)
names(c1) <- c("Date", names(macro_input))
c <- data.table(c1)
a <- data.table(var_info)

#################################
# CRE Model
#################################
 source("StepFun.R")
 fix_vars0 <- c("1")
 v110_model2_cre_qg_sep=StepFun(a,b,c, tier=1,         #indicate which tier of variables to consider
                                y='cre_qg~',       #indicate response variable
                                thresh=c(0.05, 0.01, 0.001),    #significance level for SE based p-value and LR test based p-value for each tier
                                criteria='SE.p', #variable selection criteria; other values='bic', 'LR.p', 'SE.p', 'rsq'
                                vars0 = c("1"),          #model 0 variables
                                fix_vars0,    #indicate which variables are fixed
                                out.print=T         #indicate wheter intermediate output will be printed
 )

 fix_vars0=c("1", "hpi_yg_lag4", "ndpi_grw_yoy_lag4", "spr10_yd")
 v110_model2_cre_qg_sep=StepFun(a,b,c, tier=2,         #indicate which tier of variables to consider
                                y='cre_qg~',       #indicate response variable
                                thresh=c(0.05, 0.01, 0.001),    #significance level for SE based p-value and LR test based p-value for each tier
                                criteria='SE.p', #variable selection criteria; other values='bic', 'LR.p', 'SE.p', 'rsq'
                                vars0 = c("1", "hpi_yg_lag4", "ndpi_grw_yoy_lag4", "spr10_yd"),          #model 0 variables
                                fix_vars0,    #indicate which variables are fixed
                                out.print=T         #indicate wheter intermediate output will be printed
 )
 fix_vars0=c("1", "hpi_yg_lag4", "ndpi_grw_yoy_lag4", "spr10_yd", "tr3m_yd_lag3")
 v110_model2_cre_qg_sep=StepFun(a,b,c, tier=3,         #indicate which tier of variables to consider
                                y='cre_qg~',       #indicate response variable
                                thresh=c(0.05, 0.01, 0.001),    #significance level for SE based p-value and LR test based p-value for each tier
                                criteria='SE.p', #variable selection criteria; other values='bic', 'LR.p', 'SE.p', 'rsq'
                                vars0 = c("1", "hpi_yg_lag4", "ndpi_grw_yoy_lag4", "spr10_yd", "tr3m_yd_lag3"),          #model 0 variables
                                fix_vars0,    #indicate which variables are fixed
                                out.print=T         #indicate wheter intermediate output will be printed
 )

# Display final model
# Algorithm selected model
alg_model <- v110_model2_cre_qg_sep[[1]]$final_model

# Save the model coefficients
coef_alg_cre_eb <- as.data.frame(alg_model$coefficients)
write.csv(coef_alg_cre_eb, "coef_alg_cre_eb.csv")

# Plot the model drivers
gg_plot_df <- c[,c("Date","hpi_yg_lag4","ndpi_grw_yoy_lag4","spr10_yd","tr3m_yd_lag3")]
gg_plot_df <- as.data.frame(gg_plot_df)
gg_plot_df$Date <-  as.Date(gg_plot_df$Date,"%m/%d/%Y")
gg_plot_df_drivers <- melt(gg_plot_df, id = "Date")

gg_plot_drivers <- ggplot(data = gg_plot_df_drivers, mapping = aes(x = Date, y = value, group = variable, color = variable)) + geom_line() + theme(legend.position = 'bottom') + theme(legend.title=element_blank()) + xlab("Date") + ylab("Value") + ggtitle("Model Drivers") + theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=15)) + theme(legend.position="bottom")
gg_plot_drivers

ggsave("cre_eb_drivers.png", width = 7, height = 7)

# Final demeaned model
# Demean the model variables
x1 <- c$hpi_yg_lag4- mean(c$hpi_yg_lag4)
x2 <- c$ndpi_grw_yoy_lag4- mean(c$ndpi_grw_yoy_lag4)
x3 <- c$spr10_yd- mean(c$spr10_yd)
x4 <- c$tr3m_yd_lag3- mean(c$tr3m_yd_lag3)

mu1 <- mean(c$hpi_yg_lag4)
mu2 <- mean(c$ndpi_grw_yoy_lag4)
mu3 <- mean(c$spr10_yd)
mu4 <- mean(c$tr3m_yd_lag3)

sd1 <- sd(c$hpi_yg_lag4)
sd2 <- sd(c$ndpi_grw_yoy_lag4)
sd3 <- sd(c$spr10_yd)
sd4 <- sd(c$tr3m_yd_lag3)

# Model
out <- lm(b$cre_qg~x1+x2+x3+x4)
summary(out)

out_res<-rstandard(out)
out_fit<-fitted.values(out)

# Durbin Watson
durbinWatsonTest(out,3)

# Save the model coefficients
coef_final_model <- as.data.frame(summary(out)$coefficients)
write.csv(coef_final_model, "coef_final_model.csv")

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
plot(out_fit,out_res, ylab="Residuals", xlab="Q-o-Q Growth Rate", main="CRE Ending Balance") 
abline(0, 0)

#################################
# Stationarity
#################################
summary(ur.df(na.remove(c$hpi_yg_lag4), type='drift', lags=6, selectlags = 'BIC'))
summary(ur.df(na.remove(c$ndpi_grw_yoy_lag4), type='drift', lags=6, selectlags = 'BIC'))
summary(ur.df(na.remove(c$spr10_yd), type='drift', lags=6, selectlags = 'BIC'))
summary(ur.df(na.remove(c$tr3m_yd_lag3), type='drift', lags=6, selectlags = 'BIC'))

pp.test(na.remove(c$ndpi_grw_yoy_lag4))

summary(ur.df(na.remove(x1), type='drift', lags=6, selectlags = 'BIC'))
summary(ur.df(na.remove(x2), type='drift', lags=6, selectlags = 'BIC'))
summary(ur.df(na.remove(x3), type='drift', lags=6, selectlags = 'BIC'))
summary(ur.df(na.remove(x4), type='drift', lags=6, selectlags = 'BIC'))

#################################
#implement the model
#################################

ndata <- nrow(b1)
npred<-9
output<-as.data.frame(matrix(0, ndata+npred,7))

D1 <- which(endbal$year==2003 & endbal$q==1)
D2 <- which(endbal$year==2015 & endbal$q==4)
input <- endbal[D1:D2,]
output[1:ndata, 1]=input$cre_bal
output[1:ndata, 2]=out$fitted.values
dummy1 <- ndata-1
output[2:ndata, 3]= exp(log(output[1:dummy1, 1]) + output[2:ndata, 2])
# plot(output[2:ndata, 3])
# lines(output[2:ndata, 1], col='red')

#PREDICT
aaaa <- which(base$year==2016 & base$q==1)
bbbb <- which(base$year==2018 & base$q==1)

indx <- cbind(base$hpi_yg_lag4-mu1,base$ndpi_grw_yoy_lag4-mu2,base$spr10_yd-mu3, base$tr3m_yd_lag3-mu4)
xreg_base <- as.data.frame(indx[aaaa:bbbb,])
names(xreg_base) <- c("x1", "x2", "x3", "x4")
fitted.base <- as.data.frame(predict(out, xreg_base))

indx <- cbind(adverse$hpi_yg_lag4-mu1,adverse$ndpi_grw_yoy_lag4-mu2,adverse$spr10_yd-mu3, adverse$tr3m_yd_lag3-mu4)
xreg_adverse <- as.data.frame(indx[aaaa:bbbb,])
names(xreg_adverse) <- c("x1", "x2", "x3", "x4")
fitted.adverse <- predict(out, xreg_adverse)

indx <- cbind(severe$hpi_yg_lag4-mu1,severe$ndpi_grw_yoy_lag4-mu2,severe$spr10_yd-mu3, severe$tr3m_yd_lag3-mu4)
xreg_severe <- as.data.frame(indx[aaaa:bbbb,])
names(xreg_severe) <- c("x1", "x2", "x3", "x4")
fitted.severe <- predict(out, xreg_severe)

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
eb_projections <- cbind(date1,output)
write.csv(as.data.frame(cbind(date1,output)), "cre EB Projections.csv", row.names = F)

eb_projections_df_p <- eb_projections[,c("date1","Historical","estimated_base_bal","adverse_bal","severe_bal")]
colnames(eb_projections_df_p) <- c("Date","Actual","Base","Adverse","Severe")
eb_projections_df_p$Fitted <- append(eb_projections_df_p$Base[1:52],rep(NA,npred))
eb_projections_df_p <- melt(eb_projections_df_p, id="Date")

# Plot of projections
eb_projections_plot <- ggplot(eb_projections_df_p, aes(x = Date, y = value, color = variable, group = variable)) + 
  geom_line() + 
  xlab("Date") + ylab("Ending Balance ($)") + ggtitle("CRE Ending Balance") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=15)) + theme(legend.title=element_blank())
eb_projections_plot


#######################
# Sensitivity Analysis
#######################
##HPI 
ndata<-nrow(b1)
npred<-9
output<-as.data.frame(matrix(0, ndata+npred,7))

D1<-which(endbal$year==2003 & endbal$q==1)
D2<-which(endbal$year==2015 & endbal$q==4)
input<-endbal[D1:D2,]
output[1:ndata, 1]<-input$cre_bal
output[1:ndata, 2]<-out$fitted.values
dummy1<-ndata-1
output[2:ndata, 3]<- exp(log(output[1:dummy1, 1]) + output[2:ndata, 2])
plot(output[2:ndata, 3])
lines(output[2:ndata, 1], col='red')

#PREDICT
aaaa <- which(base$year==2016 & base$q==1)
bbbb <- which(base$year==2018 & base$q==1)

indx=cbind(base$hpi_yg_lag4-mu1,base$ndpi_grw_yoy_lag4-mu2,base$spr10_yd-mu3, base$tr3m_yd_lag3-mu4)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2", "x3", "x4")
fitted.base=as.data.frame(predict(out, xreg_base))

indx_1sd=cbind(base$hpi_yg_lag4-mu1+sd1,base$ndpi_grw_yoy_lag4-mu2,base$spr10_yd-mu3, base$tr3m_yd_lag3-mu4)
xreg_1sd=as.data.frame(indx_1sd[aaaa:bbbb,])
names(xreg_1sd)=c("x1", "x2", "x3", "x4")
fitted.1sd=as.data.frame(predict(out, xreg_1sd))

indx_2sd=cbind(base$hpi_yg_lag4-mu1+2*sd1,base$ndpi_grw_yoy_lag4-mu2,base$spr10_yd-mu3, base$tr3m_yd_lag3-mu4)
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
hpi_df <- as.data.frame(cbind(date1[53:61],output2))
write.csv(hpi_df, "cre EB sensitivity hpi.csv", row.names = F)

colnames(hpi_df) <- c("Date","Baseline","1_std","2_std")

# Plot
hpi_df_gg <- melt(hpi_df, id = "Date")
hpi_df_gg_p <- ggplot(data = hpi_df_gg, mapping = aes(x = Date, y = value, group = variable, color = variable)) + geom_line() + theme(legend.position = 'bottom') + theme(legend.title=element_blank()) + xlab("Date") + ylab("CRE Ending Balance ($)") + ggtitle("HPI Yearly Growth Rate") + theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=15)) + theme(legend.position="bottom")
hpi_df_gg_p

ggsave("hpi_sensitive.png", width = 7, height = 7)

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
plot(output[2:ndata, 3])
lines(output[2:ndata, 1], col='red')

#PREDICT
aaaa=which(base$year==2016 & base$q==1)
bbbb=which(base$year==2018 & base$q==1)

indx=cbind(base$hpi_yg_lag4-mu1,base$ndpi_grw_yoy_lag4-mu2,base$spr10_yd-mu3, base$tr3m_yd_lag3-mu4)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2", "x3", "x4")
fitted.base=as.data.frame(predict(out, xreg_base))

indx_1sd=cbind(base$hpi_yg_lag4-mu1,base$ndpi_grw_yoy_lag4-mu2+sd2,base$spr10_yd-mu3, base$tr3m_yd_lag3-mu4)
xreg_1sd=as.data.frame(indx_1sd[aaaa:bbbb,])
names(xreg_1sd)=c("x1", "x2", "x3", "x4")
fitted.1sd=as.data.frame(predict(out, xreg_1sd))

indx_2sd=cbind(base$hpi_yg_lag4-mu1,base$ndpi_grw_yoy_lag4+2*sd2-mu2,base$spr10_yd-mu3, base$tr3m_yd_lag3-mu4)
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
ndpi_df <- as.data.frame(cbind(date1[53:61],output2))
write.csv(ndpi_df, "cre EB sensitivity ndpi.csv", row.names = F)

colnames(ndpi_df) <- c("Date","Baseline","1_std","2_std")

# Plot
ndpi_df_gg <- melt(ndpi_df, id = "Date")
ndpi_df_gg_p <- ggplot(data = ndpi_df_gg, mapping = aes(x = Date, y = value, group = variable, color = variable)) + geom_line() + theme(legend.position = 'bottom') + theme(legend.title=element_blank()) + xlab("Date") + ylab("CRE Ending Balance ($)") + ggtitle("Nominal Disposable Inc.") + theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=15)) + theme(legend.position="bottom")
ndpi_df_gg_p

ggsave("ndpi_sensitive.png", width = 7, height = 7)

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

indx=cbind(base$hpi_yg_lag4-mu1,base$ndpi_grw_yoy_lag4-mu2,base$spr10_yd-mu3, base$tr3m_yd_lag3-mu4)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2", "x3", "x4")
fitted.base=as.data.frame(predict(out, xreg_base))

indx_1sd=cbind(base$hpi_yg_lag4-mu1,base$ndpi_grw_yoy_lag4-mu2,base$spr10_yd-mu3+sd3, base$tr3m_yd_lag3-mu4)
xreg_1sd=as.data.frame(indx_1sd[aaaa:bbbb,])
names(xreg_1sd)=c("x1", "x2", "x3", "x4")
fitted.1sd=as.data.frame(predict(out, xreg_1sd))

indx_2sd=cbind(base$hpi_yg_lag4-mu1,base$ndpi_grw_yoy_lag4-mu2,base$spr10_yd-mu3+2*sd3, base$tr3m_yd_lag3-mu4)
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
spr10_df <- as.data.frame(cbind(date1[53:61],output2))
write.csv(spr10_df, "cre EB sensitivity spr10.csv", row.names = F)

colnames(spr10_df) <- c("Date","Baseline","1_std","2_std")

# Plot
spr10_df_gg <- melt(spr10_df, id = "Date")
spr10_df_gg_p <- ggplot(data = spr10_df_gg, mapping = aes(x = Date, y = value, group = variable, color = variable)) + geom_line() + theme(legend.position = 'bottom') + theme(legend.title=element_blank()) + xlab("Date") + ylab("CRE Ending Balance ($)") + ggtitle("10 Yr. Treasury Credit Spr.") + theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=15)) + theme(legend.position="bottom")
spr10_df_gg_p

ggsave("10yr_spread_sensitive.png", width = 7, height = 7)

# tr3m
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

indx=cbind(base$hpi_yg_lag4-mu1,base$ndpi_grw_yoy_lag4-mu2,base$spr10_yd-mu3, base$tr3m_yd_lag3-mu4)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2", "x3", "x4")
fitted.base=as.data.frame(predict(out, xreg_base))

indx_1sd=cbind(base$hpi_yg_lag4-mu1,base$ndpi_grw_yoy_lag4-mu2,base$spr10_yd-mu3, base$tr3m_yd_lag3-mu4+sd4)
xreg_1sd=as.data.frame(indx_1sd[aaaa:bbbb,])
names(xreg_1sd)=c("x1", "x2", "x3", "x4")
fitted.1sd=as.data.frame(predict(out, xreg_1sd))

indx_2sd=cbind(base$hpi_yg_lag4-mu1,base$ndpi_grw_yoy_lag4-mu2,base$spr10_yd-mu3, base$tr3m_yd_lag3-mu4+2*sd4)
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

tr3m_df <- as.data.frame(cbind(date1[53:61],output2))
colnames(tr3m_df) <- c("Date","Baseline","1_std","2_std")
write.csv(tr3m_df, "cre EB sensitivity tr3m.csv", row.names = F)

# Plot
tr3m_df_gg <- melt(tr3m_df, id = "Date")
tr3m_df_gg_p <- ggplot(data = tr3m_df_gg, mapping = aes(x = Date, y = value, group = variable, color = variable)) + geom_line() + theme(legend.position = 'bottom') + theme(legend.title=element_blank()) + xlab("Date") + ylab("CRE Ending Balance ($)") + ggtitle("3-Month US Treasury Rate") + theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=15)) + theme(legend.position="bottom")
tr3m_df_gg_p

ggsave("3m_treasury_sensitive.png", width = 7, height = 7)

########################
#95% Confidence Interval
########################

#Generate the 95% confidence interval for the base case. Default in R is 95%. 
Date_ci = as.Date(as.yearqtr(paste(base$year[aaaa:bbbb],base$q[aaaa:bbbb],sep="-")))
indx=cbind(base$hpi_yg_lag4-mu1,base$ndpi_grw_yoy_lag4-mu2,base$spr10_yd-mu3, base$tr3m_yd_lag3-mu4)
xreg_base=as.data.frame(indx[aaaa:bbbb,])
names(xreg_base)=c("x1", "x2", "x3", "x4")
fitted.base=as.data.frame(predict(out, xreg_base, interval = "confidence"))
base_ci_df <- as.data.frame(cbind(Date_ci,fitted.base*100))
colnames(base_ci_df) <- c("Date", "Fcst","Lower","Upper")
write.csv(base_ci_df, "CRE Confidence Interval Data.csv", row.names = F)

base_ci_df <- melt(base_ci_df, id="Date")


gg_in_df <- data.frame(Date = as.Date(b1$Date,format = "%m/%d/%Y"),Actual = b1$cre_qg*100, Fitted = output$estimated_base[1:length(b1$cre_qg)]*100)
gg_in_df <- melt(gg_in_df, id="Date")

gg_fcst_df_ci <- rbind(gg_in_df,base_ci_df)

# Plot the historical actual and fitted with base 95% forecast
cre_EB_fcst_plot_ci <- ggplot(gg_fcst_df_ci, aes(x = Date, y = value, color = variable, group = variable)) + 
  geom_line() + 
  xlab("Date") + ylab("Loss Severity (%)") + ggtitle("CRE Ending Balance Growth Rate Forecast and 95% CI") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=15)) + theme(legend.title=element_blank())
cre_EB_fcst_plot_ci

ggsave("CRE_End_BAL_Base_95_Fcst-Plot.png", width = 7, height = 7)


###################################################################
# Boot strap analysis

#demean it
x1_b <- c$hpi_yg_lag4- mean(c$hpi_yg_lag4)
x2_b <- c$ndpi_grw_yoy_lag4- mean(c$ndpi_grw_yoy_lag4)
x3_b <- c$spr10_yd- mean(c$spr10_yd)
x4_b <- c$tr3m_yd_lag3- mean(c$tr3m_yd_lag3)

# Make up the data set
df_total_dev = data.frame(cre_qg = b$cre_qg, hpi_yg_lag4 = x1_b, ndpi_grw_yoy_lag4 = x2_b, spr10_yd = x3_b, tr3m_yd_lag3 = x4_b)

# Boot strap the CRE regression coefficients
model <- "cre_qg ~ hpi_yg_lag4 + ndpi_grw_yoy_lag4 + spr10_yd + tr3m_yd_lag3"
summary(lm(model, data = df_total_dev))

# Bootstrap 95% CI for regression coefficients 
library(boot)
# function to obtain regression weights 
bs = function(data, indices, formula) {
  d = data[indices,] # allows boot to select sample 
  fit = lm(formula, data=d)
  return(coef(fit))
}
# bootstrapping function
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

# Plot coefficient density
par(mfrow=c(1,1))
for (i in 1:length(names(results$t0))){
  plot(density(results$t[,i]), main = paste(names(results$t0)[i],"Density",sep=" - "))
}


# Back-testing Analysis

bt_endbal_df <- endbal
bt_endbal_df$Date <- as.Date(bt_endbal_df$date, "%m/%d/%Y")

# Macro Variables
bt_macro_df <- as.data.frame(c)
bt_macro_df$Date <- as.Date(bt_macro_df$Date, "%m/%d/%Y")
bt_macro_df$year <- year(bt_macro_df$Date)
bt_macro_df$q <- quarter(bt_macro_df$Date)
bt_macro_df$hpi_yg_lag4_dm <- bt_macro_df$hpi_yg_lag4 - mean(bt_macro_df$hpi_yg_lag4)
bt_macro_df$ndpi_grw_yoy_lag4_dm <- bt_macro_df$ndpi_grw_yoy_lag4 - mean(bt_macro_df$ndpi_grw_yoy_lag4)
bt_macro_df$spr10_yd_dm <- bt_macro_df$spr10_yd - mean(bt_macro_df$spr10_yd)
bt_macro_df$tr3m_yd_lag3_dm <- bt_macro_df$tr3m_yd_lag3 - mean(bt_macro_df$tr3m_yd_lag3)

# Merge the end bal and macro data
bt_df <- merge(x = bt_endbal_df, y = bt_macro_df, by.x = "Date", by.y = "Date")

# Partition the data
in1 <- which(bt_df$year.x==2003 & bt_df$q.y==1)
in2 <- which(bt_df$year.x==2014 & bt_df$q.y==4)
out1 <- which(bt_df$year.x==2015 & bt_df$q.y==1)
out2 <- which(bt_df$year.x==2015 & bt_df$q.y==4)
insample <- bt_df[in1:in2,]
outsample <- bt_df[out1:out2,]

# Estimate the model on the insample portion
out_bt <- lm(cre_qg ~ hpi_yg_lag4_dm + ndpi_grw_yoy_lag4_dm + spr10_yd_dm + tr3m_yd_lag3_dm, data = insample)
summary(out_bt)

# Add the fitted values to the insample data
insample$fitted <- out_bt$fitted.values

# Forecast added to the out of sample data
outsample$fitted <- predict(out_bt, outsample)

# Append the insample and out of sample data and select the columns
bt_df_final <- rbind(insample, outsample)
bt_df_final <- bt_df_final[,c("Date","cre_bal","fitted")]
fitted_bal <- exp(log(bt_df_final[1:(nrow(bt_df_final)-1), 2]) + bt_df_final[2:nrow(bt_df_final), 3])
bt_df_final$fitted_bal <- append(NA, fitted_bal)
bt_df_final$label <- append(rep("Fitted",nrow(insample)),rep("Forecast",nrow(outsample)))

# Plot
bt_df_final_p <- melt(bt_df_final[,c("Date","cre_bal","fitted_bal")], id = "Date")
bt_df_final_plot <- ggplot(data = bt_df_final_p, mapping = aes(x = Date, y = value, group = variable, color = variable)) + geom_line() + theme(legend.position = 'bottom') + theme(legend.title=element_blank()) + xlab("Date") + ylab("CRE Ending Balance ($)") + ggtitle("Out-of-Sample Forecast") + theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=15)) + theme(legend.position="bottom")
bt_df_final_plot

# Table 
fcst_table <- bt_df_final[out1:out2,c("Date","cre_bal","fitted_bal")]
fcst_table$p_error <- round(100*(fcst_table$fitted_bal-fcst_table$cre_bal)/fcst_table$cre_bal,2)
row.names(fcst_table) <- NULL

fcst_table







