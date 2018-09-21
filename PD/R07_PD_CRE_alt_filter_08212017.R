##############################################################################
## File Name: R07_PD_CRE.R
## Author: KPMG
## Date: 7/5/2017 Created
## Purpose: To build PD model for BOH CRE portfolio 
##############################################################################
#setwd("//useomvfs77/MCLP/Common/Clients/Bank of Hope/Model Development/PD Models/R Code/Misc Analysis/Alternative Filter")
setwd("C:/Users/ic07949/Desktop/KPMG/Model Development/development code and data/PD")

requirements <- c("dplyr","reshape2","data.table","zoo","ggplot2","pROC","boot","tidyr","lazyeval","Hmisc","corrplot","car")
for(rr in requirements){
  if(! rr %in% installed.packages()) install.packages(rr)
}
for(requirement in requirements){if(!(requirement %in% installed.packages())) install.packages(requirement)}
lapply(requirements, require, character.only=T)

## load data
cre_dev <- readRDS("df_boh_base_v2_08212017.RDS")
cre_dev$fileDate <- as.Date(cre_dev$fileDate, "%Y-%m-%d")

# CRE and Date filter
cre_dev <- filter(cre_dev, portfolio_id == "CRE" & as.Date(fileDate) <= as.Date("2016-03-31") & as.Date(fileDate) >= as.Date("2007-12-31") )

# Getting description of the current sample
y_filter_1 <- describe(cre_dev$y)
bal_sum_1 <- sum(cre_dev$current_balance)

## create final input variables
## (1) variable prop_res: 1-4 residential and multifamily
##     BBCN: 10, 11
##     wilshire: 11, 12

cre_dev$property_type <- ifelse(cre_dev$callReportCodeDescr %in% c("Conv 5+ Residential Prop","CONVENTIONAL 5+ RESIDENTIAL"),10,cre_dev$property_type)

cre_dev$prop_res <- ifelse( (cre_dev$boh_id == "bbcn" & cre_dev$property_type %in% c(10 , 11) ) |
                            (cre_dev$boh_id == "wilshire" & cre_dev$property_type %in% c(11 , 12) )  ,
                           1,
                           0)

# Missing Wilshire Property type filter
cre_dev <- filter(cre_dev, !(boh_id == "wilshire" & is.na(property_type)))

## (2a) variable prop_retail: Retail shopping center
##     BBCN: 15, 16, 17, 18
##     wilshire: 1
cre_dev$prop_retail <- ifelse( (cre_dev$boh_id == "bbcn" & cre_dev$property_type %in% c(15, 16, 17, 18) ) |
                                      (cre_dev$boh_id == "wilshire" & cre_dev$property_type == 1 )  ,
                                    1,
                                    0)
## (2b) variable prop_auto: Gas Stations, Car Washes, and Auto Repair Centers
##     BBCN: 33, 34, 36
##     wilshire: 7, 8, 16
cre_dev$prop_auto <- ifelse( (cre_dev$boh_id == "bbcn" & cre_dev$property_type %in% c(33, 34, 36) ) |
                               (cre_dev$boh_id == "wilshire" & cre_dev$property_type %in% c(7, 8, 16) )  ,
                             1,
                             0)

## (2c) variable prop_hotel: Hotels and Motels
##     BBCN: 28,29
##     wilshire: 5
cre_dev$prop_hotel <- ifelse( (cre_dev$boh_id == "bbcn" & cre_dev$property_type %in% c(28, 29) ) |
                                (cre_dev$boh_id == "wilshire" & cre_dev$property_type == 5 )  ,
                              1,
                              0)

## (3) variable boh_rating1
cre_dev$boh_rating1 <- ifelse(cre_dev$boh_rating %in% c(0,1,2,3), "R1",
                              ifelse(cre_dev$boh_rating %in% c(4,1000), "R2",
                                     ifelse(cre_dev$boh_rating %in% c(2000,3000), "R3", "error")  
                                     ))
#R1 and R2 variables
cre_dev$boh_rating1_R1 <- ifelse(cre_dev$boh_rating1 == "R1",1, 0)
cre_dev$boh_rating1_R2 <- ifelse(cre_dev$boh_rating1 == "R2",1, 0)

#Lagged R1 and R2
cre_dev <- cre_dev %>% group_by(account_id) %>% mutate(boh_rating1_R1_l = lag(boh_rating1_R1)) %>% as.data.frame()
cre_dev <- cre_dev %>% group_by(account_id) %>% mutate(boh_rating1_R2_l = lag(boh_rating1_R2)) %>% as.data.frame()

# Update lagged R1 and R2 values with current value
cre_dev$boh_rating1_R1_l <- ifelse(is.na(cre_dev$boh_rating1_R1_l),cre_dev$boh_rating1_R1,cre_dev$boh_rating1_R1_l)
cre_dev$boh_rating1_R2_l <- ifelse(is.na(cre_dev$boh_rating1_R2_l),cre_dev$boh_rating1_R2,cre_dev$boh_rating1_R2_l)

## (4) variable rgdp_qg_lag_2_neg
cre_dev$rgdp_qg_lag_2_neg <- ifelse(cre_dev$rgdp_qg_lag_2 >= 0, 0, cre_dev$rgdp_qg_lag_2 )

## (5) variable CAUR_yd_3
cre_dev$CAUR_yd_3 <- ifelse(cre_dev$CAUR_yd >= 3, 3, cre_dev$CAUR_yd)

## (6) variable CAHPI_ag_6
cre_dev$CAHPI_ag_6 <- ifelse(cre_dev$CAHPI_ag >= 6, 6, cre_dev$CAHPI_ag)

## (7) variable POB_95
cre_dev$POB_95 <- ifelse(cre_dev$POB <= 95, 95, cre_dev$POB)

## (8) Wilshire Dummy
cre_dev$wilshire_d <- ifelse(cre_dev$boh_id == "wilshire",1,0)

# Getting description of the current sample
y_filter_2 <- describe(cre_dev$y)
bal_sum_2 <- sum(cre_dev$current_balance)

# Number of default events per period
def_events_df <- cre_dev[,which(colnames(cre_dev) %in% c("fileDate","y")),drop=F]
def_events_df_sum <- def_events_df %>% group_by(fileDate) %>% summarise(Defaults = sum(y)) %>% data.frame()
Obs <- def_events_df %>% group_by(fileDate) %>% tally() %>% data.frame()
def_events_df_sum <- merge(def_events_df_sum, Obs)
colnames(def_events_df_sum) <- c("Date","No. of Defaults","Observations")

## Partition the data into training and testing samples
set.seed(2017)
# Sample fraction
sample_fraction <- .8
cre_dev_training <- cre_dev %>% sample_frac(sample_fraction)

# Getting description of the in sample
y_filter_in <- describe(cre_dev_training$y)
bal_sum_in <- sum(cre_dev_training$current_balance)

##Out of sample data
cre_dev_testing <- cre_dev[-which(rownames(cre_dev) %in% rownames(cre_dev_training)),]

# Getting description of the out sample
y_filter_out <- describe(cre_dev_testing$y)
bal_sum_out <- sum(cre_dev_testing$current_balance)

# Table of dependent variable sample stats
dep_var_filter_stats <- as.data.frame(rbind(y_filter_1$counts, y_filter_2$counts,y_filter_in$counts, y_filter_out$counts))

bal_sum_stats <- as.data.frame(rbind(bal_sum_1,bal_sum_2,bal_sum_in,bal_sum_out))

dep_var_filter_stats <- cbind(dep_var_filter_stats,bal_sum_stats)
rownames(dep_var_filter_stats) <- c("CRE & (2007 Q4 - 2016 Q1)","Wilshire Property Type","80% In-sample","20% Out-of-sample")
colnames(dep_var_filter_stats)[8] <- "Sum of Curr. Bal."
dep_var_filter_stats

###################################
## Logistic regression - Training

# Main Model
model <- y ~ boh_rating1_R1_l + boh_rating1_R2_l + POB_95 + CAUR_yd + NCREIF_Property_Index_ag_lag_1 + prop_res
fit <- glm(model, family = binomial(link = "logit"), data = cre_dev_training)
summary(fit)

# Multicollinearity of fitted model
vif(fit)

# Model regressed on full sample
fit_full <- glm(model, family = binomial(link = "logit"), data = cre_dev)
summary(fit_full)

# McFadden's pseudo R squared for the main model
pR2 <- 1 - fit$deviance / fit$null.deviance 
pR2

###################################
# Output the main regression table in academic format
stargazer::stargazer(fit, fit_full, type = "text", model.numbers = F, column.labels = c("Train","Full","Wilshire"))

# Save the model coefficients
coef_cre <- as.data.frame(summary(fit)$coefficients)

coef_cre$X <- rownames(coef_cre)

# In-sample Prediction
prob <- predict(fit,type=c("response"))
cre_dev_training$p_hat <- prob
roc_in_df <- data.frame(y = cre_dev_training$y, prob)

# Find AUC
auc_in <- round(auc(roc_in_df$y, roc_in_df$prob),4)
roc_in <- roc(y ~ prob, data = roc_in_df) 

## get ROC and AUC 
plot(roc_in, main =paste0("CRE PD ROC IN \n AUC = ", auc_in))

# Out-of-sample #
## Out-sample prediction p_hat for each account
predict_out <- predict(fit, cre_dev_testing, type="response")
cre_dev_testing$p_hat <- predict_out

## ROC and AUC
roc_out <-  data.frame(predict = predict_out, y = cre_dev_testing$y)
roc_out_plot <- roc(y ~ predict, data = roc_out)
auc_out <- round(as.numeric(roc_out_plot$auc),4)

plot(roc_out_plot, main =paste0("CRE PD ROC OUT \n AUC = ", auc_out))

## Quarterly average PD in-sample
cre_pd_quarterly_in <- subset(cre_dev_training, select = c(fileDate, y, p_hat))
cre_pd_quarterly_in <- aggregate(cre_pd_quarterly_in[,2:3], list(cre_pd_quarterly_in$fileDate), mean)
setnames(cre_pd_quarterly_in, old = c("Group.1","y","p_hat"),
         new = c("fileDate", "Actual", "Fitted"))
cre_pd_quarterly_in <- melt(cre_pd_quarterly_in, id = "fileDate")

cbPalette <- c("#000000", "#0072B2")

cre_pd_training_plot <- ggplot(cre_pd_quarterly_in, aes(x=fileDate, y = value, color=variable)) + 
  geom_line() + scale_colour_manual(values=cbPalette) + xlab("Date") + ylab("Default Rate") + ggtitle("Ave. Default Rate CRE - In-sample") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=15)) + theme(legend.title=element_blank()) + theme(legend.position="bottom")
cre_pd_training_plot

## Quarterly average PD out-sample
cre_pd_quarterly_out <- subset(cre_dev_testing, select = c(fileDate, y, p_hat))
cre_pd_quarterly_out <- aggregate(cre_pd_quarterly_out[,2:3], list(cre_pd_quarterly_out$fileDate), mean)
setnames(cre_pd_quarterly_out, old = c("Group.1","y","p_hat"),
         new = c("fileDate", "Actual", "Fitted"))
cre_pd_quarterly_out <- melt(cre_pd_quarterly_out, id = "fileDate")

cbPalette <- c("#000000", "#0072B2")

cre_pd_out_plot <- ggplot(cre_pd_quarterly_out, aes(x=fileDate, y = value, color=variable)) + 
  geom_line() + scale_colour_manual(values=cbPalette) + xlab("Date") + ylab("Default Rate") + ggtitle("Ave. Default Rate CRE - Out-of-sample") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=15)) + theme(legend.title=element_blank()) + theme(legend.position="bottom")
cre_pd_out_plot

###########################################
## Forecast for 3 scenarios

for(scenario in c("base", "adverse", "severe")){

  print(paste0("==== ", scenario, " ===="))
  
  cre_forecast <- readRDS(paste0("df_boh_",scenario,"_v2", ".RDS"))
  cre_forecast$fileDate <- as.Date(cre_forecast$fileDate, "%Y-%m-%d")
  cre_forecast <- filter(cre_forecast, portfolio_id == "CRE" & as.Date(fileDate) > as.Date("2016-03-31") )
  
  ## create final input variables
  ## (1) variable prop_res: 1-4 residential and multifamily
  ##     BBCN: 10, 11
  ##     wilshire: 11, 12
  cre_forecast$property_type <- ifelse(cre_forecast$callReportCodeDescr %in% c("Conv 5+ Residential Prop","CONVENTIONAL 5+ RESIDENTIAL"),10,cre_forecast$property_type)
  
  cre_forecast$prop_res <- ifelse( (cre_forecast$boh_id == "bbcn" & cre_forecast$property_type %in% c(10 , 11) ) |
                                     (cre_forecast$boh_id == "wilshire" & cre_forecast$property_type %in% c(11 , 12) )  ,
                                   1,
                                   0)
  
  # Missing Wilshire Property type filter
  cre_forecast <- filter(cre_forecast, !(boh_id == "wilshire" & is.na(property_type)))

  
  ## (2a) variable prop_retail: Retail shopping center
  ##     BBCN: 15, 16, 17, 18
  ##     wilshire: 1
  cre_forecast$prop_retail <- ifelse( (cre_forecast$boh_id == "bbcn" & cre_forecast$property_type %in% c(15, 16, 17, 18) ) |
                                        (cre_forecast$boh_id == "wilshire" & cre_forecast$property_type == 1 )  ,
                                      1,
                                      0)
  ## (2b) variable prop_auto: Gas Stations, Car Washes, and Auto Repair Centers
  ##     BBCN: 33, 34, 36
  ##     wilshire: 7, 8, 16
  cre_forecast$prop_auto <- ifelse( (cre_forecast$boh_id == "bbcn" & cre_forecast$property_type %in% c(33, 34, 36) ) |
                                (cre_forecast$boh_id == "wilshire" & cre_forecast$property_type %in% c(7, 8, 16) )  ,
                              1,
                              0)
  ## (2c) variable prop_hotel: Hotels and Motels
  ##     BBCN: 28,29
  ##     wilshire: 5
  cre_forecast$prop_hotel <- ifelse( (cre_forecast$boh_id == "bbcn" & cre_forecast$property_type %in% c(28, 29) ) |
                                  (cre_forecast$boh_id == "wilshire" & cre_forecast$property_type == 5 )  ,
                                1,
                                0)
  
  
  ## (3) variable boh_rating1
  cre_forecast$boh_rating1 <- ifelse(cre_forecast$boh_rating %in% c(0,1,2,3), "R1",
                                     ifelse(cre_forecast$boh_rating %in% c(4,1000), "R2",
                                            ifelse(cre_forecast$boh_rating %in% c(2000,3000), "R3", "error")  
                                     ))
  
  cre_forecast$boh_rating1_R1 <- ifelse(cre_forecast$boh_rating1 == "R1",1, 0)
  cre_forecast$boh_rating1_R2 <- ifelse(cre_forecast$boh_rating1 == "R2",1, 0)
  
  #Lagged R1 and R2
  cre_forecast <- cre_forecast %>% group_by(account_id) %>% mutate(boh_rating1_R1_l = lag(boh_rating1_R1))
  cre_forecast <- cre_forecast %>% group_by(account_id) %>% mutate(boh_rating1_R2_l = lag(boh_rating1_R2))
  
  # Update lagged values with current value
  cre_forecast$boh_rating1_R1_l <- ifelse(is.na(cre_forecast$boh_rating1_R1_l),cre_forecast$boh_rating1_R1,cre_forecast$boh_rating1_R1_l)
  cre_forecast$boh_rating1_R2_l <- ifelse(is.na(cre_forecast$boh_rating1_R2_l),cre_forecast$boh_rating1_R2,cre_forecast$boh_rating1_R2_l)
  
  ## (4) variable rgdp_qg_lag_2_neg
  cre_forecast$rgdp_qg_lag_2_neg <- ifelse(cre_forecast$rgdp_qg_lag_2 >= 0, 0, cre_forecast$rgdp_qg_lag_2 )
  
  ## (5) variable CAUR_yd_3
  cre_forecast$CAUR_yd_3 <- ifelse(cre_forecast$CAUR_yd >= 3, 3, cre_forecast$CAUR_yd)
  
  ## (6) variable CAHPI_ag_6
  cre_forecast$CAHPI_ag_6 <- ifelse(cre_forecast$CAHPI_ag >= 6, 6, cre_forecast$CAHPI_ag)
  
  ## (7) variable POB_95
  cre_forecast$POB_95 <- ifelse(cre_forecast$POB <= 95, 95, cre_forecast$POB)
  
  ## Wilshire Dummy
  cre_forecast$wilshire_d <- ifelse(cre_forecast$boh_id == "wilshire",1,0)
  
  
  ## PD forecast p_hat for each account
  cre_forecast <- as.data.table(cre_forecast)
  cre_forecast$p_hat <- as.matrix (cre_forecast[, coef_cre$X[-1],with = FALSE]) %*% coef_cre$Estimate[-1] + 
    coef_cre$Estimate[1] 
  cre_forecast$p_hat <- 1/(1+exp(-cre_forecast$p_hat))
  
  
  ## quarterly average PD
  cre_pd_quarterly_9Q <- subset(cre_forecast, select = c(fileDate, p_hat))
  cre_pd_quarterly_9Q <- aggregate(cre_pd_quarterly_9Q[,2], list(cre_pd_quarterly_9Q$fileDate), mean)
  
  setnames(cre_pd_quarterly_9Q, old = c("Group.1","p_hat"),
           new = c("fileDate", "value"))
  cre_pd_quarterly_9Q$variable <- scenario
  cre_pd_quarterly_9Q <- cre_pd_quarterly_9Q[,c(1,3,2)]
  assign(paste0("cre_pd_quarterly_",scenario), cre_pd_quarterly_9Q)
}

# Connect the historical and forecast data
cre_pd_quarterly_9Q <- rbind(cre_pd_quarterly_base, cre_pd_quarterly_adverse, cre_pd_quarterly_severe)
cre_pd_quarterly_all <- rbind(cre_pd_quarterly_in, cre_pd_quarterly_9Q)
setnames(cre_pd_quarterly_all, old = c("variable", "value"), new = c("scenario","PD"))

## Forecast plot
cbPalette <- c("#000000", "#0072B2", "#006600", "#E69F00", "#D55E00")
cre_pd_plot <- ggplot(cre_pd_quarterly_all, aes(x = fileDate, y = PD, color = scenario)) + 
  geom_line() + scale_colour_manual(values=cbPalette) + 
  ggtitle("BOH CRE PD") + xlab("Date") + ylab("Default Rate") + ggtitle("Average Default Rate CRE") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=15)) + theme(legend.title=element_blank()) + theme(legend.position="bottom")
cre_pd_plot

# Summary stats per sample
options(scipen=999)

sum_nms <- c("y","prop_res","boh_rating1_R1_l","boh_rating1_R2_l","CAUR_yd_3","POB_95","NCREIF_Property_Index_ag_lag_1")
training_df <- as.data.frame.matrix(cre_dev_training)
testing_df <- as.data.frame.matrix(cre_dev_testing)

# Make the summary stats table between the samples
cre_dev_mean <- apply(cre_dev[,which(colnames(cre_dev) %in% sum_nms),drop=F],2,function (x) round(mean(x),4))
cre_dev_in_mean <- apply(training_df[,which(colnames(training_df) %in% sum_nms),drop=F],2,function (x) round(mean(x),4))
cre_dev_out_mean <- apply(testing_df[,which(colnames(testing_df) %in% sum_nms),drop=F],2,function (x) round(mean(x),4))

cre_dev_sd <- apply(cre_dev[,which(colnames(cre_dev) %in% sum_nms),drop=F],2,function (x) round(sd(x),4))
cre_dev_in_sd <- apply(training_df[,which(colnames(training_df) %in% sum_nms),drop=F],2,function (x) round(sd(x),4))
cre_dev_out_sd <- apply(testing_df[,which(colnames(testing_df) %in% sum_nms),drop=F],2,function (x) round(sd(x),4))

cre_dev_max <- apply(cre_dev[,which(colnames(cre_dev) %in% sum_nms),drop=F],2,function (x) round(max(x),4))
cre_dev_in_max <- apply(training_df[,which(colnames(training_df) %in% sum_nms),drop=F],2,function (x) round(max(x),4))
cre_dev_out_max <- apply(testing_df[,which(colnames(testing_df) %in% sum_nms),drop=F],2,function (x) round(max(x),4))

cre_dev_min <- apply(cre_dev[,which(colnames(cre_dev) %in% sum_nms),drop=F],2,function (x) round(min(x),4))
cre_dev_in_min <- apply(training_df[,which(colnames(training_df) %in% sum_nms),drop=F],2,function (x) round(min(x),4))
cre_dev_out_min <- apply(testing_df[,which(colnames(testing_df) %in% sum_nms),drop=F],2,function (x) round(min(x),4))

cre_dev_n <- apply(cre_dev[,which(colnames(cre_dev) %in% sum_nms),drop=F],2, length)
cre_dev_in_n <- apply(training_df[,which(colnames(training_df) %in% sum_nms),drop=F],2, length)
cre_dev_out_n <- apply(testing_df[,which(colnames(testing_df) %in% sum_nms),drop=F],2, length)

cre_df_sample_stats <- rbind(
  cre_dev_mean, cre_dev_in_mean, cre_dev_out_mean,
  cre_dev_sd, cre_dev_in_sd, cre_dev_out_sd,
  cre_dev_max, cre_dev_in_max, cre_dev_out_max,
  cre_dev_min, cre_dev_in_min, cre_dev_out_min,
  cre_dev_n, cre_dev_in_n, cre_dev_out_n
  )

rownames(cre_df_sample_stats) <- c("Mean (All Obs)","Mean (Train)","Mean (Test)","SD (All Obs)","SD (Train)","SD (Test)","Max (All Obs)","Max (Train)","Max (Test)","Min (All Obs)","Min (Train)","Min (Test)","Obs (All Obs)","Obs (Train)","Obs (Test)")

cre_df_sample_stats

#################
# Coefficient Stability
# Repeated Sample Forecasts

sample_number <- 10
sample_fraction <- .8

set.seed(20170808)
cre_dev_training_s <- cre_dev %>% sample_frac(sample_fraction)
cre_dev_testing_s <- cre_dev[-which(rownames(cre_dev) %in% rownames(cre_dev_training_s)),]

# Sample from the df
predict_s <- list()
predict_date_s <- list()
s_name <- list()
for (i in 1:sample_number){
  ##In sample data
  set.seed(i)
  df_sample_in <- cre_dev %>% sample_frac(sample_fraction)
  ##Out of sample data
  df_sample_out <- cre_dev[-which(rownames(cre_dev) %in% rownames(df_sample_in)),]
  ##Estimate the model
  logit_s <- glm(model, family = binomial(link = "logit"), data = df_sample_in)
  predict_s[[i]] <- predict(logit_s, df_sample_out, type="response")
  predict_date_s[[i]] <- df_sample_out$fileDate
  s_name[[i]] <- rep(paste("Sample",i,sep="_"),nrow(df_sample_out))
}

predict_s_tmp <- data.frame(Predict = unlist(predict_s))
predict_date_s_tmp <- data.frame(Date = as.Date(unlist(predict_date_s)))
s_name_tmp <- data.frame(Sample = unlist(s_name))

Prediction_df <- cbind(predict_date_s_tmp, s_name_tmp, predict_s_tmp)
Prediction_df1 <- aggregate(x = Prediction_df[,"Predict"],FUN = mean,by = list(Date = Prediction_df$Date, Sample = Prediction_df$Sample))
names(Prediction_df1)[names(Prediction_df1) == 'x'] <- 'value'

actual_df <- aggregate(x = cre_dev_testing_s[,"y"],FUN = mean,by = list(Date = cre_dev_testing_s$fileDate))
names(actual_df)[names(actual_df) == 'x'] <- 'value'
actual_df$Sample <- "Actual"

Prediction_df_gg <- rbind(actual_df, Prediction_df1)

# Plot of all Forecasts 
sample_fcst_p <- ggplot(data = Prediction_df_gg, aes(x = Date, y = value, group = Sample, color = Sample)) + geom_line() + xlab("Date") + ylab("Default Rate") + ggtitle("Ave. Default Rate CRE Out-of-Sample") + theme(plot.title = element_text(hjust = 0.5)) +  theme(text = element_text(size=12)) + theme(legend.title=element_blank())

sample_fcst_p

##################################
# Manual boot strapping - coefficients and p-values

# Make random samples 
# Sample Number
sample_number <- 100
sample_fraction <- .8

# Round the sample size to a whole number
# Uses the sample fraction set above to partition in-out samples
sample_size <- round(nrow(cre_dev)*sample_fraction)

# Sample from the df
df_samples <- list()
coeff_l <- list()
pval_l <- list()

start.time <- Sys.time()
start.time
for (i in 1:sample_number){
  ##In sample data
  set.seed(i)
  df_sample <- cre_dev[sample(nrow(cre_dev), sample_size, replace = FALSE), ]
  logit <- glm(model, family = binomial(link = "logit"), data = df_sample)
  coeff_l[[i]] <- round(coef(summary(logit))[,1],5)
  pval_l[[i]] <- round(coef(summary(logit))[,4],5)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Turn into data frames
pval_boot_df <- as.data.frame(do.call("rbind",pval_l))
pval_boot_df <- data.frame(Sample=seq(from=1,to=sample_number,by=1),pval_boot_df)

coef_boot_df <- as.data.frame(do.call("rbind",coeff_l))
coef_boot_df <- data.frame(Sample=seq(from=1,to=sample_number,by=1),coef_boot_df)

# P-value Histograms
gg_p_df <- list()
for (i in 2:ncol(pval_boot_df)){
  gg_p_df[[i]] <- melt(pval_boot_df[,c(1,i)], id = "Sample")

  pval_h_plot <- ggplot(gg_p_df[[i]], aes(value)) + geom_histogram(fill = "#006600") + xlab("Value") + ylab("Frequency") + ggtitle(paste("P-Value",gg_p_df[[i]][1,2],sep=" - ")) + theme(text = element_text(size=12)) + theme(legend.title=element_blank()) + theme(plot.title = element_text(hjust = 0.5)) + geom_vline(xintercept=0)
  suppressMessages(print(pval_h_plot))
}

# Coefficient Densities
gg_c_df <- list()
for (i in 2:ncol(coef_boot_df)){
  gg_c_df[[i]] <- melt(coef_boot_df[,c(1,i)], id = "Sample")

  coef_d_plot <- ggplot(gg_c_df[[i]], mapping = aes(x = value, group = variable, fill=variable)) + geom_density() + ggtitle(paste("Coef. Density",gg_c_df[[i]][1,2],sep=" - ")) + theme(text = element_text(size=12)) + theme(legend.title=element_blank()) + theme(plot.title = element_text(hjust = 0.5))+ xlab("Value") + ylab("Density") + theme(legend.position="none") + scale_fill_manual(values=c("#003399")) + scale_colour_manual(values=c("black")) + geom_vline(xintercept=0)
  suppressMessages(print(coef_d_plot))
}
time.taken


