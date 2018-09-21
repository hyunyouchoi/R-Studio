##############################################################################
## File Name: R07_PD_CRE.R
## Author: KPMG
## Date: 7/5/2017 Created
## Purpose: To build PD model for BOH CRE portfolio 
##############################################################################
#setwd("C:/Users/doxborrow/Desktop/BoH/Modeling/PD Models")
#setwd("//useomvfs77/mclp/Common/Clients/Bank of Hope/Model Development/PD Models")

setwd("C:/Users/ic07949/Desktop/dataset")

requirements <- c("dplyr","reshape2","data.table","zoo","ggplot2","pROC","boot","tidyr","lazyeval","Hmisc","corrplot","car")
for(rr in requirements){
  if(! rr %in% installed.packages()) install.packages(rr)
}
for(requirement in requirements){if(!(requirement %in% installed.packages())) install.packages(requirement)}
lapply(requirements, require, character.only=T)

## load data
#cre_dev <- read_alt.csv("./Data output/df_boh_base_alt.csv")
cre_dev <- readRDS("df_boh_base.RDS")
cre_dev$fileDate <- as.Date(cre_dev$fileDate, "%Y-%m-%d")

main_data <- cre_dev


########################################################################
# CRE and Date filter
cre_dev <- filter(cre_dev, portfolio_id == "CRE" & as.Date(fileDate) <= as.Date("2016-03-31") & as.Date(fileDate) >= as.Date("2007-12-31") )

# Getting description of the current sample
y_filter_1 <- describe(cre_dev$y)
bal_sum_1 <- sum(cre_dev$current_balance)

##########################
# Plot of CRE vs CI Default
cre_dev_p <- cre_dev

# CI data for plot
ci_dev_p <- filter(main_data, portfolio_id == "CI" & as.Date(fileDate) <= as.Date("2016-03-31") & as.Date(fileDate) >= as.Date("2007-12-31") )

cre_default <- cre_dev_p %>% group_by(fileDate) %>% summarise(Default_CRE = 100*mean(y)) %>% data.frame()
ci_default<- ci_dev_p %>% group_by(fileDate) %>% summarise(Defaulter_CI = 100*mean(y)) %>% data.frame()

gg_df <- merge(cre_default, ci_default)
gg_df <- data.frame(Date = gg_df[,1],CRE = gg_df$Default_CRE,CI = gg_df$Defaulter_CI) %>% reshape2::melt(id.vars = 'Date')

default_cre_ci_p <- ggplot(data = gg_df, mapping = aes(x = Date, y = value, group = variable, color = variable)) + geom_line() + theme(legend.position = 'bottom') + theme(legend.title=element_blank()) + xlab("Date") + ylab("Percentage %") + ggtitle("Default Rate Percentage CRE vs. CI") + theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=15)) + theme(legend.position="bottom")

default_cre_ci_p

# save Plot
ggsave("./R output/CRE-CI_Default-Rate-Per_alt.png", width = 7, height = 7)

# Balance filter
cre_dev <- filter(cre_dev, !(current_balance == 0 & y == 0))

# Getting description of the current sample
y_filter_2 <- describe(cre_dev$y)
bal_sum_2 <- sum(cre_dev$current_balance)

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
y_filter_3 <- describe(cre_dev$y)
bal_sum_3 <- sum(cre_dev$current_balance)

# Number of default events per period
def_events_df <- cre_dev[,which(colnames(cre_dev) %in% c("fileDate","y")),drop=F]
def_events_df_sum <- def_events_df %>% group_by(fileDate) %>% summarise(Defaults = sum(y)) %>% data.frame()
Obs <- def_events_df %>% group_by(fileDate) %>% tally() %>% data.frame()
def_events_df_sum <- merge(def_events_df_sum, Obs)
colnames(def_events_df_sum) <- c("Date","No. of Defaults","Observations")
write_alt.csv(def_events_df_sum, "cre_def_events_df_sum_cre_input_alt.csv")

## Partition the data into training and testing samples
set.seed(20170808)
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
dep_var_filter_stats <- as.data.frame(rbind(y_filter_1$counts, y_filter_2$counts, y_filter_3$counts,y_filter_in$counts, y_filter_out$counts))

bal_sum_stats <- as.data.frame(rbind(bal_sum_1,bal_sum_2,bal_sum_3,bal_sum_in,bal_sum_out))

dep_var_filter_stats <- cbind(dep_var_filter_stats,bal_sum_stats)
rownames(dep_var_filter_stats) <- c("CRE & (2007 Q4 - 2016 Q1)","Balance Filter","Full Sample","80% In-sample","20% Out-of-sample")
colnames(dep_var_filter_stats)[8] <- "Sum of Curr. Bal."
dep_var_filter_stats
write_alt.csv(dep_var_filter_stats, "./R Output/CRE_dep_var_filter_stats_alt.csv")

# Descriptive statistics per boh_id
describe(cre_dev[,"boh_id",drop=F])

boh_id_y_stats <- cre_dev %>% group_by(boh_id) %>% summarise(mean=mean(y),sum=sum(y)) %>% as.data.frame()
boh_id_y_stats

boh_id_y_q_hist <- cre_dev %>% group_by(fileDate,boh_id) %>% summarise(mean=mean(y)) %>% as.data.frame()
boh_id_y_q_hist

boh_id_y_q_hist_p <- ggplot(boh_id_y_q_hist, aes(x=fileDate, y = mean, color=boh_id, group=boh_id)) + 
  geom_line() + xlab("Date") + ylab("Default Rate") + ggtitle("Ave. Default Rate CRE - BOH ID") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=15)) + theme(legend.title=element_blank()) + theme(legend.position="bottom")
boh_id_y_q_hist_p

ggsave("./R output/CRE_PD_actual_fitted_insample_alt.png", width = 7, height = 7)

###################################
## Logistic regression - Training

# Main Model
model <- y ~ boh_rating1_R1_l + boh_rating1_R2_l + POB_95 + CAUR_yd + NCREIF_Property_Index_ag_lag_1 
fit <- glm(model, family = binomial(link = "logit"), data = cre_dev_training)
summary(fit)

# Multicollinearity of fitted model
vif(fit)

# Model regressed on full sample
fit_full <- glm(model, family = binomial(link = "logit"), data = cre_dev)
summary(fit_full)


# McFadden's pseudo R squared for a fitted model
pR2 <- 1 - fit$deviance / fit$null.deviance 
pR2

###################################
# Output the main regression table in academic format
stargazer::stargazer(fit,fit_full, type = "text", out = "logit_CRE_PD_alt.txt", model.numbers = F, column.labels = c("Train","Full"))

# Save the model coefficients
coef_cre <- as.data.frame(summary(fit)$coefficients)
write_alt.csv(coef_cre, "./R Output/coef_cre_alt_alt.csv")

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

# save Plot
pdf("./R output/CRE_ROC_AUC.pdf")
  plot(roc_in, main =paste0("CRE PD ROC IN \n AUC = ", auc_in)) 
dev.off()

# Out-of-sample #
## Out-sample prediction p_hat for each account
predict_out <- predict(fit, cre_dev_testing, type="response")
cre_dev_testing$p_hat <- predict_out

## ROC and AUC
roc_out <-  data.frame(predict = predict_out, y = cre_dev_testing$y)
roc_out_plot <- roc(y ~ predict, data = roc_out)
auc_out <- round(as.numeric(roc_out_plot$auc),4)

plot(roc_out_plot, main =paste0("CRE PD ROC OUT \n AUC = ", auc_out))

pdf(paste0("./R output/CRE_ROC_AUC_OUT.pdf"))
  plot(roc_out_plot, main =paste0("CRE PD ROC OUT \n AUC = ", auc_out))
dev.off()

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

ggsave("./R output/CRE_PD_actual_fitted_insample_alt.png", width = 7, height = 7)

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

ggsave("./R output/CRE_PD_actual_fitted_outsample_alt.png", width = 7, height = 7)

###########################################
## Forecast for 3 scenarios

for(scenario in c("base", "adverse", "severe")){

  print(paste0("==== ", scenario, " ===="))
  
  cre_forecast <- readRDS(paste0("./Data output/df_boh_",scenario, ".RDS"))
  cre_forecast$fileDate <- as.Date(cre_forecast$fileDate, "%Y-%m-%d")
  cre_forecast <- filter(cre_forecast, portfolio_id == "CRE" & as.Date(fileDate) > as.Date("2016-03-31") )
  
  # Balance filter
  cre_forecast <- filter(cre_forecast, !(current_balance == 0 & y ==0))
  
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

ggsave("./R output/CRE_PD_actual_fitted_forecast_alt.png", width = 7, height = 7)

## output results
write_alt.csv(cre_pd_quarterly_all, "./R output/CRE_PD_quarterly_actual_fitted_forecast_alt.csv", row.names = FALSE)

# Summary stats per sample
options(scipen=999)

sum_nms <- c("y","prop_res","boh_rating1_R1_l","boh_rating1_R2_l","CAUR_yd","POB_95","NCREIF_Property_Index_ag_lag_1")
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

write_alt.csv(cre_df_sample_stats, "./R output/cre_df_sample_stats_alt.csv")

#####################
# Profile Plots
#####################
# Function to generate the plots:
source("./R Code/Current Model/pd_bin_plot2.R")

##########
# Plot profile POB
pob_breaks <- c(-Inf, 0,94,Inf)
cre_dev$pob_bins <- cut(cre_dev$POB,breaks = pob_breaks)
suppressMessages(pd_bin_plot2(data = cre_dev, date = "fileDate", dep_var = "y", estimate = NA, bins = "pob_bins", title = "POB - PRF", profile = T))

ggsave(paste("./R output/CRE_Default_Rate-POB_PRF_alt.png"), width = 5, height = 5)
##########
# Plot profile risk rating
cre_dev$risk_bins <- ifelse(cre_dev$boh_rating %in% c(0,1,2,3), "R1",ifelse(cre_dev$boh_rating %in% c(4,1000), "R2",ifelse(cre_dev$boh_rating %in% c(2000,3000), "R3", NA)))
suppressMessages(pd_bin_plot2(data = cre_dev, date = "fileDate", dep_var = "y", estimate = NA, bins = "risk_bins", title = "Risk Rating - PRF", profile = T))

ggsave(paste("./R output/CRE_Default_Rate-RISK_PRF_alt.png"), width = 5, height = 5)
##########
# Plot profile CA UR
cre_dev$caur_bins <- cut(cre_dev$CAUR_yd,breaks = 3)
suppressMessages(pd_bin_plot2(data = cre_dev, date = "fileDate", dep_var = "y", estimate = NA, bins = "caur_bins", title = "CA UR - PRF", profile = T))

ggsave(paste("./R output/CRE_Default_Rate-CAUR_PRF_alt.png"), width = 5, height = 5)
##########
# Plot profile CA HPI
cre_dev$cahpi_bins <- cut(cre_dev$CAHPI_ag,breaks = 3)
suppressMessages(pd_bin_plot2(data = cre_dev, date = "fileDate", dep_var = "y", estimate = NA, bins = "cahpi_bins", title = "CA HPI - PRF", profile = T))

ggsave(paste("./R output/CRE_Default_Rate-CAHPI_PRF_alt.png"), width = 5, height = 5)
##########
# Plot profile GDP
cre_dev$gdp_bins <- cut(cre_dev$rgdp_qg, breaks = 4)
suppressMessages(pd_bin_plot2(data = cre_dev, date = "fileDate", dep_var = "y", estimate = NA, bins = "gdp_bins", title = "GDP - PRF", profile = T))

ggsave(paste("./R output/CRE_Default_Rate-GDP_PRF_alt.png"), width = 5, height = 5)
##########
# Plot profile CRE Index
cre_dev$cre_bins <- cut(cre_dev$NCREIF_Property_Index_ag, breaks = 5)
suppressMessages(pd_bin_plot2(data = cre_dev, date = "fileDate", dep_var = "y", estimate = NA, bins = "cre_bins", title = "CRE - PRF", profile = T))

ggsave(paste("./R output/CRE_Default_Rate-CREINDEX_PRF_alt.png"), width = 5, height = 5)

##########
# Plot profile NY HPI Index
cre_dev$nyhpi_bins <- cut(cre_dev$NYHPI_ag, breaks = 3)
suppressMessages(pd_bin_plot2(data = cre_dev, date = "fileDate", dep_var = "y", estimate = NA, bins = "nyhpi_bins", title = "NY HPI - PRF", profile = T))

ggsave(paste("./R output/CRE_Default_Rate-NYHPI_PRF_alt.png"), width = 5, height = 5)

#####################
# Insample Plots
#####################

##########
# Plot insample average PD by POB
pob_breaks <- c(-Inf, 0,94,Inf)
cre_dev_training$pob_bins <- cut(cre_dev_training$POB,breaks = pob_breaks)
suppressMessages(pd_bin_plot2(data = cre_dev_training, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "pob_bins", title = "POB - IN", profile = F))

ggsave(paste("./R output/CRE_Default_Rate-POB_IN_alt.png"), width = 5, height = 5)
##########
# Plot insample average PD by risk rating
cre_dev_training$risk_bins <- ifelse(cre_dev_training$boh_rating %in% c(0,1,2,3), "R1",ifelse(cre_dev_training$boh_rating %in% c(4,1000), "R2",ifelse(cre_dev_training$boh_rating %in% c(2000,3000), "R3", NA)))
suppressMessages(pd_bin_plot2(data = cre_dev_training, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "risk_bins", title = "Risk Rating - IN", profile = F))

ggsave(paste("./R output/CRE_Default_Rate-Risk_IN_alt.png"), width = 5, height = 5)
##########
# Plot insample average PD by CA UR
cre_dev_training$caur_bins <- cut(cre_dev_training$CAUR_yd,breaks = 3)
suppressMessages(pd_bin_plot2(data = cre_dev_training, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "caur_bins", title = "CA UR - IN", profile = F))

ggsave(paste("./R output/CRE_Default_Rate-CAUR_IN_alt.png"), width = 5, height = 5)
##########
# Plot insample average PD by CA HPI
cre_dev_training$cahpi_bins <- cut(cre_dev_training$CAHPI_ag,breaks = 4)
suppressMessages(pd_bin_plot2(data = cre_dev_training, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "cahpi_bins", title = "CA HPI - IN", profile = F))

ggsave(paste("./R output/CRE_Default_Rate-CAHPI_IN_alt.png"), width = 5, height = 5)
##########
# Plot insample average PD by GDP
cre_dev_training$gdp_bins <- cut(cre_dev_training$rgdp_qg, breaks = 4)
suppressMessages(pd_bin_plot2(data = cre_dev_training, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "gdp_bins", title = "GDP - IN", profile = F))

ggsave(paste("./R output/CRE_Default_Rate-GDP_IN_alt.png"), width = 5, height = 5)

##########
# Plot profile CRE Index
cre_dev_training$cre_bins <- cut(cre_dev_training$NCREIF_Property_Index_ag, breaks = 4)
suppressMessages(pd_bin_plot2(data = cre_dev_training, date = "fileDate", dep_var = "y", estimate = "y", bins = "cre_bins", title = "CRE - IN", profile = F))

ggsave(paste("./R output/CRE_Default_Rate-CREIndex_IN_alt.png"), width = 5, height = 5)

#####################
# Out of sample plots
#####################

##########
# Plot out sample average PD by POB
pob_breaks <- c(-Inf, 0,94,Inf)
cre_dev_testing$pob_bins <- cut(cre_dev_testing$POB,breaks = pob_breaks)
suppressMessages(pd_bin_plot2(data = cre_dev_testing, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "pob_bins", title = "POB - OUT", profile = F))

ggsave(paste("./R output/CRE_Default_Rate-POB_OUT_alt.png"), width = 5, height = 5)
##########
# Plot out sample average PD by risk rating
cre_dev_testing$risk_bins <- ifelse(cre_dev_testing$boh_rating %in% c(0,1,2,3), "R1",ifelse(cre_dev_testing$boh_rating %in% c(4,1000), "R2",ifelse(cre_dev_testing$boh_rating %in% c(2000,3000), "R3", NA)))
suppressMessages(pd_bin_plot2(data = cre_dev_testing, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "risk_bins", title = "Risk Rating - OUT", profile = F))

ggsave(paste("./R output/CRE_Default_Rate-Risk_OUT_alt.png"), width = 5, height = 5)
##########
# Plot out sample average PD by CA UR
cre_dev_testing$caur_bins <- cut(cre_dev_testing$CAUR_yd,breaks = 3)
suppressMessages(pd_bin_plot2(data = cre_dev_testing, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "caur_bins", title = "CA UR - OUT", profile = F))

ggsave(paste("./R output/CRE_Default_Rate-CAUR_OUT_alt.png"), width = 5, height = 5)
##########
# Plot out sample average PD by CA HPI
cre_dev_testing$cahpi_bins <- cut(cre_dev_testing$CAHPI_ag,breaks = 4)
suppressMessages(pd_bin_plot2(data = cre_dev_testing, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "cahpi_bins", title = "CA HPI - OUT", profile = F))

ggsave(paste("./R output/CRE_Default_Rate-CAHPI_OUT_alt.png"), width = 5, height = 5)
##########
# Plot out sample average PD by GDP
cre_dev_testing$gdp_bins <- cut(cre_dev_testing$rgdp_qg, breaks = 4)
suppressMessages(pd_bin_plot2(data = cre_dev_testing, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "gdp_bins", title = "GDP - OUT", profile = F))

ggsave(paste("./R output/CRE_Default_Rate-GDP_OUT_alt.png"), width = 5, height = 5)

##########
# Plot profile CRE Index
cre_dev_testing$cre_bins <- cut(cre_dev_testing$NCREIF_Property_Index_ag, breaks = 4)
suppressMessages(pd_bin_plot2(data = cre_dev_testing, date = "fileDate", dep_var = "y", estimate = "y", bins = "cre_bins", title = "CRE - OUT", profile = F))

ggsave(paste("./R output/CRE_Default_Rate-CREIndex_OUT_alt.png"), width = 5, height = 5)

#################
# Coefficient Stability
# Repeated Sample Forecasts

sample_number <- 10
sample_fraction <- .8

set.seed(20170808)
cre_dev_training_s <- cre_dev %>% sample_frac(sample_fraction)
cre_dev_testing_s <- cre_dev[-which(rownames(cre_dev) %in% rownames(cre_dev_training_s)),]

# Round the sample size to a whole number
# Uses the sample fraction set above to partition in-out samples
sample_size <- round(nrow(cre_dev)*sample_fraction)

# Sample from the df
predict_s <- list()
for (i in 1:sample_number){
  ##In sample data
  set.seed(i)
  df_sample_in <- cre_dev[sample(nrow(cre_dev), sample_size, replace = FALSE), ]
  ##Out of sample data
  df_sample_out <- cre_dev[-which(rownames(cre_dev) %in% rownames(df_sample_in)),]
  ##Estimate the model
  logit_s <- glm(model, family = binomial(link = "logit"), data = df_sample_in)
  predict_s[[i]] <- predict(logit_s, df_sample_out, type="response")
}

# Make a df for the predict df and assign names
header_predict <- paste("Sample_", seq(1:sample_number),sep="")
predict_s_df <- data.frame(predict_s)
colnames(predict_s_df) <- header_predict

# Make data frame of all predictions
test_out_df <- data.frame(cre_dev_testing_s,predict_s_df)
test_out_df <- test_out_df %>% group_by(fileDate) %>% mutate(pd_actual = mean(y)) %>% data.frame()

fcst_df_nms <- c("fileDate",header_predict,"pd_actual")
test_out_df <- na.omit(test_out_df[,which(colnames(test_out_df) %in% fcst_df_nms), drop = F])

test_out_df <- aggregate(x = test_out_df[,-1],
                         FUN = mean,
                         by = list(Date = test_out_df$fileDate))
# Plot of all Forecasts 
predict_samples_gg <- melt(test_out_df, id = "Date")
sample_fcst_p <- ggplot(data = predict_samples_gg, aes(x = Date, y = value, group = variable, color = variable)) + geom_line() + xlab("Date") + ylab("Default Rate") + ggtitle("Ave. Default Rate CRE Out-of-Sample") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=12)) + theme(legend.title=element_blank())

sample_fcst_p

ggsave(paste("./R output/cre_sample_fcst_plot_alt.png"), width = 5, height = 5)

##################################
# Manual boot strapping - coefficients and p-values

# Make random samples 
# Sample Number
sample_number <- 500
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
write_alt.csv(pval_boot_df, "./R output/cre_pval_boot_df_alt.csv", row.names = T)

coef_boot_df <- as.data.frame(do.call("rbind",coeff_l))
coef_boot_df <- data.frame(Sample=seq(from=1,to=sample_number,by=1),coef_boot_df)
write_alt.csv(coef_boot_df, "./R output/cre_coef_boot_df_alt.csv", row.names = T)

# P-value Histograms
gg_p_df <- list()
for (i in 2:ncol(pval_boot_df)){
  gg_p_df[[i]] <- melt(pval_boot_df[,c(1,i)], id = "Sample")

  pval_h_plot <- ggplot(gg_p_df[[i]], aes(value)) + geom_histogram(fill = "#006600") + xlab("Value") + ylab("Frequency") + ggtitle(paste("P-Value",gg_p_df[[i]][1,2],sep=" - ")) + theme(text = element_text(size=12)) + theme(legend.title=element_blank()) + theme(plot.title = element_text(hjust = 0.5)) + geom_vline(xintercept=0)
  suppressMessages(print(pval_h_plot))

  ggsave( paste0(paste("./R output/", paste("CRE P-Value Histogram",gg_p_df[[i]][1,2],sep=" - "),sep=""),"_alt.png") , width = 5, height = 5)
}

# Coefficient Densities
gg_c_df <- list()
for (i in 2:ncol(coef_boot_df)){
  gg_c_df[[i]] <- melt(coef_boot_df[,c(1,i)], id = "Sample")

  coef_d_plot <- ggplot(gg_c_df[[i]], mapping = aes(x = value, group = variable, fill=variable)) + geom_density() + ggtitle(paste("Coef. Density",gg_c_df[[i]][1,2],sep=" - ")) + theme(text = element_text(size=12)) + theme(legend.title=element_blank()) + theme(plot.title = element_text(hjust = 0.5))+ xlab("Value") + ylab("Density") + theme(legend.position="none") + scale_fill_manual(values=c("#003399")) + scale_colour_manual(values=c("black")) + geom_vline(xintercept=0)
  suppressMessages(print(coef_d_plot))

  ggsave( paste0(paste("./R output/", paste("CRE Coefficient Density",gg_c_df[[i]][1,2],sep=" - "),sep=""),"_alt.png") , width = 5, height = 5)
}
time.taken




