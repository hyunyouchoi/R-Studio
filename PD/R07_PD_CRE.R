##############################################################################
## File Name: R07_PD_CRE.R
## Author: KZ
## Date: 7/5/2017 Created
## Purpose: To build PD model for BOH CRE portfolio accoring to 
##          "07 - cre - model.sas"
## Download 8/7/2017
##############################################################################

#setwd("//useomvfs77/mclp/Common/Clients/Bank of Hope/Model Development/PD Models")
setwd("C:/Users/ic07949/Desktop/dataset/Data output")

requirements <- c("dplyr", "reshape2", "data.table","zoo","ggplot2","pROC","boot","tidyr","lazyeval","Hmisc")
for(rr in requirements){
  if(! rr %in% installed.packages()) install.packages(rr)
}
for(requirement in requirements){if(!(requirement %in% installed.packages())) install.packages(requirement)}
lapply(requirements, require, character.only=T)

## load data
cre_dev <- read.csv("df_boh_base.csv")
cre_dev$fileDate <- as.Date(cre_dev$fileDate, "%Y-%m-%d")

# Getting description of the current sample
y_filter_1 <- describe(cre_dev$y)
bal_sum_1 <- sum(cre_dev$current_balance)

main_data <- cre_dev

cre_dev <- filter(cre_dev, portfolio_id == "CRE" & as.Date(fileDate) <= as.Date("2016-03-31") &
                      as.Date(fileDate) >= as.Date("2007-12-31") )

# Getting description of the current sample
y_filter_2 <- describe(cre_dev$y)
bal_sum_2 <- sum(cre_dev$current_balance)

##########################
# Plot of CRE vs CI - start
# Data frames for the plots
# Using the main data
cre_dev_p <- cre_dev

ci_dev_p <- filter(main_data, portfolio_id == "CI" & as.Date(fileDate) <= as.Date("2016-03-31") &
                     as.Date(fileDate) >= as.Date("2007-12-31") )

cre_default<- cre_dev_p %>% group_by(fileDate) %>% summarise(Defaulters_CRE = sum(y)) %>% data.frame()
cre_nondefault <- cre_dev_p %>% group_by(fileDate) %>% filter(y==0) %>% count() %>% data.frame()
colnames(cre_nondefault) <- c("fileDate","Nondefaulters_CRE")
cre_default <- merge(cre_default, cre_nondefault, by.x = "fileDate", by.y = "fileDate")
cre_default$cre_default_pct <- round(100*cre_default$Defaulters_CRE/lag(cre_default$Nondefaulters_CRE),3)

ci_default<- ci_dev_p %>% group_by(fileDate) %>% summarise(Defaulters_CI = sum(y)) %>% data.frame()
ci_nondefault <- ci_dev_p %>% group_by(fileDate) %>% filter(y==0) %>% count() %>% data.frame()
colnames(ci_nondefault) <- c("fileDate","Nondefaulters_CI")
ci_default <- merge(ci_default, ci_nondefault, by.x = "fileDate", by.y = "fileDate")
ci_default$ci_default_pct <- round(100*ci_default$Defaulters_CI/lag(ci_default$Nondefaulters_CI),3)

gg_df <- merge(cre_default, ci_default)

gg_df <- data.frame(Date = gg_df[,1],
                    CRE = gg_df$cre_default_pct,
                    CI = gg_df$ci_default_pct) %>%
  reshape2::melt(id.vars = 'Date')

default_cre_ci_p <- ggplot(data = gg_df, mapping = aes(x = Date, y = value, group = variable, color = variable)) + geom_line() + theme(legend.position = 'bottom') + theme(legend.title=element_blank()) + xlab("Date") + ylab("Percentage %") + ggtitle("Default Rate Percentage CRE vs. CI") + theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=15))

default_cre_ci_p

# save Plot
ggsave("./R output/CRE-CI_Default-Rate-Per.png", width = 7, height = 7)

# Plot of CRE vs CI - end
##########################

## delete obs with naicsCode = 0
# cre_dev <- filter(cre_dev, naicsCode != 0)
# cre_dev <- filter(cre_dev, !is.na(POB))

cre_dev <- filter(cre_dev, !(current_balance == 0 & y ==0))

# Getting description of the current sample
y_filter_3 <- describe(cre_dev$y)
bal_sum_3 <- sum(cre_dev$current_balance)

## delete obs with property_descr in ('missing', 'None')
cre_dev <- filter(cre_dev, !(is.na(property_type) | property_type == 0))

cre_dev_mean_3 <- apply(cre_dev[,which(colnames(cre_dev) %in% c("y")),drop=F],2,function (x) round(mean(x),4))
cre_dev_n_3 <- apply(cre_dev[,which(colnames(cre_dev) %in% c("y")),drop=F],2, length)
cre_dev_mean_3
cre_dev_n_3

## create final input variables
## (1) variable prop_res: 1-4 residential and multifamily
##     BBCN: 10, 11
##     wilshire: 11, 12
cre_dev$prop_res <- ifelse( (cre_dev$boh_id == "bbcn" & cre_dev$property_type %in% c(10 , 11) ) |
                            (cre_dev$boh_id == "wilshire" & cre_dev$property_type %in% c(11 , 12) )  ,
                           1,
                           0)
table(cre_dev$prop_res)

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
table(cre_dev$prop_retail)

## (3) variable boh_rating1
cre_dev$boh_rating1 <- ifelse(cre_dev$boh_rating %in% c(0,1,2,3), "R1",
                              ifelse(cre_dev$boh_rating %in% c(4,1000), "R2",
                                     ifelse(cre_dev$boh_rating %in% c(2000,3000), "R3", "error")  
                                     ))

cre_dev$boh_rating1_R1 <- ifelse(cre_dev$boh_rating1 == "R1",1, 0)
cre_dev$boh_rating1_R2 <- ifelse(cre_dev$boh_rating1 == "R2",1, 0)

#Lagged R1 and R2
cre_dev <- cre_dev %>% group_by(account_id) %>% mutate(boh_rating1_R1_l = lag(boh_rating1_R1)) %>% as.data.frame()
cre_dev <- cre_dev %>% group_by(account_id) %>% mutate(boh_rating1_R2_l = lag(boh_rating1_R2)) %>% as.data.frame()

# Update lagged values with current value
cre_dev$boh_rating1_R1_l <- ifelse(is.na(cre_dev$boh_rating1_R1_l),cre_dev$boh_rating1_R1,cre_dev$boh_rating1_R1_l)
cre_dev$boh_rating1_R2_l <- ifelse(is.na(cre_dev$boh_rating1_R2_l),cre_dev$boh_rating1_R2,cre_dev$boh_rating1_R2_l)

## (4) variable rgdp_qg_lag_2_neg
cre_dev$rgdp_qg_lag_2_neg <- ifelse(cre_dev$rgdp_qg_lag_2 >= 0, 0, cre_dev$rgdp_qg_lag_2 )
mean(cre_dev$rgdp_qg_lag_2_neg )

## (5) variable CAUR_yd_3
cre_dev$CAUR_yd_3 <- ifelse(cre_dev$CAUR_yd >= 3, 3, cre_dev$CAUR_yd)
mean(cre_dev$CAUR_yd_3)

## (6) variable CAHPI_ag_6
cre_dev$CAHPI_ag_6 <- ifelse(cre_dev$CAHPI_ag >= 6, 6, cre_dev$CAHPI_ag)
mean(cre_dev$CAHPI_ag_6)

## (7) variable POB_95
cre_dev$POB_95 <- ifelse(cre_dev$POB <= 95, 95, cre_dev$POB)
mean(cre_dev$POB_95)

## Wilshire Dummy
cre_dev$wilshire_d <- ifelse(cre_dev$boh_id == "wilshire",1,0)

# Getting description of the current sample
y_filter_4 <- describe(cre_dev$y)
bal_sum_4 <- sum(cre_dev$current_balance)

## Sampling the data
set.seed(20170502)
# Sample fraction
sample_fraction <- .8
cre_dev_training <- cre_dev %>% sample_frac(sample_fraction)
describe(cre_dev_training$fileDate)

# Getting description of the in sample
y_filter_in <- describe(cre_dev_training$y)
bal_sum_in <- sum(cre_dev_training$current_balance)

##Out of sample data
cre_dev_outsample <- cre_dev[-which(rownames(cre_dev) %in% rownames(cre_dev_training)),]

# Getting description of the out sample
y_filter_out <- describe(cre_dev_outsample$y)
bal_sum_out <- sum(cre_dev_outsample$current_balance)

# Table of dependent variable sample stats
dep_var_filter_stats <- as.data.frame(rbind(y_filter_1$counts, y_filter_2$counts, y_filter_3$counts, y_filter_4$counts, y_filter_in$counts, y_filter_out$counts))

bal_sum_stats <- as.data.frame(rbind(bal_sum_1,bal_sum_2,bal_sum_3,bal_sum_4,bal_sum_in,bal_sum_out))

dep_var_filter_stats <- cbind(dep_var_filter_stats,bal_sum_stats)
rownames(dep_var_filter_stats) <- c("Input","CRE & (2007-2016)","Balance","Full Sample","In-sample","Out-of-sample")
colnames(dep_var_filter_stats)[8] <- "Sum of Curr. Bal."

write.csv(dep_var_filter_stats, "CRE_dep_var_filter_stats.csv")

###################################
## run logistic regression - insample

model <- y ~ prop_res + boh_rating1_R1_l + boh_rating1_R2_l + CAUR_yd_3  + POB_95

model <- y ~ boh_rating1_R1_l + boh_rating1_R2_l + CAUR_yd_3  + POB_95
fit <- glm(model, family = binomial(link = "logit"), data = cre_dev_training)

summary(fit)

#model_w <- y ~ prop_res + boh_rating1_R1_l + boh_rating1_R2_l + CAUR_yd_3  + POB_95 + wilshire_d

model_w <- y ~ boh_rating1_R1_l + boh_rating1_R2_l + CAUR_yd_3  + POB_95 

fit_w <- glm(model_w, family = binomial(link = "logit"), data = cre_dev_training)

summary(fit_w)

# Wilshire model comparison
stargazer::stargazer(fit,fit_w, type = "text", out = "W_compare_logit_CRE_PD.txt")

# McFadden's pseudo R squared for a fitted model
pR2 <- 1 - fit$deviance / fit$null.deviance 

###################################
# Output the regression table in academic format
stargazer::stargazer(fit, type = "text", out = "compare_logit_CRE_PD.txt")

coef_cre <- as.data.frame(summary(fit)$coefficients)
write.csv(coef_cre, "coef_cre.csv")

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
pdf("CRE_ROC_AUC.pdf")
  plot(roc_in, main =paste0("CRE PD ROC IN \n AUC = ", auc_in)) 
dev.off()

# Out-of-sample #
## get out-sample prediction p_hat for each account
predict_out <- predict(fit, cre_dev_outsample, type="response")
cre_dev_outsample$p_hat <- predict_out

## get ROC and AUC
roc_out <-  data.frame(predict = predict_out, y = cre_dev_outsample$y)
roc_out_plot <- roc(y ~ predict, data = roc_out)
auc_out <- round(as.numeric(roc_out_plot$auc),4)

plot(roc_out_plot, main =paste0("CRE PD ROC OUT \n AUC = ", auc_out))

pdf(paste0("CRE_ROC_AUC_OUT.pdf"))
  plot(roc_out_plot, main =paste0("CRE PD ROC OUT \n AUC = ", auc_out))
dev.off()

## get quarterly average PD in-sample
cre_pd_quarterly <- subset(cre_dev_training, select = c(fileDate, y, p_hat))

cre_default <- cre_pd_quarterly %>% group_by(fileDate) %>% summarise(Defaulters_CRE = sum(y)) %>% data.frame()
cre_nondefault <- cre_pd_quarterly %>% group_by(fileDate) %>% filter(y==0) %>% count() %>% data.frame()
colnames(cre_nondefault) <- c("fileDate","Nondefaulters_CRE")
cre_default <- merge(cre_default, cre_nondefault, by.x = "fileDate", by.y = "fileDate")
cre_default$cre_pd <- round(cre_default$Defaulters_CRE/lag(cre_default$Nondefaulters_CRE),3)
cre_pd_quarterly <-  merge(cre_pd_quarterly, cre_default, by.x = "fileDate", by.y = "fileDate")
cre_pd_quarterly <- aggregate(cre_pd_quarterly[,c("p_hat","cre_pd")], list(cre_pd_quarterly$fileDate), mean)
colnames(cre_pd_quarterly) <- c("fileDate","Fitted","Actual")
cre_pd_quarterly <- melt(cre_pd_quarterly, id = "fileDate")

cbPalette <- c("#000000", "#0072B2")

cre_pd_training_plot <- ggplot(cre_pd_quarterly, aes(x=fileDate, y = value, color=variable)) + 
  geom_line() + scale_colour_manual(values=cbPalette) + xlab("Date") + ylab("Default Rate") + ggtitle("Ave. Default Rate CRE - In-sample") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=15)) + theme(legend.title=element_blank())
cre_pd_training_plot

ggsave("./R output/CRE_PD_actual_fitted_insample.png", width = 7, height = 7)

## get quarterly average PD out-sample
cre_pd_quarterly_out <- subset(cre_dev_outsample, select = c(fileDate, y, p_hat))

cre_default <- cre_pd_quarterly_out %>% group_by(fileDate) %>% summarise(Defaulters_CRE = sum(y)) %>% data.frame()
cre_nondefault <- cre_pd_quarterly_out %>% group_by(fileDate) %>% filter(y==0) %>% count() %>% data.frame()
colnames(cre_nondefault) <- c("fileDate","Nondefaulters_CRE")
cre_default <- merge(cre_default, cre_nondefault, by.x = "fileDate", by.y = "fileDate")
cre_default$cre_pd <- round(cre_default$Defaulters_CRE/lag(cre_default$Nondefaulters_CRE),3)
cre_pd_quarterly_out <-  merge(cre_pd_quarterly_out, cre_default, by.x = "fileDate", by.y = "fileDate")
cre_pd_quarterly_out <- aggregate(cre_pd_quarterly_out[,c("p_hat","cre_pd")], list(cre_pd_quarterly_out$fileDate), mean)
colnames(cre_pd_quarterly_out) <- c("fileDate","Fitted","Actual")
cre_pd_quarterly_out <- melt(cre_pd_quarterly_out, id = "fileDate")

cbPalette <- c("#000000", "#0072B2")

cre_pd_out_plot <- ggplot(cre_pd_quarterly_out, aes(x=fileDate, y = value, color=variable)) + 
  geom_line() + scale_colour_manual(values=cbPalette) + xlab("Date") + ylab("Default Rate") + ggtitle("Ave. Default Rate CRE - Out-of-sample") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=15)) + theme(legend.title=element_blank())
cre_pd_out_plot

ggsave("./R output/CRE_PD_actual_fitted_outsample.png", width = 7, height = 7)

###########################################
## Forecast for 3 scenarios
setwd("C:/Users/ic07949/Desktop/dataset/Data output")

for(scenario in c("base", "adverse", "severe")){

#for(scenario in c("severe")){  
  print(paste0("==== ", scenario, " ===="))
  
  cre_forecast <- read.csv(paste0("df_boh_",scenario, ".csv"))
  cre_forecast$fileDate <- as.Date(cre_forecast$fileDate, "%Y-%m-%d")
  cre_forecast <- filter(cre_forecast, portfolio_id == "CRE" & as.Date(fileDate) > as.Date("2016-03-31") )
  
  ## delete obs with naicsCode = 0
  # cre_forecast <- filter(cre_forecast, naicsCode != 0)
  # cre_forecast <- filter(cre_forecast, !is.na(POB))
  
  cre_forecast <- filter(cre_forecast, !(current_balance == 0 & y ==0))
  
  ## create final input variables
  ## (1) variable prop_res: 1-4 residential and multifamily
  ##     BBCN: 10, 11
  ##     wilshire: 11, 12
  cre_forecast$prop_res <- ifelse( (cre_forecast$boh_id == "bbcn" & cre_forecast$property_type %in% c(10 , 11) ) |
                                     (cre_forecast$boh_id == "wilshire" & cre_forecast$property_type %in% c(11 , 12) )  ,
                                   1,
                                   0)
  
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
  
  
  ## get pd forecast p_hat for each account
  cre_forecast <- as.data.table(cre_forecast)
  cre_forecast$p_hat <- as.matrix (cre_forecast[, coef_cre$X[-1],with = FALSE]) %*% coef_cre$Estimate[-1] + 
    coef_cre$Estimate[1] 
  cre_forecast$p_hat <- 1/(1+exp(-cre_forecast$p_hat))
  
  
  ## get quarterly average PD
  cre_pd_quarterly_9Q <- subset(cre_forecast, select = c(fileDate, p_hat))
  cre_pd_quarterly_9Q <- aggregate(cre_pd_quarterly_9Q[,2], list(cre_pd_quarterly_9Q$fileDate), mean)
  
  setnames(cre_pd_quarterly_9Q, old = c("Group.1","p_hat"),
           new = c("fileDate", "value"))
  cre_pd_quarterly_9Q$variable <- scenario
  cre_pd_quarterly_9Q <- cre_pd_quarterly_9Q[,c(1,3,2)]
  assign(paste0("cre_pd_quarterly_",scenario), cre_pd_quarterly_9Q)
}


cre_pd_quarterly_9Q <- rbind(cre_pd_quarterly_base, cre_pd_quarterly_adverse, cre_pd_quarterly_severe)
cre_pd_quarterly_all <- rbind(cre_pd_quarterly, cre_pd_quarterly_9Q)
setnames(cre_pd_quarterly_all, old = c("variable", "value"), 
         new = c("scenario","PD"))


## final plot
cbPalette <- c("#000000", "#0072B2", "#006600", "#E69F00", "#D55E00")
cre_pd_plot <- ggplot(cre_pd_quarterly_all, aes(x = fileDate, y = PD, color = scenario)) + 
  geom_line() + scale_colour_manual(values=cbPalette) + 
  ggtitle("BOH CRE PD") + xlab("Date") + ylab("Default Rate") + ggtitle("Average Default Rate CRE") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=15))
cre_pd_plot

ggsave("./R output/CRE_PD_actual_fitted_forecast.png", width = 7, height = 7)

## output results
write.csv(cre_pd_quarterly_all, "./R output/CRE_PD_quarterly_actual_fitted_forecast.csv", row.names = FALSE)

# Summary stats per sample
options(scipen=999)

sum_nms <- c("y","prop_res","boh_rating1_R1_l","boh_rating1_R2_l","CAUR_yd_3","POB_95","CAHPI_ag_lag_1")
training_df <- as.data.frame.matrix(cre_dev_training)
testing_df <- as.data.frame.matrix(cre_dev_outsample)

summary(training_df)

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

rownames(cre_df_sample_stats) <- c("Mean (All Obs)","Mean (Training)","Mean (Test)","SD (All Obs)","SD (Training)","SD (Test)","Max (All Obs)","Max (Training)","Max (Test)","Min (All Obs)","Min (Training)","Min (Test)","Obs (All Obs)","Obs (Training)","Obs (Test)")

write.csv(cre_df_sample_stats, "./R output/cre_df_sample_stats.csv")

# Number of default events per period
def_events_df <- cre_dev[,which(colnames(cre_dev) %in% c("fileDate","y")),drop=F]
def_events_df_sum <- def_events_df %>% group_by(fileDate) %>% summarise(Defaults = sum(y)) %>% data.frame()
Obs <- def_events_df %>% group_by(fileDate) %>% tally() %>% data.frame()
def_events_df_sum <- merge(def_events_df_sum, Obs)
colnames(def_events_df_sum) <- c("Date","No. of Defaults","Observations")
write.csv(def_events_df_sum, "./R output/cre_def_events_df_sum.csv")

##########
# Plots

# Profile bins
cre_dev$pob_bins <- cut(cre_dev$POB,breaks = 6)
cre_dev$risk_bins <- ifelse(cre_dev$boh_rating %in% c(0,1,2,3), "R1",
                                     ifelse(cre_dev$boh_rating %in% c(4,1000), "R2",
                                            ifelse(cre_dev$boh_rating %in% c(2000,3000), "R3", "error")  
                                     ))

cre_dev$caur_bins <- cut(cre_dev$CAUR_yd,breaks = 4)
cre_dev$cahpi_bins <- cut(cre_dev$CAHPI_ag,breaks = 4)
cre_dev$gdp_bins <- cut(cre_dev$rgdp_qg_lag_2_neg, breaks = 2)

# In-sample bins
cre_dev_training$pob_bins <- cut(cre_dev_training$POB,breaks = 6)
cre_dev_training$risk_bins <- ifelse(cre_dev_training$boh_rating %in% c(0,1,2,3), "R1",
                                     ifelse(cre_dev_training$boh_rating %in% c(4,1000), "R2",
                                            ifelse(cre_dev_training$boh_rating %in% c(2000,3000), "R3", "error")  
                                     ))

cre_dev_training$caur_bins <- cut(cre_dev_training$CAUR_yd,breaks = 4)
cre_dev_training$cahpi_bins <- cut(cre_dev_training$CAHPI_ag,breaks = 4)
cre_dev_training$gdp_bins <- cut(cre_dev_training$rgdp_qg_lag_2_neg, breaks = 2)

# Out-sample bins
cre_dev_outsample$pob_bins <- cut(cre_dev_outsample$POB,breaks = 6)
cre_dev_outsample$risk_bins <- ifelse(cre_dev_outsample$boh_rating %in% c(0,1,2,3), "R1",
                                     ifelse(cre_dev_outsample$boh_rating %in% c(4,1000), "R2",
                                            ifelse(cre_dev_outsample$boh_rating %in% c(2000,3000), "R3", "error")  
                                     ))

cre_dev_outsample$caur_bins <- cut(cre_dev_outsample$CAUR_yd,breaks = 4)
cre_dev_outsample$cahpi_bins <- cut(cre_dev_outsample$CAHPI_ag,breaks = 4)
cre_dev_outsample$gdp_bins <- cut(cre_dev_outsample$rgdp_qg_lag_2_neg, breaks = 2)

######################
# Function to generate the plots:
pd_bin_plot <- function(data, date, dep_var, estimate, bins, title, profile){
  require(lazyeval)
  require(dplyr)
  defaulters <- data %>% group_by_(date, bins) %>% summarise_(Defaulters = interp(~sum(var, na.rm = F), var = as.name(dep_var))) %>% data.frame()
  nondefaulters <- data %>% group_by_(date, bins) %>% filter_(interp(~ var == 0, var = as.name(dep_var))) %>% count_() %>% data.frame()
  
  df <- merge(defaulters, nondefaulters)
  df <- df %>% group_by_(bins) %>% mutate(pd_actual = Defaulters/lag(n)) %>% data.frame() %>% na.omit
  #df <- df %>% group_by_(bins) %>% mutate(pd_actual = Defaulters/n) %>% data.frame() %>% na.omit
  colnames(df) <- c("Date","bins","Defaulters","Nondefaulters","PD_Actual")
  
  df <- aggregate(df$PD_Actual, list(df$bins), mean)
  colnames(df) <- c("Bins","Actual")
  
  obs <- data %>% group_by_(bins) %>% count_()
  
  if (profile == T){
    
    
    df_2 <- obs
    colnames(df_2) <- c("Bins","Observations")
    
    df <- merge(df_2, df)
    
    df <- df[with(df,order(Bins)),]
    row.names(df) <- NULL
    
    layout(rbind(1,2), heights=c(7,1))  # put legend on bottom 1/8th of the chart
    
    blue <- rgb(0, 0, 1, alpha=0.2)
    green <- rgb(.5, 1, .5, alpha=0.2)
    par(mar = c(5,5,2,5))
    with(df, plot(as.numeric(row.names(df)), Actual, col="green4", type = "l", xaxt = "n",lwd=2,main = paste("Default Rate",title, sep=" - "),
                  ylab="Default Rate",xlab=names(df)[1],
                  ylim=c(0,max(df[,3]))))
    par(new = T)
    bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=blue))
    axis(side = 4)
    mtext(side = 4, line = 3, 'Observation Count')
    axis(1, at=bp, labels=df[,1])
    
    # setup for no margins on the legend
    par(mar=c(0, 0, 0, 0))
    # c(bottom, left, top, right)
    plot.new()
    
    legend("center", 
           legend=c("Actual","Obs."),
           lty=1,lwd=5, col=c("green4",blue), ncol=2)
    
    
  } else if (profile == F){
    
    
    estimate <- data %>% group_by_(bins) %>% summarise_(Estimate = interp(~mean(var, na.rm = F), var = as.name(estimate))) %>% data.frame()
    df_2 <- merge(obs, estimate)
    colnames(df_2) <- c("Bins","Observations","Estimate")
    
    
    df <- merge(df_2, df)
    
    df <- df[with(df,order(Bins)),]
    row.names(df) <- NULL
    
    layout(rbind(1,2), heights=c(7,1))  # put legend on bottom 1/8th of the chart
    
    green <- rgb(.5, 1, .5, alpha=0.2)
    par(mar = c(5,5,2,5))
    with(df, plot(as.numeric(row.names(df)), Estimate, col="red", type = "l", xaxt = "n",lwd=2,main = paste("Default Rate",title, sep=" - "),
                  ylab="Default Rate",xlab=names(df)[1],
                  ylim=c(0,max(apply(df[,3:4], 2, max)))))
    par(new = T)
    with(df, plot(as.numeric(row.names(df)), Actual, col="blue", xaxt = "n",type = "l", lwd=2,
                  ylab="", xlab="", ylim=c(0,max(apply(df[,3:4], 2, max)))))
    par(new = T)
    bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=green))
    axis(side = 4)
    mtext(side = 4, line = 3, 'Observation Count')
    axis(1, at=bp, labels=df[,1])
    
    # setup for no margins on the legend
    par(mar=c(0, 0, 0, 0))
    # c(bottom, left, top, right)
    plot.new()
    legend("center", 
           legend=c("Estimate", "Actual","Obs."),
           lty=1,lwd=5, col=c("red3","blue", green), ncol=3)
    
  }
}


#####################
# Profile Plots
#####################

##########
# Plot profile POB
pd_bin_plot(data = cre_dev, date = "fileDate", dep_var = "y", estimate = NA, bins = "pob_bins", title = "POB - IN", profile = T)

##########
# Plot profile risk rating
pd_bin_plot(data = cre_dev, date = "fileDate", dep_var = "y", estimate = NA, bins = "risk_bins", title = "Risk Rating - IN", profile = T)

##########
# Plot profile CA UR
pd_bin_plot(data = cre_dev, date = "fileDate", dep_var = "y", estimate = NA, bins = "caur_bins", title = "CA UR - IN", profile = T)

##########
# Plot profile CA HPI
pd_bin_plot(data = cre_dev, date = "fileDate", dep_var = "y", estimate = NA, bins = "cahpi_bins", title = "CA HPI - IN", profile = T)

##########
# Plot profile GDP
pd_bin_plot(data = cre_dev, date = "fileDate", dep_var = "y", estimate = NA, bins = "gdp_bins", title = "GDP - IN", profile = T)

#####################
# Insample Plots
#####################

##########
# Plot insample average PD by POB
pdf(paste("./R output", "Default_Rate-POB_IN.pdf" ,sep ="/"), height = 5, width = 10)
pd_bin_plot(data = cre_dev_training, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "pob_bins", title = "POB - IN", profile = F)
dev.off()

pd_bin_plot(data = cre_dev_training, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "pob_bins", title = "POB - IN", profile = F)

##########
# Plot insample average PD by risk rating

pdf(paste("./R output", "Default_Rate-RiskIN.pdf" ,sep ="/"), height = 5, width = 10)
pd_bin_plot(data = cre_dev_training, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "risk_bins", title = "Risk Rating - IN", profile = F)
dev.off()

pd_bin_plot(data = cre_dev_training, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "risk_bins", title = "Risk Rating - IN", profile = F)

##########
# Plot insample average PD by CA UR
pdf(paste("./R output", "Default_Rate-CAURIN.pdf" ,sep ="/"), height = 5, width = 10)
pd_bin_plot(data = cre_dev_training, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "caur_bins", title = "CA UR - IN", profile = F)
dev.off()

pd_bin_plot(data = cre_dev_training, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "caur_bins", title = "CA UR - IN", profile = F)
##########
# Plot insample average PD by CA HPI
pdf(paste("./R output", "Default_Rate-CAHPIIN.pdf" ,sep ="/"), height = 5, width = 10)
pd_bin_plot(data = cre_dev_training, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "cahpi_bins", title = "CA HPI - IN", profile = F)
dev.off()

pd_bin_plot(data = cre_dev_training, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "cahpi_bins", title = "CA HPI - IN", profile = F)
##########
# Plot insample average PD by GDP
pdf(paste("./R output", "Default_Rate-GDPIN.pdf" ,sep ="/"), height = 5, width = 10)
pd_bin_plot(data = cre_dev_training, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "gdp_bins", title = "GDP - IN", profile = F)
dev.off()

pd_bin_plot(data = cre_dev_training, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "gdp_bins", title = "GDP - IN", profile = F)


#####################
# Out of sample plots
#####################

##########
# Plot insample average PD by POB

pdf(paste("./R output", "Default_Rate-POB_OUT.pdf" ,sep ="/"), height = 5, width = 10)
pd_bin_plot(data = cre_dev_outsample, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "pob_bins", title = "POB - OUT", profile = F)
dev.off()

pd_bin_plot(data = cre_dev_outsample, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "pob_bins", title = "POB - OUT", profile = F)
##########
# Plot insample average PD by risk rating

pdf(paste("./R output", "Default_Rate-Risk_OUT.pdf" ,sep ="/"), height = 5, width = 10)
pd_bin_plot(data = cre_dev_outsample, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "risk_bins", title = "Risk Rating - OUT", profile = F)
dev.off()

pd_bin_plot(data = cre_dev_outsample, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "risk_bins", title = "Risk Rating - OUT", profile = F)
##########
# Plot insample average PD by CA UR
pdf(paste("./R output", "Default_Rate-CAUR_OUT.pdf" ,sep ="/"), height = 5, width = 10)
pd_bin_plot(data = cre_dev_outsample, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "caur_bins", title = "CA UR - OUT", profile = F)
dev.off()

pd_bin_plot(data = cre_dev_outsample, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "caur_bins", title = "CA UR - OUT", profile = F)
##########
# Plot insample average PD by CA HPI
pdf(paste("./R output", "Default_Rate-CAHPI_OUT.pdf" ,sep ="/"), height = 5, width = 10)
pd_bin_plot(data = cre_dev_outsample, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "cahpi_bins", title = "CA HPI - OUT", profile = F)
dev.off()

pd_bin_plot(data = cre_dev_outsample, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "cahpi_bins", title = "CA HPI - OUT", profile = F)
##########
# Plot insample average PD by GDP
pdf(paste("./R output", "Default_Rate-GDP_OUT.pdf" ,sep ="/"), height = 5, width = 10)
pd_bin_plot(data = cre_dev_outsample, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "gdp_bins", title = "GDP - OUT", profile = F)
dev.off()

pd_bin_plot(data = cre_dev_outsample, date = "fileDate", dep_var = "y", estimate = "p_hat",bins = "gdp_bins", title = "GDP - OUT", profile = F)

#################
# Coefficient Stability

# Generate in and out samples and forecast
# Make random samples 
# Sample Number
sample_number <- 10
sample_fraction <- .8

set.seed(20170502)
cre_dev_training <- cre_dev %>% sample_frac(sample_fraction)
cre_dev_outsample <- cre_dev[-which(rownames(cre_dev) %in% rownames(cre_dev_training)),]

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
test_out_df <- data.frame(cre_dev_outsample,predict_s_df)
test_out_df <- test_out_df %>% group_by(fileDate) %>% mutate(defaulters = sum(y)) %>% data.frame()
test_out_df <- test_out_df %>% group_by(fileDate) %>% filter(y==0) %>% mutate(nondefaulters = n()) %>% data.frame()
test_out_df <- test_out_df %>% group_by(fileDate) %>% mutate(pd_actual = defaulters/lag(nondefaulters)) %>% data.frame()

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

ggsave(paste("./R output/cre_sample_fcst_plot.png"), width = 5, height = 5)

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
write.csv(pval_boot_df, "./R output/cre_pval_boot_df.csv", row.names = T)

coef_boot_df <- as.data.frame(do.call("rbind",coeff_l))
coef_boot_df <- data.frame(Sample=seq(from=1,to=sample_number,by=1),coef_boot_df)
write.csv(coef_boot_df, "./R output/cre_coef_boot_df.csv", row.names = T)

# P-value Histograms
gg_p_df <- list()
for (i in 2:ncol(pval_boot_df)){
  gg_p_df[[i]] <- melt(pval_boot_df[,c(1,i)], id = "Sample")
  
  pval_h_plot <- ggplot(gg_p_df[[i]], aes(value)) + geom_histogram(fill = "#006600") + xlab("Value") + ylab("Frequency") + ggtitle(paste("P-Value",gg_p_df[[i]][1,2],sep=" - ")) + theme(text = element_text(size=12)) + theme(legend.title=element_blank()) + theme(plot.title = element_text(hjust = 0.5)) + geom_vline(xintercept=0)
  print(pval_h_plot)
  
  ggsave( paste0(paste("./R output/", paste("CRE P-Value Histogram",gg_p_df[[i]][1,2],sep=" - "),sep=""),".png") , width = 5, height = 5)
}

# Coefficient Densities
gg_c_df <- list()
for (i in 2:ncol(coef_boot_df)){
  gg_c_df[[i]] <- melt(coef_boot_df[,c(1,i)], id = "Sample")
  
  coef_d_plot <- ggplot(gg_c_df[[i]], mapping = aes(x = value, group = variable, fill=variable)) + geom_density() + ggtitle(paste("Coef. Density",gg_c_df[[i]][1,2],sep=" - ")) + theme(text = element_text(size=12)) + theme(legend.title=element_blank()) + theme(plot.title = element_text(hjust = 0.5))+ xlab("Value") + ylab("Density") + theme(legend.position="none") + scale_fill_manual(values=c("#003399")) + scale_colour_manual(values=c("black")) + geom_vline(xintercept=0)
  print(coef_d_plot)
  
  ggsave( paste0(paste("./R output/", paste("CRE Coefficient Density",gg_c_df[[i]][1,2],sep=" - "),sep=""),".png") , width = 5, height = 5)
}
time.taken






