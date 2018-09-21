##############################################################################
## File Name: R06_PD_CI.R
## Author: KZ
## Date: 5/1/2017 Created
## Purpose: To build PD model for BOH CI portfolio accoring to 
##          "06 - ci -model.sas"
##############################################################################
#setwd("//useomvfs77/mclp/Common/Clients/Bank of Hope/Model Development/PD Models")
setwd("C:/Users/doxborrow/Desktop/BoH/Modeling/PD Models")



requirements <- c("dplyr", "reshape2", "data.table","zoo","ggplot2","pROC","boot")
for(rr in requirements){
  if(! rr %in% installed.packages()) install.packages(rr)
}
require(dplyr)
require(reshape2)
require(data.table)
require(zoo)
require(ggplot2)
require(pROC)
require(boot)
require(tidyr)

## load data
ci_dev <- read.csv("./Data output/df_boh_base.csv")
ci_dev$fileDate <- as.Date(ci_dev$fileDate, "%Y-%m-%d")
ci_dev <- filter(ci_dev, portfolio_id == "CI" & as.Date(fileDate) <= as.Date("2016-03-31") &
                   as.Date(fileDate) >= as.Date("2008-03-31") )

## in the SAS code, also need to delete obs with loan_spread_v < 0.
## only a 9 obs (out of 288564 obs) with loan_spread_v < 0, can ignore.

## delete obs with naicsCode = 0
ci_dev <- filter(ci_dev, naicsCode != 0)
ci_dev <- filter(ci_dev, !is.na(POB))

## create final input variables
## (1) variable boh_rating1
ci_dev$boh_rating1 <- ifelse(ci_dev$boh_rating %in% c(1,2), "R1",
                             ifelse(ci_dev$boh_rating %in% c(0,3,4,1000), "R2",
                             ifelse(ci_dev$boh_rating %in% c(2000), "R3",
                             ifelse(ci_dev$boh_rating %in% c(3000,4000), "R4", "error")  
                              )))

table(ci_dev$boh_rating)
table(ci_dev$boh_rating1)
ci_dev$boh_rating1_R1 <- ifelse(ci_dev$boh_rating1 == "R1",1, 0)
ci_dev$boh_rating1_R2 <- ifelse(ci_dev$boh_rating1 == "R2",1, 0)
ci_dev$boh_rating1_R4 <- ifelse(ci_dev$boh_rating1 == "R4",1, 0)


## (2) variable naics_code3
ci_dev$naicsCode_ch <- substr(as.character(ci_dev$naicsCode),1,2)
ci_dev$naicsCode_2d <- as.numeric(as.character(ci_dev$naicsCode_ch))
ci_dev$naics_code3 <- ifelse(ci_dev$naicsCode_2d %in% c(72,44,45), "c",
                             ifelse(ci_dev$naicsCode_2d %in% c(42), "d",
                             ifelse(ci_dev$naicsCode_2d %in% c(31,32,33), "g", "h")
                             ))
table(ci_dev$naics_code3)
ci_dev$naics_code3_c <- ifelse(ci_dev$naics_code3 == "c",1, 0)
ci_dev$naics_code3_d <- ifelse(ci_dev$naics_code3 == "d",1, 0)
ci_dev$naics_code3_g <- ifelse(ci_dev$naics_code3 == "g",1, 0)


## (3) variable season
ci_dev$season <- ifelse(ci_dev$q == 1, "sp",
                        ifelse(ci_dev$q == 2, "su",
                               ifelse(ci_dev$q ==3, "fa", "wi")))
table(ci_dev$season)
ci_dev$season_fall <- ifelse(ci_dev$season == "fa", 1, 0)
ci_dev$season_summer <- ifelse(ci_dev$season == "su", 1, 0)
ci_dev$season_winter <- ifelse(ci_dev$season == "wi", 1, 0)


## (4) variable cahpi_ag_lag_3_n12
ci_dev$cahpi_ag_lag_3_n12 <- ifelse(ci_dev$CAHPI_ag_lag_3 <= -12, -12, ci_dev$CAHPI_ag_lag_3)
min(ci_dev$cahpi_ag_lag_3_n12)

## (5) variable dpd0129_0
ci_dev$dpd0129_0 <- ifelse(ci_dev$dpd0129 == 0, 1, 0)
table(ci_dev$dpd0129_0)

## (6) variables POB_5 and POB_50
ci_dev$POB_5 <- ifelse(ci_dev$POB >= 5, 5, ci_dev$POB)

ci_dev$POB_50 <- ifelse(ci_dev$POB >= 50, 50, 
                        ifelse(ci_dev$POB <= 5, 5, ci_dev$POB))

mean(ci_dev$POB_5)
mean(ci_dev$POB_50)

## Sampling.  60% of the data
set.seed(20170501)
ci_dev_training <- ci_dev %>% sample_frac(0.6)

##Out of sample data
ci_dev_outsample <- ci_dev[-which(rownames(ci_dev) %in% rownames(ci_dev_training)),]

# Convert to data tables
ci_dev_training <- as.data.table(ci_dev_training)
ci_dev_outsample <- as.data.table(ci_dev_outsample)


## run logistic regression
model <- y ~ boh_rating1_R1 + boh_rating1_R2 + boh_rating1_R4 +  
  naics_code3_c + naics_code3_d +
  season_fall + season_summer + season_winter + CAUR_yd_lag_3 +
  cahpi_ag_lag_3_n12 + dpd0129_0 + POB_5 + POB_50


fit <- glm( model, family = binomial(link = "logit"), data = ci_dev_training)

summary(fit)

coef_ci <- as.data.frame(summary(fit)$coefficients)
write.csv(coef_ci, "./R Output/coef_ci.csv")

coef_ci$X <- rownames(coef_ci)

## get in-sample prediction p_hat for each account
ci_dev_training$p_hat <- as.matrix (ci_dev_training[, coef_ci$X[-1],with = FALSE]) %*% coef_ci$Estimate[-1] + 
  coef_ci$Estimate[1] 
ci_dev_training$p_hat <- 1/(1+exp(-ci_dev_training$p_hat))

## get out-sample prediction p_hat for each account
predict_out <- predict(fit, ci_dev_outsample, type="response")
ci_dev_outsample$p_hat <- predict_out

## get ROC and AUC 
ci_roc <- roc(y ~ p_hat, data = ci_dev_training)
ci_auc <- auc(ci_dev_training$y, ci_dev_training$p_hat)
ci_auc <- round(as.numeric(as.character(ci_auc)),4)
plot(ci_roc, main =paste0("CI PD ROC IN \n AUC = ", ci_auc)) 

pdf("./R output/CI_ROC_AUC.pdf")
  plot(ci_roc, main =paste0("CI PD ROC IN \n AUC = ", ci_auc)) 
dev.off()

## get ROC and AUC - out-sample
roc_out <-  data.frame(predict = predict_out, y = ci_dev_outsample$y)
roc_out_plot <- roc(y ~ predict, data = roc_out)
auc_out <- round(as.numeric(roc_out_plot$auc),4)

plot(roc_out_plot, main =paste0("CI PD ROC OUT \n AUC = ", auc_out))

pdf(paste0("./R output/CI_ROC_AUC_OUT.pdf"))
plot(roc_out_plot, main =paste0("CI PD ROC OUT \n AUC = ", auc_out))
dev.off()


## get quarterly average PD
# ci_pd_quarterly <- ddply(ci_dev_training, .(fileDate), summarize, average_pd = mean(p_hat))
ci_pd_quarterly <- subset(ci_dev_training, select = c(fileDate, y, p_hat))
ci_pd_quarterly <- aggregate(ci_pd_quarterly[,2:3], list(ci_pd_quarterly$fileDate), mean)
setnames(ci_pd_quarterly, old = c("Group.1","y","p_hat"),
         new = c("fileDate", "actual", "fitted"))
ci_pd_quarterly <- melt(ci_pd_quarterly, id = "fileDate")

cbPalette <- c("#000000", "#0072B2")

ci_pd_training_plot <- ggplot(ci_pd_quarterly, aes(x=fileDate, y = value, color=variable)) + 
  geom_line() + scale_colour_manual(values=cbPalette) + xlab("Date") + ylab("Default Rate") + ggtitle("Ave. Default Rate CI In-Sample") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=15)) + theme(legend.title=element_blank())
ci_pd_training_plot

pdf("./R output/CI_PD_actual_fitted_insample.pdf", height = 5, width = 10)
ci_pd_training_plot
dev.off()


## get quarterly average PD out-sample
ci_pd_quarterly_out <- subset(ci_dev_outsample, select = c(fileDate, y, p_hat))
ci_pd_quarterly_out <- aggregate(ci_pd_quarterly_out[,2:3], list(ci_pd_quarterly_out$fileDate), mean)
setnames(ci_pd_quarterly_out, old = c("Group.1","y","p_hat"),
         new = c("fileDate", "Actual", "Fitted"))
ci_pd_quarterly_out <- melt(ci_pd_quarterly_out, id = "fileDate")

cbPalette <- c("#000000", "#0072B2")

ci_pd_out_plot <- ggplot(ci_pd_quarterly_out, aes(x=fileDate, y = value, color=variable)) + 
  geom_line() + scale_colour_manual(values=cbPalette) + xlab("Date") + ylab("Default Rate") + ggtitle("Ave. Default Rate CI Out-of-Sample") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=15)) + theme(legend.title=element_blank())
ci_pd_out_plot

pdf("./R output/CI_PD_actual_fitted_outsample.pdf", height = 5, width = 10)
ci_pd_out_plot
dev.off()


###########################################
## Forecast for 3 scenarios

for(scenario in c("base", "adverse", "severe")){
  
  print(paste0("==== ", scenario, " ===="))
  
  ci_forecast <- read.csv(paste0("./Data output/df_boh_",scenario, ".csv"))
  ci_forecast$fileDate <- as.Date(ci_forecast$fileDate, "%Y-%m-%d")
  ci_forecast <- filter(ci_forecast, portfolio_id == "CI" & as.Date(fileDate) > as.Date("2016-03-31") )
  
  ## delete obs with naicsCode = 0
  ci_forecast <- filter(ci_forecast, naicsCode != 0)
  ci_forecast <- filter(ci_forecast, !is.na(POB))
  
  ## create final input variables
  ## (1) variable boh_rating1
  ci_forecast$boh_rating1 <- ifelse(ci_forecast$boh_rating %in% c(1,2), "R1",
                                    ifelse(ci_forecast$boh_rating %in% c(0,3,4,1000), "R2",
                                           ifelse(ci_forecast$boh_rating %in% c(2000), "R3",
                                                  ifelse(ci_forecast$boh_rating %in% c(3000,4000), "R4", "error")  
                                           )))
  
  ci_forecast$boh_rating1_R1 <- ifelse(ci_forecast$boh_rating1 == "R1",1, 0)
  ci_forecast$boh_rating1_R2 <- ifelse(ci_forecast$boh_rating1 == "R2",1, 0)
  ci_forecast$boh_rating1_R4 <- ifelse(ci_forecast$boh_rating1 == "R4",1, 0)
  
  
  ## (2) variable naics_code3
  ci_forecast$naicsCode_ch <- substr(as.character(ci_forecast$naicsCode),1,2)
  ci_forecast$naicsCode_2d <- as.numeric(as.character(ci_forecast$naicsCode_ch))
  ci_forecast$naics_code3 <- ifelse(ci_forecast$naicsCode_2d %in% c(72,44,45), "c",
                                    ifelse(ci_forecast$naicsCode_2d %in% c(42), "d",
                                           ifelse(ci_forecast$naicsCode_2d %in% c(31,32,33), "g", "h")
                                    ))
  
  ci_forecast$naics_code3_c <- ifelse(ci_forecast$naics_code3 == "c",1, 0)
  ci_forecast$naics_code3_d <- ifelse(ci_forecast$naics_code3 == "d",1, 0)
  ci_forecast$naics_code3_g <- ifelse(ci_forecast$naics_code3 == "g",1, 0)
  
    ## (3) variable season
  ci_forecast$season <- ifelse(ci_forecast$q == 1, "sp",
                               ifelse(ci_forecast$q == 2, "su",
                                      ifelse(ci_forecast$q ==3, "fa", "wi")))
  
  ci_forecast$season_fall <- ifelse(ci_forecast$season == "fa", 1, 0)
  ci_forecast$season_summer <- ifelse(ci_forecast$season == "su", 1, 0)
  ci_forecast$season_winter <- ifelse(ci_forecast$season == "wi", 1, 0)
  
  ## (4) variable cahpi_ag_lag_3_n12
  ci_forecast$cahpi_ag_lag_3_n12 <- ifelse(ci_forecast$CAHPI_ag_lag_3 <= -12, -12, ci_forecast$CAHPI_ag_lag_3)
  
  ## (5) variable dpd0129_0
  ci_forecast$dpd0129_0 <- ifelse(ci_forecast$dpd0129 == 0, 1, 0)
  
  ## (6) variables POB_5 and POB_50
  ci_forecast$POB_5 <- ifelse(ci_forecast$POB >= 5, 5, ci_forecast$POB)
  
  ci_forecast$POB_50 <- ifelse(ci_forecast$POB >= 50, 50, 
                               ifelse(ci_forecast$POB <= 5, 5, ci_forecast$POB))
  
  ## get pd forecast p_hat for each account
  ci_forecast <- as.data.table(ci_forecast)
  ci_forecast$p_hat <- as.matrix (ci_forecast[, coef_ci$X[-1],with = FALSE]) %*% coef_ci$Estimate[-1] + 
    coef_ci$Estimate[1] 
  ci_forecast$p_hat <- 1/(1+exp(-ci_forecast$p_hat))
  
  ## get quarterly average PD
  ci_pd_quarterly_9Q <- subset(ci_forecast, select = c(fileDate, p_hat))
  ci_pd_quarterly_9Q <- aggregate(ci_pd_quarterly_9Q[,2], list(ci_pd_quarterly_9Q$fileDate), mean)
  
  setnames(ci_pd_quarterly_9Q, old = c("Group.1","p_hat"),
           new = c("fileDate", "value"))
  ci_pd_quarterly_9Q$variable <- scenario
  ci_pd_quarterly_9Q <- ci_pd_quarterly_9Q[,c(1,3,2)]
  assign(paste0("ci_pd_quarterly_",scenario), ci_pd_quarterly_9Q)
}

ci_pd_quarterly_9Q <- rbind(ci_pd_quarterly_base, ci_pd_quarterly_adverse, ci_pd_quarterly_severe)
ci_pd_quarterly_all <- rbind(ci_pd_quarterly, ci_pd_quarterly_9Q)
setnames(ci_pd_quarterly_all, old = c("variable", "value"), 
         new = c("scenario","PD"))


## final plot
cbPalette <- c("#000000", "#0072B2", "#006600", "#E69F00", "#D55E00")
ci_pd_plot <- ggplot(ci_pd_quarterly_all, aes(x = fileDate, y = PD, color = scenario)) + 
  geom_line() + scale_colour_manual(values=cbPalette) + 
  ggtitle("BOH CI PD") + xlab("Date") + ylab("Default Rate") + ggtitle("Average Default Rate CI") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=15))
ci_pd_plot

pdf("./R output/CI_PD_actual_fitted_forecast.pdf", height = 5, width = 10)
  ci_pd_plot
dev.off()

## output results
write.csv(ci_pd_quarterly_all, "./R output/CI_PD_quarterly_actual_fitted_forecast.csv", row.names = FALSE)

# Summary stats per sample
options(scipen=999)

#sum_nms <- c("y","boh_rating1_R1","boh_rating1_R2","boh_rating1_R1","boh_rating1_R2","boh_rating1_R4","naics_code3_c","naics_code3_d", "season_fall","season_summer","season_winter","CAUR_yd_lag_3","cahpi_ag_lag_3_n12","dpd0129_0","POB_5","POB_50")

sum_nms <- c("y","season_fall","season_summer","season_winter","CAUR_yd_lag_3","cahpi_ag_lag_3_n12","dpd0129","POB")


training_df <- as.data.frame.matrix(ci_dev_training)
testing_df <- as.data.frame.matrix(ci_dev_outsample)

summary(training_df)

# Make the summary stats table between the samples
ci_dev_mean <- apply(ci_dev[,which(colnames(ci_dev) %in% sum_nms),drop=F],2,function (x) round(mean(x),4))
ci_dev_in_mean <- apply(training_df[,which(colnames(training_df) %in% sum_nms),drop=F],2,function (x) round(mean(x),4))
ci_dev_out_mean <- apply(testing_df[,which(colnames(testing_df) %in% sum_nms),drop=F],2,function (x) round(mean(x),4))

ci_dev_sd <- apply(ci_dev[,which(colnames(ci_dev) %in% sum_nms),drop=F],2,function (x) round(sd(x),4))
ci_dev_in_sd <- apply(training_df[,which(colnames(training_df) %in% sum_nms),drop=F],2,function (x) round(sd(x),4))
ci_dev_out_sd <- apply(testing_df[,which(colnames(testing_df) %in% sum_nms),drop=F],2,function (x) round(sd(x),4))

ci_dev_max <- apply(ci_dev[,which(colnames(ci_dev) %in% sum_nms),drop=F],2,function (x) round(max(x),4))
ci_dev_in_max <- apply(training_df[,which(colnames(training_df) %in% sum_nms),drop=F],2,function (x) round(max(x),4))
ci_dev_out_max <- apply(testing_df[,which(colnames(testing_df) %in% sum_nms),drop=F],2,function (x) round(max(x),4))

ci_dev_min <- apply(ci_dev[,which(colnames(ci_dev) %in% sum_nms),drop=F],2,function (x) round(min(x),4))
ci_dev_in_min <- apply(training_df[,which(colnames(training_df) %in% sum_nms),drop=F],2,function (x) round(min(x),4))
ci_dev_out_min <- apply(testing_df[,which(colnames(testing_df) %in% sum_nms),drop=F],2,function (x) round(min(x),4))

ci_dev_n <- apply(ci_dev[,which(colnames(ci_dev) %in% sum_nms),drop=F],2, length)
ci_dev_in_n <- apply(training_df[,which(colnames(training_df) %in% sum_nms),drop=F],2, length)
ci_dev_out_n <- apply(testing_df[,which(colnames(testing_df) %in% sum_nms),drop=F],2, length)

ci_df_sample_stats <- rbind(
  ci_dev_mean, ci_dev_in_mean, ci_dev_out_mean,
  ci_dev_sd, ci_dev_in_sd, ci_dev_out_sd,
  ci_dev_max, ci_dev_in_max, ci_dev_out_max,
  ci_dev_min, ci_dev_in_min, ci_dev_out_min,
  ci_dev_n, ci_dev_in_n, ci_dev_out_n
)


rownames(ci_df_sample_stats) <- c("Mean (All Obs)","Mean (Training)","Mean (Test)","SD (All Obs)","SD (Training)","SD (Test)","Max (All Obs)","Max (Training)","Max (Test)","Min (All Obs)","Min (Training)","Min (Test)","Obs (All Obs)","Obs (Training)","Obs (Test)")

write.csv(ci_df_sample_stats, "./R output/ci_df_sample_stats.csv")

##################

# Plots

# Generate Data profiling bins
ci_dev$pob_bins <- cut(ci_dev$POB,breaks = 6)
ci_dev$caur_bins <- cut(ci_dev$CAUR_yd,breaks = 14)

breaks_hpi = c(-24,-12,-2,3,7,10)
ci_dev$cahpi_bins <- cut(ci_dev$CAHPI_ag,breaks = breaks_hpi)
ci_dev$pastdue_bins_1_29 <- ifelse(ci_dev$dpd0129<=29,ci_dev$dpd0129,NA)
ci_dev$pastdue_bins <- cut(ci_dev$pastdue_bins_1_29,breaks = 5) #days past due

ci_dev$naics_grp <- ifelse(as.numeric(substr(ci_dev$naicsCode,start=1,stop=2)) == 42,"G1",
  ifelse(as.numeric(substr(ci_dev$naicsCode,start=1,stop=2)) %in% c(31,32,33) ,"G2",
    ifelse(as.numeric(substr(ci_dev$naicsCode,start=1,stop=2)) %in% c(72,44,45),"G3","Other")))


# Generate in sample bins:
#ci_dev_training$risk_bins <- ifelse(ci_dev_training$boh_rating1_R1==1,"R1",ifelse(ci_dev_training$boh_rating1_R2==1,"R2","R4"))
ci_dev_training$risk_bins <- ifelse(ci_dev_training$boh_rating1=="R1","R1",ifelse(ci_dev_training$boh_rating1=="R2","R2",ifelse(ci_dev_training$boh_rating1=="R3","R3",ifelse(ci_dev_training$boh_rating1=="R4","R4","Other"))))

ci_dev_training$season_bins  <- ifelse(ci_dev_training$season_fall==1,"Fall",ifelse(ci_dev_training$season_summer==1,"Summer",ifelse(ci_dev_training$season_winter==1,"Winter","Other")))
ci_dev_training$caur_bins <- cut(ci_dev_training$CAUR_yd_lag_3,breaks = 2)
ci_dev_training$cahpi_bins <- cut(ci_dev_training$cahpi_ag_lag_3_n12,breaks = 3)
ci_dev_training$pob5_bins <- cut(ci_dev_training$POB_5,breaks = 2)
ci_dev_training$pob50_bins <- cut(ci_dev_training$POB_50,breaks = 6)

ci_dev_training$pob_bins <- cut(ci_dev_training$POB,breaks = 6)

ci_dev_training$naics_grp <- ifelse(as.numeric(substr(ci_dev_training$naicsCode,start=1,stop=2)) == 42,"G1",
                           ifelse(as.numeric(substr(ci_dev_training$naicsCode,start=1,stop=2)) %in% c(31,32,33) ,"G2",
                                  ifelse(as.numeric(substr(ci_dev_training$naicsCode,start=1,stop=2)) %in% c(72,44,45),"G3","Other")))


# Generate out sample bins:
#ci_dev_outsample$risk_bins <- ifelse(ci_dev_outsample$boh_rating1_R1==1,"R1",ifelse(ci_dev_outsample$boh_rating1_R2==1,"R2","R4"))

ci_dev_outsample$risk_bins <- ifelse(ci_dev_outsample$boh_rating1=="R1","R1",ifelse(ci_dev_outsample$boh_rating1=="R2","R2",ifelse(ci_dev_outsample$boh_rating1=="R3","R3",ifelse(ci_dev_outsample$boh_rating1=="R4","R4","Other"))))


ci_dev_outsample$season_bins  <- ifelse(ci_dev_outsample$season_fall==1,"Fall",ifelse(ci_dev_outsample$season_summer==1,"Summer",ifelse(ci_dev_outsample$season_winter==1,"Winter","Other")))
ci_dev_outsample$caur_bins <- cut(ci_dev_outsample$CAUR_yd_lag_3,breaks = 2)
ci_dev_outsample$cahpi_bins <- cut(ci_dev_outsample$cahpi_ag_lag_3_n12,breaks = 3)
ci_dev_outsample$pob5_bins <- cut(ci_dev_outsample$POB_5,breaks = 2)
ci_dev_outsample$pob50_bins <- cut(ci_dev_outsample$POB_50,breaks = 6)

ci_dev_outsample$pob_bins <- cut(ci_dev_outsample$POB,breaks = 6)


ci_dev_outsample$naics_grp <- ifelse(as.numeric(substr(ci_dev_outsample$naicsCode,start=1,stop=2)) == 42,"G1",
                                    ifelse(as.numeric(substr(ci_dev_outsample$naicsCode,start=1,stop=2)) %in% c(31,32,33) ,"G2",
                                           ifelse(as.numeric(substr(ci_dev_outsample$naicsCode,start=1,stop=2)) %in% c(72,44,45),"G3","Other")))


# DATA PROFILING PLOTS ###################################

#Making average actual default and plot df by data and bin - DATA PROFILING
# POB
defaulters <- ci_dev %>% group_by(fileDate, pob_bins) %>% summarise(Defaulters = sum(y)) %>% data.frame()
nondefaulters <- ci_dev %>% group_by(fileDate, pob_bins) %>% filter(y==0) %>% count() %>% data.frame()
actual_df <- merge(defaulters, nondefaulters, by.x = c("fileDate","pob_bins"), by.y = c("fileDate","pob_bins"))
actual_df$total_obs <- actual_df$Defaulters+actual_df$n

actual_df <- actual_df %>% group_by(pob_bins) %>% mutate(pd_actual = 100*Defaulters/lag(n)) %>% data.frame() %>% na.omit
#actual_df <- actual_df %>% group_by(pob_bins) %>% mutate(pd_actual = 100*Defaulters/n) %>% data.frame() %>% na.omit
colnames(actual_df) <- c("fileDate","bins","Defaulters","Nondefaulters","Observations","PD_Actual")
df <- actual_df %>% group_by(bins) %>% summarise(pd_actual_m = mean(PD_Actual), obs_sum = sum(Observations)) %>% data.frame()
colnames(df) <- c("POB", "PD", "Observations")

# Start Plot
blue <- rgb(0, 0, 1, alpha=0.2)
pdf(paste("./R output", paste("CI Default Rate - ",colnames(df)[1],"_profile.pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))
with(df, plot(as.numeric(row.names(df)), PD, col="green4", type = "l", xaxt = "n",lwd=2,main = paste("Default Rate",colnames(df)[1], sep=" - "),
              ylab="Default Rate (%)",xlab=names(df)[1],
              ylim=c(0,max(apply(df[,2,drop=F], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=blue))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("topleft",
       legend=c("PD","Obs."),
       lty=1,lwd=5, col=c("green4",blue))
axis(1, at=bp, labels=df[,1])
dev.off()
# End Plot

#Making average actual default and plot df by data and bin - DATA PROFILING
# CAUR
caur_breaks <- c(-1.25,-0.75,0,1,2,3)
ci_dev$caur_bins <- cut(ci_dev$CAUR_yd,breaks = caur_breaks)
defaulters <- ci_dev %>% group_by(fileDate, caur_bins) %>% summarise(Defaulters = sum(y)) %>% data.frame()
nondefaulters <- ci_dev %>% group_by(fileDate, caur_bins) %>% filter(y==0) %>% count() %>% data.frame()
actual_df <- merge(defaulters, nondefaulters, by.x = c("fileDate","caur_bins"), by.y = c("fileDate","caur_bins"))
actual_df$total_obs <- actual_df$Defaulters+actual_df$n

actual_df <- actual_df %>% group_by(caur_bins) %>% mutate(pd_actual = 100*Defaulters/lag(n)) %>% data.frame() %>% na.omit
#actual_df <- actual_df %>% group_by(caur_bins) %>% mutate(pd_actual = 100*Defaulters/n) %>% data.frame() %>% na.omit
colnames(actual_df) <- c("fileDate","bins","Defaulters","Nondefaulters","Observations","PD_Actual")
df <- actual_df %>% group_by(bins) %>% summarise(pd_actual_m = mean(PD_Actual), obs_sum = sum(Observations)) %>% data.frame()
colnames(df) <- c("CAUR", "PD", "Observations")

# Start Plot
blue <- rgb(0, 0, 1, alpha=0.2)
pdf(paste("./R output", paste("CI Default Rate - ",colnames(df)[1],"_profile.pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))
with(df, plot(as.numeric(row.names(df)), PD, col="green4", type = "l", xaxt = "n",lwd=2,main = paste("Default Rate",colnames(df)[1], sep=" - "),
              ylab="Default Rate (%)",xlab=names(df)[1],
              ylim=c(0,max(apply(df[,2,drop=F], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=blue))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("topright",
       legend=c("PD","Obs."),
       lty=1,lwd=5, col=c("green4",blue))
axis(1, at=bp, labels=df[,1])
dev.off()
# End Plot

#Making average actual default and plot df by data and bin - DATA PROFILING
# CAHPI
defaulters <- ci_dev %>% group_by(fileDate, cahpi_bins) %>% summarise(Defaulters = sum(y)) %>% data.frame()
nondefaulters <- ci_dev %>% group_by(fileDate, cahpi_bins) %>% filter(y==0) %>% count() %>% data.frame()
actual_df <- merge(defaulters, nondefaulters, by.x = c("fileDate","cahpi_bins"), by.y = c("fileDate","cahpi_bins"))
actual_df$total_obs <- actual_df$Defaulters+actual_df$n

actual_df <- actual_df %>% group_by(cahpi_bins) %>% mutate(pd_actual = 100*Defaulters/lag(n)) %>% data.frame() %>% na.omit
#actual_df <- actual_df %>% group_by(cahpi_bins) %>% mutate(pd_actual = 100*Defaulters/n) %>% data.frame() %>% na.omit
colnames(actual_df) <- c("fileDate","bins","Defaulters","Nondefaulters","Observations","PD_Actual")
df <- actual_df %>% group_by(bins) %>% summarise(pd_actual_m = mean(PD_Actual), obs_sum = sum(Observations)) %>% data.frame()
colnames(df) <- c("CAHPI", "PD", "Observations")

# Start Plot
blue <- rgb(0, 0, 1, alpha=0.2)
pdf(paste("./R output", paste("CI Default Rate - ",colnames(df)[1],"_profile.pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))
with(df, plot(as.numeric(row.names(df)), PD, col="green4", type = "l", xaxt = "n",lwd=2,main = paste("Default Rate",colnames(df)[1], sep=" - "),
              ylab="Default Rate (%)",xlab=names(df)[1],
              ylim=c(0,max(apply(df[,2,drop=F], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=blue))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("topleft",
       legend=c("PD","Obs."),
       lty=1,lwd=5, col=c("green4",blue))
axis(1, at=bp, labels=df[,1])
dev.off()
# End Plot

#Making average actual default and plot df by data and bin - DATA PROFILING
# PAST DUE
defaulters <- ci_dev %>% group_by(fileDate, pastdue_bins) %>% summarise(Defaulters = sum(y)) %>% data.frame()
nondefaulters <- ci_dev %>% group_by(fileDate, pastdue_bins) %>% filter(y==0) %>% count() %>% data.frame()
actual_df <- merge(defaulters, nondefaulters, by.x = c("fileDate","pastdue_bins"), by.y = c("fileDate","pastdue_bins"))
actual_df$total_obs <- actual_df$Defaulters+actual_df$n

actual_df <- actual_df %>% group_by(pastdue_bins) %>% mutate(pd_actual = 100*Defaulters/lag(n)) %>% data.frame() %>% na.omit
colnames(actual_df) <- c("fileDate","bins","Defaulters","Nondefaulters","Observations","PD_Actual")
df <- actual_df %>% group_by(bins) %>% summarise(pd_actual_m = mean(PD_Actual), obs_sum = sum(Observations)) %>% data.frame()
colnames(df) <- c("Past Due", "PD", "Observations")

# Start Plot
blue <- rgb(0, 0, 1, alpha=0.2)
pdf(paste("./R output", paste("CI Default Rate - ",colnames(df)[1],"_profile.pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))
with(df, plot(as.numeric(row.names(df)), PD, col="green4", type = "l", xaxt = "n",lwd=2,main = paste("Default Rate",colnames(df)[1], sep=" - "),
              ylab="Default Rate (%)",xlab=names(df)[1],
              ylim=c(0,max(apply(df[,2,drop=F], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=blue))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("topleft",
       legend=c("PD","Obs."),
       lty=1,lwd=5, col=c("green4",blue))
axis(1, at=bp, labels=df[,1])
dev.off()
# End Plot

#Making average actual default and plot df by data and bin - DATA PROFILING
# LOAN RATING
defaulters <- ci_dev %>% group_by(fileDate, boh_rating) %>% summarise(Defaulters = sum(y)) %>% data.frame()
nondefaulters <- ci_dev %>% group_by(fileDate, boh_rating) %>% filter(y==0) %>% count() %>% data.frame()
actual_df <- merge(defaulters, nondefaulters, by.x = c("fileDate","boh_rating"), by.y = c("fileDate","boh_rating"))
actual_df$total_obs <- actual_df$Defaulters+actual_df$n

actual_df <- actual_df %>% group_by(boh_rating) %>% mutate(pd_actual = 100*Defaulters/lag(n)) %>% data.frame() %>% na.omit
colnames(actual_df) <- c("fileDate","bins","Defaulters","Nondefaulters","Observations","PD_Actual")
df <- actual_df %>% group_by(bins) %>% summarise(pd_actual_m = mean(PD_Actual), obs_sum = sum(Observations)) %>% data.frame()
colnames(df) <- c("Loan Rating", "PD", "Observations")

# Start Plot
blue <- rgb(0, 0, 1, alpha=0.2)
pdf(paste("./R output", paste("CI Default Rate - ",colnames(df)[1],"_profile.pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))
with(df, plot(as.numeric(row.names(df)), PD, col="green4", type = "l", xaxt = "n",lwd=2,main = paste("Default Rate",colnames(df)[1], sep=" - "),
              ylab="Default Rate (%)",xlab=names(df)[1],
              ylim=c(0,max(apply(df[,2,drop=F], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=blue))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("topleft",
       legend=c("PD","Obs."),
       lty=1,lwd=5, col=c("green4",blue))
axis(1, at=bp, labels=df[,1])
dev.off()
# End Plot





# IN SAMPLE PLOTS #######################

#Making average actual default and plot df by data and bin - IN SAMPLE
# Risk IN
defaulters <- ci_dev_training %>% group_by(fileDate, risk_bins) %>% summarise(Defaulters = sum(y)) %>% data.frame()
nondefaulters <- ci_dev_training %>% group_by(fileDate, risk_bins) %>% filter(y==0) %>% count() %>% data.frame()
actual_df <- merge(defaulters, nondefaulters, by.x = c("fileDate","risk_bins"), by.y = c("fileDate","risk_bins"))
actual_df <- actual_df %>% group_by(risk_bins) %>% mutate(pd_actual = Defaulters/lag(n)) %>% data.frame() %>% na.omit
colnames(actual_df) <- c("fileDate","bins","Defaulters","Nondefaulters","PD_Actual")
actual_df <- aggregate(actual_df$PD_Actual, list(actual_df$bins), mean)
colnames(actual_df) <- c("bins","PD_Actual")

# Observation numbers and estiamte and final plot df
estimate_df <- as.data.frame(count(ci_dev_training, risk_bins))
estimate_df$mean <- aggregate(ci_dev_training$p_hat, list(ci_dev_training$risk_bins), mean)[,2]
colnames(estimate_df) <- c("bins","Observations","Mean")
df <- merge(estimate_df, actual_df)
colnames(df) <- c("Risk", "Observations", "Estimate", "Actual")
df <- df %>% filter(Risk != "R4")

# Start Plot
green <- rgb(.5, 1, .5, alpha=0.2)
pdf(paste("./R output", paste("CI Default Rate - ",colnames(df)[1],".pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))
with(df, plot(as.numeric(row.names(df)), Estimate, col="red", type = "l", xaxt = "n",lwd=2,main = paste("In-sample: Default Rate",colnames(df)[1], sep=" - "),
              ylab="Default Rate",xlab=names(df)[1],
              ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
with(df, plot(as.numeric(row.names(df)), Actual, col="blue", xaxt = "n",type = "l", lwd=2,
              ylab="", xlab="", ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=green))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("topleft",
       legend=c("Estimate", "Actual","Obs."),
       lty=1,lwd=5, col=c("red3","blue", green))
axis(1, at=bp, labels=df[,1])
dev.off()
# End Plot


# Season IN
defaulters <- ci_dev_training %>% group_by(fileDate, season_bins) %>% summarise(Defaulters = sum(y)) %>% data.frame()
nondefaulters <- ci_dev_training %>% group_by(fileDate, season_bins) %>% filter(y==0) %>% count() %>% data.frame()
actual_df <- merge(defaulters, nondefaulters, by.x = c('fileDate','season_bins'), by.y = c('fileDate','season_bins'))
actual_df <- actual_df %>% group_by(season_bins) %>% mutate(pd_actual = Defaulters/lag(n)) %>% data.frame() %>% na.omit
colnames(actual_df) <- c("fileDate","bins","Defaulters","Nondefaulters","PD_Actual")
actual_df <- aggregate(actual_df$PD_Actual, list(actual_df$bins), mean)
colnames(actual_df) <- c("bins","PD_Actual")

# Observation numbers and estiamte and final plot df
estimate_df <- as.data.frame(count(ci_dev_training, season_bins))
estimate_df$mean <- aggregate(ci_dev_training$p_hat, list(ci_dev_training$season_bins), mean)[,2]
colnames(estimate_df) <- c("bins","Observations","Mean")
df <- merge(estimate_df, actual_df)
colnames(df) <- c("Season", "Observations", "Estimate", "Actual")

# Start Plot
green <- rgb(.5, 1, .5, alpha=0.2)
pdf(paste("./R output", paste("CI Default Rate - ",colnames(df)[1],".pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))
with(df, plot(as.numeric(row.names(df)), Estimate, col="red", type = "l", xaxt = "n",lwd=2,main = paste("In-sample: Default Rate",colnames(df)[1], sep=" - "),
              ylab="Default Rate",xlab=names(df)[1],
              ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
with(df, plot(as.numeric(row.names(df)), Actual, col="blue", xaxt = "n",type = "l", lwd=2,
              ylab="", xlab="", ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=green))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("topleft",
       legend=c("Estimate", "Actual","Obs."),
       lty=1,lwd=5, col=c("red3","blue", green))
axis(1, at=bp, labels=df[,1])
dev.off()
# End Plot




# CA UR IN
defaulters <- ci_dev_training %>% group_by(fileDate, caur_bins) %>% summarise(Defaulters = sum(y)) %>% data.frame()
nondefaulters <- ci_dev_training %>% group_by(fileDate, caur_bins) %>% filter(y==0) %>% count() %>% data.frame()
actual_df <- merge(defaulters, nondefaulters, by.x = c('fileDate','caur_bins'), by.y = c('fileDate','caur_bins'))
actual_df <- actual_df %>% group_by(caur_bins) %>% mutate(pd_actual = Defaulters/lag(n)) %>% data.frame() %>% na.omit
colnames(actual_df) <- c("fileDate","bins","Defaulters","Nondefaulters","PD_Actual")
actual_df <- aggregate(actual_df$PD_Actual, list(actual_df$bins), mean)
colnames(actual_df) <- c("bins","PD_Actual")

# Observation numbers and estiamte and final plot df
estimate_df <- as.data.frame(count(ci_dev_training, caur_bins))
estimate_df$mean <- aggregate(ci_dev_training$p_hat, list(ci_dev_training$caur_bins), mean)[,2]
colnames(estimate_df) <- c("bins","Observations","Mean")
df <- merge(estimate_df, actual_df)
colnames(df) <- c("CA_UR", "Observations", "Estimate", "Actual")

# Start Plot
green <- rgb(.5, 1, .5, alpha=0.2)
pdf(paste("./R output", paste("CI Default Rate - ",colnames(df)[1],".pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))
with(df, plot(as.numeric(row.names(df)), Estimate, col="red", type = "l", xaxt = "n",lwd=2,main = paste("In-sample: Default Rate",colnames(df)[1], sep=" - "),
              ylab="Default Rate",xlab=names(df)[1],
              ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
with(df, plot(as.numeric(row.names(df)), Actual, col="blue", xaxt = "n",type = "l", lwd=2,
              ylab="", xlab="", ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=green))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("topleft",
       legend=c("Estimate", "Actual","Obs."),
       lty=1,lwd=5, col=c("red3","blue", green))
axis(1, at=bp, labels=df[,1])
dev.off()
# End Plot




# CA HPI IN
defaulters <- ci_dev_training %>% group_by(fileDate, cahpi_bins) %>% summarise(Defaulters = sum(y)) %>% data.frame()
nondefaulters <- ci_dev_training %>% group_by(fileDate, cahpi_bins) %>% filter(y==0) %>% count() %>% data.frame()
actual_df <- merge(defaulters, nondefaulters, by.x = c('fileDate','cahpi_bins'), by.y = c('fileDate','cahpi_bins'))
actual_df <- actual_df %>% group_by(cahpi_bins) %>% mutate(pd_actual = Defaulters/lag(n)) %>% data.frame() %>% na.omit
colnames(actual_df) <- c("fileDate","bins","Defaulters","Nondefaulters","PD_Actual")
actual_df <- aggregate(actual_df$PD_Actual, list(actual_df$bins), mean)
colnames(actual_df) <- c("bins","PD_Actual")

# Observation numbers and estiamte and final plot df
estimate_df <- as.data.frame(count(ci_dev_training, cahpi_bins))
estimate_df$mean <- aggregate(ci_dev_training$p_hat, list(ci_dev_training$cahpi_bins), mean)[,2]
colnames(estimate_df) <- c("bins","Observations","Mean")
df <- merge(estimate_df, actual_df)
colnames(df) <- c("CA_HPI", "Observations", "Estimate", "Actual")

# Start Plot
green <- rgb(.5, 1, .5, alpha=0.2)
pdf(paste("./R output", paste("CI Default Rate - ",colnames(df)[1],".pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))
with(df, plot(as.numeric(row.names(df)), Estimate, col="red", type = "l", xaxt = "n",lwd=2,main = paste("In-sample: Default Rate",colnames(df)[1], sep=" - "),
              ylab="Default Rate",xlab=names(df)[1],
              ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
with(df, plot(as.numeric(row.names(df)), Actual, col="blue", xaxt = "n",type = "l", lwd=2,
              ylab="", xlab="", ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=green))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("topright",
       legend=c("Estimate", "Actual","Obs."),
       lty=1,lwd=5, col=c("red3","blue", green))
axis(1, at=bp, labels=df[,1])
dev.off()
# End Plot



# POB 5 IN
defaulters <- ci_dev_training %>% group_by(fileDate, pob5_bins) %>% summarise(Defaulters = sum(y)) %>% data.frame()
nondefaulters <- ci_dev_training %>% group_by(fileDate, pob5_bins) %>% filter(y==0) %>% count() %>% data.frame()
actual_df <- merge(defaulters, nondefaulters, by.x = c('fileDate','pob5_bins'), by.y = c('fileDate','pob5_bins'))
actual_df <- actual_df %>% group_by(pob5_bins) %>% mutate(pd_actual = Defaulters/lag(n)) %>% data.frame() %>% na.omit
colnames(actual_df) <- c("fileDate","bins","Defaulters","Nondefaulters","PD_Actual")
actual_df <- aggregate(actual_df$PD_Actual, list(actual_df$bins), mean)
colnames(actual_df) <- c("bins","PD_Actual")

# Observation numbers and estiamte and final plot df
estimate_df <- as.data.frame(count(ci_dev_training, pob5_bins))
estimate_df$mean <- aggregate(ci_dev_training$p_hat, list(ci_dev_training$pob5_bins), mean)[,2]
colnames(estimate_df) <- c("bins","Observations","Mean")
df <- merge(estimate_df, actual_df)
colnames(df) <- c("POB_5", "Observations", "Estimate", "Actual")

# Start Plot
green <- rgb(.5, 1, .5, alpha=0.2)
pdf(paste("./R output", paste("CI Default Rate - ",colnames(df)[1],".pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))
with(df, plot(as.numeric(row.names(df)), Estimate, col="red", type = "l", xaxt = "n",lwd=2,main = paste("In-sample: Default Rate",colnames(df)[1], sep=" - "),
              ylab="Default Rate",xlab=names(df)[1],
              ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
with(df, plot(as.numeric(row.names(df)), Actual, col="blue", xaxt = "n",type = "l", lwd=2,
              ylab="", xlab="", ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=green))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("topleft",
       legend=c("Estimate", "Actual","Obs."),
       lty=1,lwd=5, col=c("red3","blue", green))
axis(1, at=bp, labels=df[,1])
dev.off()




# POB 50 IN
defaulters <- ci_dev_training %>% group_by(fileDate, pob50_bins) %>% summarise(Defaulters = sum(y)) %>% data.frame()
nondefaulters <- ci_dev_training %>% group_by(fileDate, pob50_bins) %>% filter(y==0) %>% count() %>% data.frame()
actual_df <- merge(defaulters, nondefaulters, by.x = c('fileDate','pob50_bins'), by.y = c('fileDate','pob50_bins'))
actual_df <- actual_df %>% group_by(pob50_bins) %>% mutate(pd_actual = Defaulters/lag(n)) %>% data.frame() %>% na.omit
colnames(actual_df) <- c("fileDate","bins","Defaulters","Nondefaulters","PD_Actual")
actual_df <- aggregate(actual_df$PD_Actual, list(actual_df$bins), mean)
colnames(actual_df) <- c("bins","PD_Actual")

# Observation numbers and estiamte and final plot df
estimate_df <- as.data.frame(count(ci_dev_training, pob50_bins))
estimate_df$mean <- aggregate(ci_dev_training$p_hat, list(ci_dev_training$pob50_bins), mean)[,2]
colnames(estimate_df) <- c("bins","Observations","Mean")
df <- merge(estimate_df, actual_df)
colnames(df) <- c("POB_50", "Observations", "Estimate", "Actual")


# Start Plot
green <- rgb(.5, 1, .5, alpha=0.2)
pdf(paste("./R output", paste("CI Default Rate - ",colnames(df)[1],".pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))
with(df, plot(as.numeric(row.names(df)), Estimate, col="red", type = "l", xaxt = "n",lwd=2,main = paste("In-sample: Default Rate",colnames(df)[1], sep=" - "),
              ylab="Default Rate",xlab=names(df)[1],
              ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
with(df, plot(as.numeric(row.names(df)), Actual, col="blue", xaxt = "n",type = "l", lwd=2,
              ylab="", xlab="", ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=green))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("topleft",
       legend=c("Estimate", "Actual","Obs."),
       lty=1,lwd=5, col=c("red3","blue", green))
axis(1, at=bp, labels=df[,1])
dev.off()


# POB IN
defaulters <- ci_dev_training %>% group_by(fileDate, pob_bins) %>% summarise(Defaulters = sum(y)) %>% data.frame()
nondefaulters <- ci_dev_training %>% group_by(fileDate, pob_bins) %>% filter(y==0) %>% count() %>% data.frame()
actual_df <- merge(defaulters, nondefaulters, by.x = c('fileDate','pob_bins'), by.y = c('fileDate','pob_bins'))
actual_df <- actual_df %>% group_by(pob_bins) %>% mutate(pd_actual = Defaulters/lag(n)) %>% data.frame() %>% na.omit
colnames(actual_df) <- c("fileDate","bins","Defaulters","Nondefaulters","PD_Actual")
actual_df <- aggregate(actual_df$PD_Actual, list(actual_df$bins), mean)
colnames(actual_df) <- c("bins","PD_Actual")

# Observation numbers and estiamte and final plot df
estimate_df <- as.data.frame(count(ci_dev_training, pob_bins))
estimate_df$mean <- aggregate(ci_dev_training$p_hat, list(ci_dev_training$pob_bins), mean)[,2]
colnames(estimate_df) <- c("bins","Observations","Mean")
df <- merge(estimate_df, actual_df)
colnames(df) <- c("POB", "Observations", "Estimate", "Actual")


# Start Plot
green <- rgb(.5, 1, .5, alpha=0.2)
pdf(paste("./R output", paste("CI Default Rate - ",colnames(df)[1],".pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))
with(df, plot(as.numeric(row.names(df)), Estimate, col="red", type = "l", xaxt = "n",lwd=2,main = paste("In-sample: Default Rate",colnames(df)[1], sep=" - "),
              ylab="Default Rate",xlab=names(df)[1],
              ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
with(df, plot(as.numeric(row.names(df)), Actual, col="blue", xaxt = "n",type = "l", lwd=2,
              ylab="", xlab="", ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=green))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("bottomleft",
       legend=c("Estimate", "Actual","Obs."),
       lty=1,lwd=5, col=c("red3","blue", green))
axis(1, at=bp, labels=df[,1])
dev.off()


# NAICS Group IN
defaulters <- ci_dev_training %>% group_by(fileDate, naics_grp) %>% summarise(Defaulters = sum(y)) %>% data.frame()
nondefaulters <- ci_dev_training %>% group_by(fileDate, naics_grp) %>% filter(y==0) %>% count() %>% data.frame()
actual_df <- merge(defaulters, nondefaulters, by.x = c('fileDate','naics_grp'), by.y = c('fileDate','naics_grp'))
actual_df <- actual_df %>% group_by(naics_grp) %>% mutate(pd_actual = Defaulters/lag(n)) %>% data.frame() %>% na.omit
#actual_df <- actual_df %>% group_by(naics_grp) %>% mutate(pd_actual = Defaulters/n) %>% data.frame() %>% na.omit
colnames(actual_df) <- c("fileDate","bins","Defaulters","Nondefaulters","PD_Actual")
actual_df <- aggregate(actual_df$PD_Actual, list(actual_df$bins), mean)
colnames(actual_df) <- c("bins","PD_Actual")

# Observation numbers and estiamte and final plot df
estimate_df <- as.data.frame(count(ci_dev_training, naics_grp))
estimate_df$mean <- aggregate(ci_dev_training$p_hat, list(ci_dev_training$naics_grp), mean)[,2]
colnames(estimate_df) <- c("bins","Observations","Mean")
df <- merge(estimate_df, actual_df)
colnames(df) <- c("NAICS Group", "Observations", "Estimate", "Actual")

# Start Plot
green <- rgb(.5, 1, .5, alpha=0.2)
#pdf(paste("./R output", paste("CI Default Rate - ",colnames(df)[1],".pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))
with(df, plot(as.numeric(row.names(df)), Estimate, col="red", type = "l", xaxt = "n",lwd=2,main = paste("In-sample: Default Rate",colnames(df)[1], sep=" - "),
              ylab="Default Rate",xlab=names(df)[1],
              ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
with(df, plot(as.numeric(row.names(df)), Actual, col="blue", xaxt = "n",type = "l", lwd=2,
              ylab="", xlab="", ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=green))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("bottomright",
       legend=c("Estimate", "Actual","Obs."),
       lty=1,lwd=5, col=c("red3","blue", green))
axis(1, at=bp, labels=df[,1])
#dev.off()




########################################################################

# OUT of SAMPLE



#Making average actual default and plot df by data and bin - OUT SAMPLE
# Risk OUT
defaulters <- ci_dev_outsample %>% group_by(fileDate, risk_bins) %>% summarise(Defaulters = sum(y)) %>% data.frame()
nondefaulters <- ci_dev_outsample %>% group_by(fileDate, risk_bins) %>% filter(y==0) %>% count() %>% data.frame()
actual_df <- merge(defaulters, nondefaulters, by.x = c("fileDate","risk_bins"), by.y = c("fileDate","risk_bins"))
actual_df <- actual_df %>% group_by(risk_bins) %>% mutate(pd_actual = Defaulters/lag(n)) %>% data.frame() %>% na.omit
colnames(actual_df) <- c("fileDate","bins","Defaulters","Nondefaulters","PD_Actual")
actual_df <- aggregate(actual_df$PD_Actual, list(actual_df$bins), mean)
colnames(actual_df) <- c("bins","PD_Actual")

# Observation numbers and estiamte and final plot df
estimate_df <- as.data.frame(count(ci_dev_outsample, risk_bins))
estimate_df$mean <- aggregate(ci_dev_outsample$p_hat, list(ci_dev_outsample$risk_bins), mean)[,2]
colnames(estimate_df) <- c("bins","Observations","Mean")
df <- merge(estimate_df, actual_df)
colnames(df) <- c("Risk", "Observations", "Estimate", "Actual")
df <- df %>% filter(Risk != "R4")

# Start Plot
green <- rgb(.5, 1, .5, alpha=0.2)
pdf(paste("./R output", paste("CI Default Rate - ",colnames(df)[1],"_OUT.pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))
with(df, plot(as.numeric(row.names(df)), Estimate, col="red", type = "l", xaxt = "n",lwd=2,main = paste("Out-of-sample: Default Rate",colnames(df)[1], sep=" - "),
              ylab="Default Rate",xlab=names(df)[1],
              ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
with(df, plot(as.numeric(row.names(df)), Actual, col="blue", xaxt = "n",type = "l", lwd=2,
              ylab="", xlab="", ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=green))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("topleft",
       legend=c("Estimate", "Actual","Obs."),
       lty=1,lwd=5, col=c("red3","blue", green))
axis(1, at=bp, labels=df[,1])
dev.off()
# End Plot



# Season OUT
defaulters <- ci_dev_outsample %>% group_by(fileDate, season_bins) %>% summarise(Defaulters = sum(y)) %>% data.frame()
nondefaulters <- ci_dev_outsample %>% group_by(fileDate, season_bins) %>% filter(y==0) %>% count() %>% data.frame()
actual_df <- merge(defaulters, nondefaulters, by.x = c('fileDate','season_bins'), by.y = c('fileDate','season_bins'))
actual_df <- actual_df %>% group_by(season_bins) %>% mutate(pd_actual = Defaulters/lag(n)) %>% data.frame() %>% na.omit
colnames(actual_df) <- c("fileDate","bins","Defaulters","Nondefaulters","PD_Actual")
actual_df <- aggregate(actual_df$PD_Actual, list(actual_df$bins), mean)
colnames(actual_df) <- c("bins","PD_Actual")

# Observation numbers and estiamte and final plot df
estimate_df <- as.data.frame(count(ci_dev_outsample, season_bins))
estimate_df$mean <- aggregate(ci_dev_outsample$p_hat, list(ci_dev_outsample$season_bins), mean)[,2]
colnames(estimate_df) <- c("bins","Observations","Mean")
df <- merge(estimate_df, actual_df)
colnames(df) <- c("Season", "Observations", "Estimate", "Actual")

# Start Plot
green <- rgb(.5, 1, .5, alpha=0.2)
pdf(paste("./R output", paste("CI Default Rate - ",colnames(df)[1],"_OUT.pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))
with(df, plot(as.numeric(row.names(df)), Estimate, col="red", type = "l", xaxt = "n",lwd=2,main = paste("Out-of-sample: Default Rate",colnames(df)[1], sep=" - "),
              ylab="Default Rate",xlab=names(df)[1],
              ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
with(df, plot(as.numeric(row.names(df)), Actual, col="blue", xaxt = "n",type = "l", lwd=2,
              ylab="", xlab="", ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=green))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("topleft",
       legend=c("Estimate", "Actual","Obs."),
       lty=1,lwd=5, col=c("red3","blue", green))
axis(1, at=bp, labels=df[,1])
dev.off()
# End Plot




# CA UR OUT
defaulters <- ci_dev_outsample %>% group_by(fileDate, caur_bins) %>% summarise(Defaulters = sum(y)) %>% data.frame()
nondefaulters <- ci_dev_outsample %>% group_by(fileDate, caur_bins) %>% filter(y==0) %>% count() %>% data.frame()
actual_df <- merge(defaulters, nondefaulters, by.x = c('fileDate','caur_bins'), by.y = c('fileDate','caur_bins'))
actual_df <- actual_df %>% group_by(caur_bins) %>% mutate(pd_actual = Defaulters/lag(n)) %>% data.frame() %>% na.omit
colnames(actual_df) <- c("fileDate","bins","Defaulters","Nondefaulters","PD_Actual")
actual_df <- aggregate(actual_df$PD_Actual, list(actual_df$bins), mean)
colnames(actual_df) <- c("bins","PD_Actual")

# Observation numbers and estiamte and final plot df
estimate_df <- as.data.frame(count(ci_dev_outsample, caur_bins))
estimate_df$mean <- aggregate(ci_dev_outsample$p_hat, list(ci_dev_outsample$caur_bins), mean)[,2]
colnames(estimate_df) <- c("bins","Observations","Mean")
df <- merge(estimate_df, actual_df)
colnames(df) <- c("CA_UR", "Observations", "Estimate", "Actual")

# Start Plot
green <- rgb(.5, 1, .5, alpha=0.2)
pdf(paste("./R output", paste("CI Default Rate - ",colnames(df)[1],"_OUT.pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))
with(df, plot(as.numeric(row.names(df)), Estimate, col="red", type = "l", xaxt = "n",lwd=2,main = paste("Out-of-sample: Default Rate",colnames(df)[1], sep=" - "),
              ylab="Default Rate",xlab=names(df)[1],
              ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
with(df, plot(as.numeric(row.names(df)), Actual, col="blue", xaxt = "n",type = "l", lwd=2,
              ylab="", xlab="", ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=green))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("topleft",
       legend=c("Estimate", "Actual","Obs."),
       lty=1,lwd=5, col=c("red3","blue", green))
axis(1, at=bp, labels=df[,1])
dev.off()
# End Plot




# CA HPI OUT
defaulters <- ci_dev_outsample %>% group_by(fileDate, cahpi_bins) %>% summarise(Defaulters = sum(y)) %>% data.frame()
nondefaulters <- ci_dev_outsample %>% group_by(fileDate, cahpi_bins) %>% filter(y==0) %>% count() %>% data.frame()
actual_df <- merge(defaulters, nondefaulters, by.x = c('fileDate','cahpi_bins'), by.y = c('fileDate','cahpi_bins'))
actual_df <- actual_df %>% group_by(cahpi_bins) %>% mutate(pd_actual = Defaulters/lag(n)) %>% data.frame() %>% na.omit
colnames(actual_df) <- c("fileDate","bins","Defaulters","Nondefaulters","PD_Actual")
actual_df <- aggregate(actual_df$PD_Actual, list(actual_df$bins), mean)
colnames(actual_df) <- c("bins","PD_Actual")

# Observation numbers and estiamte and final plot df
estimate_df <- as.data.frame(count(ci_dev_outsample, cahpi_bins))
estimate_df$mean <- aggregate(ci_dev_outsample$p_hat, list(ci_dev_outsample$cahpi_bins), mean)[,2]
colnames(estimate_df) <- c("bins","Observations","Mean")
df <- merge(estimate_df, actual_df)
colnames(df) <- c("CA_HPI", "Observations", "Estimate", "Actual")

# Start Plot
green <- rgb(.5, 1, .5, alpha=0.2)
pdf(paste("./R output", paste("CI Default Rate - ",colnames(df)[1],"_OUT.pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))
with(df, plot(as.numeric(row.names(df)), Estimate, col="red", type = "l", xaxt = "n",lwd=2,main = paste("Out-of-sample: Default Rate",colnames(df)[1], sep=" - "),
              ylab="Default Rate",xlab=names(df)[1],
              ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
with(df, plot(as.numeric(row.names(df)), Actual, col="blue", xaxt = "n",type = "l", lwd=2,
              ylab="", xlab="", ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=green))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("topright",
       legend=c("Estimate", "Actual","Obs."),
       lty=1,lwd=5, col=c("red3","blue", green))
axis(1, at=bp, labels=df[,1])
dev.off()
# End Plot



# POB 5 OUT
defaulters <- ci_dev_outsample %>% group_by(fileDate, pob5_bins) %>% summarise(Defaulters = sum(y)) %>% data.frame()
nondefaulters <- ci_dev_outsample %>% group_by(fileDate, pob5_bins) %>% filter(y==0) %>% count() %>% data.frame()
actual_df <- merge(defaulters, nondefaulters, by.x = c('fileDate','pob5_bins'), by.y = c('fileDate','pob5_bins'))
actual_df <- actual_df %>% group_by(pob5_bins) %>% mutate(pd_actual = Defaulters/lag(n)) %>% data.frame() %>% na.omit
colnames(actual_df) <- c("fileDate","bins","Defaulters","Nondefaulters","PD_Actual")
actual_df <- aggregate(actual_df$PD_Actual, list(actual_df$bins), mean)
colnames(actual_df) <- c("bins","PD_Actual")

# Observation numbers and estiamte and final plot df
estimate_df <- as.data.frame(count(ci_dev_outsample, pob5_bins))
estimate_df$mean <- aggregate(ci_dev_outsample$p_hat, list(ci_dev_outsample$pob5_bins), mean)[,2]
colnames(estimate_df) <- c("bins","Observations","Mean")
df <- merge(estimate_df, actual_df)
colnames(df) <- c("POB_5", "Observations", "Estimate", "Actual")

# Start Plot
green <- rgb(.5, 1, .5, alpha=0.2)
pdf(paste("./R output", paste("CI Default Rate - ",colnames(df)[1],"_OUT.pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))
with(df, plot(as.numeric(row.names(df)), Estimate, col="red", type = "l", xaxt = "n",lwd=2,main = paste("Out-of-sample: Default Rate",colnames(df)[1], sep=" - "),
              ylab="Default Rate",xlab=names(df)[1],
              ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
with(df, plot(as.numeric(row.names(df)), Actual, col="blue", xaxt = "n",type = "l", lwd=2,
              ylab="", xlab="", ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=green))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("topleft",
       legend=c("Estimate", "Actual","Obs."),
       lty=1,lwd=5, col=c("red3","blue", green))
axis(1, at=bp, labels=df[,1])
dev.off()




# POB 50 OUT
defaulters <- ci_dev_outsample %>% group_by(fileDate, pob50_bins) %>% summarise(Defaulters = sum(y)) %>% data.frame()
nondefaulters <- ci_dev_outsample %>% group_by(fileDate, pob50_bins) %>% filter(y==0) %>% count() %>% data.frame()
actual_df <- merge(defaulters, nondefaulters, by.x = c('fileDate','pob50_bins'), by.y = c('fileDate','pob50_bins'))
actual_df <- actual_df %>% group_by(pob50_bins) %>% mutate(pd_actual = Defaulters/lag(n)) %>% data.frame() %>% na.omit
colnames(actual_df) <- c("fileDate","bins","Defaulters","Nondefaulters","PD_Actual")
actual_df <- aggregate(actual_df$PD_Actual, list(actual_df$bins), mean)
colnames(actual_df) <- c("bins","PD_Actual")

# Observation numbers and estiamte and final plot df
estimate_df <- as.data.frame(count(ci_dev_outsample, pob50_bins))
estimate_df$mean <- aggregate(ci_dev_outsample$p_hat, list(ci_dev_outsample$pob50_bins), mean)[,2]
colnames(estimate_df) <- c("bins","Observations","Mean")
df <- merge(estimate_df, actual_df)
colnames(df) <- c("POB_50", "Observations", "Estimate", "Actual")


# Start Plot
green <- rgb(.5, 1, .5, alpha=0.2)
pdf(paste("./R output", paste("CI Default Rate - ",colnames(df)[1],"_OUT.pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))
with(df, plot(as.numeric(row.names(df)), Estimate, col="red", type = "l", xaxt = "n",lwd=2,main = paste("Out-of-sample: Default Rate",colnames(df)[1], sep=" - "),
              ylab="Default Rate",xlab=names(df)[1],
              ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
with(df, plot(as.numeric(row.names(df)), Actual, col="blue", xaxt = "n",type = "l", lwd=2,
              ylab="", xlab="", ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=green))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("topleft",
       legend=c("Estimate", "Actual","Obs."),
       lty=1,lwd=5, col=c("red3","blue", green))
axis(1, at=bp, labels=df[,1])
dev.off()



# POB OUT
defaulters <- ci_dev_outsample %>% group_by(fileDate, pob_bins) %>% summarise(Defaulters = sum(y)) %>% data.frame()
nondefaulters <- ci_dev_outsample %>% group_by(fileDate, pob_bins) %>% filter(y==0) %>% count() %>% data.frame()
actual_df <- merge(defaulters, nondefaulters, by.x = c('fileDate','pob_bins'), by.y = c('fileDate','pob_bins'))
actual_df <- actual_df %>% group_by(pob_bins) %>% mutate(pd_actual = Defaulters/lag(n)) %>% data.frame() %>% na.omit
colnames(actual_df) <- c("fileDate","bins","Defaulters","Nondefaulters","PD_Actual")
actual_df <- aggregate(actual_df$PD_Actual, list(actual_df$bins), mean)
colnames(actual_df) <- c("bins","PD_Actual")

# Observation numbers and estiamte and final plot df
estimate_df <- as.data.frame(count(ci_dev_outsample, pob_bins))
estimate_df$mean <- aggregate(ci_dev_outsample$p_hat, list(ci_dev_outsample$pob_bins), mean)[,2]
colnames(estimate_df) <- c("bins","Observations","Mean")
df <- merge(estimate_df, actual_df)
colnames(df) <- c("POB", "Observations", "Estimate", "Actual")


# Start Plot
green <- rgb(.5, 1, .5, alpha=0.2)
pdf(paste("./R output", paste("CI Default Rate - ",colnames(df)[1],"_OUT.pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))
with(df, plot(as.numeric(row.names(df)), Estimate, col="red", type = "l", xaxt = "n",lwd=2,main = paste("In-sample: Default Rate",colnames(df)[1], sep=" - "),
              ylab="Default Rate",xlab=names(df)[1],
              ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
with(df, plot(as.numeric(row.names(df)), Actual, col="blue", xaxt = "n",type = "l", lwd=2,
              ylab="", xlab="", ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=green))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("bottomleft",
       legend=c("Estimate", "Actual","Obs."),
       lty=1,lwd=5, col=c("red3","blue", green))
axis(1, at=bp, labels=df[,1])
dev.off()



# NAICS Code Graph

ci_dev$naics_nms <- ifelse(as.numeric(substr(ci_dev$naicsCode,start=1,stop=2)) == 23,"Const.",
  ifelse(as.numeric(substr(ci_dev$naicsCode,start=1,stop=2)) %in% c(31,32,33) ,"Manu.",
  ifelse(as.numeric(substr(ci_dev$naicsCode,start=1,stop=2)) == 42,"Whole. Trade",
  ifelse(as.numeric(substr(ci_dev$naicsCode,start=1,stop=2)) %in% c(44,45),"Retail",
  ifelse(as.numeric(substr(ci_dev$naicsCode,start=1,stop=2)) %in% c(48,49),"Transport.",
  ifelse(as.numeric(substr(ci_dev$naicsCode,start=1,stop=2)) == 51,"Info.",
  ifelse(as.numeric(substr(ci_dev$naicsCode,start=1,stop=2)) == 53,"Real Est. & Rent",
  ifelse(as.numeric(substr(ci_dev$naicsCode,start=1,stop=2)) == 54,"Sci. & Tech.",
  ifelse(as.numeric(substr(ci_dev$naicsCode,start=1,stop=2)) == 56,"Waste Mng.",
  ifelse(as.numeric(substr(ci_dev$naicsCode,start=1,stop=2)) == 62,"Health Care",
  ifelse(as.numeric(substr(ci_dev$naicsCode,start=1,stop=2)) == 71,"Arts & Ent.",
  ifelse(as.numeric(substr(ci_dev$naicsCode,start=1,stop=2)) == 72,"Acc. & Food",
  ifelse(as.numeric(substr(ci_dev$naicsCode,start=1,stop=2)) == 00,"Error","Other")))))))))))))

defaulters <- ci_dev %>% group_by(fileDate, naics_nms) %>% summarise(Defaulters = sum(y)) %>% data.frame()
nondefaulters <- ci_dev %>% group_by(fileDate, naics_nms) %>% filter(y==0) %>% count() %>% data.frame()
actual_df <- merge(defaulters, nondefaulters, by.x = c("fileDate","naics_nms"), by.y = c("fileDate","naics_nms"))
actual_df$total_obs <- actual_df$Defaulters+actual_df$n

actual_df <- actual_df %>% group_by(naics_nms) %>% mutate(pd_actual = 100*Defaulters/lag(n)) %>% data.frame() %>% na.omit
colnames(actual_df) <- c("fileDate","bins","Defaulters","Nondefaulters","Observations","PD_Actual")
df <- actual_df %>% group_by(bins) %>% summarise(pd_actual_m = mean(PD_Actual), obs_sum = sum(Observations)) %>% data.frame()
colnames(df) <- c("naics_nms", "PD", "Observations")
df <- df %>% filter(naics_nms != "Other")

# Start Plot
blue <- rgb(0, 0, 1, alpha=0.2)
#pdf(paste("./R output", paste("CI Default Rate - ",colnames(df)[1],"_profile.pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(10,5,2,5))
with(df, plot(as.numeric(row.names(df)), PD, col="green4", type = "o",xaxt = "n",lwd=2,main = "Default Rate - NAICS",
              ylab="Default Rate (%)",xlab=NA,
              ylim=c(0,max(apply(df[,2,drop=F], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=blue))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("topleft",
       legend=c("PD","Obs."),
       lty=1,lwd=5, col=c("green4",blue))
axis(1, at=bp, labels=df[,1], las=2, cex.axis=1.4)
#dev.off()
# End Plot

# Grouped Bar Plot
funProp <- function(testCol) {
  df[, testCol]/max(df[, testCol])
}

df$var.a.prop <- funProp("PD")
df$var.b.prop <- funProp("Observations")

barplot(t(as.matrix(df[, c("var.a.prop", "var.b.prop")])), beside = TRUE,
        yaxt = "n", names.arg = df$naics_nms,las=2,col=c("green4",blue),main = "Default Rate - NAICS",
        ylab="Default Rate (%)", cex.names=1.4)
axis(2, at = seq(0, max(df$var.a.prop), length.out = 10),
     labels = round(seq(0, max(df$PD), length.out = 10),2))

axis(4, at = seq(0, max(df$var.b.prop), length.out = 10),
     labels = round(seq(0, max(df$Observations), length.out = 10), 0))
mtext(side = 4, line = 3, 'Observation Count')
legend("topleft",
       legend=c("PD","Obs."),
       lty=1,lwd=5, col=c("green4",blue))



#################
# Coefficient Stability


## run logistic regression
model <- y ~ boh_rating1_R1 + boh_rating1_R2 + boh_rating1_R4 +  
  naics_code3_c + naics_code3_d +
  season_fall + season_summer + season_winter + CAUR_yd_lag_3 +
  cahpi_ag_lag_3_n12 + dpd0129_0 + POB_5 + POB_50


fit <- glm( model, family = binomial(link = "logit"), data = ci_dev_training)

summary(fit)

# Make random samples 
# Sample Number
sample_number <- 10
# Sample fraction
sample_fraction <- .6
# Round the sample size to a whole number
sample_size <- round(nrow(ci_dev)*sample_fraction)

# Sample from the df
df_samples <- list()
df_samples_out <- list()
for (i in 1:sample_number){
  ##In sample data
  df_samples[[i]] <- ci_dev[sample(nrow(ci_dev), sample_size, replace = FALSE), ]
  ##Out of sample data
  df_samples_out[[i]] <- ci_dev[-which(rownames(ci_dev) %in% rownames(df_samples[[i]])),]
}

# Estimate the original model
logit_s <- list()
for (i in 1:length(df_samples)) {
  logit_s[[i]] <- glm(model, family = binomial(link = "logit"), data = df_samples[[i]])
}

# Predict for each ols model
predict_s <- list()
for (i in 1:length(df_samples)) {
  predict_s[[i]] <- predict(logit_s[[i]], df_samples_out[[i]], type="response")
}

# Make a df for the predict df and assign names
header_predict <- paste("Sample_", seq(1:length(df_samples)),sep="")

predict_s_df <- data.frame(predict_s)
colnames(predict_s_df) <- header_predict

# Make data frame of all predictions

test_out_df <- data.frame(ci_dev_outsample,predict_s_df)
test_out_df <- test_out_df %>% group_by(fileDate) %>% mutate(defaulters = sum(y)) %>% data.frame()
test_out_df <- test_out_df %>% group_by(fileDate) %>% filter(y==0) %>% mutate(nondefaulters = n()) %>% data.frame()
test_out_df <- test_out_df %>% group_by(fileDate) %>% mutate(pd_actual = defaulters/lag(nondefaulters)) %>% data.frame()

fcst_df_nms <- c("fileDate",header_predict,"pd_actual")
test_out_df <- na.omit(test_out_df[,which(colnames(test_out_df) %in% fcst_df_nms), drop = F])

test_out_df <- aggregate(x = test_out_df[,-1],
                     FUN = mean,
                     by = list(Date = test_out_df$fileDate))

head(test_out_df)

# Plot of all Forecasts 
predict_samples_gg <- melt(test_out_df, id = "Date")

ggplot(data = predict_samples_gg, aes(x = Date, y = value, group = variable, color = variable)) + geom_line() + xlab("Date") + ylab("Default Rate") + ggtitle("Ave. Default Rate CI Out-of-Sample") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(text = element_text(size=15)) + theme(legend.title=element_blank())


# Bootstrap the regression

# Bootstrap 95% CI for regression coefficients 
library(boot)
# function to obtain regression weights 
bs = function(data, indices, formula) {
  d = data[indices,] # allows boot to select sample 
  fit = glm(formula, family = binomial(link = "logit"), data=d)
  return(coef(fit))
}
# bootstrapping with 100 replications 
results = boot(
  data=ci_dev_training, 
  statistic=bs, 
  R=20, 
  formula=model)


Names = names(results$t0)
SEs = sapply(data.frame(results$t), sd)
Coefs = as.numeric(results$t0)
zVals = Coefs / SEs
Pvals = 2*pnorm(-abs(zVals))

Formatted_Results = cbind(Names, Coefs, SEs, zVals, Pvals)


summary(glm(model, family = binomial(link = "logit"), data = ci_dev_training))

# Pot coefficient density

for (i in 1:length(names(results$t0))){
plot(density(results$t[,i]), main = paste(names(results$t0)[i],"Density",sep=" - "))
}




























