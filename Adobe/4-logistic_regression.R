################################################################################
# 
# 
# Program: 4-logistic_regression.R
# Author: Irene Hyunyou Choi
# Purpose: run logistic regression and find the important predictor 
# 
#
#
# R-version: R version 3.3.4 (2016-06-21)
# -- "Bug in Your Hair" Copyright (C) 2016 The R Foundation
# for Statistical Computing Platform: x86_64-apple-darwin13.4.0 (64-bit)
################################################################################


### Environment Settings #######################################################
pth_inputs = "C:/Users/hong3/Desktop/OULAD2/inputs"
pth_lib = "C:/Users/hong3/Desktop/OULAD2/library"
pth_out = "C:/Users/hong3/Desktop/OULAD2"
### No need to make changes below after this line ##############################

source(paste(pth_lib,"/dev-support.R", sep=""))


library("data.table")
library("dplyr")
library("ggplot2")
library("lubridate")
library("scales")
library("zoo")
library("plyr")
library("corrplot")
library("tidyr")  

library("cluster")
library("fpc")

library("foreign")
library("nnet")

library("reshape2")
library("pROC")
library('ROCR')
library("car")

##########################################################################################

#                                Load Data

###########################################################################################

df_studentInfo <- read.csv(concat(pth_out,"/df_studentInfo.csv"))

colSums(is.na(df_studentInfo))

#create binary value for logistic model
df_studentInfo$final_result<- ifelse(df_studentInfo$final_result.x %in% c("Pass", "Distinction"), 1,0)

df_studentInfo$region <- as.integer(df_studentInfo$region)

x_data <- df_studentInfo[,c("gender", "region", "highest_education","imd_band" ,                 
                            "age_band", "num_of_prev_attempts", "studied_credits", "disability"                
                            ,"date_registration", "module_presentation_length", "count_date"                
                            , "sum_click_sum", "frequency", "avg_date_submission","final_result")]
target <- df_studentInfo[, 21]

##########################################################################################

#                                variable selection Process (Forward)

###########################################################################################
model.null = glm(final_result ~ 1, 
                 data=x_data,
                 family = binomial(link="logit"))


model.full = glm(final_result ~ gender+region+highest_education+imd_band+age_band+num_of_prev_attempts+studied_credits+disability+
                               date_registration+module_presentation_length+count_date+sum_click_sum+frequency+avg_date_submission,
                 data=x_data,
                 family = binomial(link="logit")
)

step(model.null,
     scope = list(upper=model.full),
     direction="both",
     test="Chisq",
     data=x_data)

##########################################################################################

#                                Model fit

###########################################################################################
model <- final_result ~ count_date + gender + studied_credits + highest_education + 
  imd_band + num_of_prev_attempts + module_presentation_length + 
  sum_click_sum + frequency + disability + age_band + date_registration

fit <- glm(model, family = binomial(link = "logit"), data = x_data)
summary(fit)

# Multicollinearity of fitted model
vif(fit)

# exclude count_date , sums_click_sum
model2 <- final_result ~ gender + studied_credits + highest_education + 
  imd_band + num_of_prev_attempts + module_presentation_length + 
  frequency + disability + age_band + date_registration

fit2 <- glm(model2, family = binomial(link = "logit"), data = x_data)
summary(fit2)

# Output the main regression table 
a <- stargazer::stargazer(fit,fit2, type = "text", model.numbers = F, column.labels = c("Fit","Fit2"))

write.csv(a, concat(pth_out,"/a.csv"))
# Save the model coefficients
coef_table <- as.data.frame(summary(fit2)$coefficients)
coef_table$X <- rownames(coef_table)

##########################################################################################

#                                Prediction

###########################################################################################
prob <- predict(fit2,type=c("response"))
roc_in_df <- data.frame(x_data$final_result, prob)

# Find AUC
auc_in <- round(auc(roc_in_df$x_data.final_result, roc_in_df$prob),4)
roc_in <- roc(final_result~ prob, data = x_data) 

## get ROC and AUC 
p <- plot(roc_in, main =paste0("Logit Model ROC\n AUC = ", auc_in))



##########################################################################################

#             Manual boot strapping - coefficients and p-values

###########################################################################################
par(mfrow=c(4,4))
# Make random samples 
# Sample Number
sample_number <- 10
sample_fraction <- .8

# Round the sample size to a whole number
# Uses the sample fraction set above to partition in-out samples
sample_size <- round(nrow(x_data)*sample_fraction)

# Sample from the df
df_samples <- list()
coeff_l <- list()
pval_l <- list()

start.time <- Sys.time()
start.time
for (i in 1:sample_number){
  ##In sample data
  set.seed(i)
  df_sample <- x_data[sample(nrow(x_data), sample_size, replace = FALSE), ]
  logit <- glm(model2, family = binomial(link = "logit"), data = df_sample)
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

