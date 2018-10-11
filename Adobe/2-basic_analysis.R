################################################################################
# 
# 
# Program: 2-basic_analysis.R
# Author: Irene Hyunyou Choi
# Purpose: find the data limitation and do basic analysis using data table from 1-merge_data
# 
#
#
# R-version: R version 3.3.4 (2016-06-21)
# -- "Bug in Your Hair" Copyright (C) 2016 The R Foundation
# for Statistical Computing Platform: x86_64-apple-darwin13.4.0 (64-bit)
################################################################################

### Environment Settings #######################################################
pth_inputs = "C:/Users/ic07949/Desktop/personal/adobe/inputs"
pth_lib = "C:/Users/ic07949/Desktop/personal/adobe/library"
pth_out = "C:/Users/ic07949/Desktop/personal/adobe"
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
library("reshape")

##########################################################################################

#                                Load Data

###########################################################################################

df_assessments  <- read.csv(concat(pth_out,"/df_assessments.csv"))
assessments_courses  <- read.csv(concat(pth_out,"/assessments_courses.csv"))

############################################################################################
# check missing value : score has 173 missing value 0.09% from total dataset.
colSums(is.na(df_assessments))
#fill in missing value to 0 
df_assessments[df_assessments$score == 'NA',] <- 0

##########################################################################################

#                           assessments data limitation

###########################################################################################

df_assessments_v2 <- assessments_courses %>% group_by(code_module, code_presentation) %>% dplyr::summarise(total_weight = sum(weight))
df_assessments <- merge(x = df_assessments, y = df_assessments_v2, by =c("code_module", "code_presentation"))
df_assessments$score_weight <- (as.integer(df_assessments$score)*as.integer(df_assessments$weight))/as.integer(df_assessments$total_weight)


# student's weight
df_assessments_v3 <- df_assessments %>% group_by(code_module, code_presentation, id_student) %>% dplyr::summarise(sum_weight = sum(weight))
df_assessments <- merge(x = df_assessments, y = df_assessments_v3, by =c("code_module", "code_presentation", "id_student"))

# total observation is 173912

# with final exam missing data decrease by 19033
df_assessments_v4 <- df_assessments[df_assessments$total_weight == df_assessments$sum_weight,]

# create the dataset will full score total 19033 observations
studentInfo_result <- studentInfo[,c("code_module","code_presentation","id_student","final_result")]

df_assessments_v5 <- merge(x = df_assessments_v4, y = studentInfo_result, by =c("code_module", "code_presentation", "id_student"))
df_assessments_v5$weight <- as.numeric(df_assessments_v5$weight)
df_assessments_v5$score_weight <- as.integer(as.character(df_assessments_v5$score))*df_assessments_v5$weight
df_assessments_v5$weighted_score <- as.numeric(df_assessments_v5$score_weight) / as.integer(df_assessments_v5$total_weight)

#compute final grade
df_assessments_v6 <- df_assessments_v5 %>% group_by(code_module, code_presentation, id_student) %>% dplyr::summarise(sum_score = sum(weighted_score))
df_assessments_v6 <- merge(x = df_assessments_v6, y = studentInfo_result, by =c("code_module", "code_presentation", "id_student"))

# plot socre range for each final results - cannot tell the 
p <- ggplot(df_assessments_v6 , aes(x=final_result, y=sum_score, color =final_result)) + geom_boxplot()+
     theme(legend.position="none")+
     labs(x = "Final Result", y = "Score" )

ggsave( concat(pth_out,"/image_socre_range_final_results.png"), plot=p, width=15, height=11, unit="cm", dpi=500)


df_assessments_v7 <- df_assessments %>% group_by(id_assessment) %>% dplyr::summarise(k =n())
assessments_courses <- merge(x = assessments_courses, y = df_assessments_v7, by =c("id_assessment"), all.x = TRUE)

sum(is.na(assessments_courses$k))# total 18 eaxm scores are missing

##########################################################################################

#                           correlation with final result 

###########################################################################################
coursesInfo  <- read.csv(concat(pth_out,"/coursesInfo.csv"))

df_cousesInfo <- coursesInfo[, c(8,9,10,11,12,13,14,17,18,19,20)]

cor_coursesInfo <- cor(df_cousesInfo)
round(cor_coursesInfo, 2)

#corrplot
corrplot(cor_coursesInfo, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)


#heatmap
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = cor_coursesInfo, col = col, symm = TRUE)

##########################################################################################

#                            

###########################################################################################



