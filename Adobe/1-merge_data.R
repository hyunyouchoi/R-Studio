################################################################################
# 
# 
#Program: 1-Merge Data.R
# Author: Irene Hyunyou Choi
# Purpose: create the dataset with appropriate merge process for data analysis and model developemnt
# 
#
#
# R-version: R version 3.3.4 (2016-06-21)
# -- "Bug in Your Hair" Copyright (C) 2016 The R Foundation
# for Statistical Computing Platform: x86_64-apple-darwin13.4.0 (64-bit)
################################################################################
### Environment Settings #######################################################
pth_inputs = "C:/Users/hong3/Desktop/OULAD1/inputs"
pth_lib = "C:/Users/hong3/Desktop/OULAD1/library"
pth_out = "C:/Users/hong3/Desktop/OULAD1"
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

assessments <- read.csv(concat(pth_inputs,"/assessments.csv"))
courses <- read.csv(concat(pth_inputs,"/courses.csv"))
studentAssessment <- read.csv(concat(pth_inputs,"/studentAssessment.csv"))
studentInfo <- read.csv(concat(pth_inputs,"/studentInfo.csv"))
studentRegistration <-read.csv(concat(pth_inputs,"/studentRegistration.csv"))
studentVle <- read.csv(concat(pth_inputs,"/studentVle.csv"))
vle <- read.csv(concat(pth_inputs,"/Vle.csv"))

##count the missing value####

colSums(is.na(assessments))
colSums(is.na(courses))
colSums(is.na(studentAssessment))
colSums(is.na(studentInfo))
colSums(is.na(studentRegistration))
colSums(is.na(studentVle))
colSums(is.na(vle))

##########################################################################################

#                                Assessments Data

###########################################################################################
#Fill in assessments' missing value
assessments_courses <- merge(assessments, courses, by=c("code_module", "code_presentation"))
assessments_courses$date[is.na(assessments_courses$date)] <- assessments_courses$module_presentation_length[is.na(assessments_courses$date)]

write.csv(assessments_courses, concat(pth_out,"/assessments_courses.csv"))
  
#df_assessments = course +assessment +s.asssessment
df_assessments <- merge(studentAssessment, assessments_courses , by=c("id_assessment"))
df_assessments$submission <- df_assessments$date_submitted - df_assessments$date

write.csv(df_assessments,  concat(pth_out,"/df_assessments.csv"))

##########################################################################################

#                                Student Vle

###########################################################################################
#sum clicks by same date and website
df_student_final <- studentInfo[,c("code_module","code_presentation","id_student", "final_result")]

df_studentVle <- studentVle %>%
  group_by(code_module, code_presentation, id_student)%>%
  dplyr::summarise(count_date = n(), sum_click_sum = sum(sum_click))

df_studentVle  <- merge(df_studentVle , df_student_final, by=c("code_module", "code_presentation", "id_student"))
df_studentVle$frequency <- df_studentVle$sum_click_sum / df_studentVle$count_date

df_studentVle_v1 <- df_studentVle %>%
  group_by(code_module, code_presentation, final_result)%>%
  dplyr::summarise(avg_count = mean(frequency), avg_click = mean(sum_click_sum))

df_studentVle_v1$id <- paste(df_studentVle_v1$code_module, df_studentVle_v1$code_presentation)

write.csv(df_studentVle_v1, concat(pth_out,"/df_studentVle_v1.csv"))

df_studentVle_v2 <- studentVle %>%
  group_by(code_module, code_presentation, id_student)%>%
  dplyr::summarise(avg_date = mean(date), sum_click_sum = sum(sum_click))

##########################################################################################

#                                Student Registration

###########################################################################################
df_studentRegistration  <- merge(studentRegistration , df_student_final, by=c("code_module", "code_presentation", "id_student"))
df_studentRegistration$id <- paste(studentRegistration$code_module, studentRegistration$code_presentation)

#check missingvalue and drop a column
colSums(is.na(df_studentRegistration))
df_studentReg <- df_studentRegistration[ c(1:4,6:7)]

#fill in missing value in registration column
df_studentReg <- df_studentReg[complete.cases(df_studentReg), ]

df_studentReg_v1 <- df_studentReg%>%
  group_by(id, final_result)%>%
  dplyr::summarise(avg_date_reg = mean(date_registration))

write.csv(df_studentReg_v1,concat(pth_out, "/df_studentReg_v1.csv"))


##########################################################################################

#                                Student Info 

#create the master dataset to build models and visualize the basic analysis
###########################################################################################
df_studentInfo <- studentInfo

#convert all categorical variables to numeric variables 

df_studentInfo$gender <- as.integer(df_studentInfo$gender)
df_studentInfo$gender <- factor(ifelse(as.numeric(df_studentInfo$gender)==2, 1,0))

df_studentInfo$highest_education <- as.numeric(factor(df_studentInfo$highest_education , levels=c("No Formal quals" ,
                                                                                            "Lower Than A Level", "A Level or Equivalent",
                                                                                            "HE Qualification", "Post Graduate Qualification")))

df_studentInfo$age_band <- as.numeric(factor(df_studentInfo$age_band , levels=c("0-35","35-55", "55<=")))

df_studentInfo$imd_band <- as.numeric(factor(df_studentInfo$imd_band , levels=c("0-10%","10-20%", "20-30%",
                                                                          "30-40%", "40-50%", "50-60%",
                                                                          "60-70%", "70-80%", "80-90%",
                                                                          "90-100%")))

df_studentInfo$imd_band[is.na(df_studentInfo$imd_band)] <- 5

df_studentInfo$disability <- as.integer(df_studentInfo$disability)

df_studentInfo <- merge(x = df_studentInfo, y = studentRegistration, by =c("code_module", "code_presentation", "id_student"))

df_studentInfo <- merge(x = df_studentInfo, y = courses, by =c("code_module", "code_presentation"))

#add column sum click and average date for each student and module,  frequency clicks for each student and module
df_studentInfo <- merge(x = df_studentInfo,y = df_studentVle, by =c("code_module", "code_presentation", "id_student"))

sub_df_assessments <- df_assessments%>%
                  group_by(code_module, code_presentation, id_student)%>%
                  dplyr::summarise(avg_date_submission = mean(submission))

df_studentInfo <- merge(x = df_studentInfo,y = sub_df_assessments, by =c("code_module", "code_presentation", "id_student"))

drops <- c("X", "date_unregistration", "final_result.y")
df_studentInfo <- df_studentInfo[, !(names(df_studentInfo) %in% drops)]

df_studentInfo <- df_studentInfo[complete.cases(df_studentInfo), ]

df_studentInfo$id <- paste(df_studentInfo $code_module, df_studentInfo $code_presentation)

write.csv(df_studentInfo, concat(pth_out, "/df_studentInfo.csv"))

###########################################################################################

#                                Course Info 

###########################################################################################
code_final_result <- studentInfo[,c("code_module","code_presentation","final_result")]
code_final_result$id <- paste(code_final_result$code_module, code_final_result$code_presentation)
courses$id <- paste(courses$code_module, courses$code_presentation)
assessments_courses$id <- paste(assessments_courses$code_module, assessments_courses$code_presentation)

a <- table(code_final_result$final_result, code_final_result$id)

b <- t(do.call("rbind", list(a)))
b <- data.table(b)
b$id <- courses$id

b$total <- b$Distinction+b$Fail+b$Pass+b$Withdrawn
b$Pro_D <- b$Distinction/b$total
b$Pro_F <- b$Fail/b$total
b$Pro_P <- b$Pass/b$total
b$Pro_W <- b$Withdrawn/b$total

#create new data frame for bar plot
c <- data.frame(id = rep((b$id), each =4),
                result = rep(c("D", "F", "P", "W"),22),
                len = c(a))
#write.csv(c, "c.csv")

# Stacked barplot with multiple groups
p <- ggplot(data=c, aes(x=id, y=len, fill=result)) +
            geom_bar(stat="identity")+
            theme(legend.position="bottom")+
            labs(x = "module presentation", y = "Number" )+
            theme(axis.text.x = element_text(angle=90))

ggsave( concat(pth_out, "/image_number_of_result.png"), plot=p, width=15, height=11, unit="cm", dpi=500)


########create new data frame for bar plot
d <- b[,c("Pro_D","Pro_F","Pro_P","Pro_W")]
d <- t(d)


e <- data.frame(id = rep((b$id), each =4),
                result = rep(c("Pro_D", "Pro_F", "Pro_P", "Pro_W"),22),
                len = c(d))

p <- ggplot(data=e, aes(x=id, y=len, fill=result)) +
      geom_bar(stat="identity")+
      theme(legend.position="bottom")+
      theme(axis.text.x = element_text(angle=90))+
      labs(x = "module presentation", y = "Percent(%)" )

ggsave(concat(pth_out, "/image-_percentage_of_result_per_pt.png"), plot=p, width=15, height=11, unit="cm", dpi=500)


# add assessments info

df_assessments_type <- assessments[,c("code_module","code_presentation","assessment_type")]
df_assessments_type$id <- paste(df_assessments_type$code_module, df_assessments_type$code_presentation)

a1<- table(df_assessments_type$assessment_type, df_assessments_type$id)

b1 <- t(do.call("rbind", list(a1)))
b1 <- data.table(b1)
b1$id <- courses$id

coursesInfo <- merge(b, b1, by=c("id"))
coursesInfo <- merge(coursesInfo, courses, by=c("id"))

######## Assessments weight sum

test <- assessments%>% group_by(code_module, code_presentation, assessment_type)%>% dplyr::summarise(sum_weight = sum(weight))
test$id <-  paste(test$code_module, test$code_presentation)

test <- dcast(test, id ~ assessment_type, value.var = "sum_weight")
test$CMA[is.na(test$CMA)] <- 0

names(test)[2] <- ("weighted_CMA")
names(test)[3] <- paste("weighted_Exam")
names(test)[4] <- paste("weighted_TMA")


coursesInfo <- merge(coursesInfo, test, by=c("id"))

write.csv(coursesInfo, concat(pth_out, "/df_coursesInfo.csv"))

###########################################################################################

#                                Shiny Data

###########################################################################################
#
studentInfo$id <- paste(studentInfo$code_module, studentInfo$code_presentation)

write.csv(studentInfo, "shiny_studentInfo.csv")

#
assessments$id <- paste(assessments$code_module, assessments$code_presentation)

shiny_assessments<- assessments %>% group_by(id, assessment_type)%>%
  dplyr::summarise(k = n(), sum_weight = sum(weight))

write.csv(shiny_assessments, "shiny_assessments.csv")

#

assessments_courses$id <- paste(assessments_courses$code_module, assessments_courses$code_presentation)

shiny_assessments_courses<- assessments_courses %>% group_by(id, assessment_type)%>%
  dplyr::summarise(k = n())

write.csv(shiny_assessments_courses, "shiny_assessments_courses.csv")


