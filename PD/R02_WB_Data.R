##############################################################################
## File Name: R02_WB_Data.R
## Author: KZ
## Date: 5/1/2017 Created
## Purpose: To import and clean Wilshre data accoring to "02 - WB data.sas"
## Download : 8/9/2017
##############################################################################
setwd("C:/Users/ic07949/Desktop/KPMG/Model Development/development code and data/PD/Dataset/Wilshire")


requirements <- c("dplyr", "reshape2", "data.table","zoo")
for(rr in requirements){
  if(! rr %in% installed.packages()) install.packages(rr)
}
require(dplyr)
require(reshape2)
require(data.table)
require(zoo)


## Import Wilshire Data (SAS File 02, Line 1 to 69)
wilshire <- read.csv("df_final_wilshire_sorted.csv")
names(wilshire)[names(wilshire)=="Note.Number"] <- "note_number"
names(wilshire)[names(wilshire)=="Non.Accrual.Code"] <- "non_accrual_code"
names(wilshire)[names(wilshire)=="NAP...NAIP...NAIP.in.GL"] <- "WB_balance"
names(wilshire)[names(wilshire)=="Rate.Over.Split"] <- "interest_rate"
wilshire$filedate <- as.Date(wilshire$filedate, "%Y-%m-%d")
wilshire$originationdate <- as.Date(wilshire$originationdate, "%Y-%m-%d")
wilshire$maturitydate <- as.Date(wilshire$maturitydate,"%Y-%m-%d")


wilshire_chargeoffs <- read.csv("wilshire charge offs cleaned.csv")

rates <- fread("rates2.csv")

wilshire_acquired_idx1 <- read.csv("Wilshire_aquired_list_20090930.csv")
names(wilshire_acquired_idx1)[names(wilshire_acquired_idx1)=="Note.Number"] <- "note_number"
wilshire_acquired_idx1 <- subset(wilshire_acquired_idx1, Mirae == "M", select = c(note_number,Mirae))

wilshire_acquired_idx2 <- read.csv("Wilshire_aquired_list_20131231.csv")
names(wilshire_acquired_idx2)[names(wilshire_acquired_idx2)=="Note_Number"] <- "note_number"
wilshire_acquired_idx2 <- filter(wilshire_acquired_idx2, Bank %in% c("A", "S", "M"))

acq <- c(unique(wilshire_acquired_idx1$note_number), unique(wilshire_acquired_idx2$note_number))

## create a label in Wilshire for acquired loans. (SAS File 02, Line 71 to 77)
wilshire$acquired_identifier <- ifelse(wilshire$note_number %in% acq,
                                      paste("acquired_wilshire"),
                                      paste("Wilshire_originated"))

table(wilshire$acquired_identifier)


## merge wilshire and chargeoffs
### Assign first_co_date to all entries for specific note_number. Assign charge-off amount to only the quarter where it occurs

wilshire$quarterDate = as.yearqtr(wilshire$filedate,"%Y-%m-%d" )
wilshire_chargeoffs$quarterDate = as.yearqtr(wilshire_chargeoffs$first_co_date,"%Y-%m-%d" )


wilshire_coInd = subset(wilshire_chargeoffs, select = c(note_number,quarterDate, co_ind))
wilshire_firstCoDate = subset(wilshire_chargeoffs, select = c(note_number, first_co_date))

wilshire <- merge(x = wilshire, y = wilshire_coInd, by = c("note_number", "quarterDate"), all.x = TRUE)
wilshire <- merge(x = wilshire, y = wilshire_firstCoDate, by = c("note_number"), all.x = TRUE)


## create event and other variables (SAS File 02, Line 90 to 146)
wilshire$co_ind <- ifelse( is.na(wilshire$co_ind ), 0, wilshire$co_ind )
wilshire$y <- ifelse( wilshire$non_accrual_code %in% c(2,4) | wilshire$co_ind == 1 ,
                     1,
                     ifelse(  wilshire$non_accrual_code %in% c(0,9) & wilshire$co_ind != 1 ,
                             0, 111))

table(wilshire$y)

wilshire$yr_maturity <- year(wilshire$maturitydate)
wilshire$yr_file <- year(wilshire$filedate) 
wilshire$mn_maturity <- month(wilshire$maturitydate)
wilshire$mn_file <- month(wilshire$filedate)
wilshire$q_file <- quarter(wilshire$filedate)
wilshire$ttm_m=  12*(wilshire$yr_maturity - wilshire$yr_file ) + (
  wilshire$mn_maturity - wilshire$mn_file)

wilshire$loan_age_q <- (as.yearqtr(wilshire$filedate) - as.yearqtr(wilshire$originationdate)
                       ) * 4
wilshire$term_q <- (as.yearqtr(wilshire$maturitydate) - as.yearqtr(wilshire$originationdate)
                       ) * 4
wilshire$POB <- 100 * wilshire$loan_age_q / wilshire$term_q

wilshire$POB <- ifelse(wilshire$term_q ==0,100,wilshire$POB)

## create variable min_non_acc_date (SAS File 02, Line 147 to 170)
indx_wilshire <- subset(wilshire, y==1, select = c(note_number,filedate))
indx_wilshire <- as.data.table(indx_wilshire[order(indx_wilshire$note_number, indx_wilshire$filedate),])
indx_wilshire <- indx_wilshire %>% group_by(note_number)%>% filter(row_number(filedate) == 1)
names(indx_wilshire)[names(indx_wilshire)=="filedate"] <- "min_non_acc_date"
wilshire <- merge(x = wilshire, y = indx_wilshire, by = "note_number", all.x = TRUE) 
wilshire$f_non_acc_date <- ifelse(is.na(wilshire$first_co_date), as.Date(wilshire$min_non_acc_date), 
                                  ifelse (as.Date(wilshire$first_co_date) <= as.Date(wilshire$min_non_acc_date), 
                                          as.Date(wilshire$first_co_date),
                                          as.Date(wilshire$min_non_acc_date)))

wilshire$f_non_acc_date <- as.Date(wilshire$f_non_acc_date)

## merge with the rate data set (SAS File 02, Line 173 to 179)
rates <- subset(rates, select = -c(date,month))
setnames(rates, old = c("year","q"), new = c("yr_file","q_file"))
wilshire <- merge(x = wilshire, y = rates, by = c("yr_file","q_file"), all.x = TRUE) 

## clean up data set (SAS File 02, Line 182 to 196)
wilshire_df <- filter(wilshire, yr_maturity > 2006)


wilshire_df <- filter(wilshire_df,  !(!is.na(as.yearqtr(first_co_date, '%Y-%m-%d')) & as.yearqtr(filedate, '%Y-%m-%d')>
                                        as.yearqtr(first_co_date, '%Y-%m-%d') ))
wilshire_df <- filter(wilshire_df,  !(!is.na(as.yearqtr(min_non_acc_date, '%Y-%m-%d')) & as.yearqtr(filedate, '%Y-%m-%d')>
                                        as.yearqtr(min_non_acc_date, '%Y-%m-%d') ))
wilshire_df$boh_id <- "wilshire"

## create portfolio_id: CRE or CI (SAS File 02, Line 198 to 207)
wilshire_df$class_code2 <- as.numeric(as.character(wilshire_df$Class.Code))
wilshire_df <- filter(wilshire_df, class_code2 %in% c(2,3,5,6,10,13,20, 21,30,31,32,33,34,35,
                                                     36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,59,60,61,63,99))


## clean up data
wilshire_df <- filter(wilshire_df, !(is.na(WB_balance)))
wilshire_df <- filter(wilshire_df, !(interest_rate == 0 | is.na(interest_rate)))


## only need to create variable property_type for CRE model
wilshire_df$property_type <- wilshire_df$Property.Type.Code
wilshire_df$property_type <- as.numeric(wilshire_df$property_type)
table(wilshire_df$Property.Type.Code)
table(wilshire_df$property_type)


## create CRE/C&I portfolio ID
wilshire_df$portfolio_id <- ifelse(wilshire_df$class_code2 %in% c(2,3,5,6,10,13,20),
                                   "CRE",
                                   ifelse(wilshire_df$class_code2 %in% c(21,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,
                                                                         46,47,48,49,50,59,60,61,63,99), 
                                          "CI", "error"))
table(wilshire_df$portfolio_id)

## filter out error portfolio_id 
wilshire_df <- filter(wilshire_df, portfolio_id != "error")


## change the loan Rating system 
table(wilshire_df$Loan.Rating.Code1)
wilshire_df$boh_rating <- ifelse(wilshire_df$Loan.Rating.Code1 ==0, 0,
                                 ifelse(wilshire_df$Loan.Rating.Code1 == 1000, 1,
                                 ifelse(wilshire_df$Loan.Rating.Code1 == 2000, 2,
                                 ifelse(wilshire_df$Loan.Rating.Code1 == 3000, 3,
                                 ifelse(wilshire_df$Loan.Rating.Code1 == 4000, 4,
                                 ifelse(wilshire_df$Loan.Rating.Code1 == 5000, 4,
                                 ifelse(wilshire_df$Loan.Rating.Code1 == 6000, 1000, 
                                 ifelse(wilshire_df$Loan.Rating.Code1 == 7000, 2000,
                                 ifelse(wilshire_df$Loan.Rating.Code1 == 8000, 3000,
                                 ifelse(wilshire_df$Loan.Rating.Code1 == 9000, 4000, 111)
                                 )))))))))

table(wilshire_df$boh_rating)

## clean up data set (SAS File 02 Line 549 to 601) and create final data set for Wilshire
df_final_wilshire <- as.data.frame(wilshire_df)
df_final_wilshire <- as.data.table(df_final_wilshire)
df_final_wilshire <- subset(df_final_wilshire, select = -c(Name.1, Collateral.Address ))
setnames(df_final_wilshire, old = c("Times.Past.Due.01.To.29.Days", "WB_balance","Original.Note.Amount",
                                    "filedate", "yr_file","q_file","mn_file","note_number","originationdate",
                                    "maturitydate","Fixed.or.Variable.Interest.Rate","f_non_acc_date",
                                    "NAICS.Code"),
         new = c("dpd0129","current_balance", "original_balance",
                 "fileDate","year","q","month","account_id","origination_date",
                 "maturity_date","interest_rate_type","first_nonacc_date",
                 "naicsCode"))

df_final_wilshire$dpd0129 <- substr(as.character(df_final_wilshire$dpd0129),1,3)

df_final_wilshire$dpd0129 <- as.numeric(as.character(df_final_wilshire$dpd0129))
## Warning message: NAs introduced by coercion

df_final_wilshire$dpd0129 <- ifelse(is.na(df_final_wilshire$dpd0129), 0, df_final_wilshire$dpd0129)
## The way R produces "NA" when changing character to numeric is different from SAS
## Thus, R results have more dpd0129 = 0.  R: 88430 obs, SAS: 88376

## 0 observation with loan_spread_v>100 (or interest_rate = 1234.56) (SAS File 02 Line 582 to 585)
df_final_wilshire <- filter(df_final_wilshire, interest_rate != 1234.56)

df_final_wilshire$callReportCodeDescr <- "wilshire"

df_final_wilshire <- subset(df_final_wilshire, select = c(fileDate, account_id, boh_id, acquired_identifier,
                                                  portfolio_id, original_balance, origination_date, maturity_date,
                                                  current_balance, interest_rate, interest_rate_type, 
                                                  loan_age_q,  POB, boh_rating, DCR, 
                                                  dpd0129, first_nonacc_date,
                                                  naicsCode, property_type, tb1m, tb3m, tb6m, tb1y, tb2y,
                                                  tb3y, tb5y, tb7y, tb10y,tb20y, tb30y, year, q, month, y,callReportCodeDescr))

save(df_final_wilshire, file = "Data output/df_final_wilshire.RData")
write.csv(df_final_wilshire, file = "Data output/df_final_wilshire.csv", row.names = FALSE)
