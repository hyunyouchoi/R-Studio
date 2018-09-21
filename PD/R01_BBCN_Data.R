##############################################################################
## File Name: R01_BBCN_Data.R
## Author: KZ
## Date: 5/1/2017 Created
## Purpose: To import and clean BBCN data accoring to "01 - BBCN data.sas"
## Download 8/8/2017
##############################################################################
setwd("C:/Users/ic07949/Desktop/dataset")

requirements <- c("dplyr", "reshape2", "data.table","zoo")
for(rr in requirements){
  if(! rr %in% installed.packages()) install.packages(rr)
}
require(dplyr)
require(reshape2)
require(data.table)
require(zoo)


## Import BBCN Data (SAS File 01, Line 1 to 53)
BBCN_df <- read.csv("data request bottom-up.csv")
names(BBCN_df)[1] <- paste("fileDate")
BBCN_df$fileDate <- as.Date(BBCN_df$fileDate, "%Y-%m-%d")
BBCN_df$originationDate <- as.Date(BBCN_df$originationDate, "%Y-%m-%d")
BBCN_df$maturityDate <- as.Date(BBCN_df$maturityDate,"%Y-%m-%d")
BBCN_df$non_acc_date <- as.Date(BBCN_df$nonAccrualDate, "%Y-%m-%d")

rates <- fread("rates2.csv")

acquired_bbcn_raw <- fread("acquired loan identifier bbcn.csv")

acquired_loans <- unique(acquired_bbcn_raw$Note_Number)


## Create label in BBCN for acquired loans (SAS File 01, Line 54 to 104)
BBCN_df$acquired_identifier <- ifelse(BBCN_df$accountNo %in% acquired_loans,
                                      paste("acquired_bbcn"),
                                      paste("bbcn_originated"))

table(BBCN_df$acquired_identifier)

#write.csv(BBCN_df, file = "BBCN_df_test.csv", row.names = FALSE)

## Create y for default event (SAS File 01, Line 118 to 125)
BBCN_df$y <- ifelse(BBCN_df$amtChargedOff > 0 | (BBCN_df$amtChargedOff == 0 & 
                                                BBCN_df$nonAccrualFlag != 0),
                    1, 0)

table(BBCN_df$y)




####################################################################################################

## Create time to maturity and POB (SAS File 01, Line 127 to 181)
BBCN_df$loan_age_q <- (as.yearqtr(BBCN_df$fileDate) - as.yearqtr(BBCN_df$originationDate)
                       ) * 4
BBCN_df$term_q <- (as.yearqtr(BBCN_df$maturityDate) - as.yearqtr(BBCN_df$originationDate)
                   ) * 4
BBCN_df$POB <- 100 * BBCN_df$loan_age_q / BBCN_df$term_q

BBCN_df$POB <- ifelse(BBCN_df$term_q ==0,100,BBCN_df$POB)

## find the date for the first default event (SAS File 01, Line 224 to 240)
indx_bbcn <- subset(BBCN_df, y==1, select = c("accountNo","non_acc_date"))
indx_bbcn <- as.data.table(indx_bbcn[order(indx_bbcn$accountNo, indx_bbcn$non_acc_date),])
indx_bbcn <- indx_bbcn %>% group_by(accountNo)%>% filter(row_number(non_acc_date) == 1)
names(indx_bbcn)[names(indx_bbcn)=="non_acc_date"] <- "min_non_acc_date"
#### 20184 obs in indx_bbcn

# BBCN_df <- BBCN_df[-grep("non_acc_date", colnames(BBCN_df))] 
BBCN_df <- merge(x = BBCN_df, y = indx_bbcn, by = "accountNo", all.x = TRUE) 


## Clean up data
BBCN_df$yr_maturity <- year(BBCN_df$maturityDate)
BBCN_df$yr_file <- year(BBCN_df$fileDate) 
BBCN_df$mn_maturity <- month(BBCN_df$maturityDate)
BBCN_df$mn_file <- month(BBCN_df$fileDate)
BBCN_df$q_file <- quarter(BBCN_df$fileDate)
BBCN_df$yr_min_non_acc_date <- year(BBCN_df$min_non_acc_date)
BBCN_df$mn_min_non_acc_date <- month(BBCN_df$min_non_acc_date)
BBCN_df$ttm_m=  12*(BBCN_df$yr_maturity - BBCN_df$yr_file ) + (
  BBCN_df$mn_maturity - BBCN_df$mn_file)

BBCN_df <- filter(BBCN_df, maturityDate > 2006)

# BBCN_df <- filter(BBCN_df, yr_maturity >= yr_file )
# BBCN_df <- filter(BBCN_df, !(yr_maturity == yr_file & (mn_file - mn_maturity)>2) )

#### 414335 obs.


## Create CRE/C&I portfolio ID (SAS File 01, Line 254 to 331)
BBCN_df$portfolio_id <- "NULL"

## trim leading or trailing blanks for vairable callReportCodeDescr
## (because R Reads blanks)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
BBCN_df$callReportCodeDescr <- trim(BBCN_df$callReportCodeDescr)

## (SAS File 01, Line 258 to 274)
BBCN_df$portfolio_id <- ifelse(BBCN_df$callReportCodeDescr %in% c("COMMERCIAL (GENERAL PLEDGE)") & 
                                 BBCN_df$loanTypeDescr %in% c("Commercial Line (18)", "Commercial Line (18)","Commercial Line (18)",
                                                              "Commercial Line (18)", "Commercial Term Loan (20)",
                                                              "Commercial Term Loan (20)","Commercial Term Loan (20)",
                                                              "Commercial Term Loan (20)","Comml LOC - Other Gov Gty (19)",
                                                              "Comml LOC - Other Gov Gty (19)","Discounted Acceptance (33)",
                                                              "Export Working Capital Program (38)","Performance Bond L/C (44)",
                                                              "Purchase Advance (31)","Purchase Advance (31)",
                                                              "Purchase Advance (31)","SBA 172 Loan (66)",
                                                              "SBA ARC Loans (62)","SBA ARC Loans (62)",
                                                              "SBA ARC Loans (62)","SBA Express LOC (64)",
                                                              "SBA Express LOC (64)", "SBA SOHO Loan (65)",
                                                              "SBA Term Loans (61)","SBA Term Loans (61)",
                                                              "SBA Term Loans (61)","Standby L/C (43)",
                                                              "Standby L/C (43)","Trust Receipt (30)",
                                                              "Working Capital Advance (37)","Working Capital Advance (37)",
                                                              "Working Capital Advance (37)"),
                               "CI",
                               BBCN_df$portfolio_id)
#### !!! SAS only reads up to 32 characters. But R reads them all. So It's "Export Working Capital Program (38)",
####     not "Export Working Capital Program (3"  !!!!


## (SAS File 01, Line 276 to 279)
BBCN_df$portfolio_id <- ifelse(BBCN_df$portfolio_id == c("NULL") &
                               BBCN_df$callReportCodeDescr %in% c("COMMERCIAL (GENERAL PLEDGE)") & 
                               BBCN_df$loanTypeDescr %in% c("Commercial Real Estate (71)", "SBA Real Estate (60)", 
                                                              "SBA Real Estate (60)", "SBA Real Estate (60)"),
                               "CRE",
                               BBCN_df$portfolio_id)


## (SAS File 01, Line 281 to 283)
BBCN_df$portfolio_id <- ifelse(BBCN_df$portfolio_id == c("NULL") &
                               BBCN_df$callReportCodeDescr %in% c("CONVENTIONAL 5+ RESIDENTIAL",
                                                                  "Conv 5+ Residential Prop",
                                                                  "NON-FARM NON -RESIDENTIAL",
                                                                  "Other nonfarm nonresi property", 
                                                                  "Owner-occupied nonfarm nonresi",
                                                                  "SECURED BY FARMEDLAND") ,
                               "CRE",
                               BBCN_df$portfolio_id)


## (SAS File 01, Line 285 to 286)
BBCN_df$portfolio_id <- ifelse(BBCN_df$portfolio_id == c("NULL") &
                               BBCN_df$callReportCodeDescr %in% c("Check Credit & Rev Credit Plan",
                               "Com'l Loan - International Dpt",
                               "Com'l Loans - Borrowing Based"),
                               "CI",
                               BBCN_df$portfolio_id)

## (SAS File 01, Line 288 to 322)
BBCN_df$portfolio_id <- ifelse(BBCN_df$portfolio_id == c("NULL") &
                               BBCN_df$callReportCodeDescr %in% c("Commercial Loans") & 
                                 BBCN_df$loanTypeDescr %in% c("Bankers Health Group","Commercial Lease (25)",
                                                              "Commercial Line (18)",
                                                              "Commercial Term Loan (20)",
                                                              "Comml Asset-Based LOC (22)",
                                                              "Comml LOC - Other Gov Gty (19)",
                                                              "Comml Term - Other Gov Gty (21)",
                                                              "Discounted Acceptance (33)",
                                                              "Export Working Capital Program (38)",
                                                              "Express Line (26)",
                                                              "Master Comm LOC (01)",
                                                              "Master Comm LOC Sublimit (03)",
                                                              "Master ILOC (02)",
                                                              "Master ILOC Sublimit (04)",
                                                              "ODP LOC - Business",
                                                              "Performance Bond L/C (44)",
                                                              "Professional Line of Credit (51)",
                                                              "Professional Term Loan (50)",
                                                              "Purchase Advance (31)",
                                                              "Purchase Advance-Comm (27)",
                                                              "SBA 172 Loan (66)",
                                                              "SBA ARC Loans (62)",
                                                              "SBA Express LOC (64)",
                                                              "SBA Express Loan (63)", 
                                                              "SBA SOHO Loan (65)",
                                                              "SBA Small Loan Advantage",
                                                              "SBA Term Loans (61)",
                                                              "Signature Line (11)",
                                                              "Simple Line of Credit (24)",
                                                              "Simple Loan - Commercial (23)",
                                                              "Standby L/C (43)",
                                                              "Syndicated Leveraged Lending",
                                                              "Trust Receipt (30)",
                                                              "Working Capital Advance (37)",
                                                              "Working Capital Advance-Comm (28)"),
                               "CI",
                               BBCN_df$portfolio_id)

#### Same problem. SAS only reads up to 32 characters. But R reads them all. So It's "Export Working Capital Program (38)",
####     not "Export Working Capital Program (3"  !!!!

## (SAS File 01, Line 324 to 326)
BBCN_df$portfolio_id <- ifelse(BBCN_df$portfolio_id == c("NULL") &
                               BBCN_df$callReportCodeDescr %in% c("Commercial Loans") & 
                               BBCN_df$loanTypeDescr %in% c("Comm RE - Revolving LOC (74)","Commercial Real Estate (71)",
                                                              "SBA Real Estate (60)"),
                               "CRE",
                               BBCN_df$portfolio_id)

## (SAS File 01,Line 328 to 330)
BBCN_df$portfolio_id <- ifelse(BBCN_df$portfolio_id == c("NULL") &
                                 BBCN_df$callReportCodeDescr %in% c("INTERNATIONAL",
                                                                    "Other Installment loans") ,
                               "CI",
                               BBCN_df$portfolio_id)

BBCN_df$portfolio_id <- ifelse(BBCN_df$portfolio_id == c("NULL"),
                              "error",
                               BBCN_df$portfolio_id)


table(BBCN_df$portfolio_id)


## delete portfolio_id == "error".
BBCN_df <- filter(BBCN_df, portfolio_id != "error")


## delete the observations after the default date
BBCN_df <- filter(BBCN_df, !(yr_file > yr_min_non_acc_date & (!is.na(yr_min_non_acc_date)) ) )
BBCN_df <- filter(BBCN_df, !((yr_file == yr_min_non_acc_date) & (mn_file - mn_min_non_acc_date > 2)  &
                               (!is.na(yr_min_non_acc_date))))
#### 330012 obs.

## !! SAS File 01, Line 346 to 359 can be ignored because it doesn't affect the final results.

## clean multiple default events for 2 accounts  (SAS File 01, Line 361 to 369)
BBCN_df <- filter(BBCN_df, !(accountNo %in% c(26506643, 23506889) & (yr_file >= 2009) & (mn_file > 03) ))
BBCN_df$min_non_acc_date <- ifelse(BBCN_df$accountNo %in% c(26506643, 23506889),
                                   as.Date("2009-03-31"), BBCN_df$min_non_acc_date)
BBCN_df$min_non_acc_date <- as.Date(as.numeric(BBCN_df$min_non_acc_date))
BBCN_df$yr_min_non_acc_date <- ifelse(BBCN_df$accountNo %in% c(26506643, 23506889),
                                      2009, BBCN_df$yr_min_non_acc_date)
BBCN_df$mn_min_non_acc_date <- ifelse(BBCN_df$accountNo %in% c(26506643, 23506889),
                                      3, BBCN_df$mn_min_non_acc_date)
            

## add variables (SAS File 01, Line 375 to 383)
BBCN_df$account_id <- BBCN_df$accountNo;
BBCN_df$boh_id <- "bbcn"


## !! SAS File 01, Line 386 to 427 can be ignored because it doesn't affect the final results.
## only need to create variable property_type for CRE model
BBCN_df$property_type <- as.character(BBCN_df$collateralPropertyType)
BBCN_df$property_type <- as.numeric(BBCN_df$property_type)
table(BBCN_df$property_type)
table(BBCN_df$collateralPropertyType)

## create variable boh_rating (SAS File 01, Line 431 to 446)
#### !! variable loanRatingDescr2 has different length in R. 
####    (length in SAS is shorter because developer forgot to use guessingrows in SAS proc import)
BBCN_df$boh_rating <- ifelse(BBCN_df$loanRatingDescr2 == c("Substandard"), 2000,
                             ifelse(BBCN_df$loanRatingDescr2 == c("Sp Mention"), 1000,
                                    ifelse(BBCN_df$loanRatingDescr2 == c("Doubtful"), 3000,
                                           ifelse(BBCN_df$loanRatingDescr2 == c("Loss"), 4000,
                                                  ifelse(BBCN_df$loanRatingDescr2 == c("Pass-4"), 4,
                                                         ifelse(BBCN_df$loanRatingDescr2 == c("Pass-3"), 3,
                                                                ifelse(BBCN_df$loanRatingDescr2 == c("Pass-2"), 2, 
                                                                       ifelse(BBCN_df$loanRatingDescr2 == c("Pass-1"), 1, 111)
                                                                       )))))))


table(BBCN_df$loanRatingDescr2)
table(BBCN_df$boh_rating)

## clean up dcr2  (SAS File 01, Line 449 to 489)
## Only change character var to numeric
BBCN_df$dcr2 <- as.numeric(as.character(BBCN_df$DCR))
BBCN_df$dcr2 <- floor(BBCN_df$dcr2*1000)/1000  #keep 3 decimal places
BBCN_df$dcr2 <- ifelse(BBCN_df$dcr2 %in%  c(0.001,0.002,0.003,0.004,0.005,0.007),
                       BBCN_df$dcr2*1000,
                       BBCN_df$dcr2)
BBCN_df$DCR <- BBCN_df$dcr2

## remove the letter of credit observations (SAS File 01, Line 492 to 500)
BBCN_df <- filter(BBCN_df, productDescr != c("Letter of Credits"))
BBCN_df$interest_rate <- as.numeric(BBCN_df$interestRate)


## add rates and create Final DF (SAS File 01, Line 183 to 211, Line 507 to 523)
rates <- subset(rates, select = -c(date,month))
setnames(rates, old = c("year","q"), new = c("yr_file","q_file"))
BBCN_df <- merge(x = BBCN_df, y = rates, by = c("yr_file","q_file"), all.x = TRUE) 

## delete loan_spread_v > 10000  (SAS File 01, Line 574 to 577). only 2 obs with fixVar == "NULL".
# BBCN_df$loan_spread_v_gt10000 <- ifelse(BBCN_df$fixVar == "NULL", 1,0)
# BBCN_df <- filter(BBCN_df, loan_spread_v_gt10000 != 1)    
## but SAS data file interim.df_final_bbcn2 did not exclude these two obs.


## create final data set for bbcn
df_final_bbcn <- as.data.frame(BBCN_df)
df_final_bbcn <- as.data.table(df_final_bbcn)
setnames(df_final_bbcn, old = c("timesPD01To29D", "fixVar","originalBal","originationDate",
                                "maturityDate","min_non_acc_date","origLoanToValue","currentNetBookBal",
                                "yr_file","q_file","mn_file"),
         new = c("dpd0129","interest_rate_type","original_balance","origination_date",
                 "maturity_date","first_nonacc_date","org_ltv","current_balance",
                 "year","q","month"))


df_final_bbcn <- subset(df_final_bbcn, select = c(fileDate, account_id, boh_id, acquired_identifier,
                                                  portfolio_id, original_balance, origination_date, maturity_date,
                                                  current_balance, interest_rate, interest_rate_type, 
                                                  loan_age_q,  POB, boh_rating, DCR, 
                                                  dpd0129, first_nonacc_date,
                                                  naicsCode, property_type, tb1m, tb3m, tb6m, tb1y, tb2y,
                                                  tb3y, tb5y, tb7y, tb10y,tb20y, tb30y, year, q, month, y))


save(df_final_bbcn, file = "Data output/df_final_bbcn.RData")
write.csv(df_final_bbcn, file = "Data output/df_final_bbcn.csv", row.names = FALSE)
