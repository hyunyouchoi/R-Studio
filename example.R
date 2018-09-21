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

BBCN_df <- filter(BBCN_df,  ((fileDate >= "2012-06-30") & (fileDate <= "2016-03-31") ))

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

####################################################################################################################################
####################################################################################################################################
BBCN_df2 <- BBCN_df

BBCN_df2$PurposeCode  <- "NULL"

BBCN_df2$PurposeCode <- ifelse(BBCN_df2$PurposeCode  == c("NULL") &
                                 BBCN_df2$callReportCodeDescr %in% c("Conv 5+ Residential Prop"), 180, BBCN_df2$PurposeCode)

BBCN_df2$PurposeCode <- ifelse(BBCN_df2$PurposeCode  == c("NULL") &
                                 BBCN_df2$callReportCodeDescr %in% c("NON-FARM NON -RESIDENTIAL", "Other nonfarm nonresi property"), 190, BBCN_df2$PurposeCode)

BBCN_df2$PurposeCode <- ifelse(BBCN_df2$PurposeCode  == c("NULL") &
                                 BBCN_df2$callReportCodeDescr %in% c("Owner-occupied nonfarm nonresi"), 200, BBCN_df2$PurposeCode)

BBCN_df2$PurposeCode <- ifelse(BBCN_df2$PurposeCode  == c("NULL") &
                                 BBCN_df2$callReportCodeDescr %in% c("Commercial Loans"), 510, BBCN_df2$PurposeCode)

BBCN_df2$PurposeCode <- ifelse(BBCN_df2$PurposeCode  == c("NULL") &
                                 BBCN_df2$callReportCodeDescr %in% c("Com'l Loan - International Dpt"), 511, BBCN_df2$PurposeCode)

BBCN_df2$PurposeCode <- ifelse(BBCN_df2$PurposeCode  == c("NULL") &
                                 BBCN_df2$callReportCodeDescr %in% c("Leasing"), 650, BBCN_df2$PurposeCode)

BBCN_df2$PurposeCode <- ifelse(BBCN_df2$PurposeCode == c("NULL"), "error", BBCN_df2$PurposeCode)


table(BBCN_df2$PurposeCode)

###########################################################################################################################

BBCN_df2$portfolio_id2  <- "NULL"

BBCN_df2$portfolio_id2 <- ifelse(BBCN_df2$portfolio_id2== c("NULL") &
                                 BBCN_df2$PurposeCode %in% c("180","190","200"),
                                 "CRE", BBCN_df2$portfolio_id2)

BBCN_df2$portfolio_id2 <- ifelse(BBCN_df2$portfolio_id2== c("NULL") &
                                   BBCN_df2$PurposeCode %in% c("510","511","650"),
                                 "CI", BBCN_df2$portfolio_id2)
table(BBCN_df2$portfolio_id2 )
