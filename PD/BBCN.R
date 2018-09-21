###################################################################
# Project: Bank of Hope
# PD Models
###################################################################  

########
# BBCN 
########

library(lubridate)

# Read the raw data
bbcn=read.csv("BBCN-Bottom-up-SGmod6.csv", header = T, nrows = 100000)

# Read the treasury rates
df_rates=read.csv("rates2.csv", header = T)
df_rates$date=NULL
df_rates$month=NULL
# Read the acquired loans data
df_acquired= read.csv("acquired loan identifier bbcn.csv", header = T)
df2=as.data.frame(df_acquired$Note_Number)
colnames(df2)=c("accountno")

# Create acquired identifier
bbcn$acquired_identifier<-0 # 0 represents bbcn_originated
bbcn$acquired_identifier[which(bbcn$accountno%in%unique(df2$accountno))]<-1 # 1 represents bbcn_acquired

# Create Y
bbcn$Y<-0
bbcn$Y[which(bbcn$amtChargedOff>0||bbcn$nonAccrualFlag%in%c(1,2,3,4,9))]<-1

# Clean up unnecessaruy observations
bbcn$Yr_origination<-year(mdy(bbcn$originationDate))
bbcn$Mn_origination<-month(mdy(bbcn$originationDate))

bbcn$Q_origination<-1
bbcn$Q_origination[which(bbcn$Mn_origination%in%c(4,5,6))]<-2
bbcn$Q_origination[which(bbcn$Mn_origination%in%c(7,8,9))]<-3
bbcn$Q_origination[which(bbcn$Mn_origination%in%c(10,11,12))]<-4

bbcn$Yr_maturity<-year(mdy(bbcn$maturityDate))
bbcn$Mn_maturity<-month(mdy(bbcn$maturityDate))

bbcn$Q_maturity<-1
bbcn$Q_maturity[which(bbcn$Mn_maturity%in%c(4,5,6))]<-2
bbcn$Q_maturity[which(bbcn$Mn_maturity%in%c(7,8,9))]<-3
bbcn$Q_maturity[which(bbcn$Mn_maturity%in%c(10,11,12))]<-4

bbcn$Yr_file<-year(mdy(bbcn$fileDate))
bbcn$Mn_file<-month(mdy(bbcn$fileDate))

bbcn$Q_file<-1
bbcn$Q_file[which(bbcn$Mn_file%in%c(4,5,6))]<-2
bbcn$Q_file[which(bbcn$Mn_file%in%c(7,8,9))]<-3
bbcn$Q_file[which(bbcn$Mn_file%in%c(10,11,12))]<-4

bbcn$ttm_m<-12*(bbcn$Yr_maturity-bbcn$Yr_file)+(bbcn$Mn_maturity-bbcn$Mn_file)
bbcn$ttm_q<-4*(bbcn$Yr_maturity-bbcn$Yr_file)+(bbcn$Q_maturity-bbcn$Q_file)

bbcn$loan_age_q<-4*(bbcn$Yr_file-bbcn$Yr_origination)+(bbcn$Q_file-bbcn$Q_origination)

bbcn$term_q<-4*(bbcn$Yr_maturity-bbcn$Yr_origination)+(bbcn$Q_maturity-bbcn$Q_origination)

bbcn$pob<-100*bbcn$loan_age_q/bbcn$term_q


# get the double digit Naics code 
# Sina Comment 1 NAICS code

temp<-as.data.frame(bbcn$naicsCode)
temp$temp<-as.numeric(as.character(substr(as.character(temp[,1]),1,2)))
temp$temp[which(temp$`bbcn$naicsCode`==0),]
temp[,3]="Other"
temp[which(temp$temp==23) ,3]<-"Construction"
temp[which(temp$temp%in% c(31,32,33)) ,3]<-"Manufacturing"
temp[which(temp$temp==42) ,3]<-"Wholesale Trade"
temp[which(temp$temp%in% c(44,45)) ,3]<-"Retail"
temp[which(temp$temp%in% c(48,49)) ,3]<-"Transportaion"
temp[which(temp$temp==51) ,3]<-"Information"
temp[which(temp$temp== 53) ,3]<-"Real Estate & Rental"
temp[which(temp$temp==54) ,3]<-"Science & Technology"
temp[which(temp$temp==56) ,3]<-"Waste Management"
temp[which(temp$temp==62) ,3]<-"Health Care"
temp[which(temp$temp==71) ,3]<-"Arts & Entertainment"
temp[which(temp$temp==72) ,3]<-"Accomodation & Food"
temp[which(temp$temp==61) ,3]<-"Educational Services"
temp[which(temp$temp==55) ,3]<-"Management of Companies"
temp[which(temp$temp==22) ,3]<-"Utilities"
temp[which(temp$temp==11) ,3]<-"Agriculture"
temp[which(temp$temp==92) ,3]<-"Public Administration"
temp[which(temp$temp==52) ,3]<-"Finance & Insurance"
temp[which(temp$temp%in%c(0,99)) ,3]<-"error"
bbcn$naics2dig<-temp$temp
bbcn$naics<-temp[,3]
rm(temp)

# Rates 
colnames(df_rates)=c("tb1m",  "tb3m",  "tb6m",  "tb1y",  "tb2y",  "tb3y",  "tb5y"  ,"tb7y",  "tb10y", "tb20y","tb30y", "Yr_file",  "Q_file" )
bbcn<-merge(bbcn, df_rates, by=c("Yr_file","Q_file"), all.x = T)

bbcn$spread_v<-0
bbcn$spread_v[which(bbcn$fixVar=="V")]<-100*bbcn$interestRate[which(bbcn$fixVar=="V")]
bbcn$spread_v[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=1))]<-100*bbcn$interestRate[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=1))]-bbcn$tb1m[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=1))]
bbcn$spread_v[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=4&&bbcn$ttm_m>1))]<-100*bbcn$interestRate[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=4&&bbcn$ttm_m>1))]-bbcn$tb3m[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=4&&bbcn$ttm_m>1))]
bbcn$spread_v[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=9&&bbcn$ttm_m>4))]<-100*bbcn$interestRate[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=9&&bbcn$ttm_m>4))]-bbcn$tb6m[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=9&&bbcn$ttm_m>4))]
bbcn$spread_v[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=18&&bbcn$ttm_m>9))]<-100*bbcn$interestRate[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=18&&bbcn$ttm_m>9))]-bbcn$tb1y[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=18&&bbcn$ttm_m>9))]
bbcn$spread_v[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=30&&bbcn$ttm_m>18))]<-100*bbcn$interestRate[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=30&&bbcn$ttm_m>18))]-bbcn$tb2y[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=30&&bbcn$ttm_m>18))]
bbcn$spread_v[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=48&&bbcn$ttm_m>30))]<-100*bbcn$interestRate[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=48&&bbcn$ttm_m>30))]-bbcn$tb3y[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=48&&bbcn$ttm_m>30))]
bbcn$spread_v[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=72&&bbcn$ttm_m>48))]<-100*bbcn$interestRate[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=72&&bbcn$ttm_m>48))]-bbcn$tb5y[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=72&&bbcn$ttm_m>48))]
bbcn$spread_v[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=102&&bbcn$ttm_m>72))]<-100*bbcn$interestRate[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=102&&bbcn$ttm_m>72))]-bbcn$tb7y[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=102&&bbcn$ttm_m>72))]
bbcn$spread_v[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=180&&bbcn$ttm_m>102))]<-100*bbcn$interestRate[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=180&&bbcn$ttm_m>102))]-bbcn$tb10y[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=180&&bbcn$ttm_m>102))]
bbcn$spread_v[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=300&&bbcn$ttm_m>180))]<-100*bbcn$interestRate[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=300&&bbcn$ttm_m>180))]-bbcn$tb20y[(which(bbcn$fixVar=="F"&&bbcn$ttm_m<=300&&bbcn$ttm_m>180))]
bbcn$spread_v[(which(bbcn$fixVar=="F"&&bbcn$ttm_m>300))]<-100*bbcn$interestRate[(which(bbcn$fixVar=="F"&&bbcn$ttm_m>300))]-bbcn$tb30y[(which(bbcn$fixVar=="F"&&bbcn$ttm_m>300))]

#first non accrual date 
unique_accountNo<-unique(bbcn$accountNo)

temp<-0

for(i in 1:length(unique_accountNo)){
  
  temp[i]<-as.Date(min(mdy(bbcn$nonAccrualDate[which(bbcn$accountNo==unique_accountNo[i])])), origin="1970-01-01")
  
}

temp<-cbind(unique_accountNo,as.Date(temp,origin = "1970-01-01"))
colnames(temp)<-c("accountNo","min_nonAccDate")
bbcn2<-merge(bbcn,temp,by="accountNo", all.x = T)

rm(i)
rm(temp)
rm(unique_accountNo)

# remove unnecessary observations 
bbcn2$deleter<-0
bbcn2$deleter[which(bbcn2$Yr_maturity<=2006)]<-1
bbcn2$deleter[which( (bbcn2$Yr_maturity== bbcn2$Yr_file) && (bbcn2$Mn_file-bbcn2$Mn_maturity>2))]<-1
bbcn2<-bbcn2[which(bbcn2$deleter==0),]
bbcn2$deleter=NULL

bbcn2$Yr_min_nonAccDate<-year(as.Date(bbcn2$min_nonAccDate,origin="1970-01-01"))
bbcn2$Mn_min_nonAccDate<-month(as.Date(bbcn2$min_nonAccDate,origin="1970-01-01"))


bbcn2$deleter<-0
bbcn2$deleter[which(bbcn2$Yr_file>bbcn2$Yr_min_nonAccDate)]<-1
bbcn2$deleter[which(bbcn2$Yr_file==bbcn2$Yr_min_nonAccDate&&(bbcn2$Mn_file-bbcn2$Mn_min_nonAccDate)>2)]<-1
bbcn2$deleter[is.na(bbcn2$Yr_min_nonAccDate)]=0
bbcn2$deleter[bbcn2$currentNetBookBal==0]=1
bbcn2<-bbcn2[which(bbcn2$deleter==0),]
bbcn2$deleter=NULL


# Portfolio ID 
bbcn2$portfolio_id<-"other"
unique(bbcn2$callReportCodeDescr)

bbcn2[which(grepl("COMMERCIAL (GENERAL PLEDGE)", bbcn2$callReportCodeDescr) && 
        grepl("Commercial Line (18)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("COMMERCIAL (GENERAL PLEDGE)", bbcn2$callReportCodeDescr) && 
        grepl("Commercial Term Loan (20)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("COMMERCIAL (GENERAL PLEDGE)", bbcn2$callReportCodeDescr) && 
        grepl("Comml LOC - Other Gov Gty (19)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("COMMERCIAL (GENERAL PLEDGE)", bbcn2$callReportCodeDescr) && 
        grepl("Comml Term - Other Gov Gty (21)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("COMMERCIAL (GENERAL PLEDGE)", bbcn2$callReportCodeDescr) && 
        grepl("Discounted Acceptance (33)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("COMMERCIAL (GENERAL PLEDGE)", bbcn2$callReportCodeDescr) && 
        grepl("Export Working Capital Program (38)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("COMMERCIAL (GENERAL PLEDGE)", bbcn2$callReportCodeDescr) && 
        grepl("Performance Bond L/C (44)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("COMMERCIAL (GENERAL PLEDGE)", bbcn2$callReportCodeDescr) && 
        grepl("Purchase Advance (31)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("COMMERCIAL (GENERAL PLEDGE)", bbcn2$callReportCodeDescr) && 
        grepl("SBA 172 Loan (66)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("COMMERCIAL (GENERAL PLEDGE)", bbcn2$callReportCodeDescr) && 
        grepl("SBA ARC Loans (62)", bbcn2$loanTypeDescr)),72]<-"CI"


bbcn2[which(grepl("COMMERCIAL (GENERAL PLEDGE)", bbcn2$callReportCodeDescr) && 
        grepl("SBA Express LOC (64)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("COMMERCIAL (GENERAL PLEDGE)", bbcn2$callReportCodeDescr) && 
        grepl("SBA SOHO Loan (65)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("COMMERCIAL (GENERAL PLEDGE)", bbcn2$callReportCodeDescr) && 
        grepl("SBA Term Loans (61)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("COMMERCIAL (GENERAL PLEDGE)", bbcn2$callReportCodeDescr) && 
        grepl("Standby L/C (43)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("COMMERCIAL (GENERAL PLEDGE)", bbcn2$callReportCodeDescr) && 
        grepl("Trust Receipt (30)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("COMMERCIAL (GENERAL PLEDGE)", bbcn2$callReportCodeDescr) && 
        grepl("Working Capital Advance (37)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[grepl("Check Credit & Rev Credit Plan", bbcn2$callReportCodeDescr), 72]="CI"
bbcn2[grepl("Com'l Loan - International Dpt", bbcn2$callReportCodeDescr), 72]="CI"
bbcn2[grepl("Com'l Loans - Borrowing Based", bbcn2$callReportCodeDescr), 72]="CI"


bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Bankers Health Group", bbcn2$loanTypeDescr)),72]<-"CI"


bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Commercial Lease (25)", bbcn2$loanTypeDescr)),72]<-"CI"


bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Commercial Term Loan (20)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Comml Asset-Based LOC (22)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Comml LOC - Other Gov Gty (19)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Comml Term - Other Gov Gty (21)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Discounted Acceptance (33)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Export Working Capital Program (38)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Express Line (26)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Master Comm LOC (01)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Master Comm LOC Sublimit (03)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Master ILOC (02)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Master ILOC Sublimit (04)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("ODP LOC - Business", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Performance Bond L/C (44)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Professional Line of Credit (51)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Purchase Advance (31)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Purchase Advance-Comm (27)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("SBA 172 Loan (66)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("SBA ARC Loans (62)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("SBA Express LOC (64)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("SBA Express Loan (63)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("SBA SOHO Loan (65)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("SBA Small Loan Advantage", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("SBA Term Loans (61)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Signature Line (11)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Simple Line of Credit (24)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Simple Loan - Commercial (23)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Simple Line of Credit (24)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Standby L/C (43)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Syndicated Leveraged Lending", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Trust Receipt (30)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Working Capital Advance (37)", bbcn2$loanTypeDescr)),72]<-"CI"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Working Capital Advance-Comm (28)", bbcn2$loanTypeDescr)),72]<-"CI"


bbcn2[which(grepl("INTERNATIONAL", bbcn2$callReportCodeDescr)),72]<-"CI"

bbcn2[which(grepl("Other Installment loans", bbcn2$callReportCodeDescr)),72]<-"CI"

bbcn2[which(grepl("Check Credit & Rev Credit Plan", bbcn2$callReportCodeDescr)), 72]="CI"

bbcn2[which(grepl("COMMERCIAL (GENERAL PLEDGE)", bbcn2$callReportCodeDescr) && 
              grepl("Commercial Real Estate (71)", bbcn2$loanTypeDescr)),72]<-"CRE"

bbcn2[which(grepl("COMMERCIAL (GENERAL PLEDGE)", bbcn2$callReportCodeDescr) && 
              grepl("SBA Real Estate (60)", bbcn2$loanTypeDescr)),72]<-"CRE"


bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Comm RE - Revolving LOC (74)", bbcn2$loanTypeDescr)),72]<-"CRE"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("SBA Real Estate (60)", bbcn2$loanTypeDescr)),72]<-"CRE"

bbcn2[which(grepl("Commercial Loans", bbcn2$callReportCodeDescr) && 
              grepl("Commercial Real Estate (71)", bbcn2$loanTypeDescr)),72]<-"CRE"

bbcn2[grepl("Conv 5+", bbcn2$callReportCodeDescr), 72]="CRE"
bbcn2[grepl("CONVENTIONAL 5+", bbcn2$callReportCodeDescr), 72]="CRE"
bbcn2[grepl("NON-FARM", bbcn2$callReportCodeDescr), 72]="CRE"
bbcn2[grepl("Other nonfarm", bbcn2$callReportCodeDescr), 72]="CRE"
bbcn2[grepl("Owner-occupied", bbcn2$callReportCodeDescr), 72]="CRE"
bbcn2[grepl("NON-FARM", bbcn2$callReportCodeDescr), 72]="CRE"
bbcn2[grepl("SECURED BY FARMEDLAND", bbcn2$callReportCodeDescr), 72]="CRE"

# temp=bbcn2[bbcn2$portfolio_id=="CRE",]
# temp2=bbcn2[bbcn2$portfolio_id=="CI",]

bbcn2$deleter<-0
bbcn2$deleter[which(bbcn2$accountNo %in% c(26506643, 23506889) & bbcn2$Yr_file>=2009 && bbcn2$Mn_file>3)]<-1 
bbcn3<-bbcn2[bbcn2$deleter==0,]
bbcn3$deleter=NULL


# Collateral 
bbcn3$collateralPropertyType
bbcn3$collateral_descr[bbcn3$collateralPropertyType==10]<-'1-4 Res'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==11]<-'Multifamily Res'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==12]<-'Office Condo'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==13]<-'Office Medical/Dental'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==14]<-'Office'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==10]<-'1-4 Res'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==10]<-'1-4 Res'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==10]<-'1-4 Res'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==10]<-'1-4 Res'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==10]<-'1-4 Res'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==10]<-'1-4 Res'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==10]<-'1-4 Res'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==10]<-'1-4 Res'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==10]<-'1-4 Res'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==10]<-'1-4 Res'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==10]<-'1-4 Res'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==10]<-'1-4 Res'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==10]<-'1-4 Res'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==10]<-'1-4 Res'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==10]<-'1-4 Res'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==10]<-'1-4 Res'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==10]<-'1-4 Res'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==10]<-'1-4 Res'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==10]<-'1-4 Res'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==10]<-'1-4 Res'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==10]<-'1-4 Res'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==10]<-'1-4 Res'
bbcn3$collateral_descr[bbcn3$collateralPropertyType==10]<-'1-4 Res'


# Sina fix Collateral Description Line 530-570

#Sina to check
bbcn$boh_rating<-0
bbcn$boh_rating[which(bbcn$loanRatingDescr2=="Substandard")]<-2000
bbcn$boh_rating[which(bbcn$loanRatingDescr2=="Doubtful")]<-3000
bbcn$boh_rating[which(bbcn$loanRatingDescr2=="Loss")]<-4000
bbcn$boh_rating[which(bbcn$loanRatingDescr2=="Pass-3")]<-3
bbcn$boh_rating[which(bbcn$loanRatingDescr2=="Pass-4")]<-4
bbcn$boh_rating[which(bbcn$loanRatingDescr2=="Sp Mention")]<-1000
bbcn$boh_rating[which(bbcn$loanRatingDescr2=="Pass-1")]<-1
bbcn$boh_rating[which(bbcn$loanRatingDescr2=="Pass-2")]<-2
bbcn$boh_rating[which(bbcn$loanRatingDescr2==" Pass-2")]<-2
bbcn$boh_rating[which(bbcn$loanRatingDescr2=="Pass-2 ")]<-2
bbcn$boh_rating[which(bbcn$loanRatingDescr2==" Pass-2 ")]<-2

bbcn<-bbcn[which(bbcn$productDescr!="Letter of C"),]

# Sina fix Line 657-673
