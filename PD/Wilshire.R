library(openxlsx)
library(lubridate)

wb<-read.csv("Book1.csv", header = T)

# Sina fix rates and acquired

wbco<-read.csv("wilshire charge offs cleaned.csv", header=T)

colnames(wbco)<-c("Note.Number","first_co_date","co_amt")

wb<-merge(wb,wbco, by="Note.Number", all.x=T)

rm(wbco)

wb$Y<-0
wb$Y[which(wb$Non.Accrual.Code%in%c(2,4))]<-1
wb$Y[which(wb$co_amt==1)]<-1

# Sina fix NAICS code
temp<-as.data.frame(wb$NAICS.Code)
temp$temp<-as.numeric(as.character(substr(as.character(temp[,1]),1,2)))

wb$temp_number<-temp$temp

rm(temp)

wb$Yr_origination<-year(mdy(wb$originationdate))
wb$Mn_origination<-month(mdy(wb$originationdate))

wb$Q_origination<-1
wb$Q_origination[which(wb$Mn_origination%in%c(4,5,6))]<-2
wb$Q_origination[which(wb$Mn_origination%in%c(7,8,9))]<-3
wb$Q_origination[which(wb$Mn_origination%in%c(10,11,12))]<-4

wb$Yr_maturity<-year(mdy(wb$maturitydate))
wb$Mn_maturity<-month(mdy(wb$maturitydate))

wb$Q_maturity<-1
wb$Q_maturity[which(wb$Mn_maturity%in%c(4,5,6))]<-2
wb$Q_maturity[which(wb$Mn_maturity%in%c(7,8,9))]<-3
wb$Q_maturity[which(wb$Mn_maturity%in%c(10,11,12))]<-4

wb$Yr_file<-year(mdy(wb$filedate))
wb$Mn_file<-month(mdy(wb$filedate))

wb$Q_file<-1
wb$Q_file[which(wb$Mn_file%in%c(4,5,6))]<-2
wb$Q_file[which(wb$Mn_file%in%c(7,8,9))]<-3
wb$Q_file[which(wb$Mn_file%in%c(10,11,12))]<-4

wb$ttm_m<-12*(wb$Yr_maturity-wb$Yr_file)+(wb$Mn_maturity-wb$Mn_file)
wb$ttm_q<-4*(wb$Yr_maturity-wb$Yr_file)+(wb$Q_maturity-wb$Q_file)

wb$loan_age_q<-4*(wb$Yr_file-wb$Yr_origination)+(wb$Q_file-wb$Q_origination)

wb$term_q<-4*(wb$Yr_maturity-wb$Yr_origination)+(wb$Q_maturity-wb$Q_origination)

wb$pob<-100*wb$loan_age_q/wb$term_q

# Sina fix Rate.Over.Split name

unique_accountNo<-unique(wb$Note.Number)

temp<-0

for(i in 1:length(unique_accountNo)){
  
  temp[i]<-as.Date(min(mdy(wb$filedate[which(wb$Note.Number==unique_accountNo[i]&wb$Y==1)])), origin="1970-01-01")
  
}

temp<-cbind(unique_accountNo,as.Date(temp,origin = "1970-01-01"))

colnames(temp)<-c("Note.Number","min_nonAccDate")

wb<-merge(wb,temp,by="Note.Number")

rm(i)
rm(temp)
rm(unique_accountNo)

wb$f_nonAccDate<-0
wb$f_nonAccDate[which(is.na(wb$first_co_date)==T)]<-wb$min_nonAccDate[which(is.na(wb$first_co_date)==T)]
wb$f_nonAccDate[which(is.na(wb$first_co_date)==F)]<-min(as.numeric(wb$first_co_date[which(is.na(wb$first_co_date)==F)]),as.numeric(wb$min_nonAccDate[which(is.na(wb$first_co_date)==F)]))

wb$deleter<-1
wb$deleter[which(is.na(wb$first_co_date)==T)]<-0
wb$deleter[which(mdy(wb$filedate)<=ymd(wb$first_co_date))]<-0

wb<-wb[which(wb$deleter==0),]

wb$deleter<-1
wb$deleter[which(is.na(wb$min_nonAccDate)==T)]<-0
wb$deleter[which(mdy(wb$filedate)<=as.Date(wb$min_nonAccDate,origin="1970-01-01"))]<-0

wb<-wb[which(wb$deleter==0),]

wb<-wb[which(wb$Yr_maturity>2006),]
wb<-wb[which(wb$ttm_q>0),]

wb$boh_id<-"Wilshere"

wb<-wb[which(wb$Class.Code%in%c(2,3,5,6,10,13,20,21,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,59,60,61,63,99)),]

wb<-wb[which(is.na(wb$NAP...NAIP...NAIP.in.GL)==F&wb$NAP...NAIP...NAIP.in.GL!=0),]
wb<-wb[which(is.na(wb$Rate.Over.Split)==F&wb$Rate.Over.Split!=0),]

correction<-read.csv("property_code_correctio.csv",header=T)

# Sina fix column names

wb<-merge(wb,correction,by="Note.Number", all.x=T)
wb$propertyCodeNew<-wb$Property.Type.Code
wb$propertyCodeNew[which(is.na(wb$New_Code)==F)]<-wb$New_Code[which(is.na(wb$New_Code)==F)]

# Sina fix line 605

wb$boh_rating<-0
wb$boh_rating[which(wb$Loan.Rating.Code1==0)]<-0
wb$boh_rating[which(wb$Loan.Rating.Code1==1000)]<-1
wb$boh_rating[which(wb$Loan.Rating.Code1==2000)]<-2
wb$boh_rating[which(wb$Loan.Rating.Code1==3000)]<-3
wb$boh_rating[which(wb$Loan.Rating.Code1==4000)]<-4
wb$boh_rating[which(wb$Loan.Rating.Code1==5000)]<-4
wb$boh_rating[which(wb$Loan.Rating.Code1==6000)]<-1000
wb$boh_rating[which(wb$Loan.Rating.Code1==7000)]<-2000
wb$boh_rating[which(wb$Loan.Rating.Code1==8000)]<-3000
wb$boh_rating[which(wb$Loan.Rating.Code1==9000)]<-4000
