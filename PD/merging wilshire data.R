# Set the working directory 
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("C:/Users/ic07949/Desktop/KPMG/Model Development/development code and data/PD/Dataset/Wilshire")

# Required libraries for this code
library(openxlsx)
library (lubridate)

# append the wilshire data together
df16q1=read.xlsx("Data as of 2016 0331_GLreconciled.xlsx",detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df16q1=df16q1[!is.na(df16q1[,1]), ]
df16q1$Rate.Adjuster=NULL
date=rep("03/31/2016", nrow(df16q1))
df16q1=as.data.frame(cbind(date,df16q1))
column.names=colnames(df16q1)

df15q4=read.xlsx("Data as of 2015 1231_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df15q4=df15q4[!is.na(df15q4[,1]), ]
date=rep("12/31/2015", nrow(df15q4))
df15q4=as.data.frame(cbind(date,df15q4))
colnames(df15q4)= column.names

df15q3=read.xlsx("Data as of 2015 0930_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df15q3=df15q3[!is.na(df15q3[,1]), ]
date=rep("09/30/2015", nrow(df15q3))
df15q3=as.data.frame(cbind(date,df15q3))
colnames(df15q3)= column.names

df15q2=read.xlsx("Data as of 2015 0630_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df15q2=df15q2[!is.na(df15q2[,1]), ]
date=rep("06/30/2015", nrow(df15q2))
df15q2=as.data.frame(cbind(date,df15q2))

df15q1=read.xlsx("Data as of 2015 0331_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df15q1=df15q1[!is.na(df15q1[,1]), ]
date=rep("03/31/2015", nrow(df15q1))
df15q1=as.data.frame(cbind(date,df15q1))

df14q4=read.xlsx("Data as of 2014 1231_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df14q4=df14q4[!is.na(df14q4[,1]), ]
date=rep("12/31/2014", nrow(df14q4))
df14q4=as.data.frame(cbind(date,df14q4))

df14q3=read.xlsx("Data as of 2014 0930_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df14q3=df14q3[!is.na(df14q3[,1]), ]
date=rep("09/30/2014", nrow(df14q3))
df14q3=as.data.frame(cbind(date,df14q3))

df14q2=read.xlsx("Data as of 2014 0630_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df14q2=df14q2[!is.na(df14q2[,1]), ]
date=rep("06/30/2014", nrow(df14q2))
df14q2=as.data.frame(cbind(date,df14q2))

df14q1=read.xlsx("Data as of 2014 0331_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df14q1=df14q1[!is.na(df14q1[,1]), ]
date=rep("03/31/2014", nrow(df14q1))
df14q1=as.data.frame(cbind(date,df14q1))

df13q4=read.xlsx("Data as of 2013 1231_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df13q4=df13q4[!is.na(df13q4[,1]), ]
date=rep("12/31/2013", nrow(df13q4))
df13q4=as.data.frame(cbind(date,df13q4))

df13q3=read.xlsx("Data as of 2013 0930_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df13q3=df13q3[!is.na(df13q3[,1]), ]
date=rep("09/30/2013", nrow(df13q3))
df13q3=as.data.frame(cbind(date,df13q3))

df13q2=read.xlsx("Data as of 2013 0630_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df13q2=df13q2[!is.na(df13q2[,1]), ]
date=rep("06/30/2013", nrow(df13q2))
df13q2=as.data.frame(cbind(date,df13q2))

df13q1=read.xlsx("Data as of 2013 0331_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df13q1=df13q1[!is.na(df13q1[,1]), ]
date=rep("03/31/2013", nrow(df13q1))
df13q1=as.data.frame(cbind(date,df13q1))

df12q4=read.xlsx("Data as of 2012 1231_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df12q4=df12q4[!is.na(df12q4[,1]), ]
date=rep("12/31/2012", nrow(df12q4))
df12q4=as.data.frame(cbind(date,df12q4))

df12q3=read.xlsx("Data as of 2012 0930_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df12q3=df12q3[!is.na(df12q3[,1]), ]
date=rep("09/30/2012", nrow(df12q3))
df12q3=as.data.frame(cbind(date,df12q3))

df12q2=read.xlsx("Data as of 2012 0630_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df12q2=df12q2[!is.na(df12q2[,1]), ]
date=rep("06/30/2012", nrow(df12q2))
df12q2=as.data.frame(cbind(date,df12q2))

df12q1=read.xlsx("Data as of 2012 0331_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df12q1=df12q1[!is.na(df12q1[,1]), ]
date=rep("03/31/2012", nrow(df12q1))
df12q1=as.data.frame(cbind(date,df12q1))

df11q4=read.xlsx("Data as of 2011 1231_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df11q4=df11q4[!is.na(df11q4[,1]), ]
date=rep("12/31/2011", nrow(df11q4))
df11q4=as.data.frame(cbind(date,df11q4))

df11q3=read.xlsx("Data as of 2011 0930_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df11q3=df11q3[!is.na(df11q3[,1]), ]
date=rep("09/30/2011", nrow(df11q3))
df11q3=as.data.frame(cbind(date,df11q3))

df11q2=read.xlsx("Data as of 2011 0630_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df11q2=df11q2[!is.na(df11q2[,1]), ]
date=rep("06/30/2011", nrow(df11q2))
df11q2=as.data.frame(cbind(date,df11q2))

df11q1=read.xlsx("Data as of 2011 0331_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df11q1=df11q1[!is.na(df11q1[,1]), ]
date=rep("03/31/2011", nrow(df11q1))
df11q1=as.data.frame(cbind(date,df11q1))

df10q4=read.xlsx("Data as of 2010 1231_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df10q4=df10q4[!is.na(df10q4[,1]), ]
date=rep("12/31/2010", nrow(df10q4))
df10q4=as.data.frame(cbind(date,df10q4))

df10q3=read.xlsx("Data as of 2010 0930_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df10q3=df10q3[!is.na(df10q3[,1]), ]
date=rep("09/30/2010", nrow(df10q3))
df10q3=as.data.frame(cbind(date,df10q3))

df10q2=read.xlsx("Data as of 2010 0630_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df10q2=df10q2[!is.na(df10q2[,1]), ]
date=rep("06/30/2010", nrow(df10q2))
df10q2=as.data.frame(cbind(date,df10q2))

df10q1=read.xlsx("Data as of 2010 0331_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df10q1=df10q1[!is.na(df10q1[,1]), ]
date=rep("03/31/2010", nrow(df10q1))
df10q1=as.data.frame(cbind(date,df10q1))


df09q4=read.xlsx("Data as of 2009 1231_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df09q4=df09q4[!is.na(df09q4[,1]), ]
date=rep("12/31/2009", nrow(df09q4))
df09q4=as.data.frame(cbind(date,df09q4))

df09q3=read.xlsx("Data as of 2009 0930_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df09q3=df09q3[!is.na(df09q3[,1]), ]
date=rep("09/30/2009", nrow(df09q3))
df09q3=as.data.frame(cbind(date,df09q3))

df09q2=read.xlsx("Data as of 2009 0630_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df09q2=df09q2[!is.na(df09q2[,1]), ]
date=rep("06/30/2009", nrow(df09q2))
df09q2=as.data.frame(cbind(date,df09q2))

df09q1=read.xlsx("Data as of 2009 0331_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df09q1=df09q1[!is.na(df09q1[,1]), ]
date=rep("03/31/2009", nrow(df09q1))
df09q1=as.data.frame(cbind(date,df09q1))

df08q4=read.xlsx("Data as of 2008 1231_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df08q4=df08q4[!is.na(df08q4[,1]), ]
date=rep("12/31/2008", nrow(df08q4))
df08q4=as.data.frame(cbind(date,df08q4))

df08q3=read.xlsx("Data as of 2008 0930_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df08q3=df08q3[!is.na(df08q3[,1]), ]
date=rep("09/30/2008", nrow(df08q3))
df08q3=as.data.frame(cbind(date,df08q3))

df08q2=read.xlsx("Data as of 2008 0630_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df08q2=df08q2[!is.na(df08q2[,1]), ]
date=rep("06/30/2008", nrow(df08q2))
df08q2=as.data.frame(cbind(date,df08q2))

df08q1=read.xlsx("Data as of 2008 0331_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df08q1=df08q1[!is.na(df08q1[,1]), ]
date=rep("03/31/2008", nrow(df08q1))
df08q1=as.data.frame(cbind(date,df08q1))

df07q4=read.xlsx("Data as of 2007 1231_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df07q4=df07q4[!is.na(df07q4[,1]), ]
date=rep("12/31/2007", nrow(df07q4))
df07q4=as.data.frame(cbind(date,df07q4))

df07q3=read.xlsx("Data as of 2007 0930_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df07q3=df07q3[!is.na(df07q3[,1]), ]
date=rep("09/30/2007", nrow(df07q3))
df07q3=as.data.frame(cbind(date,df07q3))

df07q2=read.xlsx("Data as of 2007 0630_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df07q2=df07q2[!is.na(df07q2[,1]), ]
date=rep("06/30/2007", nrow(df07q2))
df07q2=as.data.frame(cbind(date,df07q2))

df07q1=read.xlsx("Data as of 2007 0331_GLreconciled.xlsx", detectDates = FALSE, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = TRUE)
df07q1=df07q1[!is.na(df07q1[,1]), ]
date=rep("03/31/2007", nrow(df07q1))
df07q1=as.data.frame(cbind(date,df07q1))

Total_commit=rep("NA", nrow(df09q4))
df09q4_2=as.data.frame(cbind(df09q4[,c(1:8)], Total_commit, df09q4[,c(9:ncol(df09q4))]))
df09q4 = df09q4_2

Total_commit=rep("NA", nrow(df09q3))
df09q3_2=as.data.frame(cbind(df09q3[,c(1:8)], Total_commit, df09q3[,c(9:ncol(df09q3))]))
df09q3 = df09q3_2


Total_commit=rep("NA", nrow(df09q2))
df09q2_2=as.data.frame(cbind(df09q2[,c(1:8)], Total_commit, df09q2[,c(9:ncol(df09q2))]))
df09q2 = df09q2_2


Total_commit=rep("NA", nrow(df09q1))
df09q1_2=as.data.frame(cbind(df09q1[,c(1:8)], Total_commit, df09q1[,c(9:ncol(df09q1))]))
df09q1 = df09q1_2

Total_commit=rep("NA", nrow(df08q4))
df08q4_2=as.data.frame(cbind(df08q4[,c(1:8)], Total_commit, df08q4[,c(9:ncol(df08q4))]))
df08q4 = df08q4_2


Total_commit=rep("NA", nrow(df08q3))
df08q3_2=as.data.frame(cbind(df08q3[,c(1:8)], Total_commit, df08q3[,c(9:ncol(df08q3))]))
df08q3 = df08q3_2

Total_commit=rep("NA", nrow(df08q2))
df08q2_2=as.data.frame(cbind(df08q2[,c(1:8)], Total_commit, df08q2[,c(9:ncol(df08q2))]))
df08q2 = df08q2_2

Total_commit=rep("NA", nrow(df08q1))
df08q1_2=as.data.frame(cbind(df08q1[,c(1:8)], Total_commit, df08q1[,c(9:ncol(df08q1))]))
df08q1 = df08q1_2


Total_commit=rep("NA", nrow(df07q4))
df07q4_2=as.data.frame(cbind(df07q4[,c(1:8)], Total_commit, df07q4[,c(9:ncol(df07q4))]))
df07q4 = df07q4_2

Total_commit=rep("NA", nrow(df07q3))
df07q3_2=as.data.frame(cbind(df07q3[,c(1:8)], Total_commit, df07q3[,c(9:ncol(df07q3))]))
df07q3 = df07q3_2


Total_commit=rep("NA", nrow(df07q2))
df07q2_2=as.data.frame(cbind(df07q2[,c(1:8)], Total_commit, df07q2[,c(9:ncol(df07q2))]))
df07q2 = df07q2_2


Total_commit=rep("NA", nrow(df07q1))
df07q1_2=as.data.frame(cbind(df07q1[,c(1:8)], Total_commit, df07q1[,c(9:ncol(df07q1))]))
df07q1 = df07q1_2

colnames(df15q4)= column.names
colnames(df15q3)= column.names
colnames(df15q2)= column.names
colnames(df15q1)= column.names
colnames(df14q4)= column.names
colnames(df14q3)= column.names
colnames(df14q2)= column.names
colnames(df14q1)= column.names
colnames(df13q4)= column.names
colnames(df13q3)= column.names
colnames(df13q2)= column.names
colnames(df13q1)= column.names
colnames(df12q4)= column.names
colnames(df12q3)= column.names
colnames(df12q2)= column.names
colnames(df12q1)= column.names
colnames(df11q4)= column.names
colnames(df11q3)= column.names
colnames(df11q2)= column.names
colnames(df11q1)= column.names
colnames(df10q4)= column.names
colnames(df10q3)= column.names
colnames(df10q2)= column.names
colnames(df10q1)= column.names
colnames(df09q4)= column.names
colnames(df09q3)= column.names
colnames(df09q2)= column.names
colnames(df09q1)= column.names
colnames(df08q4)= column.names
colnames(df08q3)= column.names
colnames(df08q2)= column.names
colnames(df08q1)= column.names
colnames(df07q4)= column.names
colnames(df07q3)= column.names
colnames(df07q2)= column.names
colnames(df07q1)= column.names

df_final_wilshire= rbind( df16q1, df15q4, df15q3, df15q2, df15q1, df14q4, df14q3, df14q2, df14q1,
                         df13q4, df13q3, df13q2, df13q1, df12q4, df12q3, df12q2, df12q1,
                         df11q4, df11q3, df11q2, df11q1,df10q4, df10q3, df10q2, df10q1,
                         df09q4, df09q3, df09q2, df09q1,df08q4, df08q3, df08q2, df08q1,
                         df07q4, df07q3, df07q2, df07q1)

##################################
# set the dates. The date origins for R SAS and Excel are different. 
#This should be taken into account. 
##################################

filedate=mdy(df_final_wilshire$date)
Originationdate=as.Date(df_final_wilshire$Original.Note.Date, origin="1899-12-30")
maturitydate=as.Date(df_final_wilshire$Maturity.Date, origin="1899-12-30")
df_final_wilshire2=cbind(filedate, df_final_wilshire, maturitydate, Originationdate)
colnames(df_final_wilshire2)= c("filedate", column.names, "maturitydate", "originationdate")

df_final_wilshire2$date=NULL
df_final_wilshire2$Maturity.Date=NULL
df_final_wilshire2$Original.Note.Date=NULL

df_final_wilshire_sorted=df_final_wilshire2[order(df_final_wilshire2$Note.Number),]
indx=colnames(df_final_wilshire_sorted)

#change the DCR format
df_final_wilshire_sorted[is.na(df_final_wilshire_sorted$DCR.from.Stress.Test), 26]=10000
df_final_wilshire_sorted[,43]=as.numeric(df_final_wilshire_sorted[,26])
df_final_wilshire_sorted[is.na(df_final_wilshire_sorted[,43]), 26]= substr(df_final_wilshire_sorted[is.na(df_final_wilshire_sorted[,43]), 26], 1, nchar(df_final_wilshire_sorted[is.na(df_final_wilshire_sorted[,43]), 26])-1)
df_final_wilshire_sorted[,44]=as.numeric(df_final_wilshire_sorted[,26])
df_final_wilshire_sorted=df_final_wilshire_sorted[,-43]
colnames(df_final_wilshire_sorted)=c(indx, "DCR")

colnames(df_final_wilshire_sorted)

##########################
# Export the data
##########################
write.csv(df_final_wilshire_sorted, file = "df_final_wilshire_sorted.csv", col.names = TRUE, row.names = F)


##########################
#send the missing codes to Soo!
##########################
df_error_collateral=df_final_wilshire_sorted[which(df_final_wilshire_sorted$Collateral.Code %in% c(75,83,371)),c(1,2,3,5,8,18)]
write.csv(df_error_collateral, file = 'missing collateral codes.csv', col.names = T, row.names = F)
df_error_property=df_final_wilshire_sorted[which(df_final_wilshire_sorted$Property.Type.Code==20),c(1,2,3,5,8,17)]
write.csv(df_error_property, file = 'missing property types code.csv', col.names = T, row.names = F)


a=unique(df_error_collateral$Note.Number)
b=unique(df_error_property$Note.Number)
d=c(a,b)
write.csv(d, file = 'accounts w missing codes.csv', col.names = T, row.names = F)

