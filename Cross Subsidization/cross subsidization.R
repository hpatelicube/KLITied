library(lubridate)
library(data.table)
library(reshape)
library(dplyr)
setwd("C:\\Users\\Hitendra\\Desktop\\Kotak\\Analysis")

emp_master= read.csv("20160825145004_emp_master.csv")
rep_master= read.csv("20160825144535_reporting_master.csv")
#load("joint_calls.RDA")
load("Rec_Name_Col2.RDA")
load("conversion.RDA")
conversion=conversion[!is.na(conversion$EMPLOYEE.ID),]
# load("carrier_history.Rda")

#calculate conversion data
conversion= conversion[,c("EMPLOYEE.ID","ISSUE.DATE","PREMIUM.CHARGED")]
conversion = merge(conversion, emp_master[,c("EMPLOYEE.ID","DOJ","DOL")], all.x=TRUE)
conversion$ISSUE.DATE= as.Date(conversion$ISSUE.DATE,format="%d-%b-%y")
conversion$DOJ= as.Date(conversion$DOJ,format="%d-%m-%Y")
conversion$DOL= as.Date(conversion$DOL,format="%d-%m-%Y")
conversion$PREMIUM.CHARGED= as.character(conversion$PREMIUM.CHARGED)
conversion$PREMIUM.CHARGED= as.numeric(conversion$PREMIUM.CHARGED)

emp_master$DOJ= as.Date(emp_master$DOJ,format="%d-%m-%Y")
emp_master$DOL= as.Date(emp_master$DOL,format="%d-%m-%Y")


# for sales in march 2016
emp_march16=emp_master[emp_master$DOJ<"2016-03-01",]
emp_march16=emp_march16[is.na(emp_march16$DOL) | emp_march16$DOL >="2016-03-15",]


conversion_march16=conversion[conversion$ISSUE.DATE>="2016-03-01" & conversion$ISSUE.DATE <="2016-03-31",]
conversion_march16=aggregate(conversion_march16$PREMIUM.CHARGED,by=list(EMPLOYEE.ID=conversion_march16$EMPLOYEE.ID),sum)
names(conversion_march16)[2]="TOTAL.PREMIUM"

emp_march16=merge(emp_march16,conversion_march16,all.x=TRUE)
emp_march16$TOTAL.PREMIUM[is.na(emp_march16$TOTAL.PREMIUM)]=0

emp_march16$Tenure_march16= difftime(as.Date("2016-03-15",format="%Y-%m-%d"), emp_march16$DOJ,units = c("days"))
emp_march16$Tenure_march16= round(as.numeric(emp_march16$Tenure_march16,units="days")/365,2)

write.csv(emp_march16,"conversion_march16.csv")




conversion=conversion[conversion$EMPLOYEE.ID %in% emp_master$EMPLOYEE.ID,]

conversion$Tenure= difftime(as.Date("2016-03-15",format="%Y-%m-%d"), conversion$DOJ,units = c("days"))
conversion$Tenure= round(as.numeric(conversion$Tenure,units="days")/365,2)

write.csv(conversion,"conversion.csv")



conversion$NO.OF.DAYS.DOJ= difftime(conversion$ISSUE.DATE, conversion$DOJ,units = c("days"))
conversion$PREMIUM.CHARGED= as.character(conversion$PREMIUM.CHARGED)
conversion$PREMIUM.CHARGED= as.numeric(conversion$PREMIUM.CHARGED)
conversion= conversion[conversion$NO.OF.DAYS.DOJ<=180,]
conversion$NO.OF.DAYS.DOJ= as.numeric(conversion$NO.OF.DAYS.DOJ,units="days")
conversion= conversion[!is.na(conversion$NO.OF.DAYS.DOJ),]
conversion$NO.OF.MONTH.DOJ= ceiling(as.numeric(conversion$NO.OF.DAYS.DOJ,units="days")/30)
conversion$PREMIUM.COUNT=1

conversion2= merge(conversion,emp[,c("EMPLOYEE.ID","PERF.GRADE")],by="EMPLOYEE.ID")


performance1= melt(conversion,id= c("EMPLOYEE.ID","NO.OF.MONTH.DOJ"),measure=c("PREMIUM.CHARGED","PREMIUM.COUNT"))
performance=cast(performance1,EMPLOYEE.ID +NO.OF.MONTH.DOJ  ~ variable, sum)
emp= merge(performance, emp_master[,c("EMPLOYEE.ID","DOJ")],by="EMPLOYEE.ID")
emp= emp[as.POSIXlt(emp$DOJ,"%d/%m/%Y",tz="")>as.POSIXlt("31/03/2013","%d/%m/%Y",tz=""),]
#emp1= emp[emp$EMPLOYEE.ID==45338,]