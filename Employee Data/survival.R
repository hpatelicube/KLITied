library(readxl)   
library(lubridate)
library(randomForest)
library(reshape2)
library(reshape)
library(data.table)
library(dplyr)
library(Amelia)
library(Deducer)

setwd("C:\\Users\\Hitendra\\Desktop\\KLITied\\Employee Data")

# Reporitng Master
reporting_master <- read.csv("20160825144535_reporting_master.csv")

reporting_master$QUARTER.ENDDATE=as.Date(reporting_master$QUARTER.ENDDATE,format="%Y-%m-%d")

#employee Master
emp_master= read.csv("20160825145004_emp_master.csv")

emp_master$DOJ=as.Date(emp_master$DOJ,format="%Y-%m-%d")
emp_master$DOB=as.Date(emp_master$DOB,format="%Y-%m-%d")
emp_master$DOL=as.Date(emp_master$DOL,format="%Y-%m-%d")


emp_master$TOTAL_TENURE=ifelse(is.na(emp_master$DOL),as.Date("2016-06-30",format="%Y-%m-%d")-emp_master$DOJ,
                               emp_master$DOL-emp_master$DOJ)
emp_master$TOTAL_TENURE=emp_master$TOTAL_TENURE/365.25

emp_master$Age=ifelse(is.na(emp_master$DOL),as.Date("2016-06-30",format="%Y-%m-%d")-emp_master$DOB,
                               emp_master$DOL-emp_master$DOB)

emp_master$Age=emp_master$Age/365.25

emp_master$censor=ifelse(is.na(emp_master$DOL),0,1)

emp_master1=emp_master[emp_master$DOJ<="2016-06-30",]

library(survival)

emp_master1$age_cat=cut(as.numeric(emp_master1$Age),br=c(21,28,32,35,40,70),right = FALSE)

emp_master1$TOTAL_TENURE=round(emp_master1$TOTAL_TENURE * 2 ,0)/2

age.surv <- survfit( Surv(emp_master1$TOTAL_TENURE, emp_master1$censor)~ 
                       strata(emp_master1$age_cat), conf.type="log-log")
print(age.surv)

summary(age.surv)

x=summary(age.surv)
x1=x[x$strata=="strata(emp_master1$age_cat)=[21,28)"]


plot(age.surv, lty=c(1, 2, 3, 4, 5), xlab="Time", ylab="Survival Probability")
legend("topright", c("Group 1", "Group 2", "Group 3", "Group 4","Group 5"), lty=c(1, 2, 3, 4, 5)) 

plot.new()
legend(1, 95, legend=c("Line 1", "Line 2"),
       col=c("red", "blue"), lty=1:2, cex=0.8,
       title="Line types", text.font=4, bg='lightblue')

legend('topright', c("l1","l2","l3","l4") , 
       lty=1, col=c('red', 'blue', 'green',' brown'), bty='n', cex=.75)


plot (c(1968,2010),c(0,10),type="n", # sets the x and y axes scales 
      
      xlab="Year",ylab="Expenditures/GDP (%)") # adds titles to the axes 

plot.new()
lines(x$time[x$strata=="strata(emp_master1$age_cat)=[21,28)"],x$surv[x$strata=="strata(emp_master1$age_cat)=[21,28)"],col="red",lwd=2.5) # adds a line for defense expenditures 

lines(year,health,col="blue",lwd=2.5) # adds a line for health expenditures 

legend(2000,9.5, # places a legend at the appropriate place c("Health","Defense"), # puts text in the legend 
       
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       
       lwd=c(2.5,2.5),col=c("blue","red")) # gives the legend lines the correct color and width