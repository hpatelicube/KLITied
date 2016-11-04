library(readxl)   

setwd("C:\\Users\\Hitendra\\Desktop\\Kotak")

emp_demo1=readxl::read_excel("KLPQ - Tied Data - After 01-Apr-13.xlsx",
                             sheet = "Database")

emp_demo2=readxl::read_excel("KLPQ - Tied data - Existing 01-Apr-13.xlsx",
                             sheet = "Database - 01-Apr-13")

# names(reporting_master)[names(reporting_master) == 'LOCATION'] <- 'LOCATION.ORIGINAL'

col_names=c("EMPLOYEE.NUMBER","NAME","GRADE","DEPARTMENT","TITLE","LOCATION","ZONE",
           "APPRAISER.EMPLOYEE.NUMBER","APPRAISER.NAME","DOJ","DOJ.GROUP","DOB",
           "ASSIGNMENT.STATUS","GENDER","CITY","ORACLE.LWD","STATUS","PROFILE.SOURCE",
           "SOURCE.TYPE","No of Years Worked (Year.MM)","MARITAL.STATUS")

names(emp_demo1)=col_names

col_names=c("EMPLOYEE.NUMBER","NAME","GRADE","DEPARTMENT","TITLE","LOCATION","ZONE",
            "APPRAISER.EMPLOYEE.NUMBER","APPRAISER.NAME","DOJ","DOB",
            "GENDER","CITY","STATUS","PROFILE.SOURCE",
            "SOURCE.TYPE","No of Years Worked (Year.MM)","MARITAL.STATUS","PREVIOUS.COMPENSATION")

names(emp_demo2)=col_names

col_names=c("EMPLOYEE.NUMBER","NAME","GRADE","DEPARTMENT","TITLE","LOCATION","ZONE",
            "APPRAISER.EMPLOYEE.NUMBER","APPRAISER.NAME","DOJ","DOB",
            "GENDER","CITY","STATUS","PROFILE.SOURCE",
            "SOURCE.TYPE","MARITAL.STATUS")


