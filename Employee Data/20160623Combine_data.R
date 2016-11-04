library(readxl)
setwd("C:\\Users\\Hitendra\\Desktop\\KLITied\\Employee Data")

#employee Demograpy

emp_dem_existing=readxl::read_excel("KLPQ - Tied data - Existing 01-Apr-13V2.xlsx",
                                          sheet ="Database - 01-Apr-13")

col_names=c("EMPLOYEE.ID","NAME","GRADE","DEPARTMENT","TITLE","LOCATION","ZONE",
            "REPORTING.MANAGER.ID","REPORTING.MANAGER.NAME","DOJ","DOB",
            "GENDER","CITY","STATUS","HIRE.SOURCE","HIRE.TYPE","YRS.OF.EXP",
            "MARITAL.STATUS.JOINING","LAST.SALARY","DOL","MARITAL.STATUS.ASOF")

colnames(emp_dem_existing)=col_names


emp_dem_after=readxl::read_excel("KLPQ - Tied Data - After 01-Apr-13.xlsx",
                                    sheet ="Database")

col_names=c("EMPLOYEE.ID","NAME","GRADE","DEPARTMENT","TITLE","LOCATION","ZONE",
            "REPORTING.MANAGER.ID","REPORTING.MANAGER.NAME","DOJ","DOJ.GROUP","DOB",
            "ASSIGNMENT.STATUS","GENDER","CITY","DOL","STATUS","HIRE.SOURCE","HIRE.TYPE",
            "YRS.OF.EXP","MARITAL.STATUS.JOINING")

colnames(emp_dem_after)=col_names

emp_dem_existing$DOB=as.Date(emp_dem_existing$DOB,format="%d-%b-%Y")


col_names=c("EMPLOYEE.ID","NAME","GRADE","DEPARTMENT","TITLE","LOCATION","ZONE",
            "REPORTING.MANAGER.ID","REPORTING.MANAGER.NAME","DOJ","DOB",
            "GENDER","CITY","DOL","STATUS","HIRE.SOURCE","HIRE.TYPE",
            "YRS.OF.EXP","MARITAL.STATUS.JOINING")


emp_dem=rbind(emp_dem_existing[,col_names],emp_dem_after[,col_names])

rm(emp_dem_existing)
rm(emp_dem_after)

# carrier History

carrier_history_existing=readxl::read_excel("KLPQ - Tied data - Existing 01-Apr-13V2.xlsx",
                                    sheet ="Career Histroy")

col_names=c("EMPLOYEE.ID","NAME","DOJ","CAREER.CHANGE.DATE","CHANGE.REASON","START.DATE",
            "END.DATE","REVISED.GRADE","REVISED.DESIGNATION","LOCATION.ID","NEW.LOCATION",
            "REVISED.DEPARTMENT","REPORTING.MANAGER.ID","REPORTING.MANAGER")
            
colnames(carrier_history_existing)=col_names


carrier_history_after=readxl::read_excel("KLPQ - Tied Data - After 01-Apr-13.xlsx",
                                 sheet ="Career History")

col_names=c("EMPLOYEE.ID","NAME","DOJ","CAREER.CHANGE.DATE","CHANGE.REASON","START.DATE",
            "UPDATED.BY","END.DATE","CREATION.DATE","OLD.GRADE","ASSIGNMENT.UPDATE.DATE",
            "REVISED.GRADE","REVISED.DESIGNATION","REPORTING.MANAGER.ID",
            "REPORTING.MANAGER")

colnames(carrier_history_after)=col_names

carrier_history_after_old=readxl::read_excel("KLPQ - Tied Data.xlsx",
                                         sheet ="Career History")

col_names=c("EMPLOYEE.ID","NAME","DOJ","CAREER.CHANGE.DATE","CHANGE.REASON","START.DATE",
            "END.DATE","REVISED.GRADE","REVISED.DESIGNATION",
            "LOCATION.ID","REVISED.DEPARTMENT")

colnames(carrier_history_after_old)=col_names


carrier_history_after=merge(carrier_history_after,
                            carrier_history_after_old[,c("EMPLOYEE.ID","CHANGE.REASON",
                                                         "START.DATE",
                                                         "LOCATION.ID","REVISED.DEPARTMENT")],
                            by=c("EMPLOYEE.ID","CHANGE.REASON",
                                 "START.DATE"),all.x = TRUE)


col_names=c("EMPLOYEE.ID","NAME","DOJ","CAREER.CHANGE.DATE","CHANGE.REASON","START.DATE",
            "END.DATE","REVISED.GRADE","REVISED.DESIGNATION",
            "REPORTING.MANAGER.ID","REPORTING.MANAGER","LOCATION.ID","REVISED.DEPARTMENT")

carrier_history=rbind(carrier_history_after[,col_names],carrier_history_existing[,col_names])

save(carrier_history, file="carrier_history.Rda")

rm(carrier_history_after)
rm(carrier_history_existing)
rm(carrier_history_after_old)


# Qualification
coltypes=c(rep("text",5),"date","date")

qualification_existing=readxl::read_excel("KLPQ - Tied data - Existing 01-Apr-13V2.xlsx",
                                          sheet ="Qualification",col_types =coltypes )

col_names=c("EMPLOYEE.ID","NAME","QUALIFICATION","STATUS","INSTITUTE",
            "START.DATE","END.DATE")

colnames(qualification_existing)=col_names


qualification_after=readxl::read_excel("KLPQ - Tied Data - After 01-Apr-13.xlsx",
                                       sheet ="Qualification",col_types =coltypes)

colnames(qualification_after)=col_names

qualification=rbind(qualification_existing[,col_names],qualification_after[,col_names])

rm(qualification_existing)
rm(qualification_after)


# Previous Experience

experience_existing=readxl::read_excel("KLPQ - Tied data - Existing 01-Apr-13V2.xlsx",
                                       sheet ="Previous employer")

col_names=c("EMPLOYEE.ID","NAME","EMPLOYER.NAME","EMPLOYER.ADDRESS","INDUSTRY","START.DATE",
            "END.DATE","TENURE","SUPERVIOSR.NAME","ANNUAL.SALARY","LEAVING.REASON")

colnames(experience_existing)=col_names


experience_after=readxl::read_excel("KLPQ - Tied Data - After 01-Apr-13.xlsx",
                                    sheet ="Previous employer")

col_names=c("EMPLOYEE.ID","NAME","EMPLOYER.NAME","EMPLOYER.ADDRESS","INDUSTRY","START.DATE",
            "END.DATE","TENURE","ANNUAL.SALARY","LEAVING.REASON")

colnames(experience_after)=col_names

experience=rbind(experience_existing[,col_names],experience_after[,col_names])

rm(experience_existing)
rm(experience_after)

# Address

address_existing=readxl::read_excel("KLPQ - Tied data - Existing 01-Apr-13V2.xlsx",
                                       sheet ="Address")

address_after=readxl::read_excel("KLPQ - Tied Data - After 01-Apr-13.xlsx",
                                    sheet ="Address")

col_names=c("Employee Number","Full Name","Address Type","Address Line1","Address Line2",
            "Address Line3","Town Or City","State","Postal Code","Country" )

address=rbind(address_existing[,col_names],address_after[,col_names])

col_names=c("EMPLOYEE.ID","NAME","ADDRESS.TYPE","ADDRESS.LINE1","ADDRESS.LINE2",
            "ADDRESS.LINE3","CITY","STATE","POSTAL.CODE","COUNTRY" )

colnames(address)=col_names

rm(address_existing)
rm(address_after)


# Employee Master

emp_master=emp_dem[,c("EMPLOYEE.ID","NAME","DOJ","DOL","DOB","GENDER","HIRE.SOURCE",
                      "HIRE.TYPE","MARITAL.STATUS.JOINING")]

# assigning Qualification
emp_master$QUALIFICATION=""

for(i in 1:nrow(emp_master)){
  print(i)
  emp_id=emp_master$EMPLOYEE.ID[i]
  sub=qualification[qualification$EMPLOYEE.ID==emp_id,]
  if(nrow(sub)==1){
    qual=sub$QUALIFICATION[1]
    emp_master$QUALIFICATION[i]=qual
  }
  if(nrow(sub)>1){
    if(any(is.na(sub$START.DATE))){
      qual=sub$QUALIFICATION[1]
    }else{
      qual=sub$QUALIFICATION[sub$START.DATE==max(sub$START.DATE)]
    }
    qual=qual[length(qual)]
    emp_master$QUALIFICATION[i]=qual
  }
  
}



# previous experience

experience=experience[experience$EMPLOYER.NAME!="Fresher",]
experience=experience[experience$INDUSTRY!="Fresher",]
experience=experience[experience$START.DATE!=experience$END.DATE,]

experience$INDUSTRY1=ifelse(experience$INDUSTRY=="Insurance","Insurance","Non-Insurance")


# to calculate companys worked
company_worked=aggregate(experience$EMPLOYEE.ID,by=
                           list(EMPLOYEE.ID=experience$EMPLOYEE.ID),length)
names(company_worked)[2]="PREV.COMPANY.COUNT"

# to calcuclate average tenure 
experience$TENURE=round(as.numeric(experience$END.DATE-experience$START.DATE)/365.25,2)

avg_prev_tenure=aggregate(experience$TENURE,by=list(EMPLOYEE.ID=experience$EMPLOYEE.ID),mean)
names(avg_prev_tenure)[2]="AVG.PREV.EXP"

# to calcuclate total previous exp
total_prev_exp=aggregate(experience$TENURE,by=list(EMPLOYEE.ID=experience$EMPLOYEE.ID),sum)
names(total_prev_exp)[2]="TOTAL.PREV.EXP"

# to calculate % tenure in Insurance
prev_exp_insurance=experience[experience$INDUSTRY1=="Insurance",]
total_prev_exp_insurance=aggregate(prev_exp_insurance$TENURE,by=
                                  list(EMPLOYEE.ID=prev_exp_insurance$EMPLOYEE.ID),sum)
names(total_prev_exp_insurance)[2]="TOTAL.PREV.EXP.INSURANCE"

per_insurance=merge(total_prev_exp,total_prev_exp_insurance,all.x=TRUE)

per_insurance$TOTAL.PREV.EXP=as.numeric(per_insurance$TOTAL.PREV.EXP)
per_insurance$TOTAL.PREV.EXP.INSURANCE=as.numeric(per_insurance$TOTAL.PREV.EXP.INSURANCE)
per_insurance$TOTAL.PREV.EXP.INSURANCE[is.na(per_insurance$TOTAL.PREV.EXP.INSURANCE)]=0

per_insurance$INSURANCE.PERCENT=per_insurance$TOTAL.PREV.EXP.INSURANCE/per_insurance$TOTAL.PREV.EXP

workexp=merge(company_worked,avg_prev_tenure)
workexp=merge(workexp,total_prev_exp)
workexp=merge(workexp,per_insurance[,c("EMPLOYEE.ID","INSURANCE.PERCENT")])


emp_master=merge(emp_master,workexp,by.x="EMPLOYEE.ID",by.y = "EMPLOYEE.ID",all.x = TRUE)

# replaccing na with zero assuming fresher
emp_master$PREV.COMPANY.COUNT[is.na(emp_master$PREV.COMPANY.COUNT)]=0
emp_master$AVG.PREV.EXP[is.na(emp_master$AVG.PREV.EXP)]=0
emp_master$TOTAL.PREV.EXP[is.na(emp_master$TOTAL.PREV.EXP)]=0
emp_master$INSURANCE.PERCENT[is.na(emp_master$INSURANCE.PERCENT)]=0


#Qualification lookup

qualificaiton_lookup=read.csv("20160811_EDUCATIONLOOKUP.csv")

emp_master=merge(emp_master,qualificaiton_lookup,by="QUALIFICATION",all.x = TRUE)


emp_master$DOJ=as.Date(emp_master$DOJ,format="%Y-%m-%d")
emp_master$DOL=as.Date(emp_master$DOL,format="%Y-%m-%d")
emp_master$DOB=as.Date(emp_master$DOB,format="%Y-%m-%d")

carrier_history$START.DATE=as.Date(carrier_history$START.DATE,format="%Y-%m-%d")
carrier_history$END.DATE=as.Date(carrier_history$END.DATE,format="%Y-%m-%d")

emp_master$TOTAL_TENURE=as.numeric(emp_master$DOL-emp_master$DOJ)

write.csv(emp_master,paste(format(Sys.time(), "%Y%m%d%H%M%S"),
                                 "_emp_master.csv",sep = ""),row.names = FALSE)


# reporting Master

quarter_dates=c("2013-03-31","2013-06-30","2013-09-30","2013-12-31","2014-03-31",
                "2014-06-30","2014-09-30","2014-12-31","2015-03-31",
                "2015-06-30","2015-09-30","2015-12-31","2016-03-31",
                "2016-06-30")

quarter_dates=as.Date(quarter_dates,format="%Y-%m-%d")

reporting_master=data.frame()


left_quarter=0
left_list=c(0)

for (i in 1:nrow(emp_master)){
  print(i)
  EMPLOYEE.ID=emp_master$EMPLOYEE.ID[i]
  
  DOJ=emp_master$DOJ[emp_master$EMPLOYEE.ID==EMPLOYEE.ID]
  DOL=emp_master$DOL[emp_master$EMPLOYEE.ID==EMPLOYEE.ID]
  
  sub=carrier_history[carrier_history$EMPLOYEE.ID==EMPLOYEE.ID,]
  if(nrow(sub)==0){
    # print(EMPLOYEE.ID)
    # no_history=no_history+1
    next
  }
  #old Logic
  emp_dates=quarter_dates[quarter_dates>=DOJ]
  if(!is.na(DOL)){
    emp_dates=emp_dates[emp_dates<=DOL]
  }
  
  if(length(emp_dates)==0){
    left_quarter=left_quarter+1
    left_list=append(left_list,EMPLOYEE.ID)
    next
  }
  for (j in 1:length(emp_dates)){
    QUARTER.ENDDATE=emp_dates[j]
    
    sub1=sub[sub$END.DATE>=QUARTER.ENDDATE & sub$START.DATE<=QUARTER.ENDDATE,]
    if(nrow(sub1)>1){
      print("Error")
    }
    GRADE=sub1$REVISED.GRADE[1]
    DESIGNATION=sub1$REVISED.DESIGNATION[1]
    REPORTING.MANAGER.ID=sub1$REPORTING.MANAGER.ID[1]
    REPORTING.MANAGER=sub1$REPORTING.MANAGER[1]
    LOCATION.ID=sub1$LOCATION.ID[1]
    DEPARTMENT=sub1$REVISED.DEPARTMENT[1]
    reporting_master=rbind(reporting_master,
                           data.frame(EMPLOYEE.ID,QUARTER.ENDDATE,GRADE,DESIGNATION,
                                      REPORTING.MANAGER.ID,REPORTING.MANAGER,LOCATION.ID,
                                      DEPARTMENT))
  }
  
}


write.csv(reporting_master,paste(format(Sys.time(), "%Y%m%d%H%M%S"),
                                  "_reporting_master.csv",sep = ""),row.names = FALSE)

