## wockhardt social network analysis
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
load("carrier_history.Rda")


#conversion1= conversion[conversion$EMPLOYEE.ID==40180,]

#calculate conversion data
conversion= conversion[,c("EMPLOYEE.ID","ISSUE.DATE","PREMIUM.CHARGED")]
conversion = merge(conversion, emp_master[,c("EMPLOYEE.ID","DOJ","DOL")], all.x=TRUE)
conversion$ISSUE.DATE= as.POSIXlt(conversion$ISSUE.DATE,"%d-%b-%y",tz="")
conversion$DOJ= as.POSIXlt(conversion$DOJ,"%d-%m-%Y",tz="")
conversion$DOL= as.POSIXlt(conversion$DOL,"%d-%m-%Y",tz="")

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


#emp= read.csv("20161012emp.csv")
#emp= emp_master[as.POSIXlt(emp_master$DOJ,"%d/%m/%Y",tz="")>as.POSIXlt("31/03/2013","%d/%m/%Y",tz=""), ]

career_history= carrier_history
career_history= career_history[as.POSIXlt(career_history$CAREER.CHANGE.DATE ,"%d/%m/%Y",tz="")>as.POSIXlt("31/03/2013","%d/%m/%Y",tz=""), ]
career_history= career_history[career_history$CHANGE.REASON=="Joining",]
#sort(table(unlist(career_history$EMPLOYEE.ID)))
career_history$EMPLOYEE.ID= as.character(career_history$EMPLOYEE.ID)
career_history= career_history[difftime(as.POSIXlt(career_history$CAREER.CHANGE.DATE ,"%d/%m/%Y",tz=""), 
                                        as.POSIXlt(career_history$DOJ ,"%d/%m/%Y",tz=""), "",units = c("days"))<180,]

career_history$MANAGER.DURATION= difftime(career_history$END.DATE,career_history$START.DATE,"", units=c("days"))
career_history= as.data.frame(career_history %>% group_by(EMPLOYEE.ID) %>% top_n(n=1))

emp= merge(emp, as.data.frame(career_history[,c("EMPLOYEE.ID","REPORTING.MANAGER.ID","LOCATION.ID")]), by="EMPLOYEE.ID",all.x= TRUE)

#emp= read.csv("20161013.emp.6months.csv")

emp= emp[(as.POSIXlt(emp$DOJ,"%d/%m/%Y",tz="")< as.POSIXlt("30/01/2016" ,"%d/%m/%Y",tz="")),]
emp=emp[emp$ATTRITION==0|emp$TOTAL_TENURE>180,]

nrow(unique(emp))
write.csv(emp,"20161016.emp.6m.csv")
emp=read.csv("20161016.emp.6m.csv")

##sales funnel data- name collection
load("Rec_Name_Col2.RDA")
Rec_Name_Col= Rec_Name_Col[,c("EMPLOYEE.ID","RECRUIT.INTERNAL.ID","LEAD.CREATION.DATE","F2FCALLS.COUNT","PHONECALLS.COUNT",
                              "JOINTCALLS.COUNT")]
Rec_Name_Col= merge(emp[,c("EMPLOYEE.ID","DOJ","PERF.GRADE")], Rec_Name_Col, by= "EMPLOYEE.ID", all.y=FALSE)
Rec_Name_Col$DOJ= as.POSIXlt(Rec_Name_Col$DOJ,"%d/%m/%Y",tz="")
#Rec_Name_Col$LEAD.CREATION.DATE= as.POSIXlt(Rec_Name_Col$LEAD.CREATION.DATE,"%Y/%m/%d",tz="") 
Rec_Name_Col$DATE.DIFF.DOJ= difftime(Rec_Name_Col$LEAD.CREATION.DATE,Rec_Name_Col$DOJ,"", units=c("days"))
Rec_Name_Col$NO.OF.MONTH.DOJ= difftime(Rec_Name_Col$LEAD.CREATION.DATE,Rec_Name_Col$DOJ,"", units=c("days"))/30
Rec_Name_Col$NO.OF.MONTH.DOJ=ceiling(Rec_Name_Col$NO.OF.MONTH.DOJ)
Rec_Name_Col$F2FCALLS.COUNT=as.numeric(as.character(Rec_Name_Col$F2FCALLS.COUNT))
Rec_Name_Col$JOINTCALLS.COUNT=as.numeric(as.character(Rec_Name_Col$JOINTCALLS.COUNT))
Rec_Name_Col$PHONECALLS.COUNT=as.numeric(as.character(Rec_Name_Col$PHONECALLS.COUNT))

Rec_Name_Col= Rec_Name_Col[Rec_Name_Col$NO.OF.MONTH.DOJ<=6,]
Rec_Name_Col$COUNT=1

Rec_Name_Col[is.na(Rec_Name_Col)] <- 0

Rec= aggregate(cbind(F2FCALLS.COUNT,PHONECALLS.COUNT,JOINTCALLS.COUNT,COUNT)~EMPLOYEE.ID+NO.OF.MONTH.DOJ,
               data= Rec_Name_Col,sum, na.rm=TRUE)

colnames(Rec)[-(1:2)] <- paste("REC", colnames(Rec)[-(1:2)], sep = ".")

##nat scores

load("nat_bop.RDA")

NAT= nat_bop[,c("EMPLOYEE.ID","NAT.TEST.DATE","NAT.PERCENT","STATUS")]
NAT$STATUS = toupper(NAT[,c("STATUS")])
NAT= merge(emp[,c("EMPLOYEE.ID","DOJ")], NAT, by= "EMPLOYEE.ID", all.y=FALSE)
NAT$DOJ= as.POSIXlt(NAT$DOJ,"%d/%m/%Y",tz="")
#NAT$NAT.TEST.DATE= as.POSIXlt(NAT$NAT.TEST.DATE,"%Y/%m/%d",tz="") 
NAT$DATE.DIFF.DOJ= difftime(NAT$NAT.TEST.DATE,NAT$DOJ,"", units=c("days"))
NAT$NO.OF.MONTH.DOJ= difftime(NAT$NAT.TEST.DATE,NAT$DOJ,"", units=c("days"))/30
NAT$NO.OF.MONTH.DOJ=ceiling(NAT$NO.OF.MONTH.DOJ)
NAT= NAT[NAT$NO.OF.MONTH.DOJ<=6,]
NAT$STATUS[is.na(NAT$STATUS)]="NOA"
NAT$COUNT=1
NAT= dcast(NAT, formula=EMPLOYEE.ID+NO.OF.MONTH.DOJ~STATUS,value.var ="COUNT",sum )
NAT$TOTAL= rowSums(NAT[,-(1:2)])
colnames(NAT)[-(1:2)] <- paste("NAT", colnames(NAT)[-(1:2)], sep = ".")

#BOP
BOP=nat_bop[,c("EMPLOYEE.ID","BOP.DATE","BOP.ATTENDANCE")]
BOP[BOP$BOP.ATTENDANCE=="Yes","BOP.ATTENDANCE"]="Y"
BOP[BOP$BOP.ATTENDANCE=="No","BOP.ATTENDANCE"]="N"
BOP= merge(emp[,c("EMPLOYEE.ID","DOJ")], BOP, by= "EMPLOYEE.ID", all.y=FALSE)
BOP$DOJ= as.POSIXlt(BOP$DOJ,"%d/%m/%Y",tz="")
#BOP$BOP.TEST.DATE= as.POSIXlt(BOP$BOP.DATE,"%Y/%m/%d",tz="") 
BOP$DATE.DIFF.DOJ= difftime(BOP$BOP.DATE,BOP$DOJ,"", units=c("days"))
BOP$NO.OF.MONTH.DOJ= difftime(BOP$BOP.DATE,BOP$DOJ,"", units=c("days"))/30
BOP$NO.OF.MONTH.DOJ=ceiling(BOP$NO.OF.MONTH.DOJ)
BOP= BOP[BOP$NO.OF.MONTH.DOJ<=6,]

BOP$COUNT= 1
BOP= dcast(BOP, formula=EMPLOYEE.ID+NO.OF.MONTH.DOJ~BOP.ATTENDANCE,value.var ="COUNT",sum )
BOP$TOTAL= rowSums(BOP[,-(1:2)])
colnames(BOP)[-(1:2)] <- paste("BOP", colnames(BOP)[-(1:2)], sep = ".")

#PHF
load("PHF.RDA")
PHF= PHF[,c("EMPLOYEE.ID", "PDR.DATE")]
PHF= merge(emp[,c("EMPLOYEE.ID","DOJ")], PHF, by= "EMPLOYEE.ID", all.y=FALSE)
PHF$DOJ= as.POSIXlt(PHF$DOJ,"%d/%m/%Y",tz="")
#PHF$PHF.TEST.DATE= as.POSIXlt(PHF$PHF.DATE,"%Y/%m/%d",tz="") 
PHF$DATE.DIFF.DOJ= difftime(PHF$PDR.DATE,PHF$DOJ,"", units=c("days"))
PHF$NO.OF.MONTH.DOJ= difftime(PHF$PDR.DATE,PHF$DOJ,"", units=c("days"))/30
PHF$NO.OF.MONTH.DOJ=ceiling(PHF$NO.OF.MONTH.DOJ)
PHF= PHF[PHF$NO.OF.MONTH.DOJ<=6,]
#PHF= PHF[PHF$DATE.DIFF.DOJ<=180,]
PHF$COUNT= 1
PHF= aggregate(PHF[,c("COUNT")],by=list(PHF$EMPLOYEE.ID,PHF$NO.OF.MONTH.DOJ),FUN=sum)
colnames(PHF)= c("EMPLOYEE.ID","NO.OF.MONTH.DOJ","PHF.COUNT")

##license collection
load("E:/Work/Kotak/Project/license.Rda")
LICENSE= license
LICENSE= LICENSE[,c("EMPLOYEE.ID", "LICENSE.DATE")]
LICENSE= merge(emp[,c("EMPLOYEE.ID","DOJ")], LICENSE, by= "EMPLOYEE.ID", all.y=FALSE)
LICENSE$DOJ= as.POSIXlt(LICENSE$DOJ,"%d/%m/%Y",tz="")
#LICENSE$LICENSE.TEST.DATE= as.POSIXlt(LICENSE$LICENSE.DATE,"%Y/%m/%d",tz="") 
LICENSE$DATE.DIFF.DOJ= difftime(LICENSE$LICENSE.DATE,LICENSE$DOJ,"", units=c("days"))
LICENSE$NO.OF.MONTH.DOJ= difftime(LICENSE$LICENSE.DATE,LICENSE$DOJ,"", units=c("days"))/30
LICENSE$NO.OF.MONTH.DOJ=ceiling(LICENSE$NO.OF.MONTH.DOJ)
LICENSE= LICENSE[LICENSE$NO.OF.MONTH.DOJ<=6,]

#LICENSE= LICENSE[LICENSE$DATE.DIFF.DOJ<=180,]
LICENSE$COUNT= 1
LICENSE= aggregate(LICENSE[,c("COUNT")],by=list(LICENSE$EMPLOYEE.ID, LICENSE$NO.OF.MONTH.DOJ),FUN=sum)
colnames(LICENSE)= c("EMPLOYEE.ID","NO.OF.MONTH.DOJ","LICENSE.COUNT")


emp_master1= Reduce(function(x, y) merge(x, y, all=TRUE), list(Rec, NAT,BOP,PHF,LICENSE))
emp_master1=emp_master1[!is.na(emp_master1$EMPLOYEE.ID),]
emp_master1[is.na(emp_master1)]=""
emp_master1=merge(emp_master1,emp[,c("EMPLOYEE.ID","PERF.GRADE")], by="EMPLOYEE.ID")


emp_master1=emp_master1[emp_master1$REC_COUNT!="",]
write.csv(emp_master1,"20161016emp_master.csv")

emp=read.csv("20161019emp.csv")
loc.perf= emp[,c("LOCATION.ID","PREMIUM.CHARGED")]
loc.perf= aggregate(loc.perf, by=list(loc.perf$LOCATION.ID), FUN=mean)
x=quantile(loc.perf$PREMIUM.CHARGED,.5)
loc.perf$LOC.GRADE="L"
loc.perf$LOC.GRADE[loc.perf$PREMIUM.CHARGED>as.numeric(x)]="H"
emp= merge(emp[,-46],loc.perf[,c("LOCATION.ID","LOC.GRADE")], by= "LOCATION.ID")


x=unique(emp$LOC.GRADE)
df= data.frame()

for(i in 1:length(x)){
  emp2=emp[emp$LOC.GRADE==x[i],]
  y1=quantile(emp2$PREMIUM.CHARGED,.25)
  y2=quantile(emp2$PREMIUM.CHARGED,.75)
  emp2$PERF.GRADE="M"
  emp2$PERF.GRADE[emp2$PREMIUM.CHARGED<as.numeric(y1)]="L"
  emp2$PERF.GRADE[emp2$PREMIUM.CHARGED>as.numeric(y2)]="H"
  df=rbind(df,emp2)
}

write.csv(df,"20161019empv2.csv")

emp=read.csv("20161019empv2.csv")
emp$REC_JOINTCALLS.COUNT.AVERAGE=emp

emp= merge(emp,emp1[,c("EMPLOYEE.ID","PREMIUM.COUNT")], by="EMPLOYEE.ID", all.y=FALSE)
Rec1= merge(Rec_Name_Col ,emp[,c("EMPLOYEE.ID","PERF.GRADE")], by="EMPLOYEE.ID",all.x=FALSE)




##Prediction
emp.pred= emp[,c("LOCATION.ID","QUALIFICATION","GENDER","HIRE.SOURCE","HIRE.TYPE"
                 ,"MARITAL.STATUS.JOINING","PREV.COMPANY.COUNT","AVG.PREV.EXP","TOTAL.PREV.EXP"
                 ,"INSURANCE.PERCENT","SEGMENT","CATEGORY","SUB.CATEGORY","REC_F2FCALLS.COUNT","REC_PHONECALLS.COUNT"  
                 ,"REC_JOINTCALLS.COUNT","REC_COUNT","NAT_TOTAL","BOP.TOTAL","PHF.COUNT","LICENSE.COUNT"         
                 ,"PERF.GRADE")]



##performance curve

conversion$NO.OF.MONTHS.DOJ.BUCKET= ceiling(conversion$NO.OF.DAYS.DOJ/30)
conversion$NO.OF.MONTHS.DOL.BUCKET= ceiling(conversion$NO.OF.DAYS.DOL/30)

conversion1= conversion[conversion$DOJ>as.POSIXlt("2013/03/31","%Y/%m/%d",tz=""),  ]
conversion1= conversion1[conversion1$DOJ<as.POSIXlt("2016/06/30","%Y/%m/%d",tz=""),  ]
emp_list=  emp_master[as.POSIXlt(emp_master$DOJ,"%d/%m/%Y",tz="")<as.POSIXlt("2015/12/31","%Y/%m/%d",tz=""), c("EMPLOYEE.ID")]
conversion1= conversion1[conversion1$EMPLOYEE.ID%in%emp_list,]
performance.start= aggregate(conversion1[,c("EMPLOYEE.ID","NO.OF.MONTHS.DOJ.BUCKET","PREMIUM.CHARGED")],
                             by=list(conversion1$EMPLOYEE.ID, conversion1$NO.OF.MONTHS.DOJ.BUCKET), FUN=sum, na.rm=TRUE)
performance.start= performance.start[,c(1,2,5)]
colnames(performance.start)= c("EMPLOYEE.ID","NO.OF.DAYS.DOJ.BUCKET","PREMIUM.CHARGED")
performance.start= merge(performance.start,emp[,c("EMPLOYEE.ID","DOJ","PERF.GRADE")], by="EMPLOYEE.ID")
write.csv(performance.start,"20161020.performance.start.csv")


performance.end= aggregate(conversion[,c("EMPLOYEE.ID","NO.OF.DAYS.DOL.BUCKET","PREMIUM.CHARGED")],
                           by=list(conversion$EMPLOYEE.ID, conversion$NO.OF.DAYS.DOL.BUCKET), FUN=sum, na.rm=TRUE)
performance.end= performance.end[,c(1,2,5)]
colnames(performance.end)= c("EMPLOYEE.ID","NO.OF.DAYS.DOL.BUCKET","PREMIUM.CHARGED")

#write.csv(performance.end,"20161012.performance.end.csv")


##sales funnel per month

sales.funnel= read.csv("20161019.Sales.Funnel.month.csv")
sales.funnel[is.na(sales.funnel)]=0

emp.list= unique(sales.funnel$EMPLOYEE.ID)
months= c(1,2,3,4,5,6)

emp.list= merge(emp.list,months)
colnames(emp.list)=c("EMPLOYEE.ID","NO.OF.MONTH.DOJ")

sales.funnel2= merge(emp.list,sales.funnel, all=TRUE)
sales.funnel2= sales.funnel2[sales.funnel2$NO.OF.MONTH.DOJ>=1,]
sales.funnel2= sales.funnel2[,-18]
sales.funnel2[is.na(sales.funnel2)]=0
sales.funnel3= merge(sales.funnel2,emp[,c("EMPLOYEE.ID","PERF.GRADE")])

write.csv(sales.funnel3, "20161020.Sales.funnel.month.csv")


#longevity analysis
load("conversion.RDA")
conversion=conversion[!is.na(conversion$EMPLOYEE.ID),]

conversion= conversion[,c("EMPLOYEE.ID","ISSUE.DATE","PREMIUM.CHARGED")]
conversion = merge(conversion, emp_master[,c("EMPLOYEE.ID","DOJ","DOL")], all.x=TRUE)
conversion$ISSUE.DATE= as.POSIXlt(conversion$ISSUE.DATE,"%d-%b-%y",tz="")
conversion$DOJ= as.POSIXlt(conversion$DOJ,"%d/%m/%Y",tz="")
conversion$DOL= as.POSIXlt(conversion$DOL,"%d/%m/%Y",tz="")

conversion$NO.OF.DAYS.DOJ= difftime(conversion$ISSUE.DATE, conversion$DOJ,units = c("days"))
conversion$PREMIUM.CHARGED= as.character(conversion$PREMIUM.CHARGED)
conversion$PREMIUM.CHARGED= as.numeric(conversion$PREMIUM.CHARGED)
conversion$NO.OF.DAYS.DOJ= as.numeric(conversion$NO.OF.DAYS.DOJ,units="days")
conversion= conversion[conversion$DOJ<=as.POSIXlt("31/12/2015","%d/%m/%Y",tz=""),]
conversion= conversion[conversion$DOJ>=as.POSIXlt("01/04/2013","%d/%m/%Y",tz=""),]

conversion$NO.OF.MONTH.DOJ= ceiling(as.numeric(conversion$NO.OF.DAYS.DOJ,units="days")/30)

conversion$PREMIUM.CHARGED= as.numeric(as.character(conversion$PREMIUM.CHARGED))
conversion$PREMIUM.COUNT=1

performance1= melt(conversion,id= c("EMPLOYEE.ID","NO.OF.MONTH.DOJ"),measure=c("PREMIUM.CHARGED","PREMIUM.COUNT"))
performance=cast(performance1,EMPLOYEE.ID +NO.OF.MONTH.DOJ  ~ variable, sum)
performance= merge(performance, emp_master[,c("EMPLOYEE.ID","DOJ","DOL","TOTAL_TENURE")],by="EMPLOYEE.ID")
#performance[is.na(performance$DOL),"DOL"]=as.POSIXlt("31/06/2016","%d/%m/%Y",tz="")
#performance$TOTAL_TENURE[is.na(performance$TOTAL_TENURE)]=difftime(as.POSIXlt("31/06/2016","%d/%m/%Y",tz=""),performance$DOJ[is.na(performance$TOTAL_TENURE)],"", units=c("days"))
performance$ATTRTION="Y"
performance$ATTRTION[performance$TOTAL_TENURE<180]="N"
write.csv(performance,"20161020.longevity.csv")
long= read.csv("20161020.longevity.csv")
long= long[long$NO.OF.MONTH.DOJ<7,]
emp.list= long[,c("EMPLOYEE.ID","NO.OF.MONTH.DOJ","DOJ","DOL","TOTAL_TENURE","ATTRTION")]

unique.emp=(unique(emp.list$EMPLOYEE.ID))
df= data.frame()
for(i in 1:length(unique.emp)){
  emp2= emp.list[emp.list$EMPLOYEE.ID==unique.emp[i],]
  month= c(1:max(emp2$NO.OF.MONTH.DOJ))
  emp3= merge(unique(emp2$EMPLOYEE.ID),month)
  colnames(emp3)= c("EMPLOYEE.ID","NO.OF.MONTH.DOJ")
  df= rbind(df,emp3)
}
df= merge(df, emp.list[,c("EMPLOYEE.ID","DOJ","DOL","TOTAL_TENURE","ATTRTION")])
df= unique(df)

long2= merge(df,long, all=TRUE)
long2[is.na(long2)]=0

write.csv(long2,"20161020.longevity.v2.csv")



#performance start
perf= read.csv("20161020.performance.start.csv")
perf[is.na(perf)]=0

emp.list= unique(perf$EMPLOYEE.ID)
months= c(1,2,3,4,5,6)

emp.list= merge(emp.list,months)
colnames(emp.list)=c("EMPLOYEE.ID","NO.OF.DAYS.DOJ.BUCKET")

perf2= merge(emp.list,perf, all=TRUE)
perf2= perf2[perf2$NO.OF.DAYS.DOJ.BUCKET>=1,]
perf2[is.na(perf2)]=0
perf3= merge(perf2,emp_master[,c("EMPLOYEE.ID","DOJ")])

write.csv(perf3, "20161020.performance.start.csv")

#KLPQ analysis

emp = read.csv("20161019empv2.csv")
klpq= read.csv("20161024KLPQ.csv")
klpq1= klpq[klpq$EMPLOYEE.ID!=0,]
klpq1=klpq1[klpq1$EMPLOYEE.ID!=0,]
klpq1= unique(klpq[,-1])
klpq2 = subset(klpq1, !duplicated(klpq1[,1])) 


klpq_master= merge(emp,klpq2[,c(1,163:203)], by="EMPLOYEE.ID")
length(unique((klpq_master$EMPLOYEE.ID)))
klpq_master$ego_drive_score=cut(klpq_master$ego_drive,breaks = c(0,50,75,100),labels=c(0,1,2)) 
klpq_master$real_ideal_score=cut(klpq_master$real_ideal_match_percent ,breaks = c(0,53,90,100),labels=c(0,1,2)) 
klpq_master$ego_drive_score= as.numeric(as.character(klpq_master$ego_drive_score))
klpq_master$real_ideal_score= as.numeric(as.character(klpq_master$real_ideal_score))


klpq_master$good_impression_score= rowSums(klpq_master[,c("ego_drive_score","real_ideal_score")])

write.csv(klpq_master,"20161026klpq_masterv3.csv")


#sales joint call analysis
emp = read.csv("20161019empv2.csv")
load("E:/Work/Kotak/Project/joint_calls2.Rda")
JOINTCALL= joint_calls
#JOINTCALL= JOINTCALL[,c("EMPLOYEE.ID", "FIRST.MEETING.DATE")]
JOINTCALL= merge(emp[,c("EMPLOYEE.ID","DOJ","PERF.GRADE")], JOINTCALL, by= "EMPLOYEE.ID", all.y=FALSE)
JOINTCALL$DOJ= as.POSIXlt(JOINTCALL$DOJ,"%d/%m/%Y",tz="")
#JOINTCALL$JOINTCALL.TEST.DATE= as.POSIXlt(JOINTCALL$JOINTCALL.DATE,"%Y/%m/%d",tz="") 
JOINTCALL$DATE.DIFF.DOJ= difftime(JOINTCALL$FIRST.MEETING.DATE,JOINTCALL$DOJ,"", units=c("days"))
JOINTCALL$NO.OF.MONTH.DOJ= difftime(JOINTCALL$FIRST.MEETING.DATE,JOINTCALL$DOJ,"", units=c("days"))/30
JOINTCALL$NO.OF.MONTH.DOJ=ceiling(JOINTCALL$NO.OF.MONTH.DOJ)
JOINTCALL= JOINTCALL[JOINTCALL$NO.OF.MONTH.DOJ<=6,]
JOINTCALL$F2FCALLS.COUNT[JOINTCALL$F2FCALLS.COUNT=="Not Available"]=0
JOINTCALL$PHONECALLS.COUNT[JOINTCALL$PHONECALLS.COUNT=="Not Available"]=0
JOINTCALL$F2F.PLANNER[JOINTCALL$F2F.PLANNER=="Not Available"]=0
JOINTCALL$F2FCALLS.COUNT=as.numeric(JOINTCALL$F2FCALLS.COUNT)
JOINTCALL$PHONECALLS.COUNT=as.numeric(JOINTCALL$PHONECALLS.COUNT)
JOINTCALL$F2F.PLANNER=as.numeric(JOINTCALL$F2F.PLANNER)

JOINTCALL$COUNT=1

JOINTCALL2= aggregate(cbind(F2FCALLS.COUNT,PHONECALLS.COUNT,F2F.PLANNER,COUNT)~EMPLOYEE.ID,
               data= JOINTCALL,sum, na.rm=TRUE)

colnames(JOINTCALL2)[-(1)] <- paste("JOINTCALL", colnames(JOINTCALL2)[-(1)], sep = ".")



write.csv(JOINTCALL,"20161025Sales.Pipeline.csv")


#agent analysis
load("E:/Work/Kotak/Project/Agent1.RDa")
agent= agent[!is.na(agent$EMPLOYEE.ID),]
profile= read.csv("20161025_PRIMARYPROFESSION_V1.csv")
agent=merge(agent,profile, by="PRIMARYPROFESSION")
colnames(agent)= paste("AGENT",colnames(agent), sep="_")
agent$EMPLOYEE.ID= agent$AGENT_EMPLOYEE.ID
emp = read.csv("20161025empv2.csv")
agent2=merge(agent,emp[,c("EMPLOYEE.ID","DOJ")])
agent2$HIRE.DAYS= difftime(agent2$AGENT_SPONSERSHIPDATE,as.POSIXlt(agent2$DOJ,"%d/%m/%Y",tz="") ,"", units=c("days"))
agent2= agent2[agent2$HIRE.DAYS<=180,]
agent2$AGENT_AGE= ceiling(difftime(agent2$AGENT_SPONSERSHIPDATE,as.POSIXlt(agent2$AGENT_DATEOFBIRTH,"%d/%m/%Y",tz="") ,"", units=c("days"))/365)
agent2$AGENT_COUNT=1

agent2_count= aggregate(agent2[,c("AGENT_COUNT")],by=list(agent2$EMPLOYEE.ID),FUN=sum)
colnames(agent2_count)=c("EMPLOYEE.ID","AGENT_COUNT")

agent2_age= aggregate(agent2[,c("AGENT_AGE")],by=list(agent2$EMPLOYEE.ID),FUN=mean)
colnames(agent2_age)=c("EMPLOYEE.ID","AGENT_AGE")

#agent sales analysis
load("Agent1.RDa")
agent$NEW.PROFESSION[agent$NEW.PROFESSION=="RETIRED-ARMY"]="RETIRED"
colnames(agent)[-(23)] <- paste("AGENT", colnames(agent)[-(23)], sep = ".")
agent= agent[,c("AGENT.LA.CODE","AGENT.SPONSERSHIPDATE","AGENT.EDUCATIONALQUALIFICATION","AGENT.OTHERQUALIFICATION"            
                ,"AGENT.DATEOFBIRTH","AGENT.SEX","AGENT.MARITALSTATUS","AGENT.EMPLOYEE.ID","AGENT.NEW.PROFESSION")]
agent= agent[agent$AGENT.SPONSERSHIPDATE>as.POSIXlt("31-03-2013","%d-%m-%Y",tz="")
             & agent$AGENT.SPONSERSHIPDATE<as.POSIXlt("31-01-2016","%d-%m-%Y",tz=""), ]

agent.list= unique(agent$AGENT.LA.CODE)
months= c(1,2,3,4,5,6)
agent.list= merge(agent.list,months)
colnames(agent.list)=c("AGENT.LA.CODE","NO.OF.MONTH.DOJ")

load("conversion.Rda")

conversion2= merge(conversion, agent[,c("AGENT.LA.CODE","AGENT.SPONSERSHIPDATE")], by="AGENT.LA.CODE")
conversion2$NO.OF.MONTH.DOJ= as.numeric(round(difftime(as.POSIXlt(conversion2$ISSUE.DATE,"%d-%b-%y",tz=""), conversion2$AGENT.SPONSERSHIPDATE,units = c("days"))/30))
conversion2=conversion2[,c("AGENT.LA.CODE","NO.OF.MONTH.DOJ","RDM.LA.CODE","ISSUE.DATE","PRODUCT.CODE","PRODUCT.CATEGORY"
,"SUM.ASSURED.NEW.BUSINESS","PREMIUM.CHARGED","ZONE","EMPLOYEE.ID")]
conversion2= conversion2[conversion2$NO.OF.MONTH.DOJ<=6,]
conversion2= merge(agent.list,conversion2,by=c("AGENT.LA.CODE","NO.OF.MONTH.DOJ"), all=TRUE)

###need to work
conversion2$PREMIUM.CHARGED= as.numeric(conversion2$PREMIUM.CHARGED)
conversion2$NAME.COUNT=1
conversion3=conversion2[,c("AGENT.LA.CODE","NO.OF.MONTH.DOJ","PREMIUM.CHARGED","NAME.COUNT")]
conversion3$PREMIUM.CHARGED[is.na(conversion3$PREMIUM.CHARGED)]=0
conversion3= aggregate(cbind(PREMIUM.CHARGED,NAME.COUNT)~AGENT.LA.CODE,data= conversion3, FUN=sum, na.rm=FALSE)



conversion3$PREMIUM.CHARGED[is.na(conversion3$PREMIUM.CHARGED)]=0
conversion3$NAME.COUNT[is.na(conversion3$NAME.COUNT)]=0
conversion3= merge(conversion3,agent[,c("AGENT.LA.CODE","AGENT.DATEOFBIRTH","AGENT.SEX","AGENT.MARITALSTATUS","AGENT.EDUCATIONALQUALIFICATION","AGENT.EMPLOYEE.ID","AGENT.NEW.PROFESSION")]
                   ,by="AGENT.LA.CODE",all.x = TRUE)

write.csv(conversion3, "20161102.agent.sales.analysis.csv")

columns=c("AGENT.LA.CODE","RDM.LA.CODE","ISSUE.DATE","PRODUCT.CODE","PRODUCT.CATEGORY"
,"SUM.ASSURED.NEW.BUSINESS","PREMIUM.CHARGED","ZONE","EMPLOYEE.ID","AGENT.PRIMARYPROFESSION","AGENT.SPONSERSHIPDATE"             
,"AGENT.CURRENTSTATE","AGENT.PERMANENTSTATE","AGENT.PERMANENTPINCODE","AGENT.BASICQUALIFICATION"            
,"AGENT.BOARDNAMEFORBASICQUALIFICATION", "AGENT.YEAROFPASSINGFORBASICQUAL","AGENT.EDUCATIONALQUALIFICATION","AGENT.OTHERQUALIFICATION"            
,"AGENT.DATEOFBIRTH","AGENT.SEX","AGENT.MARITALSTATUS","AGENT.FAMILYINCOME","AGENT.EMPLOYEE.ID","AGENT.NEW.PROFESSION","NO.OF.MONTH.DOJ")

conversion2= conversion2[,columns]



write.csv(conversion2, "20161028.Agent.Sale.csv")

#agent analysis with an alternate view of agent DOL
agent.list= aggregate(NO.OF.MONTH.DOJ ~ AGENT.LA.CODE, conversion2, max)
colnames(agent.list)=c("AGENT.LA.CODE","NO.OF.MONTH.DOJ")
agent.list$NO.OF.MONTH.DOJ= as.numeric(agent.list$NO.OF.MONTH.DOJ)
sales.analysis= data.frame()
for(i in 1:max(agent.list$NO.OF.MONTH.DOJ)){
  agent.list2= agent.list[agent.list$NO.OF.MONTH.DOJ==i,]
  agent.list3= merge(agent.list2$AGENT.LA.CODE,1:i)
  colnames(agent.list3)=c("AGENT.LA.CODE","NO.OF.MONTH.DOJ")
  sales.analysis=rbind(sales.analysis,agent.list3)
}






emp_master1= Reduce(function(x, y) merge(x, y, all=TRUE), list(emp, agent2_count,agent2_age,JOINTCALL2))

emp_master1[is.na(emp_master1[,c("AGENT_COUNT","JOINTCALL.F2FCALLS.COUNT","JOINTCALL.PHONECALLS.COUNT"
                     ,"JOINTCALL.F2F.PLANNER","JOINTCALL.COUNT")])]=0            
  
  
emp_master1$AGENT_COUNT[is.na(emp_master1[,c("AGENT_COUNT")])]=0      
emp_master1$JOINTCALL.F2FCALLS.COUNT[is.na(emp_master1[,c("JOINTCALL.F2FCALLS.COUNT")])]=0            
emp_master1$JOINTCALL.PHONECALLS.COUNT[is.na(emp_master1[,c("JOINTCALL.PHONECALLS.COUNT")])]=0            
emp_master1$JOINTCALL.F2F.PLANNER[is.na(emp_master1[,c("JOINTCALL.F2F.PLANNER")])]=0            
emp_master1$JOINTCALL.COUNT[is.na(emp_master1[,c("JOINTCALL.COUNT")])]=0  

write.csv(emp_master1,"20161025_emp.csv")

##final sales funnel

name.collect= read.csv("20161025Sales.Pipeline.csv")
name.collect$NAME.COUNT= 1

name.collect= aggregate(name.collect[,c("EMPLOYEE.ID","NO.OF.MONTH.DOJ","NAME.COUNT")],
         by=list(name.collect$EMPLOYEE.ID,name.collect$NO.OF.MONTH.DOJ), FUN=sum, na.rm=TRUE)

name.collect=name.collect[,c(1,2,5)]
colnames(name.collect)=c("EMPLOYEE.ID","NO.OF.MONTH.DOJ","NAME.COUNT")
emp.list= unique(name.collect$EMPLOYEE.ID)
months= c(1,2,3,4,5,6)
emp.list= merge(emp.list,months)
colnames(emp.list)=c("EMPLOYEE.ID","NO.OF.MONTH.DOJ")
name.collect= merge(emp.list,name.collect, all=TRUE)
name.collect$NAME.COUNT[is.na(name.collect$NAME.COUNT)]=0
name.collect= merge(name.collect, emp[,c("EMPLOYEE.ID","PERF.GRADE")])


sales= read.csv("20161025.conversion.csv")
sales$SALE.COUNT=1
sales= aggregate(sales[,c("EMPLOYEE.ID","NO.OF.MONTH.DOJ","SALE.COUNT","PREMIUM.CHARGED")],
                        by=list(sales$EMPLOYEE.ID,sales$NO.OF.MONTH.DOJ), FUN=sum, na.rm=TRUE)
sales=sales[,c(1,2,5,6)]
colnames(sales)=c("EMPLOYEE.ID","NO.OF.MONTH.DOJ","SALE.COUNT","PREMIUM.CHARGED")
emp.list= unique(sales$EMPLOYEE.ID)
months= c(1,2,3,4,5,6)
emp.list= merge(emp.list,months)
colnames(emp.list)=c("EMPLOYEE.ID","NO.OF.MONTH.DOJ")
sales= merge(emp.list,sales, all=TRUE)
sales$SALE.COUNT[is.na(sales$SALE.COUNT)]=0
sales$PREMIUM.CHARGED[is.na(sales$PREMIUM.CHARGED)]=0

conversion.final= merge(name.collect,sales,by=c("EMPLOYEE.ID","NO.OF.MONTH.DOJ"))
write.csv(conversion.final, "20161026.monthly.sales.funnel.csv")




##predictive model

emp_master1= read.csv("20161025_emp.csv")

features=c("GENDER","HIRE.TYPE","MARITAL.STATUS.JOINING","AVG.PREV.EXP",
           "TOTAL.PREV.EXP","INSURANCE.PERCENT","SUB.CATEGORY","REC_F2FCALLS.COUNT","REC_PHONECALLS.COUNT","REC_JOINTCALLS.COUNT",          
            "REC_COUNT","NAT_TOTAL","BOP.TOTAL","PHF.COUNT","LICENSE.COUNT","PERF.GRADE",        "AGENT_COUNT","AGENT_AGE","JOINTCALL.COUNT")  


features=c("GENDER","HIRE.TYPE","MARITAL.STATUS.JOINING","AVG.PREV.EXP",
           "TOTAL.PREV.EXP","INSURANCE.PERCENT","SUB.CATEGORY","REC_F2FCALLS.COUNT","REC_PHONECALLS.COUNT","REC_JOINTCALLS.COUNT",          
           "REC_COUNT","NAT_TOTAL","BOP.TOTAL","PHF.COUNT","LICENSE.COUNT","PERF.GRADE",        "AGENT_COUNT","AGENT_AGE","JOINTCALL.COUNT")  

 
set=emp_master1[,features] 
set$AGENT_AGE= as.numeric(set$AGENT_AGE)

set= set[set$PERF.GRADE%in%c("H","L"),]
set$PERF.GRADE= as.factor(set$PERF.GRADE)


smp_size <- floor(0.75 * nrow(set))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(set)), size = smp_size)

train <- set[train_ind, ]
test <- set[-train_ind, ]

library(randomForest)

#missmap(train, main = "Missing values vs observed")

train<-na.omit(train)
test<-na.omit(test)

#train=train[is.finite(train$performance_curr) & is.finite(train$performance_average),]
# test=test[is.finite(test$performance_curr) & is.finite(test$performance_change),]
# train=train[is.finite(train$performance_curr),]
# test=test[is.finite(test$performance_curr),]



library(randomForest)
modFit <-randomForest(as.factor(PERF.GRADE)~.
                      , trControl=trainControl(method="cv",number=10),
                      data=train, importance=TRUE, ntree=100,mytry=5)

modFit
varImpPlot(modFit)
pred<- predict(modFit, test)
table( test$PERF.GRADE,pred)


#relative weight analysis

features=c("GENDER","HIRE.TYPE","MARITAL.STATUS.JOINING","AVG.PREV.EXP",
           "TOTAL.PREV.EXP","INSURANCE.PERCENT","SUB.CATEGORY",          
           "REC_COUNT","REC_TOTAL.COUNT.PER.LEAD" ,"NAT_TOTAL","BOP.TOTAL","PHF.COUNT","LICENSE.COUNT",
           "AGENT_COUNT","AGENT_AGE","JOINTCALL.COUNT","JOINTCALL.TOTAL.PER.LEAD",
           "PERF.GRADE","PREMIUM.CHARGED")  
emp_master1= read.csv("20161026_emp.csv")
set= emp_master1[,features]


grade=c("H","L")

set.new= data.frame()
for(i in 1:length(grade)){
  set1= set[set$PERF.GRADE==grade[i],]
  set1$PREMIUM.CHARGED.SCALED= set1$PREMIUM.CHARGED/max(set1$PREMIUM.CHARGED)*100
  set.new= rbind(set.new, set1)
}

GENDER= data.frame(GENDER= as.character(unique(set.new$GENDER)), GENDER_NUMBER=(1:length(unique(set.new$GENDER))))
HIRE.TYPE= data.frame(HIRE.TYPE= as.character(unique(set.new$HIRE.TYPE)), HIRE.TYPE_NUMBER=(1:length(unique(set.new$HIRE.TYPE))))
MARITAL.STATUS.JOINING= data.frame(MARITAL.STATUS.JOINING= as.character(unique(set.new$MARITAL.STATUS.JOINING)), MARITAL.STATUS.JOINING_NUMBER=(1:length(unique(set.new$MARITAL.STATUS.JOINING))))
SUB.CATEGORY= data.frame(SUB.CATEGORY= as.character(unique(set.new$SUB.CATEGORY)), SUB.CATEGORY_NUMBER=(1:length(unique(set.new$SUB.CATEGORY))))

set.new1= Reduce(function(x, y) merge(x, y, all=TRUE), list(set.new, GENDER,HIRE.TYPE,MARITAL.STATUS.JOINING,SUB.CATEGORY))

features=c("GENDER_NUMBER","HIRE.TYPE_NUMBER","MARITAL.STATUS.JOINING_NUMBER","AVG.PREV.EXP",
           "TOTAL.PREV.EXP","INSURANCE.PERCENT","SUB.CATEGORY_NUMBER","REC_TOTAL.COUNT.PER.LEAD",          
           "REC_COUNT","NAT_TOTAL","BOP.TOTAL","PHF.COUNT","LICENSE.COUNT","JOINTCALL.TOTAL.PER.LEAD",
           "AGENT_AGE","JOINTCALL.COUNT",
           "PREMIUM.CHARGED.SCALED")  

set.new1= set.new1[,features]

library(relaimpo)
rel.weight =lm(PREMIUM.CHARGED.SCALED~. , data= set.new1)
output = calc.relimp(rel.weight, type = c("lmg"), rela = TRUE)
output= as.data.frame(output$lmg)
output$Subcomponent= rownames(output)

write.csv(output,"20161026.weighted.average.csv")
