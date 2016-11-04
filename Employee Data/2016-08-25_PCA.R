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

attrition.rate=function(data1,dim){
  data1$cluster1= data1[,dim]
  data1$OUTPUT= as.factor(data1$OUTPUT)
  data1$value=1
  data2= data1[,c("cluster1","OUTPUT","value")]
  df2=cast(data1,cluster1~OUTPUT,sum, value = "value")
  df2$total = df2[,2]+df2[,3]
  df2$average.attr= df2[,3]/df2[,"total"]*100
  df2$average.attr[df2$total<3]= mean(df2$average.attr)
  
  return(df2)
}

feature_set=function(reporting_master,emp_master,duration,tenure_min,tenure_max){
  input_data=data.frame()
  asof=sort(unique(reporting_master$QUARTER.ENDDATE),decreasing = FALSE)
  #to=length(asof)-ceiling(duration/3)
  to=length(asof)
  tenure_min=tenure_min/12
  tenure_max=tenure_max/12
  
  for (i in 2:to){
    #select reporting data
    date_asof=as.Date(asof[i],format="%Y-%m-%d")
    date_previous=as.Date(asof[i-1],format="%Y-%m-%d")
    
    d=emp_master[((as.Date(emp_master$DOJ,format="%Y-%m-%d")<= as.Date(date_asof,format="%Y-%m-%d")) & 
                     (as.Date(emp_master$DOL,format="%Y-%m-%d")>date_asof | is.na(emp_master$DOL))),]
    
    r=reporting_master[reporting_master$QUARTER.ENDDATE==date_asof,]
    
    d=merge(d,r,all.x = TRUE,by="EMPLOYEE.ID")
    
    d$AGE=as.numeric(round((date_asof-d$DOB)/365.25,2))
    d$TENURE=as.numeric(round((date_asof-d$DOJ)/365.25,2))
    
    #select performance data
    #current performance
    # Perf_curr=employee_performance[as.Date(employee_performance$QUARTER.ENDDATE,format="%Y-%m-%d")==date_asof,c("PERSONNEL.NUMBER","ACHIEVED.PERCENT")]
    # names(Perf_curr)[2]="CURRENT.PERFORMANCE"
    
    # r=merge(r,Perf_curr,by="PERSONNEL.NUMBER",all.x=TRUE,all.y = FALSE)
    
    # Perf_prev=employee_performance[as.Date(employee_performance$QUARTER.ENDDATE,format="%Y-%m-%d")==date_previous,c("PERSONNEL.NUMBER","ACHIEVED.PERCENT")]
    # names(Perf_prev)[2]="PREVIOUS.PERFORMANCE"
    # r=merge(r,Perf_prev,by="PERSONNEL.NUMBER",all.x=TRUE,all.y = FALSE)
    # r$AVERAGE.PERFORMANCE=rowMeans(r[,c("CURRENT.PERFORMANCE","PREVIOUS.PERFORMANCE")],na.rm=TRUE)
    
    d=merge(d,emp_master[,c("EMPLOYEE.ID","NAME","DOB","DOJ")],
            by.x = "REPORTING.MANAGER.ID",by.y = "EMPLOYEE.ID",
            all.x = TRUE,suffixes = c("",".RM"))
    
    d$AGE.RM=round((date_asof-d$DOB.RM)/365.25,2)
    d$TENURE.RM=round((date_asof-d$DOJ.RM)/365.25,2)
    d$TENURE.DIFF=d$TENURE.RM-d$TENURE
    d$AGE.DIFF=d$AGE.RM-d$AGE
    
    # d$AWAY.HOME=ifelse(d$LOCATION=="Shendra" & d$LOCATION.COMBINED=="AURANGABAD",0,
    #                    ifelse(d$LOCATION=="Daman" &
    #                             d$LOCATION.COMBINED=="DAMAN",0,1)) 
    
    #filter values
    d=d[d$TENURE>=tenure_min & d$TENURE<=tenure_max,]
    
    date_leaving=date_asof %m+% months(duration)
    
    d$OUTPUT=ifelse(as.Date(d$DOL,format="%Y-%m-%d")>=date_leaving | is.na(d$DOL),0,1)
    
    d$QUARTER.ENDDATE=date_asof
    
    input_data=rbind(input_data,d)
    print(c(i,nrow(d),sum(d$OUTPUT)/nrow(d)))
  }
  
  #FLM Attrition
  RM_Attrition= attrition.rate(input_data[input_data$QUARTER.ENDDATE<='2015-12-31',],"REPORTING.MANAGER.ID")
  RM_Attrition$REPORTING.MANAGER.ID=RM_Attrition$cluster1
  RM_Attrition$RM.ATTRITION= RM_Attrition$average.attr
  input_data= merge(input_data,RM_Attrition[,c("REPORTING.MANAGER.ID","RM.ATTRITION")]
                    ,all.x=TRUE)
  # TEAM_Attrition= attrition.rate(input_data[input_data$QUARTER.ENDDATE<='2015-12-31',],"TEAM")
  # TEAM_Attrition$TEAM=TEAM_Attrition$cluster1
  # TEAM_Attrition$TEAM.ATTRITION= TEAM_Attrition$average.attr
  # input_data= merge(input_data,TEAM_Attrition[,c("TEAM","TEAM.ATTRITION")]
  #                   ,all.x=TRUE)
  
  return(input_data) 
}

features=feature_set(reporting_master,emp_master,6,0,50)

# features=features[features$QUARTER.ENDDATE=="2016-06-30",]
# write.csv(features,"20160822_features_Shendra.csv")

#modeling
features$GENDER=as.factor(features$GENDER)
features$SEGMENT=as.factor(features$SEGMENT)
features$CATEGORY=as.factor(features$CATEGORY)
features$SUB.CATEGORY=as.factor(features$SUB.CATEGORY)
features$MARITAL.STATUS=as.factor(features$MARITAL.STATUS)
features$HIRE.SOURCE=as.factor(features$HIRE.SOURCE)
features$HIRE.TYPE=as.factor(features$HIRE.TYPE)
features$MARITAL.STATUS.JOINING=as.factor(features$MARITAL.STATUS.JOINING)
features$GRADE=as.factor(features$GRADE)
features$DESIGNATION=as.factor(features$DESIGNATION)
features$LOCATION.ID=as.factor(features$LOCATION.ID)
features$DEPARTMENT=as.factor(features$DEPARTMENT)


features$AVG.PREV.EXP=as.numeric(features$AVG.PREV.EXP)
features$TOTAL.PREV.EXP=as.numeric(features$TOTAL.PREV.EXP)
features$AGE.DIFF=as.numeric(features$AGE.DIFF)
features$TENURE.DIFF=as.numeric(features$TENURE.DIFF)
features$TENURE.RM=as.numeric(features$TENURE.RM)
features$AVG.PREV.EXP=as.numeric(features$AVG.PREV.EXP)
features$AGE.RM=as.numeric(features$AGE.RM)

features[,"AVG.PREV.EXP"][is.na(features$AVG.PREV.EXP)]=0


features$TENURE.DIFF[is.na(features$TENURE.DIFF)] <- mean(features$TENURE.DIFF,na.rm=T)
features$SUB.CATEGORY[is.na(features$SUB.CATEGORY)] <- "SCIENCE"


# Converting to categorical
features$TENURE=cut(as.numeric(features$TENURE),br=c(0,0.5,1,2,3,5,50),right = FALSE)

#use category weak
features$AGE.DIFF=cut(as.numeric(features$AGE.DIFF),
                      br=c(-20,-5,0,5,10,20,50),right = FALSE)

# can use
features$TENURE.DIFF=cut(as.numeric(features$TENURE.DIFF),
                         br=c(-30,-5,0,5,10,20,50),right = FALSE)



#Assigning FLM.Attrition to new teams Average Attrition rate
features$FLM.ATTRITION[is.na(features$FLM.ATTRITION) & 
                         features$QUARTER.ENDDATE=='2016-06-30']=
  sum(features$OUTPUT[features$QUARTER.ENDDATE=='2015-12-31'])/
  nrow(features[features$QUARTER.ENDDATE=='2015-12-31',])*100

features$TEAM.ATTRITION[is.na(features$TEAM.ATTRITION) & 
                          features$QUARTER.ENDDATE=='2016-06-30']=
  sum(features$OUTPUT[features$QUARTER.ENDDATE=='2015-12-31'])/
  nrow(features[features$QUARTER.ENDDATE=='2015-12-31',])*100




cont_var=c("RM.ATTRITION","TENURE.RM","AGE.RM",
           # "TENURE","AGE.DIFF","TENURE.DIFF",
           "AVG.PREV.EXP","PREV.COMPANY.COUNT","AGE","TOTAL.PREV.EXP")

# cont_var=c("AVG.PREV.EXP","PREV.COMPANY.COUNT","AGE","TOTAL.PREV.EXP")

correlationMatrix =cor(features[, cont_var])

features$TOTAL.PREV.EXP[features$TOTAL.PREV.EXP==0]=0.01
features$PREV.COMPANY.COUNT[features$PREV.COMPANY.COUNT==0]=0.01
features$AVG.PREV.EXP[features$AVG.PREV.EXP==0]=0.01
features$RM.ATTRITION[features$RM.ATTRITION==0]=0.01
features$TENURE.RM[features$TENURE.RM<=0]=0.01
# features$TENURE[features$TENURE<=0]=0.01
# features$AGE.DIFF[features$AGE.DIFF<=0]=0.01
# features$TENURE.DIFF[features$TENURE.DIFF<=0]=0.01

features=features[!is.na(features$RM.ATTRITION),]
features=features[!is.na(features$TENURE.RM),]
features=features[!is.na(features$EMPLOYEE.ID),]

log.d <- log(features[,cont_var])

ir.pca <- prcomp(log.d,
                 center = TRUE,
                 scale. = TRUE) 
summary(ir.pca)

d1=as.data.frame(predict(ir.pca,newdata=log.d))

features=cbind(features,d1)

feat=c(
  "EMPLOYEE.ID",
  # "GENDER",
  # "SEGMENT",
  # "CATEGORY",
  "SUB.CATEGORY",
  "INSURANCE.PERCENT",
  # "MARITAL.STATUS.JOINING",
  # "AGE",
  # "AVG.PREV.EXP",
  # "TOTAL.PREV.EXP",
  # "PREV.COMPANY.COUNT",
  # "RM.ATTRITION",
  # "AGE.RM",
  # "TENURE.RM",
  "PC1",
  "PC2",
  "PC3",
  "PC4",
  "PC5",
  "TENURE",
  "TENURE.DIFF",
  "AGE.DIFF",
  # "AWAY.HOME",
  "OUTPUT"
)


train_date1=as.Date('2013-12-31',format ="%Y-%m-%d")
train_date2=as.Date('2014-06-30',format ="%Y-%m-%d")
train_date3=as.Date('2014-12-31',format ="%Y-%m-%d")
train_date4=as.Date('2015-06-30',format ="%Y-%m-%d")

test_date1=as.Date('2015-12-31',format ="%Y-%m-%d")

# train_date1=as.Date('2014-09-30',format ="%Y-%m-%d")
# train_date2=as.Date('2014-03-31',format ="%Y-%m-%d")
# train_date3=as.Date('2014-09-30',format ="%Y-%m-%d")
# train_date4=as.Date('2015-03-31',format ="%Y-%m-%d")
# 
# test_date1=as.Date('2015-09-30',format ="%Y-%m-%d")
# 

# 
# train_date1=as.Date('2014-12-31',format ="%Y-%m-%d")
# train_date2=as.Date('2014-12-31',format ="%Y-%m-%d")
# train_date3=as.Date('2015-06-30',format ="%Y-%m-%d")
# train_date4=as.Date('2015-12-31',format ="%Y-%m-%d")
# 
# test_date1=as.Date('2016-06-30',format ="%Y-%m-%d")


train=features[features$QUARTER.ENDDATE==train_date1 | features$QUARTER.ENDDATE==train_date2
               |features$QUARTER.ENDDATE==train_date3 | features$QUARTER.ENDDATE==train_date4
               ,feat]

train=train[, !names(train) %in% c("EMPLOYEE.ID")] 

test=features[features$QUARTER.ENDDATE==test_date1 ,feat]


train<-na.omit(train)
test<-na.omit(test)


#logistic regression
model <- glm(as.factor(OUTPUT) ~.,family=binomial(link='logit'),data=train)

test=test[test$SUB.CATEGORY %in% train$SUB.CATEGORY,]

pred3<-predict(model, test)

modFit2 <- data.table(ifelse(pred3 > -0.45,1,0))
table(test$OUTPUT,modFit2$V1)

rocplot(model)

#random Forest
modFit <-randomForest(as.factor(OUTPUT)~.
                      , trControl=trainControl(method="cv",number=10),
                      data=train, importance=TRUE, ntree=200,mytry=5)

modFit

varImpPlot(modFit)
pred<- predict(modFit, test)
table( test$OUTPUT,pred)





test$prob=pred3
test$OUTPUT1=ifelse(test$prob>(-1.22),1,0)




pred4<- data.table(pred3)


test$prob=pred3
test$OUTPUT=ifelse(test$prob>(-0.3),"High",ifelse(test$prob<(-0.91),"Low","Medium" ))
table(test$OUTPUT)

write.csv(test,paste(format(Sys.time(), "%Y%m%d%H%M%S"),
                     "Shendra_0-5.csv",sep = ""),row.names = FALSE)
