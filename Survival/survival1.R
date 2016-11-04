hmohiv<-read.table("http://www.ats.ucla.edu/stat/r/examples/asa/hmohiv.csv", sep=",", header = TRUE) 
library(survival)
attach(hmohiv)
mini<-hmohiv[ID<=5,]
mini

attach(mini)
mini.surv <- survfit(Surv(time, censor)~ 1, conf.type="none")
summary(mini.surv)

plot(mini.surv, xlab="Time", ylab="Survival Probability")

detach(mini)
attach(hmohiv)
hmohiv.surv <- survfit( Surv(time, censor)~ 1, conf.type="none")
summary(hmohiv.surv)

plot (hmohiv.surv,  xlab="Time", ylab="Survival Probability" )



library(KMsurv)
library(nlme)
t6m<-floor(time/6)
tall<-data.frame(t6m, censor)
die<-gsummary(tall, sum, groups=t6m)
total<-gsummary(tall, length, groups=t6m)
rm(t6m)
ltab.data<-cbind(die[,1:2], total[,2])
detach(hmohiv)
attach(ltab.data)

lt=length(t6m)
t6m[lt+1]=NA
nevent=censor
nlost=total[,2] - censor
mytable<-lifetab(t6m, 100, nlost, nevent)
mytable[,1:5]

plot(t6m[1:11], mytable[,5], type="s", xlab="Survival time in every 6 month", 
     ylab="Proportion Surviving")

plot(t6m[1:11], mytable[,5], type="b", xlab="Survival time in every 6 month", 
     ylab="Proportion Surviving")





library(survival)
library(km.ci)

detach(hmohiv)
attach(hmohiv)

hmohiv.surv <- survfit( Surv(time, censor) ~ 1)
a<-km.ci(hmohiv.surv, conf.level=0.95, tl=NA, tu=NA, method="loghall")

par(cex=.8)
plot(a, lty=2, lwd=2)
time.conf <- survfit( Surv(time, censor)~ 1)
lines(time.conf, lwd=2, lty=1)
lines(time.conf, lwd=1, lty=4, conf.int=T)
linetype<-c(1, 2, 4)
legend(40, .9, c("Kaplan-Meier", "Hall-Wellner", "Pointwise"), lty=(linetype))



detach()
attach(mini)
mini.surv <- survfit(Surv(time, censor)~ 1, conf.type="none")
summary(mini.surv)

par(cex=.8)
plot(mini.surv, xlab="Time", ylab="Survival Probability")
lines(x=c(5,5),y=c(0,.6), lty=2) 
lines(x=c(8,8),y=c(0,.3), lty=2)
lines(x=c(0,22),y=c(.25,.25), lty=2) 
lines(x=c(0,8),y=c(.5,.5), lty=2)
lines(x=c(0,5),y=c(.75,.75), lty=2)



stci = function(qn, y)
{
  temp<-data.frame(time=y$time, surv=y$surv, std.err=y$std.err)
  temp$std.err<-temp$std.err*temp$surv 
  attach(temp)
  q.lp<-temp[surv<= qn/100 -.05,][1,]
  q<-temp[surv<=qn/100,][1,]
  q.u<-temp[surv>=qn/100+.05,]
  rnm<-nrow(q.u)
  q.up<-q.u[rnm, ]
  fp = (q.up$surv - q.lp$surv)/( q.lp$time - q.up$time)
  std = (q$std.err)/fp
  lower = q$time - 1.96*std
  upper = q$time + 1.96*std
  print(rbind(c(quantile=qn, time=q$time, std.err=std, cie.lower=lower, cie.upper=upper)))
}

rm(list=ls()) #cleaning up 
hmohiv<-read.table("http://www.ats.ucla.edu/stat/r/examples/asa/hmohiv.csv", sep=",", header = TRUE) 
library(survival)
attach(hmohiv)
h.surv <- survfit(Surv(time, censor)~ 1, conf.type="log-log")
stci(75, h.surv)
stci(50, h.surv)
stci(25, h.surv)

summary(h.surv)
print(h.surv, show.rmean=T)

timestrata.surv <- survfit( Surv(time, censor)~ strata(drug), hmohiv, conf.type="log-log")
plot(timestrata.surv, lty=c(1,3), xlab="Time", ylab="Survival Probability")
legend(40, 1.0, c("Drug - No", "Drug - Yes") , lty=c(1,3) )



rm(list=ls()) #cleaning up 
minitest<-read.table("http://www.ats.ucla.edu/stat/r/examples/asa/minitest.txt", header = TRUE) 
attach(minitest)
minitest

survdiff(Surv(time, censor) ~ drug, data=minitest, rho=0)
survdiff(Surv(time, censor) ~ drug, data=minitest, rho=1)

detach(minitest)
hmohiv<-read.table("http://www.ats.ucla.edu/stat/r/examples/asa/hmohiv.csv", sep=",", header = TRUE) 

attach(hmohiv)
agecat <- cut(age, c(19.9, 29, 34, 39, 54.1))
age.surv <- survfit( Surv(time, censor)~ strata(agecat), conf.type="log-log")
print(age.surv)

plot(age.surv, lty=c(6, 1, 4, 3), xlab="Time", ylab="Survival Probability")
legend(40, 1.0, c("Group 1", "Group 2", "Group 3", "Group 4"), lty=c(6, 1, 4, 3)) 


survdiff(Surv(time, censor) ~ agecat, rho=0)
survdiff(Surv(time, censor) ~ agecat, data=hmohiv, rho=1)

a<- survfit(coxph(Surv(time,censor)~1), type="aalen")
summary(a)


h.aalen<-(-log(a$surv))
aalen.est<-cbind(time=a$time, d=a$n.event, n=a$n.risk, 
                 h.aalen, s1=a$surv)
b<-survfit(Surv(time, censor)~1)
km.est<-cbind(time=b$time, s2=b$surv)
all<-merge(data.frame(aalen.est), data.frame(km.est), by="time")
all

plot(all$time, all$s1, type="s", xlab="Survival Time (Months)", 
     ylab="Survival Probability")
points(all$time, all$s1, pch=1)
lines(all$time, all$s2, type="s")
points(all$time, all$s2, pch=3)
legend(40, .8, c("Nelson-Aalen", "Kaplan-Meier"), pch=c(1, 3))


h2<-all$d/all$n
plot.new()
plot(all$time, h2, type="p", pch=20, xlab="Survival Time (Months)", 
     ylab="Hazard Ratio")
lines(lowess(all$time, h2,f=.75, iter=5))
