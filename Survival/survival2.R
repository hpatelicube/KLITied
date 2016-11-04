library(OIsurv)
data(aids)


data(tongue)
attach(tongue)
my.surv.object <- Surv(time[type==1], delta[type==1])
my.surv.object

data(psych);
attach(psych)
my.surv.object <- Surv(age, age+time, death)
my.surv.object

