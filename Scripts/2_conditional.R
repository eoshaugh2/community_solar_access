# conditional models

#### DO NOT RUN
#### script provided for reference

source("0_prep.R")
library(lme4)

# create dummies for all vars
cmpa<-cmpr %>% inner_join(select(stcns,state,state_income)) %>% 
  mutate(cs=ifelse(typ=="community",1,0),
         lmi=ifelse(is.na(income),NA,ifelse(income<(state_income*10^-3),1,0)),
         rent=ifelse(rent_own%in%c("U",""),NA,ifelse(rent_own%in%c("R","T"),1,0)),
         mf=ifelse(dwell_type%in%c("","P"),NA,ifelse(dwell_type%in%c("A","M"),1,0))) %>%
  filter(!is.na(lmi),!is.na(rent),!is.na(mf)) %>% # 
  mutate(i=1) # for following function

# Y = CS
# every combination to compare AIC
xtrc<-function(x1,x2,x3,x4) {
  if (is.null(x2)) {
    a<-summary(glmer(cs ~ x1 + (1|state), family = binomial(link = "logit"),data = cmpa)) # the function
    b<-cmpa %>% mutate(prd=(1+exp(1)^(-(a[[10]][1,1]+a[[10]][2,1]*x1)))^-1)
  } else {
    if (is.null(x3)) {
      a<-summary(glmer(cs ~ x1 + x2 + (1|state), family = binomial(link = "logit"),data = cmpa)) # the function
      b<-cmpa %>% mutate(prd=(1+exp(1)^(-(a[[10]][1,1]+a[[10]][2,1]*x1+a[[10]][3,1]*x2)))^-1)
    } else {
      if (is.null(x4)) {
        a<-summary(glmer(cs ~ x1 +x2+x3 + (1|state), family = binomial(link = "logit"),data = cmpa)) # the function
        b<-cmpa %>% mutate(prd=(1+exp(1)^(-(a[[10]][1,1]+a[[10]][2,1]*x1+a[[10]][3,1]*x2+a[[10]][4,1]*x2)))^-1)
      } else {
        a<-summary(glmer(cs ~ x1 +x2+x3+x4 + (1|state), family = binomial(link = "logit"),data = cmpa)) # the function
        b<-cmpa %>% mutate(prd=(1+exp(1)^(-(a[[10]][1,1]+a[[10]][2,1]*x1+a[[10]][3,1]*x2+a[[10]][4,1]*x2)))^-1)
      }
    } 
  }
  b<-b %>% mutate(correct=ifelse(cs==1 & prd>=mean(cmpa$cs),1,ifelse(cs==0 & prd<mean(cmpa$cs),1,0))) # 
  ct<-c(mean(b$correct[b$cs==0]),mean(b$correct[b$cs==1]))
  l<-list()
  l[[1]]<-a[[10]]
  l[[2]]<-a[[14]][1]
  l[[3]]<-mean(ct)
  return(l)
}
itr<-list()
itr[[1]]<-xtrc(cmpa$lmi,NULL,NULL,NULL)
itr[[2]]<-xtrc(cmpa$rent,NULL,NULL,NULL)
itr[[3]]<-xtrc(cmpa$mf,NULL,NULL,NULL)
itr[[4]]<-xtrc(cmpa$non_white,NULL,NULL,NULL)

itr[[5]]<-xtrc(cmpa$lmi,cmpa$rent,NULL,NULL)
itr[[6]]<-xtrc(cmpa$lmi,cmpa$mf,NULL,NULL)
itr[[7]]<-xtrc(cmpa$lmi,cmpa$non_white,NULL,NULL)
itr[[8]]<-xtrc(cmpa$rent,cmpa$mf,NULL,NULL)
itr[[9]]<-xtrc(cmpa$rent,cmpa$non_white,NULL,NULL)
itr[[10]]<-xtrc(cmpa$mf,cmpa$non_white,NULL,NULL)

itr[[11]]<-xtrc(cmpa$lmi,cmpa$rent,cmpa$mf,NULL)
itr[[12]]<-xtrc(cmpa$lmi,cmpa$rent,cmpa$non_white,NULL)
itr[[13]]<-xtrc(cmpa$lmi,cmpa$mf,cmpa$non_white,NULL)
itr[[14]]<-xtrc(cmpa$rent,cmpa$mf,cmpa$non_white,NULL)

itr[[15]]<-xtrc(cmpa$lmi,cmpa$rent,cmpa$mf,cmpa$non_white)

# supp table: multivariate model and all univariate results
rn<-function(z) round(as.numeric(z),2)
rn4<-function(z) round(as.numeric(z),4)
clnr<-function(z) paste(rn(itr[[z]][[1]][2,1]),"* (",rn(itr[[z]][[1]][2,2]),")",
                        " [",rn4(itr[[z]][[1]][2,4]),"]",sep="")
sta<-data.frame(var=c("LMI","rent","multifamily","non_white","aic"),
                mv=c(paste(rn(itr[[15]][[1]][2:5,1]),"* ",
                           paste("(",rn(itr[[15]][[1]][2:5,2]),")",sep="")," ",
                           paste("[",rn4(itr[[15]][[1]][2:5,4]),"]",sep=""),sep=""),
                     rn(itr[[15]][[2]])),
                lmi=c(clnr(1),"","","",rn(itr[[1]][[2]])),
                rent=c("",clnr(2),"","",rn(itr[[2]][[2]])),
                mf=c("","",clnr(3),"",rn(itr[[3]][[2]])),
                nw=c("","","",clnr(4),rn(itr[[4]][[2]])))
write.csv(sta,"conditional_model_results.csv",row.names=F)

# export all AICs
aicCln<-function(z) rn(itr[[z]][[2]])
mta<-data.frame(mdl=c(1:15),vrs=c("i","r","m","w","ir","im","iw","rm","rw","mw",
                                  "irm","irw","imw","rmw","irmw"))
for (i in 1:15) mta$aic[i]<-aicCln(i)
mta<-arrange(mta,aic)
write.csv(mta,"aic.csv",row.names=F)


# separate the two types, approximation to percentage points
coeffs<-as.data.frame(itr[[15]][[1]]) %>% mutate(x=c(0:4),fctr=c(NA,"LMI","Renter","Multifamily","Non-White\nor Hispanic"),
                                                 aprx=(Estimate/4)*100,aprxSe=(`Std. Error`/4)*100) %>% 
  filter(x>0) %>% mutate(lo=aprx-1.96*aprxSe,hi=aprx+1.96*aprxSe) %>% arrange(-aprx) %>%
  mutate(x=c(1:4))
lbz<-data.frame(x=c(1:4),y1=c(33,14,12,-32),l1=c(paste("ß=",round(coeffs$Estimate[1],2),sep=""),round(coeffs$Estimate[2:4],2)),
                y2=c(28,9.5,7.5,-36.5),l2=c(paste("SE=(",round(coeffs$`Std. Error`[1],2),")",sep=""),paste("(",round(coeffs$`Std. Error`[2:4],2),")",sep="")))


