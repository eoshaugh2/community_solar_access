# ny side analysis

#### DO NOT RUN
#### script provided for reference

library(tidyverse)

source("0_prep.R")
downstate<-c("103","059","081","047","085", # all downstate counties
             "061","005","087","119")

# create figures with all adopters and participant/non-participant splits

# NY all community solar adopter data with downstate identifier
nycsa<-sta %>% filter(state=="NY") %>% mutate(county=substr(tract,3,5)) %>% 
  mutate(downstate=ifelse(county%in%downstate,1,0),
         non_white=ifelse(pred.whi>pred.bla & pred.whi>pred.his & pred.whi>pred.asi & pred.whi>pred.oth,0,1),
         typ="community") %>%
  select(c(names(cmpr),downstate))

# NY participant/non-participant
# lmiC LMI community solar program participants
# nycsl NY program data

nyrf<-rfa %>% mutate(state=substr(tract,1,2)) %>% filter(state=="36") %>%
  mutate(county=substr(tract,3,5),lmi=2) %>%
  mutate(downstate=ifelse(county%in%downstate,1,0)) %>% # remove downstate counties
  mutate(typ="rooftop",non_white=ifelse(pred.whi>pred.bla & pred.whi>pred.his & pred.whi>pred.asi & pred.whi>pred.oth,0,1)) %>%
  select(c(names(cmpr),lmi,downstate))

nycmpra<-rbind(nycsa,select(nyrf,-lmi)) # all
nycmprl<-rbind(nycsl,nyrf) # limited to program data
table(nycmpra$typ) # should match total sample sizes

# in each case 3 tests: all adopters, LMI participants, non-participants

##### income (one-sided Wilcox test)
wcx<-function(dta) {
  wc<-wilcox.test(dta$income~dta$typ,alternative="less")
  return(data.frame(v0=median(dta$income[dta$typ=="rooftop"]),v1=median(dta$income[dta$typ=="community"]),
                    stt=round(as.numeric(wc[1]),1),pv=round(as.numeric(wc[3]),5)))
} 
inca<-rbind(wcx(nycmpra), # full set (for comparison)
            wcx(filter(nycmpra,downstate==0)), # downstate
            wcx(filter(nycmprl,downstate==0,lmi%in%c(0,2))), # downstate non-participants
            wcx(filter(nycmprl,downstate==0,lmi%in%c(1,2)))) # downstate participants

##### categorical vars (X2 tests)
xsq<-function(dta) {
  xs<-chisq.test(dta$vr,dta$typ)
  return(data.frame(v0=round(mean(dta$vr[dta$typ=="rooftop"])*100,1),v1=round(mean(dta$vr[dta$typ=="community"])*100,1),
                    stt=round(as.numeric(xs[1]),1),pv=round(as.numeric(xs[3]),5)))
} 

# own_rent
ora<-nycmpra %>% filter(!(rent_own%in%c("","U"))) %>% mutate(vr=ifelse(rent_own%in%c("R","T"),1,0)) %>% select(typ,vr,downstate) 
orl<-nycmprl %>% filter(!(rent_own%in%c("","U"))) %>% mutate(vr=ifelse(rent_own%in%c("R","T"),1,0)) %>% select(typ,vr,lmi,downstate) 
xsqRent<-rbind(xsq(ora),
               xsq(filter(ora,downstate==0)),
               xsq(filter(orl,downstate==0,lmi%in%c(0,2))),
               xsq(filter(orl,downstate==0,lmi%in%c(1,2))))

# dwell_type
mfa<-nycmpra %>% filter(!(dwell_type%in%c("","P"))) %>% mutate(vr=ifelse(dwell_type%in%c("A","M"),1,0)) %>% select(typ,vr,downstate) 
mfl<-nycmprl %>% filter(!(dwell_type%in%c("","P"))) %>% mutate(vr=ifelse(dwell_type%in%c("A","M"),1,0)) %>% select(typ,vr,lmi,downstate) 
xsqMf<-rbind(xsq(mfa),
             xsq(filter(mfa,downstate==0)),
             xsq(filter(mfl,downstate==0,lmi%in%c(0,2))),
             xsq(filter(mfl,downstate==0,lmi%in%c(1,2))))

# non-white ~ categorical
rca<-nycmpra %>% mutate(vr=non_white)
rcl<-nycmprl %>% mutate(vr=non_white)
xsqRce<-rbind(xsq(rca),
              xsq(filter(rca,downstate==0)),
              xsq(filter(rcl,downstate==0,lmi%in%c(0,2))),
              xsq(filter(rcl,downstate==0,lmi%in%c(1,2))))

# export all of those results
xpt<-data.frame(var=rep(c("inc","rent","mf","race"),each=4),
                data=rep(c("all","upstate","upstate_non","upstate_part"),times=4)) %>%
                cbind(rbind(inca,xsqRent,xsqMf,xsqRce)) %>%
  rbind(data.frame(var=NA,data=c("all","upstate","upstate_non","uptate_part"),
                   v0=c(nrow(nycmpra),nrow(filter(nycmpra,downstate==0)), # sample sizes
                        nrow(filter(nycmprl,downstate==0,lmi%in%c(0,2))),
                        nrow(filter(nycmprl,downstate==0,lmi%in%c(1,2)))),
                   v1=NA,stt=NA,pv=NA))
write.csv(xpt,"ny_robustness_check.csv",row.names=F)


