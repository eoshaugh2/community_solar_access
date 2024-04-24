# analysis of unconditional models

#### DO NOT RUN
#### script provided for reference

source("0_prep.R")

##### income (one-sided Wilcox test)
wcx<-function(st) {
  dta<-cmpr %>% filter(state==st,!is.na(income))
  wc<-wilcox.test(dta$income~dta$typ,alternative="less")
  return(data.frame(state=st,v0=median(dta$income[dta$typ=="rooftop"]),v1=median(dta$income[dta$typ=="community"]),
                    stt=as.numeric(wc[1]),pv=as.numeric(wc[3]),sig=ifelse(wc[3]<0.05,1,0)))
} 
wcInc<-list()
for (i in 1:stN) wcInc[[i]]<-wcx(ssmpl[i])
wcInc<-do.call(rbind,wcInc) %>% mutate(x=c(1:nrow(.))) %>%
  inner_join(select(stcns,state,state_income)) %>% mutate(state_income=state_income*10^-3)


##### categorical vars (X2 tests)
xsq<-function(dd,vv,st) {
  dta<-dd %>% mutate(vr=vv) %>% filter(state==st)
  xs<-chisq.test(dta$vr,dta$typ)
  return(data.frame(state=st,v0=mean(dta$vr[dta$typ=="rooftop"]),v1=mean(dta$vr[dta$typ=="community"]),
                    stt=as.numeric(xs[1]),pv=as.numeric(xs[3])) %>%
           mutate(sig=ifelse(v1>v0 & pv<0.05,1,0)))
} 

# rent_own 
ora<-cmpr %>% filter(!(rent_own%in%c("","U"))) %>% mutate(rent=ifelse(rent_own%in%c("R","T"),1,0)) %>% select(state,typ,rent) 
c(mean(ora$rent[ora$typ=="rooftop"]),mean(ora$rent[ora$typ=="community"]))
xsqRent<-list()
for (i in 1:stN) xsqRent[[i]]<-xsq(ora,ora$rent,ssmpl[i])
xsqRent<-do.call(rbind,xsqRent) %>% mutate(x=c(1:nrow(.))) %>% inner_join(select(stcns,state,state_rent)) %>% rename(stshr=state_rent)

# dwell_type
mfa<-cmpr %>% filter(!(dwell_type%in%c("","P"))) %>% mutate(mf=ifelse(dwell_type%in%c("A","M"),1,0)) %>% select(state,typ,mf) 
c(mean(mfa$mf[mfa$typ=="rooftop"]),mean(mfa$mf[mfa$typ=="community"]))
xsqMf<-list()
for (i in 1:stN) xsqMf[[i]]<-xsq(mfa,mfa$mf,ssmpl[i])
xsqMf<-do.call(rbind,xsqMf) %>% mutate(x=c(1:nrow(.))) %>% inner_join(select(stcns,state,state_mf)) %>% rename(stshr=state_mf)

# non-white ~ categorical
rca<-cmpr %>% filter(!is.na(non_white))
xsqRce<-list()
for (i in 1:stN) xsqRce[[i]]<-xsq(rca,rca$non_white,ssmpl[i])
xsqRce<-do.call(rbind,xsqRce) %>% mutate(x=c(1:nrow(.))) %>% inner_join(select(stcns,state,state_non_white)) %>% rename(stshr=state_non_white)
