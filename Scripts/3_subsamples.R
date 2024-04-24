# subsample analysis

#### DO NOT RUN
#### script provided for reference

source("0_prep.R")

# community solar LMI based on identifiers from NY and OR, +FL (no LMI policy)
# spa represents state program demographics data
# lmiC represents IDs for community solar LMI program participants

#### 
lmsmpl<-c("IL","NY","OR")
lmstN<-length(lmsmpl)
ssa<-spa %>% filter(state%in%lmsmpl) %>%
  mutate(lmi=ifelse(id%in%lmiC$id,1,0),non_white=1-pred.whi,
         non_white=ifelse(pred.whi>pred.bla & pred.whi>pred.his & pred.whi>pred.asi & pred.whi>pred.oth,0,1)) %>% select(-statefp,-county,-tract,-surname) 

# rooftop LMI
# lmiR represents IDs for rooftop solar LMI program participants
lrfta<-filter(rfa,id%in%lmiR$id_attachmd)
table(lrfta$state) # for comparison
rstrct<-filter(rft,!(id%in%lmiR$id_attachmd),state%in%lmsmpl)

lmcmpr<-rbind(rstrct %>% select(state,income,dwell_type,rent_own,non_white) %>% mutate(typ="rooftop",lmi=0),
            ssa %>% select(state,lmi,income,dwell_type,rent_own,non_white) %>% mutate(typ="community")) # wilcox test automatically tests against the first factor, so "community" in this case
as.data.frame(lmcmpr %>% mutate(n=1) %>% filter(typ=="community") %>%
                group_by(state,lmi) %>% summarize(N=sum(n)))

##### income (one-sided Wilcox test)
wcx<-function(st) {
  dta<-lmcmpr %>% filter(state==st,!is.na(income),lmi==0)
  wc<-wilcox.test(dta$income~dta$typ,alternative="less")
  return(data.frame(state=st,v0=median(dta$income[dta$typ=="rooftop"]),v1=median(dta$income[dta$typ=="community"]),
                    stt=as.numeric(wc[1]),pv=as.numeric(wc[3]),sig=ifelse(wc[3]<0.05,1,0)))
} 
wcInc<-list()
for (i in 1:lmstN) wcInc[[i]]<-wcx(lmsmpl[i])
wcInc<-do.call(rbind,wcInc) %>% mutate(x=c(1:nrow(.))) %>%
  inner_join(select(stcns,state,state_income)) %>% mutate(state_income=state_income*10^-3)

##### categorical vars (X2 tests)
xsq<-function(dd,vv,st) {
  dta<-dd %>% mutate(vr=vv) %>% filter(state==st,lmi==0)
  xs<-chisq.test(dta$vr,dta$typ)
  return(data.frame(state=st,v0=mean(dta$vr[dta$typ=="rooftop"]),v1=mean(dta$vr[dta$typ=="community"]),
                    stt=as.numeric(xs[1]),pv=as.numeric(xs[3])) %>%
           mutate(sig=ifelse(v1>v0 & pv<0.05,1,0)))
} 

# own_rent 
ora<-lmcmpr %>% filter(!(rent_own%in%c("","U"))) %>% mutate(rent=ifelse(rent_own%in%c("R","T"),1,0)) %>% select(state,typ,rent,lmi) 
c(mean(ora$rent[ora$typ=="rooftop"]),mean(ora$rent[ora$typ=="community"]))
xsqRent<-list()
for (i in 1:lmstN) xsqRent[[i]]<-xsq(ora,ora$rent,lmsmpl[i])
xsqRent<-do.call(rbind,xsqRent) %>% mutate(x=c(1:nrow(.))) %>% inner_join(select(stcns,state,state_rent)) %>% rename(stshr=state_rent)

# dwell_type
mfa<-lmcmpr %>% filter(!(dwell_type%in%c("","P"))) %>% mutate(mf=ifelse(dwell_type%in%c("A","M"),1,0)) %>% select(state,typ,mf,lmi) 
c(mean(mfa$mf[mfa$typ=="rooftop"]),mean(mfa$mf[mfa$typ=="community"]))
xsqMf<-list()
for (i in 1:lmstN) xsqMf[[i]]<-xsq(mfa,mfa$mf,lmsmpl[i])
xsqMf<-do.call(rbind,xsqMf) %>% mutate(x=c(1:nrow(.))) %>% inner_join(select(stcns,state,state_mf)) %>% rename(stshr=state_mf)

# non-white ~ categorical
xsqRce<-list()
for (i in 1:lmstN) xsqRce[[i]]<-xsq(lmcmpr,lmcmpr$non_white,lmsmpl[i])
xsqRce<-do.call(rbind,xsqRce) %>% mutate(x=c(1:nrow(.))) #%>% inner_join(select(stcns,state,state_mf)) %>% rename(stshr=state_mf)

# identify key values of the LMI cohort
lma<-as.data.frame(lmcmpr %>% filter(typ=="community",lmi==1) %>% 
                mutate(rent=ifelse(is.na(rent_own),NA,ifelse(rent_own%in%c("R","T"),1,0)),
                       mf=ifelse(is.na(dwell_type),NA,ifelse(dwell_type%in%c("A","M"),1,0))) %>%
                group_by(state) %>%
                summarize(income=median(income),rent=mean(rent),mf=mean(mf),non_white=mean(non_white))) %>%
  mutate(x=c(1,2,3),sig=c(0,0,1)) # sig only applies for race

# stats in comparison to LMI ~ % roughly explained by policy
ssz<-as.data.frame(table(lmcmpr$state[lmcmpr$typ=="community"])) %>% rename(state=Var1,N=Freq)
inca<-wcInc %>% 
  inner_join(as.data.frame(filter(lmcmpr,typ=="community") %>% group_by(state) %>%
                             summarize(aInc=median(income,na.rm=T)))) %>% 
  mutate(rtio=((v1-aInc))/(v0-aInc)) %>% inner_join(ssz)
c(mean(inca$rtio),weighted.mean(inca$rtio,inca$N))
smm<-function(d1,d2,vv) d1 %>% 
  inner_join(as.data.frame(d2 %>% mutate(vr=vv) %>% filter(typ=="community") %>% group_by(state) %>%
                             summarize(vr=mean(vr,na.rm=T)))) %>% 
  mutate(rtio=((v1-vr))/(v0-vr)) %>% inner_join(ssz)
rnta<-smm(rnta,ora,ora$rent)
c(mean(rnta$rtio),weighted.mean(rnta$rtio,rnta$N))
mff<-smm(xsqMf,mfa,mfa$mf)
c(mean(mff$rtio[mff$state!="NY"]),weighted.mean(mff$rtio[mff$state!="NY"],mff$N[mff$state!="NY"])) # exclude NY since that difference was already stat insig

# 80% of state median incomes ~ stat for note in Methods
lta<-lmcmpr %>% filter(typ=="community") %>%
  inner_join(select(stcns,state,state_income)) %>% mutate(inc80=0.8*state_income*10^-3) %>%
  filter(income<inc80)
nrow(lta)
t.test(non_white~lmi,data=lta)

# export all stats
clnr<-function(z,ll) {
  a<-mutate(z,v0=round(v0,1),v1=round(v1,1)) %>% select(state,v0,v1,stt,pv) %>%
    mutate(stt=round(stt,1),pv=round(pv,2)) %>% mutate(pv=ifelse(pv==0,"~0",pv))
  colnames(a)<-c("state",paste(ll,c(1:4),sep=""))
  return(a)
} 
xpt<-clnr(wcInc,"inc") %>%
  inner_join(clnr(mutate(xsqRent,v0=v0*100,v1=v1*100),"rent")) %>%
  inner_join(clnr(mutate(xsqMf,v0=v0*100,v1=v1*100),"mf")) %>%
  inner_join(clnr(mutate(xsqRce,v0=v0*100,v1=v1*100),"nw"))
write.csv(xpt,"f5_numeric.csv",row.names=F)


