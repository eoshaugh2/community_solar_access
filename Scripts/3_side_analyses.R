# side analyses

#### DO NOT RUN
#### script provided for reference

source("0_prep.R")


#### sample MW share

#### state breakouts for sample sizes
sttz<-as.data.frame(cmpr %>% mutate(n=1) %>% group_by(state,typ) %>% summarize(N=sum(n)))
xpt<-inner_join(filter(sttz,typ=="community") %>% rename(community=N) %>% select(-typ),
                filter(sttz,typ=="rooftop") %>% rename(rooftop=N) %>% select(-typ))
write.csv(xpt,"state_samples.csv",row.names=F)

#### summary stats
# full sample and by customer type
sma<-cmpr %>% mutate(rent=ifelse(rent_own%in%c("","U"),NA,ifelse(rent_own%in%c("R","T"),1,0)),
                     mf=ifelse(dwell_type%in%c("","P"),NA,ifelse(dwell_type%in%c("A","M"),1,0))) %>%
  mutate(i=income,r=rent,m=mf,w=non_white)
stts<-as.data.frame(sma %>% 
                      summarize(income=mean(income),sdIncome=sd(i),
                                rent=mean(rent*100,na.rm=T),sdRent=sd(r*100,na.rm=T),
                                mf=mean(mf*100,na.rm=T),sdMf=sd(m*100,na.rm=T),
                                nw=mean(non_white*100),sdNw=sd(non_white*100)))
gtts<-as.data.frame(sma %>% group_by(typ) %>%
                      summarize(income=mean(income),sdIncome=sd(i),
                                rent=mean(rent*100,na.rm=T),sdRent=sd(r*100,na.rm=T),
                                mf=mean(mf*100,na.rm=T),sdMf=sd(m*100,na.rm=T),
                                nw=mean(non_white*100),sdNw=sd(non_white*100)))
smma<-matrix(as.numeric(stts),ncol=2,byrow=T) %>% round(.,1)
gmma<-cbind(matrix(as.numeric(gtts[1,2:9]),ncol=2,byrow=T),
            matrix(as.numeric(gtts[2,2:9]),ncol=2,byrow=T)) %>% round(.,1)
sttz<-cbind(data.frame(metric=c("Income","% rent","% multifamily","% non-White or Hispanic")),
            as.data.frame(cbind(smma,gmma)))
colnames(sttz)<-c("metric","allMean","allSd","commMean","commSd","roofMean","roofSd")
write.csv(sttz,"summary_stats.csv",row.names=F)
       

#### correlation table
cra<-sma %>% filter(!is.na(rent),!is.na(mf)) %>% inner_join(stcns) %>%
  mutate(lmi=ifelse(income<(state_income*10^-3),1,0))
crz<-as.matrix(select(cra,lmi,rent,mf,non_white))
tsts<-function(v1,v2) paste(round(mean(crz[,v2][crz[,v1]==1])*100,1)," (",round(cor(crz[,v1],crz[,v2]),2),")",sep="")
# go through every combination
crza<-data.frame(var=c("%LMI","%rent","%multifamily","%non-white or Hispanic"))
for (i in 1:4) crza$fs[i]<-round(mean(crz[,i])*100,1)
for (i in 1:4) crza$lmi[i]<-tsts(1,i)
for (i in 1:4) crza$rent[i]<-tsts(2,i)
for (i in 1:4) crza$mf[i]<-tsts(3,i)
for (i in 1:4) crza$non_white[i]<-tsts(4,i)
write.csv(crza,"correlation_table.csv",row.names=F)


#### race breakout
rla<-list()
for (i in 1:nrow(cmpr)) rla[[i]]<-as.numeric(cmpr[i,5:9])
for (i in 1:nrow(cmpr)) cmpr$maxRace[i]<-max(rla[[i]])
rca<-cmpr %>% 
  mutate(N=1,race=ifelse(pred.bla>=maxRace,"b",
                     ifelse(pred.his>=maxRace,"h",
                            ifelse(pred.asi>=maxRace,"a",
                                   ifelse(pred.whi>=maxRace,"w","o"))))) %>%
  mutate(asi=ifelse(race=="a",1,0),hisp=ifelse(race=="h",1,0),blk=ifelse(race=="b",1,0))
rcca<-as.data.frame(rca %>% mutate(n=1) %>% group_by(typ,race) %>%
                      summarize(n=sum(n))) %>%
  inner_join(as.data.frame(table(rca$typ)) %>% rename(typ=Var1,N=Freq)) %>%
  mutate(share=n/N)

# run the tests
xsq<-function(vv,st) {
  dta<-rca %>% mutate(vr=vv) %>% filter(state==st)
  xs<-chisq.test(dta$vr,dta$typ)
  return(data.frame(state=st,v0=mean(dta$vr[dta$typ=="rooftop"]),v1=mean(dta$vr[dta$typ=="community"]),
                    stt=as.numeric(xs[1]),pv=as.numeric(xs[3])) %>%
           mutate(sig=ifelse(v1>v0 & pv<0.05,1,0)))
} 
ssmpl<-sort(unique(cmpr$state))
stN<-length(ssmpl)
xsqAsian<-list()
for (i in 1:stN) xsqAsian[[i]]<-xsq(rca$asi,ssmpl[i])
xsqAsian<-do.call(rbind,xsqAsian) %>% rename(aSig=sig) %>% select(state,aSig,v0,v1,stt,pv)
xsqBlk<-list()
for (i in 1:stN) xsqBlk[[i]]<-xsq(rca$blk,ssmpl[i])
xsqBlk<-do.call(rbind,xsqBlk) %>% rename(bSig=sig) %>% select(state,bSig,v0,v1,stt,pv)
xsqHis<-list()
for (i in 1:stN) xsqHis[[i]]<-xsq(rca$his,ssmpl[i])
xsqHis<-do.call(rbind,xsqHis) %>% rename(hSig=sig) %>% select(state,hSig,v0,v1,stt,pv)

# export stats
rclnr<-function(z,ll) {
  a<-select(z,state,v0,v1,stt,pv) %>%
    mutate(v0=round(v0*100,1),v1=round(v1*100,1),stt=round(stt,1),pv=round(pv,2)) %>%
    mutate(pv=ifelse(pv==0,"~0",pv))
  colnames(a)<-c("state",paste(ll,c(1:4),sep=""))
  return(a)
}
rxpt<-rclnr(xsqAsian,"a") %>% inner_join(rclnr(xsqBlk,"b")) %>% inner_join(rclnr(xsqHis,"h"))
write.csv(rxpt,"f2_numeric.csv",row.names=F)


### quick stat: % of LMI community solar adopters that rent or live in MF
qta<-cmpr %>% inner_join(select(stcns,state,state_income)) %>% 
  mutate(lmi=ifelse(income<(state_income*10^-3),1,0))
aggregate(lmi~typ,data=qta,mean)
ora<-qta %>% filter(lmi==1,!(rent_own%in%c("","U"))) %>% mutate(rent=ifelse(rent_own%in%c("R","T"),1,0)) 
aggregate(rent~typ,data=ora,mean)
mfa<-qta %>% filter(lmi==1,!(dwell_type%in%c("","P"))) %>% mutate(mf=ifelse(dwell_type%in%c("A","M"),1,0)) 
aggregate(mf~typ,data=mfa,mean)
tta<-qta %>% filter(lmi==1,!(dwell_type%in%c("","P")),!(rent_own%in%c("","U"))) %>% mutate(rent=ifelse(rent_own%in%c("R","T"),1,0),mf=ifelse(dwell_type%in%c("A","M"),1,0)) %>%
  mutate(rent_or_mf=ifelse(rent==1,1,ifelse(mf==1,1,0)))
aggregate(rent_or_mf~typ,data=tta,mean)


### leasing
# tpo represents TPO data
rff<-inner_join(rft,tpo)
tpps<-aggregate(tpo~state,data=rff,mean) %>% filter(tpo>0,tpo<1) # TPO states
tcmpr<-rbind(filter(rff,tpo==0) %>% select(state,income,dwell_type,rent_own,pred.whi,pred.bla,pred.his,pred.asi,pred.oth,non_white) %>% mutate(typ="roof_own"),
             filter(rff,tpo==1) %>% select(state,income,dwell_type,rent_own,pred.whi,pred.bla,pred.his,pred.asi,pred.oth,non_white) %>% mutate(typ="roof_tpo"),
             stz %>% select(state,income,dwell_type,rent_own,pred.whi,pred.bla,pred.his,pred.asi,pred.oth,non_white) %>% mutate(typ="community")) %>% # wilcox test automatically tests against the first factor, so "community" in this case
  filter(state%in%tpps$state)
stN<-length(unique(tcmpr$state))
ssmpl<-sort(unique(tcmpr$state))
sze<-as.data.frame(table(tcmpr$state)) %>% rename(state=Var1,N=Freq)
wcx<-function(st,ttp) {
  dta<-tcmpr %>% filter(state==st,typ%in%ttp)
  wc<-wilcox.test(dta$income~dta$typ,alternative="less")
  return(data.frame(state=st,v0=median(dta$income[dta$typ==ttp[1]]),v1=median(dta$income[dta$typ=="community"]),
                    stt=as.numeric(wc[1]),pv=as.numeric(wc[3]),sig=ifelse(wc[3]<0.05,1,0)))
} 
wcIncr<-function(z) {
  l<-list()
  for (i in 1:stN) l[[i]]<-wcx(ssmpl[i],c(z,"community"))
  ll<-do.call(rbind,l) %>% mutate(x=c(1:nrow(.))) %>%
    inner_join(select(stcns,state,state_income)) %>% mutate(state_income=state_income*10^-3) %>%
    inner_join(sze) %>% mutate(pdiff=(v1-v0)/v0)
  return(ll)
}
wcTpo<-wcIncr("roof_tpo")
wcOwn<-wcIncr("roof_own")

xsq<-function(dd,vv,st,tpp) {
  dta<-dd %>% mutate(vr=vv) %>% filter(state==st,typ%in%tpp)
  xs<-chisq.test(dta$vr,dta$typ)
  return(data.frame(state=st,v0=mean(dta$vr[dta$typ==tpp[1]]),v1=mean(dta$vr[dta$typ=="community"]),
                    stt=as.numeric(xs[1]),pv=as.numeric(xs[3])) %>%
           mutate(sig=ifelse(v1>v0 & pv<0.05,1,0)))
} 
ora<-tcmpr %>% filter(!(rent_own%in%c("","U"))) %>% mutate(rent=ifelse(rent_own%in%c("R","T"),1,0)) %>% select(state,typ,rent) 
mfa<-tcmpr %>% filter(!(dwell_type%in%c("","P"))) %>% mutate(mf=ifelse(dwell_type%in%c("A","M"),1,0)) %>% select(state,typ,mf) 

xsqF<-function(dd,dv,tp) {
  l<-list()
  for (i in 1:stN) l[[i]]<-xsq(dd,dv,ssmpl[i],c(tp,"community"))
  ll<-do.call(rbind,l) %>% mutate(x=c(1:nrow(.))) %>% inner_join(select(stcns,state,state_rent)) %>% rename(stshr=state_rent) %>%
    inner_join(sze) %>% mutate(pdiff=(v1-v0)/v0)
  return(ll)
}
rnTpo<-xsqF(ora,ora$rent,"roof_tpo")
rnOwn<-xsqF(ora,ora$rent,"roof_own")
mfTpo<-xsqF(mfa,mfa$mf,"roof_tpo")
mfOwn<-xsqF(mfa,mfa$mf,"roof_own")
rcTpo<-xsqF(tcmpr,tcmpr$non_white,"roof_tpo")
rcOwn<-xsqF(tcmpr,tcmpr$non_white,"roof_own")

tpoCln<-function(z1,z2,pct) {
  a<-inner_join(select(z1,state,v1,v0) %>% rename(tpo=v0,comm=v1),
             select(z2,state,v0) %>% rename(own=v0)) %>%
    mutate(x=c(1:nrow(.)),sig=z1$sig)
  if (pct==0) b<-a
  else b<-a %>% mutate(x=c(1:nrow(.)),sig=z1$sig,comm=comm*100,own=own*100,tpo=tpo*100)
  return(b)
}
f1<-tpoCln(wcTpo,wcOwn,0)
f2<-tpoCln(rnTpo,rnOwn,1) 
f3<-tpoCln(mfTpo,mfOwn,1) 
f4<-tpoCln(rcTpo,rcOwn,1) 

# export numeric results (only need the rooftop estimates)
tpClnr<-function(z1,z2,ll) {
  a<-select(z1,state,v0,stt,pv) %>%
    mutate(v0=round(v0,1),stt=round(stt,1),pv=round(pv,2)) %>%
    mutate(pv=ifelse(pv==0,"~0",pv)) %>%
    inner_join(mutate(z2,own=round(v0,1)) %>% select(state,own)) %>%
    select(state,v0,own,stt,pv)
  colnames(a)<-c("state",paste(ll,c(1:4),sep=""))
  return(a)
}
tpXpt<-tpClnr(wcTpo,wcOwn,"i") %>%
  inner_join(tpClnr(mutate(rnTpo,v0=v0*100),mutate(rnOwn,v0=v0*100),"r")) %>% 
  inner_join(tpClnr(mutate(mfTpo,v0=v0*100),mutate(mfOwn,v0=v0*100),"m"))
write.csv(tpXpt,"f4_numeric.csv",row.names=F)

