# carveout robustness check

#### DO NOT RUN
#### script provided for reference

source("0_prep.R")

# stt represents state data
# wma deployment data from WoodMac
# gsw data from Groundswell

crva<-inner_join(wma,aggregate(LMI.Project.Capacity.KW.AC~state,data=gsw,sum) %>% mutate(lmiMW=LMI.Project.Capacity.KW.AC*10^-3)) %>%
  mutate(crv=lmiMW/cumulative_mw) %>% select(state,crv) %>% filter(state%in%c("MA")) %>%
  rbind(data.frame(state=c("CO","CT","MD"),
                   crv=c(0.05,0.2,0.1))) %>%
  filter(state%in%stz$state) # must have community solar data
crvsmpl<-sort(unique(crva$state))
crvN<-length(crvsmpl)
# states with carveouts, take out that bottom part + LMI incentives from rooftop
css<-stz %>% inner_join(crva) %>% 
  arrange(state,income) %>% mutate(i=1) %>% 
  mutate(j=ave(i,state,FUN=cumsum)) %>% 
  inner_join(aggregate(i~state,data=.,sum) %>% rename(sN=i)) %>%
  mutate(pos=j/sN) %>% mutate(lmi=ifelse(pos>=crv,0,1))

# rerun on that
# lmi incentive IDs
lrfta<-filter(rfa,id%in%lmi$id_attachmd)
rstrct<-filter(rft,!(id%in%lmi$id_attachmd),state%in%crvsmpl)

lmcmpr<-rbind(rstrct %>% select(state,income,dwell_type,rent_own,non_white) %>% mutate(typ="rooftop"),
            css %>% filter(lmi==0) %>% select(state,income,dwell_type,rent_own,non_white) %>% mutate(typ="community")) # wilcox test automatically tests against the first factor, so "community" in this case


##### income (one-sided Wilcox test)
wcx<-function(st) {
  dta<-lmcmpr %>% filter(state==st,!is.na(income))
  wc<-wilcox.test(dta$income~dta$typ,alternative="less")
  return(data.frame(state=st,med0=median(dta$income[dta$typ=="rooftop"]),med1=median(dta$income[dta$typ=="community"]),
                    w=wc[1],pv=wc[3],sig=ifelse(wc[3]<0.05,1,0)))
} 
wcInc<-list()
for (i in 1:crvN) wcInc[[i]]<-wcx(crvsmpl[i])
wcInc<-do.call(rbind,wcInc) %>% mutate(x=c(1:nrow(.))) %>%
  inner_join(select(stcns,state,state_income)) %>% mutate(state_income=state_income*10^-3)

##### categorical vars (X2 tests)
xsq<-function(dd,vv,st) {
  dta<-dd %>% mutate(vr=vv) %>% filter(state==st)
  xs<-chisq.test(dta$vr,dta$typ)
  return(data.frame(state=st,pct0=mean(dta$vr[dta$typ=="rooftop"]),pct1=mean(dta$vr[dta$typ=="community"]),
                    xsq=as.numeric(xs[1]),pv=as.numeric(xs[3])) %>%
           mutate(sig=ifelse(pct1>pct0 & pv<0.05,1,0)))
} 

# own_rent 
ora<-lmcmpr %>% filter(!(rent_own%in%c("","U"))) %>% mutate(rent=ifelse(rent_own%in%c("R","T"),1,0)) %>% select(state,typ,rent) 
c(mean(ora$rent[ora$typ=="rooftop"]),mean(ora$rent[ora$typ=="community"]))
xsqRent<-list()
for (i in 1:crvN) xsqRent[[i]]<-xsq(ora,ora$rent,crvsmpl[i])
xsqRent<-do.call(rbind,xsqRent) %>% mutate(x=c(1:nrow(.))) %>% inner_join(select(stcns,state,state_rent)) %>% rename(stshr=state_rent)

# dwell_type
mfa<-lmcmpr %>% filter(!(dwell_type%in%c("","P"))) %>% mutate(mf=ifelse(dwell_type%in%c("A","M"),1,0)) %>% select(state,typ,mf) 
c(mean(mfa$mf[mfa$typ=="rooftop"]),mean(mfa$mf[mfa$typ=="community"]))
xsqMf<-list()
for (i in 1:crvN) xsqMf[[i]]<-xsq(mfa,mfa$mf,crvsmpl[i])
xsqMf<-do.call(rbind,xsqMf) %>% mutate(x=c(1:nrow(.))) %>% inner_join(select(stcns,state,state_mf)) %>% rename(stshr=state_mf)


# non-white 
xsqRce<-list()
for (i in 1:crvN) xsqRce[[i]]<-xsq(lmcmpr,lmcmpr$non_white,crvsmpl[i])
xsqRce<-do.call(rbind,xsqRce) %>% mutate(x=c(1:nrow(.))) #%>% inner_join(select(stcns,state,state_mf)) %>% rename(stshr=state_mf)

# identify key values below carveout
lma<-as.data.frame(css %>% filter(lmi==1) %>% 
                     mutate(rent=ifelse(is.na(rent_own),NA,ifelse(rent_own%in%c("R","T"),1,0)),
                            mf=ifelse(is.na(dwell_type),NA,ifelse(dwell_type%in%c("A","M"),1,0))) %>%
                     group_by(state) %>%
                     summarize(income=median(income),rent=mean(rent),mf=mean(mf),non_white=mean(non_white))) %>%
  mutate(x=c(1,2,3),sig=c(0,0,1)) # sig only applies for race

# figure
incFgr<-ggplot(wcInc)+base()+geom_vline(xintercept=wcInc$x,color='gray80',linetype='dotted')+
  ylab("Median\nIncome\n(x$1,000)")+coord_cartesian(xlim=c(0.5,3.5),ylim=c(0,180))+
  geom_point(data=lma,aes(x,income),shape=25,color='darkorange',fill='darkorange',size=5.5)+
  geom_point(aes(x,med0),color='navy',shape=16,size=6.5)+
  geom_point(aes(x,med1,fill=factor(sig)),color='darkorange',shape=24,size=5.5)+
  scale_fill_manual(values=c("white","darkorange"),guide="none")+ # set the first to white if any instances are insignificant
  scale_x_continuous(breaks=wcInc$x,labels=wcInc$state)+
  scale_y_continuous(breaks=c(0,90,180))+
  theme(axis.title.x=element_blank(),axis.text.x=element_text(size=14))
# figure for categorical variables
fgr<-function(dd,ttl,yL) {
  if (length(unique(dd$sig))==2) grd<-c('white','darkorange')
  else grd<-'darkorange'
  if (ttl=="% Renters") lta<-lma %>% mutate(y=rent*100)
  else lta<-lma %>% mutate(y=mf*100)
  a<-ggplot(dd)+base()+geom_vline(xintercept=dd$x,color='gray80',linetype='dotted')+
    ggtitle(ttl)+ylab("%")+coord_cartesian(xlim=c(0.5,3.5),ylim=c(0,yL))+
    geom_point(aes(x,pct0*100),color='navy',shape=16,size=6.5)+
    geom_point(aes(x,pct1*100,fill=factor(sig)),color='darkorange',shape=24,size=5.5)+
    geom_point(data=lta,aes(x,y),shape=25,color='darkorange',fill='darkorange',size=5)+
    scale_fill_manual(values=grd,guide="none")+
    scale_x_continuous(breaks=dd$x,labels=dd$state)+scale_y_continuous(breaks=seq(0,yL,yL/2))+
    theme(axis.title.x=element_blank(),axis.text.x=element_text(size=14))
  return(a)
}
rcFgr<-ggplot(xsqRce)+base()+geom_vline(xintercept=xsqRce$x,color='gray80',linetype='dotted')+
  ylab("%")+coord_cartesian(xlim=c(0.5,3.5),ylim=c(0,60))+
  geom_point(aes(x,pct0*100),color='navy',shape=16,size=6.5)+
  geom_point(aes(x,pct1*100,fill=factor(sig)),color='darkorange',shape=24,size=5.5)+
  geom_point(data=lma,aes(x,non_white*100),color='darkorange',fill='white',size=5,shape=25)+
  scale_fill_manual(values=c('white','darkorange'),guide="none")+
  scale_x_continuous(breaks=xsqRce$x,labels=xsqRce$state)+scale_y_continuous(breaks=seq(0,60,30))+
  theme(axis.title.x=element_blank(),axis.text.x=element_text(size=14))

lg7<-readPNG("~/Desktop/Community/Figures/legend/leg7.png")
ggsave("~/Desktop/Community/Figures/si_carveouts.jpg",
       plot=ggdraw()+draw_plot(
         plot_grid(incFgr+ggtitle("Median Income (x$1,000)")+
                     ylab("$"),
                   fgr(xsqRent,"% Renters",90),
                   fgr(xsqMf,"% in Multifamily Buildings",80),
                   rcFgr+ggtitle("% Non-White or Hispanic"),
                   nrow=2),0,0.1,1,0.9)+
         draw_grob(rasterGrob(lg7,interpolate=T),x=0.01,y=0,width=0.85,height=0.1,hjust=-0.1),width=8,height=8)
