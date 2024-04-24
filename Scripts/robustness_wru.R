# robustness check for wru algorithm

#### DO NOT RUN
#### script provided for reference

library(tidyverse)
library(png)
library(cowplot)
library(grid)

base<-function() theme_light()+theme(axis.text.y=element_text(size=14,color='gray50'),axis.text.x=element_text(size=11,color='gray50'),axis.title=element_text(size=14,color='gray50',angle=0),axis.title.y=element_text(size=14,color='gray50',angle=0),axis.ticks=element_blank(),
                                     panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.title=element_text(size=14,face='bold',color='gray50',hjust=0.5),
                                     panel.border = element_blank(), axis.line=element_line(color='gray50'),legend.title=element_text(size=14,face='bold',color='gray50'),legend.text=element_text(size=14,color='gray50'))



# non-white ~ this is based on surname matches
# this is based on full sample
# sta community solar subscriber demographics data 
csmpl<-(as.data.frame(table(sta$state)) %>% rename(state=Var1) %>% filter(Freq>=100))$state
# rooftop solar data
# rfa rooftop solar adopter data
rsmpl<-(as.data.frame(table(rfa$state)) %>% rename(state=Var1) %>% filter(Freq>=100))$state
rft <- rfa %>% filter(state%in%rsmpl)
stz<-filter(sta,state%in%csmpl,state%in%rsmpl) # must have at least 100 on both sides
ssmpl<-sort(unique(stz$state))
rft<-filter(rft,state%in%ssmpl)
stN<-length(ssmpl)

# state demographics
rft<-rft %>% mutate(non_white=ifelse(pred.whi>pred.bla & pred.whi>pred.his & pred.whi>pred.asi & pred.whi>pred.oth,0,1))
stz<-stz %>% mutate(non_white=ifelse(pred.whi>pred.bla & pred.whi>pred.his & pred.whi>pred.asi & pred.whi>pred.oth,0,1)) 

# for sample size stats
ssz<-rbind(select(rft,surname) %>% mutate(typ="r"),select(stz,surname) %>% mutate(typ="c"))

# compile ~ restricted set requires surnames
rcmpr<-rbind(rft %>% select(state,non_white) %>% mutate(typ="rooftop"),
             filter(rft,surname!="") %>% select(state,non_white) %>% mutate(typ="rooftopR"),
             stz %>% select(state,non_white) %>% mutate(typ="community"),
             filter(stz,surname!="") %>% select(state,non_white) %>% mutate(typ="communityR")) # wilcox test automatically tests against the first factor, so "community" in this case
xsqRce<-list()
xsq<-function(st) {
  dta<-rcmpr %>% filter(state==st)
  xs<-chisq.test(dta$non_white[!(dta$typ%in%c("community","rooftop"))],dta$typ[!(dta$typ%in%c("community","rooftop"))])
  return(data.frame(state=st,pct0=mean(dta$non_white[dta$typ=="rooftop"]),pct1=mean(dta$non_white[dta$typ=="community"]),
                    pctR0=mean(dta$non_white[dta$typ=="rooftopR"]),pctR1=mean(dta$non_white[dta$typ=="communityR"]),
                    xsq=as.numeric(xs[1]),pv=as.numeric(xs[3])) %>%
           mutate(sig=ifelse(pct1>pct0 & pv<0.05,1,0)))
} 
for (i in 1:stN) xsqRce[[i]]<-xsq(ssmpl[i])
xsqRce<-do.call(rbind,xsqRce) %>% mutate(x=c(1:nrow(.))) 

xsqRce # OR here is significant, was not before, account for that in race figure
xsqRce$sig1<-ifelse(xsqRce$state=="ME",1,0)

rcFgr<-ggplot(xsqRce)+base()+geom_vline(xintercept=xsqRce$x,color='gray80',linetype='dotted')+
  ylab("% Non-White\nor Hispanic")+coord_cartesian(ylim=c(0,70))+
  geom_point(aes(x,pct0*100),color='navy',shape=16,size=6.5,alpha=0.9)+
  geom_point(aes(x,pctR0*100),color='navy',shape=15,size=6,alpha=0.9)+
  geom_point(aes(x,pct1*100,fill=factor(sig1)),color='darkorange',shape=23,size=6,alpha=0.9)+
  geom_point(aes(x,pctR1*100,fill=factor(sig)),color='darkorange',shape=22,size=6,alpha=0.9)+
  scale_fill_manual(values=c('white','darkorange'),guide="none")+
  scale_x_continuous(breaks=xsqRce$x,labels=xsqRce$state)+scale_y_continuous(breaks=seq(0,70,35))+
  theme(axis.title.x=element_blank())
lgg<-readPNG("legR1.png")

ggsave("si_robustness_race.jpg",
       plot=ggdraw()+draw_plot(rcFgr,0,0.1,1,0.9)+
         draw_grob(rasterGrob(lgg,interpolate=T),x=0.01,y=-0.03,width=0.87,height=0.18,hjust=-0.15),width=6,height=4)





