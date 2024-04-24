# prep file at base of all other scripts

#### DO NOT RUN
#### script provided as reference

library(tidyverse)
library(lubridate)
library(cowplot)
library(png)
library(cowplot)
library(grid)

lblblu<-rgb(8/255,48/255,106/255)
nrelblu<-rgb(0/255,112/255,180/255)
nvy<-rgb(26/255,71/255,111/255)
noX<-theme(axis.title.x=element_blank(),axis.text.x=element_blank())
noY<-theme(axis.title.y=element_blank(),axis.text.y=element_blank())
p25<-function(z) quantile(z,.25,na.rm=T)
p75<-function(z) quantile(z,.75,na.rm=T)
ant<-function(lb,x,y,sz,cl) annotate('text',x=x,y=y,label=lb,size=sz,color=cl)
gs<-function(x1,x2,y1,y2,cl,sz=0.3) geom_segment(x=x1,xend=x2,y=y1,yend=y2,color=cl,size=sz)

base<-function() theme_light()+theme(axis.text.y=element_text(size=7,color='gray50'),axis.text.x=element_text(size=5,color='gray50'),axis.title=element_text(size=7,color='gray50',angle=0),axis.title.y=element_text(size=7,color='gray50',angle=0),axis.ticks=element_blank(),
                                     panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.title=element_text(size=7,face='bold',color='gray50',hjust=0.5),
                                     panel.border = element_blank(), axis.line=element_line(color='gray50'),legend.title=element_text(size=7,face='bold',color='gray50'),legend.text=element_text(size=7,color='gray50'))

# sta represents community solar subscriber data
csmpl<-(as.data.frame(table(sta$state)) %>% rename(state=Var1) %>% filter(Freq>=100))$state
# rfa represents rooftop solar adopter data
rsmpl<-(as.data.frame(table(rfa$state)) %>% rename(state=Var1) %>% filter(Freq>=100))$state
rft <- rfa %>% filter(state%in%rsmpl)
stz<-filter(sta,state%in%csmpl,state%in%rsmpl) # must have at least 100 on both sides
ssmpl<-sort(unique(stz$state))
rft<-filter(rft,state%in%ssmpl)
stN<-length(ssmpl)

# state demographics
# stcns represents state census data

# non-white or hispanic variable
rft<-rft %>% mutate(non_white=ifelse(pred.whi>pred.bla & pred.whi>pred.his & pred.whi>pred.asi & pred.whi>pred.oth,0,1))
stz<-stz %>% mutate(non_white=ifelse(pred.whi>pred.bla & pred.whi>pred.his & pred.whi>pred.asi & pred.whi>pred.oth,0,1)) 

# compile
cmpr<-rbind(rft %>% select(state,income,dwell_type,rent_own,pred.whi,pred.bla,pred.his,pred.asi,pred.oth,non_white) %>% mutate(typ="rooftop"),
            stz %>% select(state,income,dwell_type,rent_own,pred.whi,pred.bla,pred.his,pred.asi,pred.oth,non_white) %>% mutate(typ="community")) # wilcox test automatically tests against the first factor, so "community" in this case
