# file to generate all figures in main text

#### this script can be run

library(tidyverse)
library(cowplot)
library(png)
library(grid)

base<-function() theme_light()+theme(axis.text.y=element_text(size=7,color='gray50'),axis.text.x=element_text(size=5,color='gray50'),axis.title=element_text(size=7,color='gray50',angle=0),axis.title.y=element_text(size=7,color='gray50',angle=0),axis.ticks=element_blank(),
                                     panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.title=element_text(size=7,face='bold',color='gray50',hjust=0.5),
                                     panel.border = element_blank(), axis.line=element_line(color='gray50'),legend.title=element_text(size=7,face='bold',color='gray50'),legend.text=element_text(size=7,color='gray50'))
stcns <- read.csv("Data/state_acs_demographics_data.csv") # state-level vars for certain figures
ant<-function(xx,yy,ll) annotate("text",x=xx,y=yy,label=ll,color='gray50',size=2.5)

##### Figure 1
f1<-read.csv("Data/f1_numeric.csv") %>% mutate(x=c(1:nrow(.))) %>% inner_join(stcns)
lgg<-readPNG("Figures/Legends/leg1.png")
ita<-f1 %>% mutate(sig=ifelse(inc4=="~0" & inc2<inc1,1,0))
incFgr<-ggplot(ita)+base()+geom_vline(xintercept=ita$x,color='gray80',linetype='dotted')+
  ylab("Median\nIncome\n(x$1,000)")+coord_cartesian(ylim=c(0,180))+
  geom_point(aes(x,inc1),color='navy',shape=16,size=2.5)+
  geom_point(aes(x,median_income*10^-3),shape=17,color='darkcyan',size=2)+
  geom_point(aes(x,inc2,fill=factor(sig)),color='darkorange',shape=23,size=2)+
  geom_point(x=1,y=180,color='white',size=3)+ant(1,180,"a.")+
  scale_fill_manual(values=c("white","darkorange"),guide="none")+ # set the first to white if any instances are insignificant
  scale_x_continuous(breaks=ita$x,labels=ita$state)+
  scale_y_continuous(breaks=c(0,90,180))+
  theme(axis.title.x=element_blank())
# figure for categorical variables
rnt<-f1 %>% mutate(sig=ifelse(rent4=="~0" & rent2>rent1,1,0),stshr=pct_rent,v0=rent1,v1=rent2)
mfa<-f1 %>% mutate(sig=ifelse(mf4=="~0" & mf2>mf1,1,0),stshr=pct_rent,v0=mf1,v1=mf2)
rca<-f1 %>% mutate(sig=ifelse((nw4=="~0" | nw4=="0.02") & nw2>nw1,1,0),v0=nw1,v1=nw2,stshr=NA)
fgr<-function(dd,ttl,yM,lbl) {
  if (length(unique(dd$sig))==2) grd<-c('white','darkorange')
  else grd<-'darkorange'
  a<-ggplot(dd)+base()+geom_vline(xintercept=dd$x,color='gray80',linetype='dotted')+
    ggtitle(ttl)+ylab("%")+coord_cartesian(ylim=c(0,yM))+
    geom_point(aes(x,v0),color='navy',shape=16,size=2.5)+
    geom_point(aes(x,stshr*100),color='darkcyan',shape=17,size=2)+
    geom_point(aes(x,v1,fill=factor(sig)),color='darkorange',shape=23,size=2)+
    geom_point(x=1,y=yM,color='white',size=3)+ant(1,y=yM,lbl)+
    scale_fill_manual(values=grd,guide="none")+
    scale_x_continuous(breaks=dd$x,labels=dd$state)+scale_y_continuous(breaks=seq(0,80,20))+
    theme(axis.title.x=element_blank())
  return(a)
}
ggsave("Figures/figure1.pdf", ### warnings are expected
       plot=ggdraw()+draw_plot(
         plot_grid(incFgr+ggtitle("Median Income (x$1,000)")+
                     ylab("$"),
                   fgr(rnt,"% Renters",60,"b."),
                   fgr(mfa,"% in Multifamily Buildings",80,"c."),
                   fgr(rca,"% Non-White or Hispanic",70,"d."),nrow=2),0,0.1,1,0.9)+
         draw_grob(rasterGrob(lgg,interpolate=T),x=0.01,y=0.03,width=0.7,height=0.1,hjust=-0.2),width=88,height=88,units="mm")



##### Figure 2
f2<-read.csv("Data/f2_numeric.csv") %>% mutate(x=c(1:nrow(.))) %>%
  mutate(asig=ifelse(a4%in%c("~0","0.03") & a2>a1,1,0),
         bsig=ifelse(b4%in%c("~0","0.03") & b2>b1,1,0),
         hsig=ifelse(h4%in%c("~0","0.01") & h2>h1,1,0))

# figure
fgr<-function(ss,rr1,rr2,ttl,yM,lb) {
  dta<-f2 %>% mutate(sg=ss,r0=rr1,r1=rr2) 
  ff<-ggplot(dta)+base()+ggtitle(ttl)+coord_cartesian(ylim=c(0,yM))+
    geom_vline(xintercept=c(1:11),color='gray80',linetype='dotted')+
    geom_point(aes(x,r0),shape=16,color='navy',size=3)+
    geom_point(aes(x,r1,fill=factor(sg)),color='darkorange',shape=23,size=3)+
    scale_fill_manual(values=c('white',"darkorange"),guide="none")+
    geom_point(x=1,y=yM,color='white')+ant(1,yM,lb)+
    scale_x_continuous(breaks=c(1:11),labels=f2$state)+
    ylab("%")+scale_y_continuous(breaks=c(0,yM/2,yM))+
    theme(axis.title.x=element_blank(),axis.text.x=element_text(size=5))
  return(ff)
}
lgg<-readPNG("Figures/Legends/leg2.png")
nY<-theme(axis.title.y=element_blank())
ggsave("Figures/figure2.pdf",
       plot=ggdraw()+draw_plot(
         plot_grid(fgr(f2$asig,f2$a1,f2$a2,"% Asian/Asian American",14,"a."),
                   fgr(f2$bsig,f2$b1,f2$b2,"% Black",60,"b.")+nY,
                   fgr(f2$hsig,f2$h1,f2$h2,"% Hispanic",30,"c.")+nY,nrow=1,rel_widths=c(17,16,16)),0,0.1,1,0.9)+
         draw_grob(rasterGrob(lgg,interpolate=T),x=0.01,y=0,width=0.8,height=0.15,hjust=-0.15),width=180,height=37,units="mm")


##### Figure 3
f3<-read.csv("Data/f3_numeric.csv") %>% mutate(x=c(1:4))
lbz<-data.frame(x=c(1:4),y1=c(33,14,12,-32),l1=c(paste("ß=",round(f3$Estimate[1],2),sep=""),round(f3$Estimate[2:4],2)),
                y2=c(28,9.5,7.5,-36.5),l2=c(paste("SE=(",round(f3$Std..Error[1],2),")",sep=""),paste("(",round(f3$Std..Error[2:4],2),")",sep="")))

# figure
ggsave("Figures/figure3.pdf",
       plot=ggplot(f3)+geom_hline(yintercept=0,color='gray50',linetype='dotted')+
         base()+geom_col(aes(x,aprx),fill='cadetblue3',width=0.7)+
         coord_cartesian(xlim=c(0.5,4.5),ylim=c(-40,40))+
         scale_y_continuous(breaks=seq(-40,40,20))+
         geom_segment(aes(x=x,xend=x,y=lo,yend=hi),color='gray50')+
         geom_segment(aes(x=x-0.05,xend=x+0.05,y=lo,yend=lo),color='gray50')+
         geom_segment(aes(x=x-0.05,xend=x+0.05,y=hi,yend=hi),color='gray50')+
         geom_text(data=lbz,aes(x,y1,label=l1),size=2,color='gray30')+
         geom_text(data=lbz,aes(x,y2,label=l2),size=2,color='gray30')+
         ylab("Adjusted\nLogit\nCoefficient\n(~Percentage\nPoints)")+
         xlab("Demographic Factor")+scale_x_continuous(breaks=c(1:4),labels=f3$fctr)+
         theme(axis.text.x=element_text(size=7)),width=88,height=50,units="mm")



##### Figure 4
f4<-read.csv("Data/f4_numeric.csv") %>% mutate(x=c(1:nrow(.))) %>% # these are the two rooftop vals: lessees and owners
  inner_join(select(f1,state,inc2,rent2,mf2,nw2) %>% rename(ci=inc2,cr=rent2,cm=mf2,cnw=nw2)) %>%
  mutate(isig=ifelse(i4%in%c("~0","0.01") & ci<i1,1,0),
         rsig=ifelse(r4%in%c("~0") & cr>r1,1,0),
         msig=ifelse(m4%in%c("~0") & cm>m1,1,0),
         nwsig=ifelse(nw4%in%c("~0") & cnw>nw1,1,0))

fgr<-function(ss,rl,ro,cs,ttl,yl,yM,lb) {
  dd<-f4 %>% mutate(sig=ss,tpo=rl,own=ro,comm=cs)
  if (length(unique(dd$sig))==2) grd<-c("white","darkorange")
  else {
    if (unique(dd$sig)==1) grd<-c("darkorange","darkorange")
    else grd<-c("white","white")
  } 
  a<-ggplot(dd)+base()+ggtitle(ttl)+
    geom_vline(xintercept=dd$x,color='gray80',linetype='dotted')+
    ylab(yl)+coord_cartesian(xlim=c(0.5,5.5),ylim=c(0,yM))+
    geom_point(aes(x,comm,fill=factor(sig)),color='darkorange',shape=23,size=3)+
    geom_point(aes(x,tpo),shape=17,color='navy',size=2.5,alpha=0.8)+
    geom_point(aes(x,own),color='navy',shape=16,size=3,alpha=0.8)+
    geom_point(x=5,y=yM,color='white')+ant(5,yM,lb)+
    scale_fill_manual(values=grd,guide="none")+ # set the first to white if any instances are insignificant
    scale_x_continuous(breaks=dd$x,labels=dd$state)+
    scale_y_continuous(breaks=c(0,yM/2,yM))+
    theme(axis.title.x=element_blank(),plot.title=element_text(size=7))
  return(a)
}
lgg<-readPNG("Figures/Legends/leg4.png")
ggsave("Figures/figure4.pdf",
       plot=
         ggdraw()+draw_plot(
           plot_grid(fgr(f4$isig,f4$i1,f4$i2,f4$ci,"Median Income ($1,000)","$",190,"a."),
                     fgr(f4$rsig,f4$r1,f4$r2,f4$cr,"% Renters","%",38,"b."),
                     fgr(f4$msig,f4$m1,f4$m2,f4$cm,"% Multifamily","%",80,"c."),
                     fgr(f4$nwsig,f4$nw1,f4$nw2,f4$cnw,"% Non-White or Hispanic","%",80,"d."),nrow=1),0,0.12,1,0.9)+
         draw_grob(rasterGrob(lgg,interpolate=T),x=0.01,y=-0.0,width=0.7,height=0.12,hjust=-0.25),width=180,height=45,units="mm")




##### Figure 5
f5<-read.csv("Data/f5_numeric.csv") %>% mutate(x=c(1:nrow(.))) # 1 = rooftop, 2 = non-participants, 3 = participants
ita<-f5 %>% mutate(v1=inc1,v2=inc2,v3=inc3,sig=ifelse(inc5=="~0" & inc2<inc1,1,0))
rnt<-f5 %>% mutate(v1=rent1,v2=rent2,v3=rent3,sig=1)
mfa<-f5 %>% mutate(v1=mf1,v2=mf2,v3=mf3,sig=1)
rca<-f5 %>% mutate(v1=nw1,v2=nw2,v3=nw3,sig=ifelse(nw2>nw1,1,0))
# figure
fgr<-function(dd,ttl,yL,lb) {
  if (length(unique(dd$sig))==2) grd<-c('white','darkorange')
  else grd<-'darkorange'

  a<-ggplot(dd)+base()+geom_vline(xintercept=dd$x,color='gray80',linetype='dotted')+
    ggtitle(ttl)+ylab("%")+coord_cartesian(xlim=c(0.5,3.5),ylim=c(0,yL))+
    geom_point(aes(x,v1),color='navy',shape=16,size=3)+
    geom_point(aes(x,v2,fill=factor(sig)),color='darkorange',shape=22,size=2.5)+
    geom_point(aes(x,v3),shape=25,color='darkorange',fill='darkorange',size=2.5)+
    geom_point(x=3,y=yL,color='white')+
    annotate("text",x=3,y=yL,label=lb,color='gray50',size=2.8)+
    scale_fill_manual(values=grd,guide="none")+
    scale_x_continuous(breaks=dd$x,labels=dd$state)+scale_y_continuous(breaks=seq(0,yL,yL/2))+
    theme(axis.title.x=element_blank(),axis.text.x=element_text(size=7))
  return(a)
}

lgg<-readPNG("Figures/Legends/leg5.png")
ggsave("Figures/figure5.pdf",
       plot=ggdraw()+draw_plot(
         plot_grid(fgr(ita,"Median Income (x$1,000)",180,"a.")+ylab("$"),
                   fgr(rnt,"% Renters",30,"b."),
                   fgr(mfa,"% in Multifamily Buildings",50,"c."),
                   fgr(rca,"% Non-White or Hispanic",60,"d."),
                   nrow=2),0,0.1,1,0.9)+
         draw_grob(rasterGrob(lgg,interpolate=T),x=0.01,y=0,width=0.85,height=0.11,hjust=-0.1),width=88,height=88,units="mm")

  

