
library(dplyr)
vocation<-with_flags%>%filter(TYPE==3)%>%filter(MSTATE=="VA")
names(vocation)
closed_vocation<-vocation%>%select(closed14_15,closed_any)

schools_per_zip<-with_flags%>%group_by(MZIP)%>%summarise(n())%>%
  arrange(desc(`n()`))

BIE<-with_flags%>%filter(BIES==1)
summary(as.numeric(with_flags$RECONSTY))
recon<-with_flags%>%filter(RECONSTF==1)
magnet<-with_flags%>%filter(MAGNET==1)
charter<-with_flags%>%filter(CHARTR==1)
shared<-with_flags%>%filter(SHARED==1)
test<-shared%>%filter(TYPE!=3)
ug<-with_flags%>%filter(UG>0)
summary(ug$closed_any)
summary(ug$UG)
pre_k<-with_flags%>%filter(GSLO=="PK")

virtual<-with_flags%>%filter(VIRTUALSTAT==1)
summary(factor(with_flags$NSLPSTATUS))
summary(pre_k$closed_any)
summary(recon$closed_any)
summary(with_flags$closed_any)
summary(recon$RECONSTY%>%as.numeric)
lat_missing<-with_flags%>%filter(is.na(LATCOD))
sum(vocation$closed_any,na.rm=TRUE)
# #save with flags as rdata
# save(with_flags,file="2013-14 school data with closure flags.RData")
# names(with_flags)
# with_flags%>%
#   group_by(MZIP)%>%
#   summarise(closed_any=sum(closed_any))%>%
#   arrange(desc(closed_any))
# test_zip<-with_flags%>%
#   filter(MZIP==78114)
# summary(with_flags$FRELCH)

# #check out flag counts
# sum(with_flags$closed14_15)
# sum(with_flags$closed15_16)
# sum(with_flags$closed16_17)
# sum(with_flags$closed17_18)
# sum(with_flags$closed_any)
