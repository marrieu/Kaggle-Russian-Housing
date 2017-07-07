

dtotal$YrMonth<-ifelse(month(dtotal$timestamp)>9,paste(year(dtotal$timestamp), month(dtotal$timestamp),sep=''),paste(year(dtotal$timestamp),"0", month(dtotal$timestamp),sep=''))

dtotal$YrMonth<-as.numeric(dtotal$YrMonth)

dtotal$YrMonth<-dtotal$YrMonth-min(dtotal$YrMonth)

dtotal$YrMonth<-ifelse(dtotal$YrMonth>4,dtotal$YrMonth-89,dtotal$YrMonth)
dtotal$YrMonth<-ifelse(dtotal$YrMonth>15,dtotal$YrMonth-89,dtotal$YrMonth)
dtotal$YrMonth<-ifelse(dtotal$YrMonth>26,dtotal$YrMonth-89,dtotal$YrMonth)
dtotal$YrMonth<-ifelse(dtotal$YrMonth>37,dtotal$YrMonth-89,dtotal$YrMonth)
dtotal$YrMonth<-ifelse(dtotal$YrMonth>48,dtotal$YrMonth-89,dtotal$YrMonth)

dtotal[!is.na(dtotal$price_doc_old),] %>% 
  group_by(YrMonth) %>% 
  summarize(mean_build_price=mean(price_doc_old)) %>%
  ggplot(aes(x=YrMonth, y=mean_build_price))+
  geom_point(color="darkred",size=3)+
  geom_smooth(method="lm",color='darkgrey')+
  scale_y_continuous(labels = dollar)+
  labs(x="Year",y="Mean Price",title="Price per Year relationship")+
  theme_minimal()

dtotal[!is.na(dtotal$price_doc),] %>% 
  group_by(YrMonth) %>% 
  summarize(mean_build_price=mean(price_doc)) %>%
  ggplot(aes(x=YrMonth, y=mean_build_price))+
  geom_point(color="darkred",size=3)+
  geom_smooth(method="lm",color='darkgrey')+
  scale_y_continuous(labels = dollar)+
  labs(x="Year",y="Mean Price",title="Price per Year relationship")+
  theme_minimal()

dtotal[!is.na(dtotal$price_doc),] %>% 
  group_by(YrMonth) %>% 
  summarize(mean_build_price=mean(price_doc)) %>%
  ggplot(aes(x=YrMonth, y=mean_build_price))+
  geom_line(color="darkred",size=3)+
  geom_smooth(method="lm",color='darkgrey')+
  scale_y_continuous(labels = dollar)+
  labs(x="Year",y="Mean Price",title="Price per Year relationship")+
  theme_minimal()

y<-lm(price_doc~YrMonth,data=dtotal)



  ggplot(aes(x=timestamp, y=(price_doc-mean(price_doc,na.rm =TRUE))/var(price_doc,na.rm =TRUE)), data=dtotal)+
  geom_point(color="darkred",size=1)+
 # geom_point(aes(x=timestamp, y=(price_doc-mean(price_doc,na.rm =TRUE))/var(price_doc,na.rm =TRUE)),color="blue",size=1)+
  geom_smooth(method="lm",color='darkgrey')+
  labs(x="Year",y="Mean Price",title="Price per Year relationship")+
  theme_minimal()
