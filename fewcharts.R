## Question 1: For every team in a conference, display the sum of points scored and mean of points scored over the time period of the dataset

load("Seasonstatisticscleaned.Rda")

library(dplyr)
library(ggplot2)
library(reshape2)
seasontbl=tbl_df(Seasonstatistics)
glimpse(seasontbl)
teamconfyrsummary=seasontbl %>% group_by(Conference,Team,Year)%>% summarise (sumpts=sum(Points),meanpts=mean(Points))

teamconfyrsummary %>% filter(Conference=="Big 12") %>% ggplot(.,aes(Year,sumpts,group=Team))+geom_line()+
  facet_wrap(~Team,nrow=2)+theme(axis.text.x=element_text(angle=-90))+ggtitle("Big 12: Sum of Points")

teamconfyrsummary %>% filter(Conference=="Big 12") %>% ggplot(.,aes(Year,meanpts,group=Team))+geom_line()+
  facet_wrap(~Team,nrow=2)+theme(axis.text.x=element_text(angle=-90)) + ggtitle("West Coast: Mean of Points")

teamconfyrsummary %>% filter(Conference=="West Coast") %>% ggplot(.,aes(Year,sumpts,group=Team))+geom_line()+
  facet_wrap(~Team,nrow=2)+theme(axis.text.x=element_text(angle=-90)) + ggtitle("West Coast: Sum of Points")

teamconfyrsummary %>% filter(Conference=="West Coast") %>% ggplot(.,aes(Year,meanpts,group=Team))+geom_line()+
  facet_wrap(~Team,nrow=2)+theme(axis.text.x=element_text(angle=-90)) + ggtitle("West Coast: Mean of Points")



## Question 2: For the year Kevin Durant played for Texas, come up with bar charts for the top 3 players of Kevin's team in each of the 15 variables for which data are available - Arrange them in a facet 

#Identify the year of Kevin's presence in Texas

seasontbl %>% filter(Player=="Kevin Durant")%>% select(Year,Team) #2006-2007, Texas Longhorns
kevinteam=seasontbl%>% filter(Year=="2006-2007",Team=="Texas Longhorns")%>% 
                          select(-(Year:Conference))%>% melt(.,id="Player")
top3 = data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("Player", "variable", "value"))))
for (i in 1:15){
  tmp= kevinteam %>%filter(variable==levels(kevinteam$variable)[i])%>% arrange(desc(value))%>%head(3)
  top3=rbind(top3,tmp)
}
ggplot(top3,aes(Player,value))+geom_bar(stat="identity")+facet_wrap(~variable,scales="free")+theme(legend.position="none")
