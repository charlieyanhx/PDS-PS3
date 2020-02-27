#1
library(ggplot2)
primaryPolls<-read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv', stringsAsFactors = F)
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%Y")
primaryPolls<-primaryPolls[primaryPolls$state%in%c("Alabama", "Arkansas", "California", "Colorado", "Maine", "Massachusetts","Minnesota", "North Carolina","Oklahoma", "Tennessee", "Texas","Utah", "Vermont","Virginia"),]
primaryPolls<-primaryPolls[primaryPolls$candidate_name%in%c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg"),]

p<-ggplot(data=primaryPolls, fill = "Cadidates")+
  geom_smooth(mapping = aes(x=start_date, y=pct, color=candidate_name)) + 
  geom_point(mapping = aes(x=start_date, y=pct, color=candidate_name), alpha=.4) +
  facet_wrap(~ state, ncol = 3,nrow=5)+
  theme_minimal() #changetheme
#graph by states

p +labs(name="Candidates", title="Plot of Polling Data by State (missing Vermount and Arkansas)",
        x ="dates", y = "support (%)")+ #change default axis names
  theme(legend.position="top",legend.title = element_text(colour="red", face="bold"),legend.background = element_rect(fill="lightblue",size=0.5, linetype="solid", colour ="darkblue"))
#change default legend

library(fivethirtyeight)
library(tidyverse)
polls <- read_csv('https://jmontgomery.github.io/PDS/Datasets/president
_primary_polls_feb2020.csv')
Endorsements <- endorsements_2020
