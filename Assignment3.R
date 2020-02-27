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


#2
library(dplyr)
library(tidyr)
library(readr)
primaryPolls<-read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%y")

#3
install.packages("fivethirtyeight")
library(fivethirtyeight)
library(tidyverse)
#polls <- read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
#Error in open.connection(con, "rb") : HTTP error 400. So I did not use that line of code you provided in the document
polls<-read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv', stringsAsFactors = F)
Endorsements <- endorsements_2020
Endorsements<-rename(Endorsements, candidate_name=endorsee) #change candidate name using dplyr function
colnames(Endorsements)[6] <- "candidate_name" #change candidate name
as_tibble(Endorsements) #Change the Endorsements dataframe into a tibble object.
polls<-filter(polls, candidate_name == c("Amy Klobuchar", "Bernard Sanders","Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg"))
#filter by names
polls<-select(polls,candidate_name, sample_size, start_date, party, pct)#subsetting the data

setdiff(Endorsements$candidate_name, polls$candidate_name) 
setdiff(polls$candidate_name,Endorsements$candidate_name) 
#output of the last two lines indicates that only "Joseph R. Biden Jr." "Bernard Sanders" are different across both tables
poll %>%
  mutate(mpg=replace(mpg, cyl==4, NA)) %>%
  as.data.frame()#rename bernie and biden
new<-polls %>% inner_join(Endorsements, by="candidate_name") #combine both data set into one dataset call "new"
new<-new%>%group_by(candidate_name) %>% summarise(numEndorsement=n())#Create a variable which indicates the number of endorsements for each of the five candidates using dplyr.
#Plot the number of endorsement each of the 5 candidates have. Save your plot as an object p
p<-ggplot(data=as_data_frame(new))+
  geom_point(mapping = aes(x=new$candidate_name, y=new$numEndorsement, color=candidate_name))

#run the following code: p + theme_dark(). Notice how you can still customize your plot without
#rerunning the plot with new options. 
p + theme_dark()

#Now, using the knowledge from the last step change the label of the X and Y axes to be more informative, add a title, and use your favorite theme. 
p+theme_minimal()+labs(title="Endorsement by Candidates",
                       x ="Names", y = "Number of Endorsement")
#4
library(tidyverse) #install.packages('tm') library(tm) #install.packages('lubridate') library(lubridate) #install.packages('wordcloud') library(wordcloud)
tweets <- read_csv('https://politicaldatascience.com/PDS/Datasets/trump_tweets.csv')