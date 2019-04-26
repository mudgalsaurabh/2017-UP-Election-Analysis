######Election Result analaysis - 2017 UP State #######
######Saurabh Mudgal  Date 20th APR ###############################
##read the input file Detailed result from eci.gov.in##########

setwd("C:\\BACP\\Module 5 - Predictive Modelling\\Mini Projet - Election Result Analysis")
getwd()

Detail_Result = read.csv(file.choose(), header=T)
View(Detail_Result)
dim(Detail_Result)
str(Detail_Result)
names(Detail_Result)
str(Detail_Result)
summary(Detail_Result)

# add new variable for winner in each constituency 


Detail_Result =  Detail_Result %>%
  group_by(Constituency.Name) %>%
  mutate(Winner = rank(-X..votes.polled))

##EDA

#univariate 

levels(Detail_Result$Constituency.Name)
levels(Detail_Result$Candidate.Name)
levels(Detail_Result$Candidate.Sex )
levels(Detail_Result$Candidate.Category )


str(Detail_Result$Party.Name )


plot(Detail_Result$Candidate.Sex,xlab = "Sex of candidate", ylab = "Count",main="Type of candidate")
boxplot(Detail_Result$Candidate.Age   ,main = "Age of candidates")
plot(Detail_Result$Candidate.Category   ,main="Candidate Caste Distribution")
plot(Detail_Result$Party.Name)
summary(Detail_Result$Total.Valid.Votes)
sd(Detail_Result$Total.valid.votes.polled..NOTA)

library("dplyr")
install.packages("ggplot2")
library("ggplot2")


### check for the missing date 

sum(is.na(Detail_Result$Constituency.No.))
sum(is.na(Detail_Result$Constituency.Name))
sum(is.na(Detail_Result$Candidate.Name))
sum(is.na(Detail_Result$Candidate.Sex))




#Detail_Result$Winner =  filter(Constituency.Name=="Ghaziabad") 

distinct(Detail_Result,   Detail_Result$Constituency.Name)

#filter data set 
Detail_Result %>%
  select( Constituency.Name,Party.Name,Total.Valid.Votes) 

filter(Detail_Result$Party.Name=="NOTA")  %>% 
      
      
    
plot(Detail_Result %>%
  select( Constituency.Name,Party.Name)  %>%
  filter(Party.Name == "BJP" | Party.Name =="BSP" | Party.Name =="SP")
)

###creating data frame for top 5 political parties 

df_imp_parties = Detail_Result %>%
                select(Detail_Result$Constituency.Name,Detail_Result$Candidate.Sex,
                        Detail_Result$Candidate.Age, Detail_Result$Candidate.Category,
                        Detail_Result$Party.Name, Detail_Result$Total.Valid.Votes,
                        Detail_Result$X..votes.polled,Detail_Result$Winner) %>%
                  filter (Detail_Result$Party.Name=="BJP" | Detail_Result$Party.Name=="SP"
                          | Detail_Result$Party.Name=="BSP" | Detail_Result$Party.Name=="INC")



View(Detail_Result)

##bivariate 
summary(Detail_Result %>%
  group_by(Constituency.Name) %>%
      summarise(mean_votes = sum(Total.Valid.Votes)) )


##create df for each constituency 
Detail_Result_const = Detail_Result %>%
                      select ()
                      group_by(Constituency.No.)  %>%
                      summarise(consti_n = distinct_(Party.Name) )
Detail_Result %>%
          group_by(Candidate.Category) 

          summarise( c= count(Total.Valid.Votes)) 


 ggplot(Detail_Result,aes(x=Party.Name, fill=Total.Valid.Votes)) +
    geom_bar() + labs(y='Votes', title = 'Vote per party')



plot(Constituency.Name~Candidate.Sex,Detail_Result)

#party and constituency 
party_consti =Detail_Result %>%
  group_by(Party.Name) %>%
  #filter(Constituency.Name=="Behat") %>%
  summarise(sort_con=n()) %>% top_n(10) 

ggplot(party_consti,aes(x=Party.Name, fill=sort_con)) +
  geom_bar() + labs(y='Constituency', title = 'party')

party_consti[order(-party_consti$sort_con),]


#party and votes
party_votes =Detail_Result %>%
  group_by(Party.Name) %>%
  #filter(Constituency.Name=="Behat") %>%
  summarise(sort_vote= sum(Total.Valid.Votes)) %>% top_n(10) 

  
  
  party_votes[order(-party_votes$sort_vote),]
  
  ggplot(party_votes,aes(x=Party.Name, fill=sort_vote)) +
    geom_bar() + labs(y='votes Percentage', title = 'party vote percentage')
  
  
  mutate(vote)

## vote percentage 
  party_votes_pt =Detail_Result %>%
    group_by(Party.Name) %>%
    #filter(Constituency.Name=="Behat") %>%
    summarise(sort_vote_pt= mean(X..votes.polled)) %>% top_n(10) 
  
  party_votes_pt[order(-party_votes_pt$sort_vote_pt),]
  
  ggplot(party_votes_pt,aes(x=Party.Name, fill=sort_vote_pt)) +
    geom_bar() + labs(y='votes', title = 'party')
  
  
  ##party and candidate gender
  
  party_votes_pt =Detail_Result %>%
    group_by(Candidate.Sex) %>%
        summarise(Gender_p= median(Party.Name)) %>% top_n(10) 
  
  party_votes_pt[order(-party_votes_pt$sort_vote_pt),]
  
  ggplot(Detail_Result,aes(x=Party.Name, fill= Detail_Result$Candidate.Sex )) +
    geom_bar() + labs(y='votes', title = 'party')
  
