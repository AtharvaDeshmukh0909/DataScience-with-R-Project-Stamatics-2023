
# Assignment - 2 
# Atharva Deshmukh
# Roll No. - 210231

###################################

library(tidyverse)
library(imager)
library(rvest)
library(dplyr)
library(stringr)


###################################



# Question - 1 (a)

data(iris)
iris %>% select(-Species) %>%pivot_longer(everything()) %>%ggplot(aes(x=name,y=value))+geom_boxplot()

irisVer <- subset(iris, Species == "versicolor")
irisSet <- subset(iris, Species == "setosa")
irisVir <- subset(iris, Species == "virginica")

par(mfrow=c(1,3),mar=c(6,3,2,1))
boxplot(irisVer[,1:4], main="Versicolor",ylim = c(0,7) )
boxplot(irisSet[,1:4], main="Setosa",ylim = c(0,7))
boxplot(irisVir[,1:4], main="Virginica",ylim = c(0,7))

par(mfrow=c(1,1),mar=c(2,3,2,1))
boxplot(irisVir[,1:5], main="Combined",ylim = c(0,9))




petal <- iris$Petal.Length
sepal <- iris$Sepal.Length
plot(petal, sepal,
     xlab = "Petal length ",
     ylab = "Sepal length ",
     main = "Petal length vs. Sepal length")

# conclusion - sepal length varies from 4.5-6 whereas
# petal length varies from 3-7

############################################

# Question - 1 (b)

getwd()
setwd("C:/Users/Atharva Deshmukh/Desktop/DataScience_R")
getwd()
boat <- load.image("boat.jpg")


flip<-function(img)
{
  col.mat <- as.array(img[, ,1, ])
  dims <- dim(col.mat)
  rot <- array(0, dim = dims)
  
  for(i in 1:dims[1])
  {
    for(j in 1:dims[2])
    {
      rot[i, j, ] <- col.mat[dims[1] - i + 1, j , ]
    }
  }
  return (as.cimg(rot))
}

par(mfrow = c(1,2))
plot(boat)
plot(flip(boat))

###########################################

# Quesetion - 1 (c)

library(MASS)
data(ships)
?ships

shipA <- subset(ships, type == "A")
shipB <- subset(ships, type == "B")
shipC <- subset(ships, type == "C")
shipD <- subset(ships, type == "D")
shipE <- subset(ships, type == "E")

incident_A <- shipA$incidents
incident_B <- shipB$incidents
incident_C <- shipC$incidents
incident_D <- shipD$incidents
incident_E <- shipE$incidents

service_A <- shipA$service/500
service_B <- shipB$service/500
service_C <- shipC$service/500
service_D <- shipD$service/500
service_E <- shipE$service/500

par(mfrow=c(1,5),mar=c(3,2,1,0))
boxplot(incident_A,service_A,names=c("Incidents","Service"), main="A",ylim=c(0,50))
boxplot(incident_B,service_B,names=c("Incidents","Service"), main="B",ylim=c(0,50))
boxplot(incident_C,service_C,names=c("Incidents","Service"), main="C",ylim=c(0,50))
boxplot(incident_D,service_D,names=c("Incidents","Service"), main="D",ylim=c(0,50))
boxplot(incident_E,service_E,names=c("Incidents","Service"), main="E",ylim=c(0,50))

## By observing the boxplots we can disprove the hypothesis stated in the question 
## Eventhough the number of incidents of Ship B is relatively high but Service of 
## Ship B is high as well , we can say that ship E is the least trust worthy as 
## number of incidents are high and service is low by observing the boxplots of all ships


##############################################################

# Question - 1 (d)

html <- read_html("https://stats.stackexchange.com/questions?tab=Votes")
title <- html %>%  html_elements(".s-post-summary--content-title a") %>% html_text()
votes_answers_views <- html %>%  html_elements(".s-post-summary--stats-item-number") %>% html_text()

votes<-c()
answers<-c()
views<-c()

for(i in 0:45)
{
  if(i%%3==1)
  {
    votes<-append(votes,votes_answers_views[i])
    
  }
  if(i%%3==2)
  {
    answers<-append(answers,votes_answers_views[i])
    
  }
  if(i%%3==0)
  {
    views<-append(views,votes_answers_views[i])
    
  }
}

combined_table<- data.frame(title,
                            votes,
                            answers,
                            views)
combined_table

########################################################

# Question - 1 (e)

# first day getting half tablet = 0
# second day getting half tablet = 1/100 * 100/100
# third day getting half tablet = 2/100 * 100/100 * 99/100
# fourth day getting half tablet = 3/100 * 100/100 * 99/100 * 98/100
# fifth day getting half tablet = 4/100 * 100/100 * 99/100 * 98/100 * 97/100

sum<-0
for(i in 1:100){
  day<-(i*(i/100))
  for(j in 1:i-1){
    day<-day*((100-j)/100)
  }
  
  sum<-sum+day
}
sum

##########################################################
