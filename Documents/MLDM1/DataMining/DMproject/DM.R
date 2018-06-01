
#reading the dataset 
library(ggplot2)
library(readr)
library(dplyr)
myData <- read.csv("C:/Users/amal_/Documents/MLDM1/DataMining/Suicide_India_2001_2012.csv" ,
                   header = TRUE, sep = ";", 
                   quote = "\"'", dec = ".")
summary(myData)
myData


print(myData[,2])
print(myData[,7])
Data1 <- myData[(myData$Typecode == 'Causes'),]
Data2 <- myData[(myData$Typecode == 'Means_adopted'),]
Data3 <- myData[(myData$Typecode == 'Professional_Profile'),]
Data4 <- myData[(myData$Typecode == 'Education_Status'),]
Data5 <- myData[(myData$Typecode == 'Social_Status'),]

print(sum(Data1$Total))
print(sum(Data2$Total))
print(sum(Data3$Total))
print(sum(Data4$Total))
print(sum(Data5$Total))



#############################################################################
#preparation of data 
#supression des lignes avec total=0 qui signifie qu'il ya 0 personnes suicidés 

myData<- myData[!(myData$Total == 0),]
summary(myData)
#delting lines with state value == total (all india),total(states),total (Uts)
myData <-myData[!(myData$State =='Total (All India)'),]
myData <-myData[!(myData$State =='Total (States)'),]
myData<- myData[!(myData$State =='Total (Uts)'),]


#taking just observations with Type_code=causes
# groups by type of causes  
groupCauses <- summarise(group_by(Data1,Type), mean(Total))
groupCauses <- groupCauses[order(groupCauses$`mean(Total)`, decreasing = T),]
topCauses <- groupCauses[1:5,]
#visualisation 
win.graph(800,600,10)
barplot(topCauses$`mean(Total)`, names.arg = topCauses$Type,
        col = rainbow(5),
        main = "Top 5 causes ofsuicide on India",
        xlab = "Causes", ylab = "number of suicides ")
###################################################################
#How people suicide 
##################################################################
means <- summarise(group_by(Data2,Type), mean(Total))
means <- means[order(means$`mean(Total)`, decreasing = T),]
topmeans <- means[1:5,]

#visualisation 
win.graph(1000,600,10)
barplot(topmeans$`mean(Total)`, names.arg = topmeans$Type,
        col = "black",
        main = "Top 5 means ofsuicide in India",
        xlab = "means", ylab = "number of suicides ")

#####################################################################
#Visualisation by years 
#####################################################################
groupyears <- summarise(group_by(Data1,Year), sum(Total))
groupyears<- groupyears[order(groupyears$`sum(Total)`, increasing = T),]
numbers <- groupyears[1:12,]
win.graph(800,600,10)
plot(numbers$Year,numbers$`sum(Total)`,caxes=FALSE,panel.first = grid(),type="o",col="red")

win.graph(800,600,10)
barplot(numbers$`sum(Total)`, names.arg = numbers$Year,
        col = rainbow(12),
        main = "numbers of suicide in India",
        xlab = "years", ylab = "number of suicides ")

########################################################################

bsyear<-Data1 %>% group_by(State,Year) %>%
        summarise(Total = sum(Total))
syear <- summarise(group_by(bsyear,State), sum(Total))
syear<- syear[order(syear$`sum(Total)`, decreasing = T),]
states <- syear[1:10,]
win.graph(1000,600,10)
barplot(states$`sum(Total)`, names.arg = states$State,
        col = "blue",
        main = "10 states most touched by suicide",
        xlab = "states", ylab = "number of suicides ")
################################################################


Genderprofile<-Data3 %>% group_by(Type,Gender) %>%
            summarise(Total = sum(Total))
pgender <- summarise(group_by(Genderprofile,Type), sum(Total))
pgender<- pgender[order(pgender$`sum(Total)`, decreasing = T),]
pg <- Genderprofile[1:12,]
win.graph(1000,600,10)
barplot(pg$Total, names.arg = pg$Type,
        col=c("darkblue","red"),
        main = "Male and Female profiles",
        xlab = "Professional profiles", ylab = "number of suicides ")
#################################################################
#Male and female visualisation 
################################################################
#female
#######################################
female <- myData[(myData$Gender == 'Female'),]
female <- summarise(group_by(female,Year),sum(Total))
female<- female[order(female$`sum(Total)`, increasing = T),]
numberfemale <- female[1:12,]
win.graph(800,600,10)
plot(numberfemale$Year,numberfemale$`sum(Total)`,caxes=FALSE,panel.first = grid(),type="o",col="red")

xf <- numberfemale$`sum(Total)`
meanfemale <- mean(xf)
#######################################
#male
#######################################

male <- myData[(myData$Gender == 'Male'),]
male <- summarise(group_by(male,Year),sum(Total))
male<- male[order(male$`sum(Total)`, increasing = T),]
numbermale <- male[1:12,]
win.graph(800,600,10)
plot(numbermale$Year,numbermale$`sum(Total)`,caxes=FALSE,panel.first = grid(),type="o",col="black")


xm <- numbermale$`sum(Total)`
meanmale <- mean(xm)
##################################################################
#mean of male and female suicides in a barplot 
#################################################################
tableau <- c(meanmale,meanfemale)
barplot(tableau, names.arg = c("male","female"),
        col = rainbow(2),
        main = "male and female suicide in India",
        xlab = "male and female", ylab = "number of suicides ") 
##############################################################
#3d representation 
#############################################################???
library(rgl)
plot3d(x=myData$Gender,
       y=myData$Age_group,
       z=myData$Total,
       xlab=colnames(myData$Gender),
       ylab=colnames(myData$Age_group),
       zlab=colnames(myData$Total))

#text3d(x=myData$Gender,    y=myData$Age_group, z=myData$Total, text=names,cex=0.8,col="blue")


################################################################
#age groups with high number of suicides 
###############################################################
#preparing data by deletion of lines with  group age= 0-100+
#because it doesn't give us a relevant information about age 
#############################################################

myData<- myData[!(myData$Age_group =='0-100+'),]

##############################################################
ages <- summarise(group_by(Data1,Age_group),sum(Total))
ages<-ages[order(ages$`sum(Total)`, decreasing = T),]
ageclass <-ages[1:5,]
win.graph(800,600,10)
barplot(ages$`sum(Total)`, names.arg = ages$Age_group,
        col = rainbow(5),
        main = "number of suicides by age groups",
        xlab = "age categorie", ylab = "number of suicides ")

#here we see that the age categories that have the highest number of suicides are : 15-29 and  30-44
#############################################################
#the age_groupe attribute contains categorical values that have to be converted. 
#then if we associate an integer to each age group it will not be relevant
#it biases the dataset because some ages will get more importance than others just because of the integer choosen. 
#A better solution is to replace each value by a binary vector containing only one 1. 
#Example if the age group of an observation is 0-14 add a column 
#at the end of data frame called 0-14 and mark 1 at this column 
#############################################################
#adding columns
myData['0-14']= 0
myData['15-29']= 0
myData['30-44']= 0
myData['45-59']=0
myData['60+']= 0

myData[myData[,6] == "0-14",8]=1
myData[myData[,6] == "15-29",9]=1
myData[myData[,6] == "30-44",10]=1
myData[myData[,6] == "45-59",11]=1
myData[myData[,6] == "60+",12]=1



