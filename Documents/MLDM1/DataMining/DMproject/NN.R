#reading the dataset 
library(ggplot2)
library(readr)
library(dplyr)
myData <- read.csv("C:/Users/amal_/Documents/MLDM1/DataMining/Suicide_India_2001_2012.csv" ,
                   header = TRUE, sep = ";", 
                   quote = "\"'", dec = ".")

Data1 <- myData[(myData$Typecode == 'Causes'),]
Data2 <- myData[(myData$Typecode == 'Means_adopted'),]
Data3 <- myData[(myData$Typecode == 'Professional_Profile'),]

#our interest is Data 3 with observations of suicides with the professional profile information about victims

#Data3<- Data3[!(Data3$Total == 0),]
Data3<- Data3[!(Data3$Age_group =='0-100+'),]

Data3['first']= 0
Data3['second']= 0
Data3['third']= 0
Data3['forth']=0
Data3['fifth']= 0
Data3[Data3[,6] == "0-14",8]=1
Data3[Data3[,6] == "15-29",9]=1
Data3[Data3[,6] == "30-44",10]=1
Data3[Data3[,6] == "45-59",11]=1
Data3[Data3[,6] == "60+",12]=1



###########################################################
Data3['Retired']= 0
Data3[Data3[,4] == "Retired Person",13]=1

Data3['Unemployed']= 0
Data3[Data3[,4] == "Unemployed",14]=1

Data3['Undertaking']= 0
Data3[Data3[,4] == "Public Sector Undertaking",15]=1

Data3['Private']= 0
Data3[Data3[,4] == "Service (Private)",16]=1

Data3['Housewife']= 0
Data3[Data3[,4] == "House wife",17]=1

Data3['Selfemployed']= 0
Data3[Data3[,4] == "Self-employed (Business activity)",18]=1

Data3['Professionalactivity']= 0
Data3[Data3[,4] == " Professional Activity",19]=1


Data3['Student']= 0
Data3[Data3[,4] == "Student",20]=1

Data3['Other']= 0
Data3[Data3[,4] == "Others (Please Specify)",21]=1

Data3['Farming']= 0
Data3[Data3[,4] == "Farming/Agriculture Activity",22]=1

Data3['Governmentservice']= 0
Data3[Data3[,4] == "Service (Government)",23]=1
###########################################################
#delete column Gender and replace it by 2 additional columns female and male (0 fale 1 true )
###########################################################
Data3['Female']= 0
Data3[Data3[,5] == "Female",24]=1

Data3['Male']= 0
Data3[Data3[,5] == "Male",25]=1

#we delete column age group 
###########################################################
Data3$Age_group <- NULL
Data3$Year <- NULL
Data3$Typecode <- NULL
Data3$Type <- NULL
Data3$Gender <- NULL
Data3$State <- NULL

###########################################################
#creating target from total (0 or !0 ) to predict chance of suicide to 
#a specific profile 

print(y)
#Data3$Total <- NULL
#our interest is Data 3 with observations of suicides with the professional profile information about victims
# load library
library(neuralnet)
# Random sampling

samplesize = 0.60 * nrow(Data3)
set.seed(80)
index = sample( seq_len ( nrow ( Data3 ) ), size = samplesize )
#scaling
#maxs <- apply(Data3, 2, max) 
#mins <- apply(Data3, 2, min)


#scaled <- as.data.frame(scale(Data3, scale =maxs-mins))
# Create training and test set
train = Data3[ index, ]
test = Data3[ -index, ]
y=train[,'Total']>0
train$Total <- NULL
n <- colnames(train)
f <- as.formula(paste("y ~",paste(n[!n %in% "y"],collapse="+")))
Data3$State <- NULL




# fit neural network
set.seed(2)
NN = neuralnet(formula=f ,data=train, hidden = c(5,3) , linear.output = T )
print(NN)
# plot neural network
plot(NN)

