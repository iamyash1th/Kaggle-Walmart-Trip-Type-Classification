# Setting working directory
setwd("G://MSBAPM/01 Spring 2017/Predictive modeling/Team project/Walmart basket analysis/01 Data/train")

#PLEASE INSTALL THESE LIBRARIES BEFORE EXECUTING THE BELOW SCRIPT
install.packages("ggplot2")
install.packages("dplyr")
install.packages("reshape2")
install.packages("caret")
install.packages("stringr")
install.packages("arules")
install.packages("arulesViz")
install.packages("MASS")
install.packages("randomForest")
install.packages("xgboost")
install.packages("Matrix")
#Loading required libraries
library(caret)
library(ggplot2)
library(dplyr)
library(reshape2)
library(stringr)

# Importing train dataset

train<-read.csv("train.csv",colClasses = c('character','character',
                                           'character','character',
                                           'numeric','character','character'))


#Creating a function to calculate number of missing values, proportion of missing values 
#and number of unique values across each column
missing_values = function(input)
{
  n = length(colnames(input)) # number of columns
  a <- NULL
  b <- NULL
  c <- NULL
  for(i in 1:n) 
  {
    a[i]=sum(is.na(input[,i])) 
    b=a/nrow(input) 
    c[i]=length(unique(input[,i])) 
  }
  result=data.frame(colnames(input),a,b,c) 
  colnames(result) = c("column Name", "# Missing Values", "% Missing Value", "Unique Values")
  return(result) 
}

##Applying the missing_values function on train
missing_train<-missing_values(train)

View(missing_train)

blank_values = function(input)
{
  n = length(colnames(input)) # number of columns
  a <- NULL
  b <- NULL 
  c <- NULL
  for(i in 1:n) 
  {
    a[i]=sum(ifelse(input[,i]=="",1,0)) 
    b=a/nrow(input) 
    c[i]=length(unique(input[,i])) 
  }
  result=data.frame(colnames(input),a,b,c) 
  colnames(result) = c("column Name", "# Blank Values", "% Blank Value", "Unique Values")
  return(result) 
}

blank_train<-blank_values(train)

View(blank_train)

#Recode blank space values in train $ FineLine to 999999

na_index<-which(train$FinelineNumber=="")

train$FinelineNumber[na_index]<-"999999"

#Recode NA's in Upc to 000000000000

na_upc_index<-which(train$Upc=="")

train$Upc[na_upc_index]<-"000000000000"

# Standardizing UPCs - Incomplete code

##Look at the lengths of strings
table(nchar(train$Upc))

##Flag the UPCs that need leading zeros 
train$is_upc_complete<-ifelse(nchar(train$Upc)>=11,1,0)

##Adding leading zeros wherever necessary

train$upc11<-as.character(ifelse(train$is_upc_complete==0,str_pad(train$Upc, 11, pad = "0"),substr(train$Upc,1,11)))

train$companycode_upc=substr(train$upc11,1,6)

train$productcode_upc=substr(train$upc11,7,11)

length(unique(train$companycode_upc))

length(unique(train$productcode_upc))

#Preprocessed Train Dataset with required columns
train_preprocessed=train[,-c(4,8,9)]

#Delete obsolete files from the workspace
rm(list=c("missing_train","train","na_index","na_upc_index","blank_values","missing_values"))

