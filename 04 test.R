#Setting working directory
setwd( "G:/MSBAPM/01 Spring 2017/Predictive modeling/Team project/Walmart basket analysis/01 Data/test/")


# Importing test dataset

test<-read.csv("test.csv",colClasses = c('character',
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

##Applying the missing_values function on test
missing_test<-missing_values(test)

View(missing_test)

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

blank_test<-blank_values(test)

View(blank_test)

rm(blank_test)

#Recode blank space values in test $ FineLine to 999999

na_index<-which(test$FinelineNumber=="")

test$FinelineNumber[na_index]<-"999999"

#Recode NA's in Upc to 000000000000

na_upc_index<-which(test$Upc=="")

test$Upc[na_upc_index]<-"000000000000"

# Standardizing UPCs - Incomplete code

##Look at the lengths of strings
table(nchar(test$Upc))

##Flag the UPCs that need leading zeros 
test$is_upc_complete<-ifelse(nchar(test$Upc)>=11,1,0)

##Adding leading zeros wherever necessary

test$upc11<-as.character(ifelse(test$is_upc_complete==0,str_pad(test$Upc, 11, pad = "0"),substr(test$Upc,1,11)))

test$companycode_upc=substr(test$upc11,1,6)

test$productcode_upc=substr(test$upc11,7,11)

length(unique(test$companycode_upc))

length(unique(test$productcode_upc))

#Preprocessed test Dataset with required columns
test_preprocessed=test[,-c(3,7,8)]

#Delete obsolete files from the workspace
rm(list=c("missing_test","test","na_index","na_upc_index","blank_values","missing_values"))


visit_neg_scancnt= test_preprocessed[test_preprocessed$ScanCount<0,c(1,3)]%>% 
  group_by(VisitNumber) %>% 
  summarise(NegScanCountSum=sum(ScanCount))


#Hypothesis: TripTypes have different weekday preferences
visit_DoWk=unique(test_preprocessed[,c(1,2)])

visit_DoWk$sunday_flag=ifelse(visit_DoWk$Weekday=="Sunday",1,0)
visit_DoWk$saturday_flag=ifelse(visit_DoWk$Weekday=="Saturday",1,0)
visit_DoWk$friday_flag=ifelse(visit_DoWk$Weekday=="Friday",1,0)

visit_DoWk=visit_DoWk[,-2]


#Hypothesis: Some trips have larger carts 

visit_cartsize= test_preprocessed%>% 
  group_by(VisitNumber) %>% 
  summarise(CartSize=sum(ScanCount),
            avg_scan_cnt=mean(ScanCount))

View(visit_cartsize)


## Create a function to count distinct set of records
CountDistinct=function(x){
  length(unique(x))
}

#Hypothesis: Some trips have specific brand preferences while other trips have variety of products in their carts

visit_unique_pdts= test_preprocessed%>% 
  group_by(VisitNumber) %>% 
  summarise(distinct_pdts=CountDistinct(productcode_upc),
            distinct_companies=CountDistinct(companycode_upc),
            distinct_fineline=CountDistinct(FinelineNumber))

rm(CountDistinct)

## Aggregate to visit, department level

test_vud=test_preprocessed[,c(1,3,4)]

test_visit_dept= test_vud%>% 
  group_by(VisitNumber,DepartmentDescription) %>% 
  summarise(ScanCountSum=sum(ScanCount))


rm(test_vud)

test_visit_dept_1=melt(test_visit_dept, id.vars = c("VisitNumber", "DepartmentDescription"))

View(test_visit_dept_1)


visit_depts=dcast(test_visit_dept_1,VisitNumber ~ DepartmentDescription, value.var = "value")

visit_depts[is.na(visit_depts)]=0

View(visit_depts)
rm(list=c("test_visit_dept_1","test_visit_dept"))

#DEPT FREQUENCY BUCKET FEATURE
dept_freq<-data.frame(table(test_preprocessed$DepartmentDescription))


dept_freq$bucket<-ifelse(dept_freq$Freq>=quantile(dept_freq$Freq,0.75),"Often",
                         ifelse(dept_freq$Freq<quantile(dept_freq$Freq,0.75) & dept_freq$Freq>quantile(dept_freq$Freq,0.25),"Moderate",
                                "Occasional"))

colnames(dept_freq)<-c("DepartmentDescription","frequency","freq_bucket")


##Add frequency bucket column to test dataset

test_freq_bucket<-merge(test_preprocessed,dept_freq,by =c("DepartmentDescription"))

test_freq_bucket<-test_freq_bucket[,c(2,4,9)]


test_freq_bucket_items= test_freq_bucket%>% 
  group_by(VisitNumber,freq_bucket) %>% 
  summarise(sum(ScanCount))

colnames(test_freq_bucket_items)=c("VisitNumber","freq_bucket","cart_size")


test_freq_bucket_items2=melt(test_freq_bucket_items, id.vars = c("VisitNumber", "freq_bucket"))



visit_dept_freq=dcast(test_freq_bucket_items2, VisitNumber ~ freq_bucket, value.var = "value")


#Delete obsolete files from the workspace
rm(list=c("test_freq_bucket_items","test_freq_bucket_items2","test_freq_bucket"))


#Join all datasets to get all predictors in a single dataframe

a=merge(visit_cartsize,visit_dept_freq)


b=merge(a,visit_DoWk)

c=merge(b,visit_unique_pdts)

test_visit_level_final=merge(x=c,y=visit_neg_scancnt,all.x = TRUE)


rm(list = c("a","b","c",
            "visit_cartsize","visit_depts","visit_DoWk",
            "visit_unique_pdts","visit_neg_scancnt"))

##Some cleaning steps

test_visit_level_final[is.na(test_visit_level_final)] <- 0


test_visit_level_final2=test_visit_level_final[,-1]


#Identify variables with very small variance  as they don't add value to the model

test_visit_level_final2$other_depts=rowSums(test_visit_level_final2[,intersect(names(final[,x+1]), names(test_visit_level_final2))])

test_visit_level_final2=test_visit_level_final2[,intersect(names(final2), names(test_visit_level_final2))]


#Predict the test records using the model built earlier

a_test_predictions<- data.frame(predict(rf,data.matrix(test_visit_level_final2),type = "prob"))

a_test_predictions<-cbind(predicted_triptype=a_test_predictions,VisitNumber=test_visit_level_final[,1])

write.csv(a_test_predictions,"yashwanth_rf.csv")
