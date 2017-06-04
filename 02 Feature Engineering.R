#Hypothesis: Trips with return product (scan <0) categorized into a single trip type
summary(train_preprocessed$ScanCount)

##Table with visit count across ScanCount and TripType 
train_returns<-data.frame(table(train_preprocessed[train_preprocessed$ScanCount<0,1]))

colnames(train_returns)<-c("TripType","Freq")

plot(train_returns$TripType,train_returns$Freq)

rm(train_returns)


visit_neg_scancnt= train_preprocessed[train_preprocessed$ScanCount<0,c(2,4)]%>% 
  group_by(VisitNumber) %>% 
  summarise(NegScanCountSum=sum(ScanCount))


#Hypothesis: TripTypes have different weekday preferences
weekday_triptype<-data.frame(prop.table(table(train_preprocessed[,c(1,3)])))


ggplot() + geom_bar(aes(y = Freq, x = TripType, fill = Weekday), data = weekday_triptype,
                    stat="identity")


rm(weekday_triptype)

visit_DoWk=unique(train_preprocessed[,c(2,3)])

visit_DoWk$sunday_flag=ifelse(visit_DoWk$Weekday=="Sunday",1,0)
visit_DoWk$saturday_flag=ifelse(visit_DoWk$Weekday=="Saturday",1,0)
visit_DoWk$friday_flag=ifelse(visit_DoWk$Weekday=="Friday",1,0)

visit_DoWk=visit_DoWk[,-2]


#Hypothesis: Some trips have larger carts 


visit_cartsize= train_preprocessed%>% 
  group_by(TripType,VisitNumber) %>% 
  summarise(CartSize=sum(ScanCount),
            avg_scan_cnt=mean(ScanCount))

View(visit_cartsize)


## Create a function to count distinct set of records
CountDistinct=function(x){
  length(unique(x))
}
  
#Hypothesis: Some trips have specific brand preferences while other trips have variety of products in their carts

visit_unique_pdts= train_preprocessed%>% 
                    group_by(VisitNumber) %>% 
                      summarise(distinct_pdts=CountDistinct(productcode_upc),
                        distinct_companies=CountDistinct(companycode_upc),
                          distinct_fineline=CountDistinct(FinelineNumber))

rm(CountDistinct)

## Aggregate to visit, department level

train_vud=train_preprocessed[,c(1,2,4,5)]

Train_visit_dept= train_vud%>% 
  group_by(TripType,VisitNumber,DepartmentDescription) %>% 
  summarise(ScanCountSum=sum(ScanCount))


rm(train_vud)

Train_visit_dept_1=melt(Train_visit_dept, id.vars = c("TripType","VisitNumber", "DepartmentDescription"))

View(Train_visit_dept_1)


visit_depts=dcast(Train_visit_dept_1,TripType+VisitNumber ~ DepartmentDescription, value.var = "value")

visit_depts[is.na(visit_depts)]=0

View(visit_depts)
rm(list=c("Train_visit_dept_1","Train_visit_dept"))


##Finding frequency of departments
dept_freq<-data.frame(table(train_preprocessed$DepartmentDescription))


dept_freq$bucket<-ifelse(dept_freq$Freq>=quantile(dept_freq$Freq,0.75),"Often",
                         ifelse(dept_freq$Freq<quantile(dept_freq$Freq,0.75) & dept_freq$Freq>quantile(dept_freq$Freq,0.25),"Moderate",
                                "Occasional"))

colnames(dept_freq)<-c("DepartmentDescription","frequency","freq_bucket")


##Add frequency bucket column to train dataset

train_freq_bucket<-merge(train_preprocessed,dept_freq,by =c("DepartmentDescription"))

train_freq_bucket<-train_freq_bucket[,c(3,5,10)]


train_freq_bucket_items= train_freq_bucket%>% 
  group_by(VisitNumber,freq_bucket) %>% 
  summarise(sum(ScanCount))

colnames(train_freq_bucket_items)=c("VisitNumber","freq_bucket","cart_size")


train_freq_bucket_items2=melt(train_freq_bucket_items, id.vars = c("VisitNumber", "freq_bucket"))



visit_dept_freq=dcast(train_freq_bucket_items2, VisitNumber ~ freq_bucket, value.var = "value")


#Delete obsolete files from the workspace
rm(list=c("blank_train","train_freq_bucket_items","train_freq_bucket_items2","train_freq_bucket"))



















#ON HOLD: Hypothesis: product bundles in a transaction differentiate trip types


#Using Association rules (refer to different code), we found that this is the most frequent item-set in the transactions
Train_visit_dept_final$Asscn_col1 = ifelse(Train_visit_dept_final$`COMM BREAD`>0 & 
                                             Train_visit_dept_final$DAIRY >0 & 
                                             Train_visit_dept_final$`GROCERY DRY GOODS`>0, 1, 0)

Train_visit_dept_final$Asscn_col2 = ifelse(Train_visit_dept_final$`DSD GROCERY`>0 & 
                                             Train_visit_dept_final$`GROCERY DRY GOODS` >0 & 
                                             Train_visit_dept_final$PRODUCE>0, 1, 0)

Train_visit_dept_final$Asscn_col3 = ifelse(Train_visit_dept_final$`COMM BREAD`>0 & 
                                             Train_visit_dept_final$`FROZEN FOODS` >0 & 
                                             Train_visit_dept_final$`PRE PACKED DELI`>0, 1, 0)


Train_visit_dept_final$Asscn_col4 = ifelse(Train_visit_dept_final$DAIRY>0 & 
                                             Train_visit_dept_final$`MEAT - FRESH & FROZEN` >0 & 
                                             Train_visit_dept_final$`PRE PACKED DELI`>0, 1, 0)


hypothesis_association=Train_visit_dept_final%>% 
  group_by(TripType) %>% 
  summarise(mean(Asscn_col1),mean(Asscn_col2),mean(Asscn_col3),mean(Asscn_col4))

hypothesis_association$TripType=as.factor(hypothesis_association$TripType)

plot(x=hypothesis_association$TripType,y=hypothesis_association$`mean(Asscn_col2)`)
plot(x=hypothesis_association$TripType,y=hypothesis_association$`mean(Asscn_col1)`)

write.csv(Train_visit_dept_final[,c(2,72,73)], "G:/MSBAPM/01 Spring 2017/Predictive modeling/Team project/Walmart basket analysis/01 Data/train/01 Ready for model/visit_association_cols.csv")

write.csv(hypothesis_association,"G://MSBAPM/01 Spring 2017/Predictive modeling/Team project/Walmart basket analysis/01 Data/train/02 Hypothesis tests/hypothesis_association.csv")

#

write.csv(Train_visit_dept_final[,c(2,72,73,74,75)],
          "G:/MSBAPM/01 Spring 2017/Predictive modeling/Team project/Walmart basket analysis/01 Data/train/01 Ready for model/visit_association_cols2.csv")
