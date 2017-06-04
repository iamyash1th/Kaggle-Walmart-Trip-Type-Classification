#Setting working directory
setwd( "G:/MSBAPM/01 Spring 2017/Predictive modeling/Team project/Walmart basket analysis/01 Data/train/01 Ready for model/")

#Load required libraries
library(MASS)
library(caret)
library(randomForest)
library(xgboost)

#Join all datasets to get all predictors in a single dataframe

a=merge(visit_cartsize,visit_dept_freq)


b=merge(a,visit_DoWk)

c=merge(b,visit_unique_pdts)

final=merge(x=c,y=visit_neg_scancnt,all.x = TRUE)


rm(list = c("a","b","c",
            "visit_cartsize","visit_dept_freq","visit_DoWk",
            "visit_unique_pdts","visit_neg_scancnt"))

##Some cleaning steps

final[is.na(final)] <- 0

final$TripType=as.factor(final$TripType)

final2=final[,-1]

rm(final)

#Identify variables with very small variance  as they don't add value to the model
x = nearZeroVar(final2)

nearzerovar_cols=data.frame(colnames(final2[,x]))

final2$other_depts=rowSums(final2[,x])

final2=final2[,-x]

attach(final2)

##################################MODEL 2 Random Forest##################################

numTrees <- 100

rf <- randomForest(formula = TripType~.,data =final2,ntree=numTrees)

importance(rf)

names(rf)

rm(train_preprocessed)


# Create a train and validation dataset
#For reproducibility, set seed for sampling
set.seed(123)

#sample random row numbers
index_train=sample(1: nrow(final), floor(0.75*nrow(final)))


################################MODEL 1 Linear Discriminant analysis##################################

lda.fit2=lda(TripType~.,data=final2 ,subset =index_train)

lda.pred2=predict(lda.fit2,final2[-index_train,])

names(lda.pred2)

table(lda.pred2$class,final2[-index_train,1])

mean(lda.pred2$class == final2[-index_train,1])


################################MODEL 3 XG Boost###################################

y=final2$TripType

#How to fill y properly : Xgboost requirement ,label must be in [0, num_class)
sortedTripType<-sort(unique(y))

target<-c(1:length(y))

for (i in 1:length(y))
{
  target[i]<-which(sortedTripType==y[i]) 
}
target<-target-1 #label must be in [0, num_class)

param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = 38,
              "eta"=0.1,
              "max.depth"=15,
              "nthread" = -1,
              "min_child_weight"=1,
              "colsample_bytree"=0.9,
              "subsample"=0.9
)

bst = xgboost(params=param, data = as.matrix(final2[,-1]), 
              label = target, nrounds=500)

# Get the feature real names
names <- dimnames(data.matrix(final2[,-1]))[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bst)
# Nice graph
xgb.plot.importance(importance_matrix[1:10,])
