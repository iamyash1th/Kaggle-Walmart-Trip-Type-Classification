setwd("G://MSBAPM/01 Spring 2017/Predictive modeling/Team project/Walmart basket analysis/01 Data/train")

#Loading required libraries
library(arules)
library(arulesViz)


trans = read.transactions("transaction_dept_for_association.csv", 
                          format = "single", 
                          sep = ",", 
                          cols = c("VisitNumber", "DepartmentDescription"),rm.duplicates=FALSE)



rules = apriori(trans, parameter=list(support=0.01, confidence=0.8));

inspect(head(sort(rules, by="lift"),1));


rules2 = apriori(trans, parameter=list(support=0.05, confidence=0.8));

inspect(head(sort(rules2, by="lift"),1));

rules3 = apriori(trans, parameter=list(support=0.05, confidence=0.6));

inspect(head(sort(rules3, by="lift"),1));

rules4 = apriori(trans, parameter=list(support=0.01, confidence=0.7));

inspect(head(sort(rules4, by="lift"),1));






plot(head(sort(rules, by="lift"),1), method="graph");
plot(head(sort(rules2, by="lift"),1), method="graph");
plot(head(sort(rules3, by="lift"),1), method="graph");
plot(head(sort(rules4, by="lift"),1), method="graph")
