library('arules')
library('arulesViz') 
dat<-read.csv("E:/C/uow/DBA/DBA/A1_success_data.csv")
#itemsets<- apriori(dat, parameter=list(minlen=1, support=0.02, target="frequent itemsets"))
#summary(itemsets)
#inspect(sort(itemsets, by = "support")) 
rules<- apriori(dat, parameter=list(support=0.001,confidence=0.6, target = "rules"))

summary(rules)
plot(rules)
plot(rules@quality)
slope<- sort(round(rules@quality$lift / rules@quality$confidence, 2))
unlist(lapply(split(slope,f=slope),length))
inspect(head(sort(rules, by="lift"), 10))
inspect(head(sort(rules, by="confidence"), 10))
inspect(head(sort(rules, by="support"), 10))



confidentRules<- rules[quality(rules)$confidence > 0.9]
confidentRules
plot(confidentRules, method="matrix", measure=c("lift", "confidence"))
highLiftRules <- head(sort(rules, by="lift"), 5) 
plot(highLiftRules, method="graph", control=list(type="items"))
test<-inspect(sort(rules, by="lift"))
test[test$rhs=="{Success=Yes}",]
test[test$rhs=="{Success=No}",]
