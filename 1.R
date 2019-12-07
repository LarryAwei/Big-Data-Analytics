app_data<-as.data.frame(read.csv("G:/homework/DBA/A1_performance_test.csv"))
app1<-app_data[app_data$approach=="approach1",]$performance
app2<-app_data[app_data$approach=="approach2",]$performance
appNo<-app_data[app_data$approach=="no_approach",]$performance
shapiro.test(app1)
shapiro.test(app2)
shapiro.test(appNo)
summary(app1)
summary(app2)
summary(appNo)
t.test(app1,appNo,var.equal = TRUE)
# t=11.93 df=379 p<2.2e-16
qt(p=0.05/2, df=379, lower.tail= FALSE)
# 1.966
# |1-11.93|>1.99, null hypthsis will be rejected

t.test(app2,appNo,var.equal = TRUE)
# t=14.021 df=401 p<2.2e-16
qt(p=0.05/2, df=401, lower.tail= FALSE)
# 1.966
# |1-14.021|>1.966, null hypthsis will be rejected

t.test(app1,app2,var.equal = TRUE)
# t=-1.9988 df=414 p=0.04629
qt(p=0.05/2, df=414, lower.tail= FALSE)
# 1.966
# |1-1.9988|<1.966, null hypthsis won't be rejected

t.test(app1,app2,var.equal = FALSE)
# t = -2.0015, df = 411.37, p-value = 0.04599
# 
model<-aov(performance~approach,data=app_data)
summary(model)
TukeyHSD(model)
