#Remove all existing environment
rm(list=ls(all=T))
#set the working directory
setwd("C:/Data science/Project/Employee absenteeism")
#check the working directory
getwd()
#load libraries
X=c("ggplot2","corrgram","DMwR","caret","randomForest","unbalanced","C50","dummies","MASS","rpart",
    "gbm","ROSE","e1071","Information","sampling","DataCombine","inTrees","readxl")
#Installing packages
install.packages(c("randomForest","unbalanced","c50","dummies","MASS","rpart","gbm","ROSE","e1071","Information","DataCombine"))
lapply(X,require,character.only=TRUE)
rm(X)

# loading Employee absenteeisam data set
Employee_abs=read_excel("Absenteeism_at_work_Project.xls")

#####Explorataory Data Analysis##########

str(Employee_abs)
head(Employee_abs)
summary(Employee_abs)
names(Employee_abs)
#Replacing the space b/w collumn name to underscore for easy to use
names(Employee_abs)=gsub(" ",'_',names(Employee_abs))
names(Employee_abs)=gsub("/",'_',names(Employee_abs))

##univariate analysis and variable consolidation
Employee_abs$Reason_for_absence=as.factor(as.character(Employee_abs$Reason_for_absence))
Employee_abs$Month_of_absence =as.factor(as.character(Employee_abs$Month_of_absence))
Employee_abs$Day_of_the_week=as.factor(as.character(Employee_abs$Day_of_the_week))
Employee_abs$Seasons=as.factor(as.character(Employee_abs$Seasons))
Employee_abs$Disciplinary_failure=as.factor(as.character(Employee_abs$Disciplinary_failure))
Employee_abs$Education=as.factor(as.character(Employee_abs$Education))
Employee_abs$Social_drinker=as.factor(as.character(Employee_abs$Social_drinker))
Employee_abs$Son=as.factor(as.character(Employee_abs$Son))
Employee_abs$Social_smoker=as.factor(as.character(Employee_abs$Social_smoker))
Employee_abs$Pet = as.factor(as.character(Employee_abs$Pet))

str(Employee_abs)

#from the data summary we can see there is variable ID.will remove it which is not usefull
Employee_abs=subset(Employee_abs,select=c(-ID))
#unique value of each count
apply(Employee_abs, 2,function(x) length(table(x)))

      
#Since month variable can contain 12 values here we are replacing 0 with NA
Employee_abs$`Month_of_absence`[Employee_abs$`Month_of_absence` %in% 0]=NA

#Divide work load avarege/day variable by 1000(as per support team advise )
Employee_abs$`Work_load_Average_day`=Employee_abs$`Work_load_Average_day`/1000

#Extract column names of numeric and categorical variables
Numeric_cnames = c('Distance_from_Residence_to_Work', 'Service_time', 'Age',
           'Work_load_Average_day', 'Transportation_expense',
           'Hit_target', 'Weight', 'Height', 
           'Body_mass_index', 'Absenteeism_time_in_hours')

cat_cnames = c('Reason_for_absence','Month_of_absence','Day_of_the_week',
               'Seasons','Disciplinary_failure', 'Education', 'Social_drinker',
               'Social_smoker', 'Son', 'Pet')
################Data Pre processing#################
#######Missing value analysis###########

missing_val=data.frame(apply(Employee_abs,2,function(x){sum(is.na(x))}))

#we found missing values in target variable 
#removing obervations in which target variable having missing values
Employee_abs=Employee_abs[(!Employee_abs$`Absenteeism_time_in_hours` %in% NA),]

#remaining missing values
missing_val=data.frame(apply(Employee_abs,2,function(x){sum(is.na(x))}))
missing_val$colnames=row.names(missing_val)
names(missing_val)[1]="Missing_percentage"
missing_val$`Missing_percentage`=(missing_val$`Missing_percentage`/nrow(Employee_abs))*100
missing_val=missing_val[order(-missing_val$`Missing_percentage`),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
write.csv(missing_val, "Miising_perc.csv", row.names = F)

##Visualizing missing value percentage########

ggplot(data = missing_val[1:3,], aes(x=reorder(colnames, -Missing_percentage),y = Missing_percentage))+
   geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
   ggtitle("Missing data percentage") + theme_bw()

df=Employee_abs
#Employee_abs=df
sum(is.na(Employee_abs))
#Missing value imputation for categorical variables#
#Mode method-
  mode <- function (x, na.rm) {
    xtab <- table(x)
    xmode <- names(which(xtab == max(xtab)))
    if (length(xmode) > 1) xmode <- ">1 mode"
    return(xmode)
  }

for(i in cat_cnames){
  print(i)
  Employee_abs[,i][is.na(Employee_abs[,i])] = mode(Employee_abs[,i])
}
str(Employee_abs)
#missing values imputation for numeric variables
#lets take one sample variable
Employee_abs$`Body_mass_index`[6]
#actual value=27
#mean method=26.70
#median method=25
#KNN=27
Employee_abs$`Body_mass_index`[6]=NA
Employee_abs$`Body_mass_index`[6]

#mean method
#Employee_abs$`Body_mass_index`[is.na(Employee_abs$`Body_mass_index`)]=mean(Employee_abs$`Body_mass_index`,na.rm = T)       

#median method
#Employee_abs$`Body_mass_index`[is.na(Employee_abs$`Body_mass_index`)]=median(Employee_abs$`Body_mass_index`,na.rm = T)       

#KNN Imputation # reload the data and perform Knn

library(VIM)
Employee_abs=kNN(Employee_abs,variable=c('Distance_from_Residence_to_Work', 'Service_time', 'Age',
                                         'Work_load_Average_day', 'Transportation_expense',
                                         'Hit_target', 'Weight', 'Height', 
                                         'Body_mass_index', 'Absenteeism_time_in_hours'), k=5)

#In KNN imputaion we get some extra variables (logical) so we delete those junk variable below 
Employee_abs=subset(Employee_abs,select=c('Distance_from_Residence_to_Work', 'Service_time', 'Age',
                                          'Work_load_Average_day', 'Transportation_expense',
                                          'Hit_target', 'Weight', 'Height', 
                                          'Body_mass_index', 'Absenteeism_time_in_hours','Reason_for_absence','Month_of_absence','Day_of_the_week',
                                          'Seasons','Disciplinary_failure', 'Education', 'Social_drinker',
                                          'Social_smoker', 'Son', 'Pet'))
#from the all above methods we can see knn is more accurate so we are imputed with knn method
sum(is.na(Employee_abs))
#now data is free from missing values
#######outlier Analysis #######
#save data for the reference 
df2=Employee_abs
#Employee_abs=df
#Box plots-Distributaion and outlier check
Numeric_index=sapply(Employee_abs,is.numeric)#selecting only numeric
Numeric_data=Employee_abs[,Numeric_index]
cnames=colnames(Numeric_data)
cnames
for(i in 1:length(cnames)){
  assign(paste0("AB",i),ggplot(aes_string(y=(cnames[i]),x="Absenteeism_time_in_hours"),
                               d=subset(Employee_abs))
         +geom_boxplot(outlier.colour = "Red",outlier.shape = 18,outlier.size = 2,
                       fill="skyblue4")+theme_gray()
         +stat_boxplot(geom = "errorbar", width=0.5)
         +labs(y=cnames[i],x="Absenteeism_time_in_hours")
         +ggtitle("Box Plot of Absenteeism for",cnames[i]))
}
#ploting plots together
gridExtra::grid.arrange(AB1,AB2,ncol=2)
gridExtra::grid.arrange(AB3,AB4,ncol=2)
gridExtra::grid.arrange(AB5,AB6,ncol=2)
gridExtra::grid.arrange(AB7,AB8,ncol=2)
gridExtra::grid.arrange(AB9,AB10,ncol=2)


#remove Outliers using boxplotmethod
#Loop to remove from all variables

#for(i in cnames){
#  print(i)
#  val=Employee_abs[,i][Employee_abs[,i]%in% boxplot.stats(Employee_abs[,i])$out]
#  Employee_abs=Employee_abs[which(!Employee_abs[,i] %in% val),]
#}

#replace all outliers with NA and Impute
for(i in cnames){
  print(i)
  val=Employee_abs[,i][Employee_abs[,i]%in% boxplot.stats(Employee_abs[,i])$out]
  Employee_abs[,i][Employee_abs[,i] %in% val]=NA
}
sum(is.na(Employee_abs))
Employee_abs=knnImputation(Employee_abs,k=3)

df1=Employee_abs
#Employee_abs=df1
#here the data is free from outliers.
########Feature Selection#########
#Correlation Analysis for continuous variables-
library(corrgram)    #Library for correlation plot

corrgram(Employee_abs[,cnames],order=FALSE,upper.panel = panel.pie,
         text.panel = panel.txt,font.labels =1,
         main="Correlation plot for Absenteeism")
#Correlated variables are = weight & Body mass index.

#Anova Test for categorical variable-
for(i in cat_cnames){
  print(i)
  Anova_result= summary(aov(formula = Absenteeism_time_in_hours~Employee_abs[,i],Employee_abs))
  print(Anova_result)
}

#redudant categorical variables- Social_smoker,Education,Seasons,Day_of_the_week

#Dimensionity Reduction
Employee_abs= subset(Employee_abs,select=-c(Weight,Social_smoker,Education,Seasons,Day_of_the_week))
dim(Employee_abs)

#######Feature Scaling#########33
df3=Employee_abs
#Employee_abs=df3
# update the numeric and categorical variables
cnames=c('Distance_from_Residence_to_Work', 'Service_time', 'Age',
         'Work_load_Average_day', 'Transportation_expense',
         'Hit_target', 'Height', 
         'Body_mass_index', 'Absenteeism_time_in_hours')
cat_cnames=c('Reason_for_absence','Month_of_absence','Disciplinary_failure', 'Social_drinker', 'Son', 'Pet')
#summary of data to check min and max values of numeric variables-

summary(Employee_abs)

#Skewness of numeric variables-
library(propagate)

for(i in cnames){
  skew = skewness(Employee_abs[,i])
  print(i)
  print(skew)
}

#log transform
Employee_abs$Absenteeism_time_in_hours = log1p(Employee_abs$Absenteeism_time_in_hours)
#Normality check
qqnorm(Employee_abs$Transportation_expense)
hist(Employee_abs$Absenteeism_time_in_hours,col="blue",main="Histogram of Absenteeism ")
hist(Employee_abs$Transportation_expense)
hist(Employee_abs$Work_load_Average_day)
#from above histogram plots we can say that data is not normally distributed
#so best method is normalization
#Normalization
for (i in cnames) {
  if(i != "Absenteeism_time_in_hours")
  {
    print(i)
    Employee_abs[,i]=(Employee_abs[,i]-min(Employee_abs[,i]))/(max(Employee_abs[,i])-min(Employee_abs[,i]))
    
  }
}
#Summary of data after all preprocessing-
summary(Employee_abs)

write.csv(Employee_abs,"Absenteeism_Pre_processed_Data.csv",row.names=FALSE)

########Model Development##########
#clean the environment
library("DataCombine")
rmExcept("Employee_abs")

#save the data for reference 
df4=Employee_abs

cat_cnames=c('Reason_for_absence','Month_of_absence','Disciplinary_failure', 'Social_drinker', 'Son', 'Pet')

#create dummy variable for categorical variables-
library(dummies)
Employee_abs = dummy.data.frame(Employee_abs, cat_cnames)

dim(Employee_abs)

#Divide the data into train and test-
set.seed(4567)
train_index= sample(1:nrow(Employee_abs),0.8*nrow(Employee_abs))
train= Employee_abs[train_index,]
test= Employee_abs[-train_index,]
#########Decision Tree Regression#######


#Model devlopment for train data

library(rpart)    #Library for regression model

DT_model= rpart(Absenteeism_time_in_hours~.,train,method="anova")
DT_model
#Prediction for train data-
DT_train=predict(DT_model,train[-9])
#Prediction for test data-
DT_test=predict(DT_model,test[-9])

#Error metrics to calculate the performance of model-
rmse= function(y,y1){
  sqrt(mean(abs(y-y1)^2))
}

rmse(train[,9],DT_train)
#rmse=0.4369


#RMSE calculation for test data-
rmse(test[,9],DT_test)
#RMSE_test= 0.5030

#r-square calculation-
#function for r-square-
rsquare=function(y,y1){
  cor(y,y1)^2
}

#r-square calculation for train data-
rsquare(train[,9],DT_train)
#r-square_train= 0.54


#r-square calculation for test data-
rsquare(test[,9],DT_test)
#r-square_test= 0.4366973

#Visulaization to check the model performance on test data-
plot(test$Absenteeism_time_in_hours,type="l",lty=1.8,col="Green",main="Decision Tree")
lines(DT_test,type="l",col="Blue")

#Write rule into drive-
write(capture.output(summary(DT_model)),"Decision_Tree_Model.txt")
#############Random Forest##########
library(randomForest)  #Library for randomforest machine learning algorithm
library(inTrees)       #Library for intree transformation

RF_model= randomForest(Absenteeism_time_in_hours~.,train,ntree=300,method="anova")
#transform ranfomforest object to inTree format-
treelist= RF2List(RF_model)

#Extract rules-
rules= extractRules(treelist,train[-9])
rules[1:5,]

#Make rules into redable format-
readable_rules= presentRules(rules,colnames(train))
readable_rules[1:5,]
#Get Rule metrics-
rule_metrics= getRuleMetric(rules,train[-9],train$Absenteeism_time_in_hours)
rule_metrics= presentRules(rule_metrics,colnames(train))
rule_metrics[1:10,]
summary(rule_metrics)
#Check model performance on train data
RF_train= predict(RF_model,train[-9])

RF_train
#Check model performance on test data
RF_test= predict(RF_model,test[-9])
RF_test


#RMSE calculation for train data-
rmse(train[,9],RF_train)
#RMSE_train=  0.2390

#RMSE calculation for test data-
rmse(test[,9],RF_test)
#RMSE_test=  0.4762

#r-square calculation for train data-
rsquare(train[,9],RF_train)
#r-square= 0.8768

#r-square calculation for test data-
rsquare(test[,9],RF_test)
#r-square= 0.4940

#Visulaization to check the model performance on test data-
plot(test$Absenteeism_time_in_hours,type="l",lty=1.8,col="Green",main="Random Forest")
lines(RF_test,type="l",col="Blue")

#write rule into drive-
write(capture.output(summary(rule_metrics)),"Random_Forest_Model.txt")
###########Linear Regression###########

#recall numeric variables to check the VIF-
numeric_index1= c("Transportation_expense","Distance_from_Residence_to_Work","Service_time",
                  "Age","Work_load_Average_day","Hit_target","Height",
                  "Body_mass_index","Absenteeism_time_in_hours")
numeric_data1= Employee_abs[,numeric_index1]
cnames1= colnames(numeric_data1)
cnames1

library(usdm)  #Library for VIF(Variance Infleation factor)
vif(numeric_data1)
vifcor(numeric_data1,th=0.7) #VIF calculation for numeric variables

#Linear regression model-
lr_model= lm(Absenteeism_time_in_hours~.,train)
summary(lr_model)

#check model performance on train data-
lr_train= predict(lr_model,train[-9])

#check model performance on test data-
lr_test= predict(lr_model,test[-9])

#RMSE calculation for train data-
rmse(train[,9],lr_train)
#RMSE_train=0.4061

#RMSE calculation for test data-
rmse(test[,9],lr_test)
#RMSE_test=0.4922

#r-square calculation for train data-
rsquare(train[,9],lr_train)
#r-square_train=0.6038

#r-square calculation for test data-
rsquare(test[,9],lr_test)
#r-square_test=0.4639

#Visulaization to check the model performance on test data-
plot(test$Absenteeism_time_in_hours,type="l",lty=1.8,col="Green",main="Linear Regression")
lines(lr_test,type="l",col="Blue")

write(capture.output(summary(lr_model)),"Linear_Regression_Model.txt")
############Gradient Boosting########
library(gbm)

#Develop Model
GB_model = gbm(Absenteeism_time_in_hours~., data = train, n.trees = 100, interaction.depth = 2)

#check model performance on train data-
GB_train = predict(GB_model, train,n.trees = 100)


#check model performance on test data-
GB_test = predict(GB_model, test, n.trees = 100)

#RMSE calculation for train data-
rmse(train[,9],GB_train)
#RMSE_train=0.408

#RMSE calculation for test data-
rmse(test[,9],GB_test)
#RMSE_test=0.5035

#r-square calculation for train data-
rsquare(train[,9],GB_train)
#r-square_train=0.7113

#r-square calculation for test data-
rsquare(test[,9],GB_test)
#r-square_test=0.4405

#Visulaization to check the model performance on test data-
plot(test$Absenteeism_time_in_hours,type="l",lty=1.8,col="Green",
     main="Gradient Boosting")
lines(GB_test,type="l",col="Blue")
##############Thank you##########