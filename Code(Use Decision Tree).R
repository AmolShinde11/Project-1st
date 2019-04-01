
#Use Decision tree 

library(DAAG) # library for CV
library(ggplot2)
#install.packages("DAAG")
library(class)
library(caret)
library(tree)
library(rpart)
library(rpart.plot)
library(randomForest)
#install.packages("ROSE")=accuracy.meas
library(ROSE)


#import data 
path="G:/Only R/R project/diabetic_data.csv"
diaibetes= read.csv(path, header=T, stringsAsFactors = F)


# summary for the all colume
summary(diabetes[1:49],)


# check for Nulls, Zeroes for all columns(chek any number in data 0,?,..etc)
col_name = colnames(diabetes) [apply(diabetes, 2, function(n) any(is.na(n)))]
if(length(col_name) > 0) print("NULLs present") else print("No NULLs")

col_name = colnames(diabetes) [apply(diabetes, 2, function(n) any(n == ""))]
if(length(col_name) > 0) print("Blanks present") else print("No Blanks")

col_name = colnames(diabetes) [apply(diabetes, 2, function(n) any(n=="?"))]
if(length(col_name)>0) 
{
  print("Qution Mark present")
  print(col_name)
}else 
  print("NO qution mark")

col_name = colnames(diabetes) [apply(diabetes, 2, function(n) any(n==0))]
print(col_name)
if(length(col_name) > 0)
{
  print("Zeroes present in columns : ")
  print(col_name)
} else 
  
  print("No Zeroes")



#########################################################################
colnames(diabetes)

#delited 7 column
diabetes2 <- subset(diabetes,select=-c(encounter_id, patient_nbr, examide,citoglipton,weight, payer_code, medical_specialty)) 


diabetes2$race[diabetes2$race == "?"] = "Other"

diabetes2$diag_1=as.factor(diabetes2$diag_1)
diabetes2$diag_2=as.factor(diabetes2$diag_2)
diabetes2$diag_3=as.factor(diabetes2$diag_3)

#convert ?=NA
diabetes2$diag_1[diabetes2$diag_1 == "?"] = "NA"
diabetes2$diag_2[diabetes2$diag_2 == "?"] = "NA"
diabetes2$diag_3[diabetes2$diag_3 == "?"] = "NA"
head(diabetes2)


#bainary to repre of reamitted  (<30=1,>30/NA=0)
diabetes2$readmittedbin <- ifelse(diabetes2$readmitted == "<30",1,0) 


# creat one colume (43+1=column)
diabetes3 <- cbind(diabetes2[c(7:13,17)], lapply(diabetes2[c(1:6,14:16,18:44)],factor))
head(diabetes3)
ncol(diabetes3)


#table to get frquency of different levels of readmission
table(diabetes3$readmitted)  #1st
prop.table(table(diabetes3$readmitted)) #2nd


# creat readmittedbin column
table(diabetes3$readmittedbin)   #  binary (0,1)(<30=1, Others=0)
prop.table(table(diabetes3$readmittedbin))



# splite race wise
racewise <- table(diabetes3$readmittedbin,diabetes3$race)
racewise


#Genderwise splite
genderwise <- table(diabetes3$readmittedbin,diabetes3$gender)
genderwise

#age wise splite
agewise <- table(diabetes3$readmittedbin,diabetes3$age)
agewise

#time in
timinhoswise <- table(diabetes3$readmittedbin,diabetes3$time_in_hos)
timinhoswise




#Graph
plot(Number_patients,col ="lightblue", xlab = " Readmission Days ", 
     main= " Frequency of Readmission", lwd =20,pch=18)

#frequncy of Readmission by readmittedbin
Number_patients_bin <- table(diabetes3$readmittedbin)
plot(Number_patients_bin,col ="lightblue", xlab = " Readmission Days ",
     main= " Frequency of Readmission", lwd =20,pch=18)


# randomly shuffle the dataset
grp = runif(nrow(diabetes3))
diabetes3 = diabetes3[order(grp),]

###########splite data (tain/test)
set.seed(111)

#1st mathod
inTrain <- createDataPartition(diabetes3$readmittedbin, p=.7, list=FALSE)
Train <- diabetes3[inTrain,]
Test <- diabetes3[-inTrain,]
table(Train$readmittedbin)
table(Test$readmittedbin)

#############################OR##########################################
#2nd method
sample_size = floor(0.7*nrow(diabetes3))
sample_ind = sample(seq_len(nrow(diabetes3)), sample_size)
train = diabetes3[sample_ind,]
test = diabetes3[-sample_ind,]
tr=nrow(train)
ts=nrow(test)

#train
table(Train$readmittedbin)
prop.table(table(Train$readmittedbin))


#Test
table(Test$readmittedbin)
prop.table(table(Test$readmittedbin))


########
##Prediction with three levels of response variable
cfit <- rpart(readmitted ~ time_in_hospital 
              + num_lab_procedures 
              + num_procedures 
              + num_medications
              + number_outpatient
              + number_emergency
              + number_inpatient
              + race + age + admission_type_id
              + discharge_disposition_id
              +admission_source_id
              + number_diagnoses + max_glu_serum 
              + A1Cresult + metformin.pioglitazone 
              + insulin, data = Train, method="class", minsplit = 20, minbucket = 5, cp = 0.001)

head(predict(cfit))






#one mathode
par(mar=c(1,1,0.25,1))
plot(cfit, branch = 0.4,uniform = TRUE, compress = TRUE)
text(cfit, pretty = 0)



rpart.predict <- predict(cfit, newdata = Train, type="class")
tail(rpart.predict)


#accuracy
cf <-confusionMatrix(rpart.predict, Train$readmitted)
cf


#Mean error rate
mean.error.rate.rpart <- 1- cf$overall[1]
mean.error.rate.rpart


par(mar=c(3,3,3,3))
plotcp(cfit,lty = 3, col = 1)
printcp(cfit)








# Cross-validation of the tree
cfit.tree <- tree(readmitted ~ time_in_hospital + num_lab_procedures + num_procedures + num_medications
                  + number_outpatient
                  + number_emergency
                  + number_inpatient
                  + race + age + admission_type_id
                  + discharge_disposition_id
                  +admission_source_id
                  + number_diagnoses 
                  + max_glu_serum + A1Cresult 
                  + metformin.pioglitazone + insulin, data = Train, method="class")

cv.cfit.tree <- cv.tree(cfit.tree, FUN = prune.misclass)
cv.cfit.tree



prune.cfit.tree <- prune.misclass(cfit.tree, best = 4)


#plot(prune.cfit.tree)
text(prune.cfit.tree, pretty = 0)

#After pruning
cfit2 = prune(cfit, cp = 0.0014)

par(mar=c(1,1,0.25,1))
plot(cfit2, branch = 0.4,uniform = TRUE, compress = TRUE)
text(cfit2)




#Using the pruned tree to predict and pulling up the mean error rate and confusion matrix

#Prediction on test set
rpart.prune.predict <- predict(cfit2, newdata = Test,type = "class")

cf.prune <-confusionMatrix(rpart.prune.predict,Test$readmitted)

#Mean error rate
mean.error.rate.rpart.prune <- 1- cf.prune$overall[1]
mean.error.rate.rpart.prune


cf.prune$table

#Decision Tree with two class response variable

cfit_bin <- rpart(readmittedbin ~time_in_hospital + num_lab_procedures + num_procedures + num_medications
                  + number_outpatient
                  + number_emergency
                  + number_inpatient
                  + race + age + admission_type_id
                  + discharge_disposition_id
                  +admission_source_id
                  + number_diagnoses + max_glu_serum 
                  + A1Cresult + metformin.pioglitazone 
                  + insulin, data = Train, method="class", minsplit = 1, minbucket = 1, cp = 0.001)

par(mar=c(2,2,0.25,1))
plot(cfit_bin, branch = 0.4,uniform = TRUE, compress = TRUE)
text(cfit_bin, pretty = 0)


rpart.predict_bin <- predict(cfit_bin, newdata = Train,type="prob")

View(Train)
head(rpart.predict_bin)

View(rpart.predict_bin)
accuracy.meas(Train$readmittedbin, rpart.predict_bin[,2])


roc.curve(Train$readmittedbin, rpart.predict_bin[,2], plotit = T)

par =TRUE

str(rpart.predict_bin)

str(objTrain$readmittedbin)




#After pruning
cfit2_bin = prune(cfit_bin, cp = 0.0001)

par(mar=c(.5,.5,.5,.5))
plot(cfit2_bin, branch = 0.4,uniform = TRUE, compress = TRUE)
text(cfit2_bin, pretty=0)
head(predict(cfit2_bin))

#Prediction on training  set
rpart.prune.predict2_bin <- predict(cfit2_bin, newdata = Train,type = "class")

cf.prune_bin <-confusionMatrix(rpart.prune.predict2_bin,Train$readmittedbin)
cf.prune_bin


#Mean error rate
mean.error.rate.rpart.prune2 <- 1- cf.prune_bin$overall[1]
mean.error.rate.rpart.prune2

