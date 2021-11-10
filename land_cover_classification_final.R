library("e1071")        # SVM methodology
library("caret")
library("rgdal")

setwd("C:/Users/trevorcaughlin/Dropbox/restorationGuyana/manuscript/insect_paper_revisions/final code and data")

full_ras=read.csv("all_objects.csv") #this csv file includes an ID column for the ID of the object, the geometry of the object
#(area, edge, and E_A--edge to area ratio), and the mean and standard deviation in each of the four bands plus NDVI.

ras<-full_ras[,-which(colnames(full_ras) %in% c("ID")==T)] 
#removing the ID column to predict on just the predictor variables

landfit<-read.csv("landcover_training_data.csv") #this csv represents the training data

landfit$Landcover<-as.factor(landfit$Landcover) #need to convert to factor for this to run

fold_it_up=createFolds(landfit$Landcover,k=10,list=T,returnTrain=T) #creating training and test data folds

mod_predict=vector("list",length=10) #creating empty lists to store output in
mod_ori=vector("list",length=10)
  
for(i in 1:10){ #this for loop iterates through each set of the 10 folds
traindat=landfit[fold_it_up[[i]],] 
testdat=landfit[-fold_it_up[[i]],]

#MODEL
#MODEL
svm_1<-svm(Landcover~., data=traindat,kernel="linear") #the support vector machine
#MODEL
#MODEL

testy=predict(svm_1,newdata=testdat) #predicting the support vector machine on the test data

mod_predict[[i]]<-testy
mod_ori[[i]]<-testdat$Landcover
}

all_pred=unlist(mod_predict) #returning the 10 folds to a single data frame
all_obs=unlist(mod_ori)

#overall accuracy
1-length(which(all_obs!=all_pred))/length(all_pred)

confused=confusionMatrix(data=all_pred,reference=all_obs) #confusion matrix

###fit full model

svm_final<-svm(Landcover~., data=landfit,kernel="linear") #this is predicting across the full model to the object data

ras_cats=predict(svm_final,newdata=ras)

full_ras$landcover=ras_cats

#next step is to merge  the predicted land cover with the spatial polygons that represent objects