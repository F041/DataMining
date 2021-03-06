setwd("C:/Users/F041/Downloads")

library(tidyverse)  # for data wrangling /discussione dei dati
library(rvest)      # for web scraping strae i dati da una pagina web
library(lubridate)  # for date formats  packeto per gestire i dati
library(stringr)    # manipuate strings 

#importazione da excel
#importazione da excel
#importazione da excel

library(xlsx)
library(tm) 
library(SnowballC)
library(wordcloud)
library(tidyr)
library(dplyr)
library(glmnet)
library(caret)

ca<-read.xlsx("calcio.xlsx", sheetName = "Sheet1",header = FALSE)
ca1<-read.xlsx("betting2019-2020to 2011-2012 (1).xlsx", sheetName = "Sheet1", header = FALSE)
ca<- ca[!grepl("ROUND",ca$X1),]#levami tutte le righe che contengono ROUND(grepl=contenere)
ca1<- ca1[!grepl("ROUND",ca1$X1),]#levami tutte le righe che contengono ROUND(grepl=contenere)
ca1$X6=as.Date(ca1$X6,format = "%d.%m.%Y");


ca=as.data.frame(ca)

head(ca)

ca<-bind_rows(ca, ca1)
#squadra
#squadra
#squadra

squadra=ca$X1
squadra=as.data.frame(squadra)
squadra$squadra=tolower(squadra$squadra)
squadra$squadra=trimws(squadra$squadra)
squadra$squadra=removeNumbers(squadra$squadra)
squadra$squadra=stripWhitespace(squadra$squadra)
squadra= separate (squadra, squadra,c("S1","S2"),"-")
squadra$S1=trimws(squadra$S1)
squadra$S2=trimws(squadra$S2)

#Punteggio
#Punteggio
#Punteggio

punt=ca$X2
punt=as.data.frame(punt)
punt$punt=tolower(punt$punt)
punt$punt=trimws(punt$punt)
punt$punt=stripWhitespace(punt$punt)
punt= separate (punt, punt,c("P1","P2","P3"),"_")
punt$P1=trimws(punt$P1)
punt$P2=trimws(punt$P2)
punt$P1=as.integer(punt$P1)
punt$P2=as.integer(punt$P2)
punt$P3=NULL


#DATE
#DATE
#DATE

library(lubridate)
class(ca$X6)
ca$mese=month(ca$X6)
ca$anno=year(ca$X6)

#rimove space
#rimove space
#rimove space

ca$X3=trimws(ca$X3)
ca$X4=trimws(ca$X4)
ca$X5=trimws(ca$X5)

#dataset finale
#dataset finale
#dataset finale
names(ca)

ca$X1=NULL
ca$X2=NULL
ca$X6=NULL

ca=cbind(squadra,punt,ca)

ca$ris=case_when(ca$P1>ca$P2~"1",
                       ca$P1<ca$P2~"2",
                       ca$P1==ca$P2~"2")
ca$ris<-as.integer(ca$ris)
ca$X3<-as.numeric(ca$X3)
ca$X4<-as.numeric(ca$X4)
ca$X5<-as.numeric(ca$X5)


dati<-ca
### Modello inferenziale e GAM Analisi ----

glm1<-glm(ris~S1+S2+X3+X4+X5+mese+anno+S1*anno+S2*anno,data=ca)
summary(glm1)
anova(glm1)
drop1(glm1)

glm3<-glm(ris~S1+S2+X3+X4+X5+mese+anno+S1*anno+S2*anno+S1*S2,data=ca)
summary(glm3) #non serve

library(gam)
gam1<-gam(ris~s(X3)+s(X4),data=dati)
plot(gam1)
#suggerisce log(X3+1)

##
ca$ris2<-ifelse(ca$ris>1,1,0)
ca$ris2sistemato<-ifelse(ca$ris2==0,1,0)


write.csv(ca,"C:/Users/F041/Downloads/CalcioTotale.csv", row.names = FALSE)



#### Preparazione dati ------
dati$S1<-as.factor(dati$S1)
dati$S2<-as.factor(dati$S2)
dati<-na.omit(dati)
split <- createDataPartition(y=dati$ris, p = 0.7, list = FALSE)
train <- dati[split,]
test <- dati[-split,]
train$ris<-as.factor((train$ris))
levels(train$ris) <- c("Win", "Bank")
train<-train[!train$S1=="cesena" | !train$S2=="cesena",]

loss_matr <- matrix(c(0, -10,-10, 0), nrow = 2);loss_matr

####  Lasso -----
set.seed(1234)
grid = expand.grid(.alpha=1,.lambda=seq(0, 1, by = 0.001))
Control=trainControl(method= "cv",number=10, classProbs=TRUE)
glm_lasso=train(ris~log(X3+1)+S1*anno+S2*anno+anno, method = "glmnet", data=train,
                trControl = Control, tuneLength=10, tuneGrid=grid, metric="Accuracy", na.action=na.exclude, 
                cost = list(loss=loss_matr))
glm_lasso
plot(glm_lasso)
getTrainPerf(glm_lasso)
plot(varImp(object=glm_lasso),main="train tuned - Variable Importance")


### Albero ------
set.seed(1)
Grid2 = expand.grid(.cp     =seq(0,1,  by=0.05)            )
cvCtrl <- trainControl(method = "cv", number=10, classProbs = TRUE)
rpartTuneCvA <- train(ris~S1+S2+log(X3+1)+S1*anno+S2*anno+anno, data = train, method = "rpart",
                      tuneLength = 10, na.action=na.exclude, tuneGrid=Grid2,
                      trControl = cvCtrl, metric="Accuracy")

rpartTuneCvA
getTrainPerf(rpartTuneCvA)
plot(varImp(object=rpartTuneCvA),main="train tuned - Variable Importance")


### Random Forest -----
set.seed(1)
Grid4 = expand.grid(.mtry   =seq(1,8,  by=2)               )
cvCtrl <- trainControl(method = "cv", number=10, classProbs = TRUE)
rfTune <- train(ris~S1+S2+log(X3+1)+S1*anno+S2*anno+anno, data = train, method = "rf",
                tuneLength = 5,
                trControl = cvCtrl, na.action=na.exclude, tuneGrid=Grid4,
                parms = list(loss=loss_matr)) # con tuneLength=5 ci mette tanto
rfTune
getTrainPerf(rfTune)
plot(varImp(object=rfTune),main="train tuned - Variable Importance")

### Neural Net -----
set.seed(2)
Grid0 = expand.grid(.size   =seq(1,7,  by=1), .decay = 0.1 )
cvCtrl <- trainControl(method = "cv", number=10, classProbs = TRUE)
NNTune <- train(ris~S1+S2+log(X3+1)+S1*anno+S2*anno+anno, data = train, method = "nnet",
                tuneLength = 4, tuneGrid=Grid0,
                trControl = cvCtrl, preProcess="range", na.action=na.exclude, parms = list(loss=loss_matr)) # Non andare oltre il 6 di tuneLength 
NNTune
getTrainPerf(NNTune)
plot(varImp(object=NNTune),main="train tuned - Variable Importance")

### Xtreme Gradient -----
set.seed(3)
cvCtrl <- trainControl(method = "cv", number=10, search="grid", classProbs = TRUE)
XGBTune <- train(ris~S1+S2+log(X3+1)+S1*anno+S2*anno+anno, data = train, method = "xgbTree",
                 tuneLength = 3,
                 trControl = cvCtrl, metric="Accuracy", na.action=na.exclude) # Non andare oltre il 6 di tuneLength 
XGBTune
getTrainPerf(XGBTune)
plot(varImp(object=XGBTune),main="train tuned - Variable Importance")

### Adaboost ------
set.seed(5)
cvCtrl <- trainControl(method = "cv", number=10, search="grid", classProbs = TRUE)
AdaBTune <- train(ris~S1+S2+X3+S1*anno+S2*anno+anno, data = train, method = "AdaBoost.M1",
                  tuneLength = 6, na.action=na.exclude, #min lenght =2
                  trControl = cvCtrl, metric="Accuracy")

AdaBTune
getTrainPerf(AdaBTune)

### Naive Bayes ------
set.seed(5)
cvCtrl <- trainControl(method = "cv", number=10, search="grid", classProbs = TRUE)
NBtune <- train(ris~S1+S2+log(X3+1)+S1*anno+S2*anno+anno, data = train, method = "nb",
                  tuneLength = 5, na.action=na.exclude,
                  trControl = cvCtrl, metric="Sens")

NBtune
getTrainPerf(NBtune)

### PLS ------

pls=train(ris~S1+S2+X3+S1*anno+S2*anno+anno, data = train, method = "pls", 
          trControl = cvControl, tuneLength=10, na.action=na.exclude)
pls
getTrainPerf(pls)

### deep boost ------
set.seed(115)
cvCtrl <- trainControl(method = "cv", number=10, search="grid", classProbs = TRUE)
Deep=train(ris~S1+S2+X3+S1*anno+S2*anno+anno, data = train, method = "deepboost", 
          trControl = cvCtrl, tuneLength=4, na.action=na.exclude)
Deep
getTrainPerf(Deep)

### KNN ------
set.seed(115)
cvCtrl <- trainControl(method = "cv", number=10, search="grid", classProbs = TRUE)
kknnT=train(ris~S1+S2+X3+S1*anno+S2*anno+anno, data = train, method = "kknn", 
           trControl = cvCtrl, tuneLength=5, na.action=na.exclude)
kknnT
getTrainPerf(kknnT)



### SVM ------
set.seed(111)
cvCtrl <- trainControl(method = "cv", number=10, search="grid", classProbs = TRUE)
svmT=train(ris~S1+S2+X3+S1*anno+S2*anno+anno, data = train, method = "svmLinear3", 
            trControl = cvCtrl, tuneLength=5, na.action=na.exclude)
svmT
getTrainPerf(svmT)

### Risultati ----
results <- resamples(list(Lasso=glm_lasso, Tree=rpartTuneCvA, RandomForest=rfTune, 
                          NeuralNet=NNTune, XGBoosting=XGBTune, NaivaBayes=NBtune, Ada=AdaBTune,
                          deep=Deep, kknn=kknnT, svm=svmT));results 
bwplot(results)

### Test -----
# estimate probs P(M)
test<-test[!test$S1=="cesena" & !test$S2=="cesena",]
test<-test[!test$S1=="torino"& !test$S2=="torino",]
test<-test[!test$S1=="reggina"& !test$S2=="reggina",]
test<-na.omit(test)
test$p1 = predict(glm_lasso       , test, "prob")[,1]
test$p2 = predict(rpartTuneCvA, test, "prob")[,1]
test$p3 = predict(rfTune    , test, "prob")[,1]
test$p4 = predict(NNTune     , test, "prob")[,1]
test$p5 = predict(XGBTune, test, "prob")[,1]
test$p6 = predict(AdaBTune, test, "prob")[,1]
test$p7 = predict(pls, test, "prob")[,1]
test$p8 = predict(NBtune, test, "prob")[,1]
test$p9 = predict(Deep, test, "prob")[,1]
test$p10 = predict(svmT, test, "prob")[,1]



# roc values
library(pROC)
r1=roc(ris ~ p1, data = test)
r2=roc(ris ~ p2, data = test)
r3=roc(ris ~ p3, data = test)
r4=roc(ris ~ p4, data = test)
r5=roc(ris ~ p5, data = test)
r6=roc(ris ~ p6, data = test)
r7=roc(ris ~ p7, data = test)
r8=roc(ris ~ p8, data = test)
r9=roc(ris ~ p9, data = test)


plot(r1) #lasso nero
plot(r2,add=T,col="red") #Tree
plot(r3,add=T,col="blue") #RF
plot(r4,add=T,col="yellow") #NN
plot(r5,add=T,col="violet") #XGB
plot(r6,add=T,col="orange") #Ada
plot(r7,add=T,col="green") #PLS
plot(r8,add=T) #NB
plot(r9,add=T) #deep
legend("bottomright", c("lasso","Tree", "Random Forest", "NN","XGB","ADA", "PLS", "NB", "Deep"),
       text.col=c("black","red","blue","yellow","violet","orange","Green"))

### Scelta soglia migliore modello -----
pROC_obj <- roc(test$ris,test$p1,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)

plot(pROC_obj, print.thres = quantile(pROC_obj$thresholds, seq(0.3,0.85,0.05))) #suggerisce un 0.516


### Risultati miglior modello, Lasso ------
test$labelsp1<-as.factor(ifelse(test$p1>0.526,2,1))
test$ris<-as.factor(test$ris)
cm<-confusionMatrix(test$labelsp1, test$ris, positive="1") #Tante metriche, bellissimo

draw_confusion_matrix <- function(cm) {
  
  total <- sum(cm$table)
  res <- as.numeric(cm$table)
  
  # Generate color gradients. Palettes come from RColorBrewer.
  greenPalette <- c("#F7FCF5","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#006D2C","#00441B")
  redPalette <- c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")
  getColor <- function (greenOrRed = "green", amount = 0) {
    if (amount == 0)
      return("#FFFFFF")
    palette <- greenPalette
    if (greenOrRed == "red")
      palette <- redPalette
    colorRampPalette(palette)(100)[10 + ceiling(90 * amount / total)]
  }
  
  # set the basic layout
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  classes = colnames(cm$table)
  rect(150, 430, 240, 370, col=getColor("green", res[1]))
  text(195, 435, classes[1], cex=1.2)
  rect(250, 430, 340, 370, col=getColor("red", res[3]))
  text(295, 435, classes[2], cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col=getColor("red", res[2]))
  rect(250, 305, 340, 365, col=getColor("green", res[4]))
  text(140, 400, classes[1], cex=1.2, srt=90)
  text(140, 335, classes[2], cex=1.2, srt=90)
  
  # add in the cm results
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}
draw_confusion_matrix(cm)


### Redditivitą ----
test$bet<-10
test$bet_res<-ifelse(test$ris ==test$labelsp1 & test$ris==1, test$bet*test$X3,0)
test$bet_res2<-ifelse(test$ris ==test$labelsp1 & test$ris==2, 
                      test$bet*((test$X4+test$X5)/2),0)
sum(test$bet_res)
sum(test$bet_res2)
sum(test$bet)

test$ROI<-((test$bet_res)+(test$bet_res2)-(test$bet))/(test$bet)
plot(test$ROI)
rev<-sum(test$bet_res)+sum(test$bet_res2)-sum(test$bet);rev
ROIf<-((sum(test$bet_res)+sum(test$bet_res2)-sum(test$bet))/sum(test$bet))*100 #alto
annualized_ROI<-ROIf/((nrow(test)/365)); annualized_ROI

### PROVA ------
set.seed(1234)
library(caretEnsemble)
classifiers<-c("kknn","rf")
models<-caretList(x=train[,c(5,6)],
                  y=train[,10],
                  trControl=trainControl(method="cv",number=10,classProbs=TRUE),
                  metric="Accuracy",methodList=classifiers, na.action=na.exclude)
