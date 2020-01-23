setwd("C:/Users/F041/Downloads")

### Librerie -----
library(tidyverse)  # for data wrangling /discussione dei dati
library(rvest)      # for web scraping strae i dati da una pagina web
library(lubridate)  # for date formats  packeto per gestire i dati
library(stringr)    # manipuate strings 
library(xlsx)
library(tm) 
library(SnowballC)
library(wordcloud)
library(tidyr)
library(dplyr)
library(glmnet)
library(caret)

### DATI PRIMA PARTE -----
{
ca<-read.xlsx("calcio.xlsx", sheetName = "Sheet1",header = FALSE)
ca<- ca[!grepl("ROUND",ca$X1),]#levami tutte le righe che contengono ROUND(grepl=contenere)
ca=as.data.frame(ca)
head(ca)

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
library(lubridate)
class(ca$X6)
ca$mese=month(ca$X6)
ca$anno=year(ca$X6)
#remove space
ca$X3=trimws(ca$X3)
ca$X4=trimws(ca$X4)
ca$X5=trimws(ca$X5)
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
}

### DATI SECONDA PARTE -----
{
datiGab<-read.xlsx("betting2019-2020to 2011-2012.xlsx", sheetName = "Sheet1", header = FALSE)
datiGab<- datiGab[!grepl("ROUND",datiGab$X1),]#levami tutte le righe che contengono ROUND(grepl=contenere)
datiGab$X6=as.Date(datiGab$X6,format = "%d.%m.%Y");
datiGab=as.data.frame(datiGab)

head(datiGab)

#datiGab<-bind_rows(datiGab, datiGab1)
#squadra
#squadra
#squadra

squadra=datiGab$X1
squadra=as.data.frame(squadra)
squadra$squadra=tolower(squadra$squadra)
squadra$squadra=trimws(squadra$squadra)
squadra$squadra=removeNumbers(squadra$squadra)
squadra$squadra=stripWhitespace(squadra$squadra)

squadra= separate (squadra, squadra,c("S1","S2"),"-")
squadra$S1=gsub("â", " ",squadra$S1)
squadra$S2=gsub("â", " ",squadra$S2)
squadra$S1=trimws(squadra$S1)
squadra$S2=trimws(squadra$S2)

#punti
punt=datiGab$X2
punt=as.data.frame(punt)
punt$punt=tolower(punt$punt)
punt$punt=trimws(punt$punt)
punt$punt=stripWhitespace(punt$punt)
punt= separate (punt, punt,c("P1","P2","P3"),":")
punt$P1=trimws(punt$P1)
punt$P2=trimws(punt$P2)
punt$P1=as.integer(punt$P1)
punt$P2=as.integer(punt$P2)
punt$P3=NULL


#DATE

library(lubridate)
class(datiGab$X6)
datiGab$mese=month(datiGab$X6)
datiGab$anno=year(datiGab$X6)

#remove space

datiGab$X3=trimws(datiGab$X3)
datiGab$X4=trimws(datiGab$X4)
datiGab$X5=trimws(datiGab$X5)

#dataset finale
names(datiGab)

datiGab$X1=NULL
datiGab$X2=NULL
datiGab$X6=NULL

datiGab=cbind(squadra,punt,datiGab)

datiGab$ris=case_when(datiGab$P1>datiGab$P2~"1",
                      datiGab$P1<datiGab$P2~"2",
                      datiGab$P1==datiGab$P2~"2")
datiGab$ris<-as.integer(datiGab$ris)
datiGab$X3<-as.numeric(datiGab$X3)
datiGab$X4<-as.numeric(datiGab$X4)
datiGab$X5<-as.numeric(datiGab$X5)


datiGab<-datiGab[,c(1:7,10,11,12)]
}

dati<-bind_rows(ca, datiGab)
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
grid = expand.grid(.alpha=1,.lambda=seq(0, 0.152, by = 0.001)) #non serve farlo arrivare a 1
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
Grid2 = expand.grid(.cp     =seq(0,0.3,  by=0.01)) #non serve farlo arrivare a 1
cvCtrl <- trainControl(method = "cv", number=10, classProbs = TRUE)
rpartTuneCvA <- train(ris~S1+S2+log(X3+1)+S1*anno+S2*anno+anno, data = train, method = "rpart",
                      tuneLength = 10, na.action=na.exclude, tuneGrid=Grid2,
                      trControl = cvCtrl, metric="Accuracy")

rpartTuneCvA
getTrainPerf(rpartTuneCvA)
plot(varImp(object=rpartTuneCvA),main="train tuned - Variable Importance")


### Random Forest -----
set.seed(1)
Grid4 = expand.grid(.mtry   =seq(1,8,  by=1))  #by da 2 a 1, ovviamente ci metterà di più
cvCtrl <- trainControl(method = "cv", number=10, classProbs = TRUE)
rfTune <- train(ris~S1+S2+log(X3+1)+S1*anno+S2*anno+anno, data = train, method = "rf",
                tuneLength = 5,
                trControl = cvCtrl, na.action=na.exclude, tuneGrid=Grid4) # con tuneLength=5 ci mette tanto
rfTune
getTrainPerf(rfTune)
plot(varImp(object=rfTune),main="train tuned - Variable Importance")

### Neural Net -----
set.seed(2)
Grid0 = expand.grid(.size   =seq(1,7,  by=1), .decay = seq(0.00,1,  by=0.1))
cvCtrl <- trainControl(method = "cv", number=10, classProbs = TRUE)
NNTune <- train(ris~S1+S2+log(X3+1)+S1*anno+S2*anno+anno, data = train, method = "nnet",
                tuneLength = 4, tuneGrid=Grid0,
                trControl = cvCtrl, preProcess="range", na.action=na.exclude) # Non andare oltre il 6 di tuneLength 
NNTune
getTrainPerf(NNTune)
plot(varImp(object=NNTune),main="train tuned - Variable Importance")

### Xtreme Gradient -----
set.seed(3)
cvCtrl <- trainControl(method = "cv", number=10, search="grid", classProbs = TRUE)
XGBTune <- train(ris~S1+S2+log(X3+1)+S1*anno+S2*anno+anno, data = train, method = "xgbTree",
                 tuneLength = 4,
                 trControl = cvCtrl, metric="Accuracy", na.action=na.exclude) # Non andare oltre il 6 di tuneLength 
XGBTune
getTrainPerf(XGBTune)

### Adaboost ------
set.seed(6)
cvCtrl <- trainControl(method = "cv", number=10, search="grid", classProbs = TRUE)
AdaBTune <- train(ris~S1+S2+X3+S1*anno+S2*anno+anno, data = train, method = "AdaBoost.M1",
                  tuneLength = 3, na.action=na.exclude, #min lenght =2
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
          trControl = cvCtrl, tuneLength=10, na.action=na.exclude)
pls
getTrainPerf(pls)

### deep boost ------
set.seed(115)
cvCtrl <- trainControl(method = "cv", number=10, search="grid", classProbs = TRUE)
Deep=train(ris~S1+S2+X3+S1*anno+S2*anno+anno, data = train, method = "deepboost", 
           trControl = cvCtrl, tuneLength=3, na.action=na.exclude)
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

plot(pROC_obj, print.thres = quantile(pROC_obj$thresholds, seq(0.55,0.8,0.025))) #suggerisce un 0.474


### Risultati miglior modello, Lasso ------
test$labelsp1<-as.factor(ifelse(test$p1>0.474,1,2))
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


### Redditività: dipende dalla quota bank scelta e dalla probabilità di abbinare la scommessa bank ----
tabellaROI<-data.frame()
for(i in seq(1.3, 1.8, by = 0.025)) #la sequenza serve a vedere il comportamento del ROI in funzione della quota
{
  
  test$bet<-ifelse(test$labelsp1==1,10,((10*i)-10))
  test$bet_res<-ifelse(test$ris ==test$labelsp1 & test$ris==1, test$bet*test$X3,0)
  test$bet_res2<-ifelse(test$ris ==test$labelsp1 & test$ris==2, 
                        10,0)
  sum(test$bet_res)
  sum(test$bet_res2)
  sum(test$bet)
  
  test$ROI<-((test$bet_res)+(test$bet_res2)-(test$bet))/(test$bet)
  plot(test$ROI)
  rev<-sum(test$bet_res)+sum(test$bet_res2)-sum(test$bet);rev
  ROIf<-((sum(test$bet_res)+sum(test$bet_res2)-sum(test$bet))/sum(test$bet))*100 #alto
  annualized_ROI<-ROIf/((nrow(test)/365)); annualized_ROI
  ROI[i]<-annualized_ROI
  test$capital<-(cumsum(test$bet_res)+cumsum(test$bet_res2)-cumsum(test$bet))
  plot(test$capital)
  print(cbind(ROI[i],i))
  x <- c(annualized_ROI,i)
  tabellaROI<-rbind(tabellaROI,x)
  colnames(tabellaROI)<-c("ROI","Quota_bank")
}

### PROVA ------
set.seed(1234)
library(caretEnsemble)
classifiers<-c("kknn","rf")
models<-caretList(x=train[,c(5,6)],
                  y=train[,10],
                  trControl=trainControl(method="cv",number=10,classProbs=TRUE),
                  metric="Accuracy",methodList=classifiers, na.action=na.exclude)