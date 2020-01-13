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
# Analisi

glm1<-glm(ris~S1+S2+X3+X4+X5+mese+anno+S1*anno+S2*anno,data=ca)
summary(glm1)
anova(glm1)
drop1(glm1)

glm3<-glm(ris~S1+S2+X3+X4+X5+mese+anno+S1*anno+S2*anno+S1*S2,data=ca)
summary(glm3) #non serve

ca$ris2<-ifelse(ca$ris>1,1,0)
ca$ris2sistemato<-ifelse(ca$ris2==0,1,0)


write.csv(ca,"C:/Users/F041/Downloads/CalcioTotale.csv", row.names = FALSE)



#### Preparazione dati ------
split <- createDataPartition(y=dati$ris, p = 0.60, list = FALSE)
train <- dati[1:2600,]
test <- dati[2601:3578,]
train$ris<-as.factor((train$ris))
levels(train$ris) <- c("Win", "Bank")




####  Lasso -----
set.seed(1234)
grid = expand.grid(.alpha=1,.lambda=seq(0, 1, by = 0.01))
Control=trainControl(method= "cv",number=10, classProbs=TRUE)
glm_lasso=train(ris~S1+S2+X3+X4+S1*anno+S2*anno+anno, method = "glmnet", data=train,
                trControl = Control, tuneLength=10, tuneGrid=grid, metric="Accuracy", na.action=na.exclude)
glm_lasso
plot(glm_lasso)
getTrainPerf(glm_lasso)

### Albero ------
set.seed(1)
cvCtrl <- trainControl(method = "cv", number=10, search="grid", classProbs = TRUE,
                       summaryFunction = twoClassSummary)
rpartTuneCvA <- train(ris~S1+S2+X3+X4+S1*anno+S2*anno+anno, data = train, method = "rpart",
                      tuneLength = 8, na.action=na.exclude,
                      trControl = cvCtrl, metric="Accuracy")

rpartTuneCvA
getTrainPerf(rpartTuneCvA)
plot(varImp(object=rpartTuneCvA),main="train tuned - Variable Importance")


### Random Forest -----
set.seed(1)
cvCtrl <- trainControl(method = "cv", number=10, search="grid", classProbs = TRUE,
                       summaryFunction = twoClassSummary)
rfTune <- train(ris~S1+S2+X3+X4+S1*anno+S2*anno+anno, data = train, method = "rf",
                tuneLength = 3,
                trControl = cvCtrl, metric="metric", na.action=na.exclude) # con tuneLength=3 ci mette tanto
rfTune
getTrainPerf(rfTune)
plot(varImp(object=rfTune),main="train tuned - Variable Importance")

### Neural Net -----
set.seed(2)
cvCtrl <- trainControl(method = "cv", number=10, search="grid", classProbs = TRUE,
                       summaryFunction = twoClassSummary)
NNTune <- train(ris~S1+S2+X3+X4+S1*anno+S2*anno+anno, data = train, method = "nnet",
                tuneLength = 4,
                trControl = cvCtrl, metric="metric", preProcess="range", na.action=na.exclude) # Non andare oltre il 6 di tuneLength 
NNTune
plot(varImp(object=NNTune),main="train tuned - Variable Importance")

### Xtreme Gradient -----
set.seed(3)
cvCtrl <- trainControl(method = "cv", number=10, search="grid", classProbs = TRUE,
                       summaryFunction = twoClassSummary)
XGBTune <- train(ris~S1+S2+X3+X4+S1*anno+S2*anno+anno, data = train, method = "xgbTree",
                 tuneLength = 2,
                 trControl = cvCtrl, metric="metric", na.action=na.exclude) # Non andare oltre il 6 di tuneLength 
XGBTune
getTrainPerf(XGBTune)
plot(varImp(object=XGBTune),main="train tuned - Variable Importance")

### Adaboost ------
set.seed(5)
cvCtrl <- trainControl(method = "cv", number=10, search="grid", classProbs = TRUE,
                       summaryFunction = twoClassSummary)
AdaBTune <- train(ris~S1+S2+X3+X4+S1*anno+S2*anno+anno, data = train, method = "adaboost",
                  tuneLength = 2, na.action=na.exclude,
                  trControl = cvCtrl, metric="metric")

AdaBTune
getTrainPerf(AdaBTune)

### Risultati ----
results <- resamples(list(Tree=rpartTuneCvA, RandomForest=rfTune, 
                          NeuralNet=NNTune, XGBoosting=XGBTune
                        , AdaBoost=AdaBTune));results 
bwplot(results)

### Test -----
# estimate probs P(M)
test$p1 = predict(glm_lasso       , test, "prob")[,-1]
test$p2 = predict(rpartTuneCvA, test, "prob")[,-1]
test$p3 = predict(rfTune    , test, "prob")[,-1]
test$p4 = predict(NNTune     , test, "prob")[,-1]
test$p5 = predict(XGBTune, test, "prob")[,-1]
test$p6 = predict(AdaBTune, test, "prob")[,-1]

# roc values
library(pROC)
r1=roc(ris ~ p1, data = test)
r2=roc(ris ~ p2, data = test)
r3=roc(ris ~ p3, data = test)
r4=roc(ris ~ p4, data = test)
r5=roc(ris ~ p5, data = test)
r6=roc(ris ~ p6, data = test)

plot(r1) #lasso nero
plot(r2,add=T,col="red") #Tree
plot(r3,add=T,col="blue") #RF
plot(r4,add=T,col="yellow") #NN
plot(r5,add=T,col="violet") #XGB
plot(r6,add=T,col="orange") #Ada
legend("bottomright", c("Tree", "Random Forest", "NN","XGB","ADA"),
       text.col=c("red","blue","yellow","violet","orange"))