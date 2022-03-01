library(spectrolab)
library(caret)
library(plyr)

## READ DATA ###########
dati <- read.csv("spectra.dat.csv", check.names = F)
wvl <- colnames(dati[,-c(1:5)])
#define spectra
spec <- dati[,-c(1:which(colnames(dati)==as.character(wvl[1]))-1)]
spec <- resample(as.spectra(spec), seq(410, 2400, by = 5) )
spec<- as.data.frame(spec)
spec<- spec[,-1]
#define class
classi <- dati$Population_ID

### 75:25 data split########
mods <- list()

for (i in 1:100){
  set.seed(i)
  inTrain <- createDataPartition(y =classi, p = .75, list = FALSE)
  training <- spec[inTrain,]
  testing <- spec[-inTrain,]
  trainclass <- classi[inTrain]
  testclass <- classi[-inTrain]
  
  ### 10K(number) folds CV, 10 repeats
  ctrl <- trainControl(method = "repeatedcv", repeats = 10, number=10, 
                       summaryFunction = multiClassSummary, returnData = F, 
                       returnResamp = "none",savePredictions = "none", classProbs = F,trim=T)
  
  ### default ncomps 3 (tuneLength), Method= softmax
  plsFit <- train(training, trainclass, method = "pls", tuneLength = 40,
                  trControl = ctrl, probMethod="softmax") 
  mods[[i]] <- plsFit
  
  print(i)
}


saveRDS(mods, "./PLSiter1_.rds")




