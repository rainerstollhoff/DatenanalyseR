# Einlesen des Datensatzes
library(tidyverse)
library(mlr)

btw17d <- read_csv("./www/btw17_direktmandate.csv",
                                locale = locale(encoding = "ISO-8859-1"))

# Preprocess Data
btw17.wahl.names <- colnames(btw17d)
colnames(btw17d) <- c("Y",paste("X",1:(ncol(btw17d)-1),sep=""))
#Zielgröße CDU/CSU
btw17d$Y <- as.numeric(btw17d$Y==2)
btw17d$Y <- as.factor(btw17d$Y)
btw17d <- as.data.frame(btw17d)

btw17d.n <-  normalizeFeatures(obj=btw17d,
                               target=colnames(btw17d)[1],
                               cols=colnames(btw17d)[-1],
                               method="standardize")

# Create Task
btw17d.task <- makeClassifTask(data=btw17d.n,
                                 target=colnames(btw17d)[1])

# Create Learners
## Alle technisch passenden anzeigen
btw17d.learners.all <- listLearners(btw17d.task,check.packages = F)
btw17d.learners.all$name

##Auswahl auf in der LV behandelte Algorithmen
btw17d.learners.subset <- c(
  "CART"= 72,
  "Logistic Regression"=45,
  "svm"=78,
  "gbm"= 23,
  "randomForest"=65,
  "nnet" = 53
)

btw17d.learners.all$name[btw17d.learners.subset]
btw17d.learners.subset <- btw17d.learners.all[btw17d.learners.subset,]

# Pakete installieren und Laden
install.packages(pkgs=unique(btw17d.learners.subset$package))
lapply(unique(btw17d.learners.subset$package),require,character.only=T)

btw17d.learners <- makeLearners(cl=btw17d.learners.subset$class)

# Get hyperparameters
lapply(btw17d.learners,getParamSet)

# Set Resampling scheme
btw17d.resample <- cv10

# Benchmark on default values
btw17d.benchmark.direct <- benchmark(learners = btw17d.learners,
                               tasks = btw17d.task,
                               resamplings = btw17d.resample,
                               measures = acc)


# Get&Tune hyperparameters
#lapply(btw17d.learners,getParamSet)
#install.packages("mlrMBO")
#install.packages("DiceKriging")
#install.packages("rgenoud")

library("mlrMBO")
library("DiceKriging")
library("rgenoud")


# rpart
getParamSet(btw17d.learners$classif.rpart)
rpart.param <- makeParamSet(
  makeNumericParam("cp",0.001,0.1),
  makeIntegerParam("maxdepth",1,10),
  makeIntegerParam("minsplit",1,10))
rpart.tune <-  tuneParams(learner = btw17d.learners$classif.rpart,
                        task = btw17d.task,
                        cv5,acc,
                        par.set = rpart.param,
                        control = makeTuneControlMBO(budget=50))
setHyperPars(btw17d.learners$classif.rpart,par.vals=rpart.tune$x)

# logreg
getParamSet(btw17d.learners$classif.logreg)

# svm
getParamSet(btw17d.learners$classif.svm)
svm.param <-  makeParamSet(
  makeNumericParam("cost",0.01,10),
  makeNumericParam("gamma",.0001,1),
  makeDiscreteParam("kernel",c("radial","polynomial")))
svm.tune <- tuneParams(learner = btw17d.learners$classif.svm,
                       task = btw17d.task,
                         cv5,acc,
                         par.set = svm.param,
                         control = makeTuneControlMBO(budget=100))
setHyperPars(btw17d.learners$classif.svm,par.vals=svm.tune$x)

# # ada
# getParamSet(btw17d.learners$classif.adaboostm1)
# ada.param <-  makeParamSet(
#   makeIntegerParam("S",1,100),
#   makeIntegerParam("I",1,100)
# )
# ada.tune <- tuneParams(learner = btw17d.learners$classif.adaboostm1,
#                        task = btw17d.task,
#                        cv5,acc,
#                        par.set = ada.param,
#                        control = makeTuneControlMBO(budget=50))
# setHyperPars(btw17d.learners$classif.adaboostm1,par.vals=ada.tune$x)

# gbm
getParamSet(btw17d.learners$classif.gbm)
gbm.param <-  makeParamSet(
  makeNumericParam("shrinkage",0.001,1),
  makeIntegerParam("interaction.depth",1,10),
  makeIntegerParam("n.trees",100,1000),
  makeDiscreteParam("distribution","bernoulli")
)
gbm.tune <- tuneParams(learner = btw17d.learners$classif.gbm,
                       task = btw17d.task,
                       cv5,acc,
                       par.set = gbm.param,
                       control = makeTuneControlMBO(budget=50))
setHyperPars(btw17d.learners$classif.gbm,par.vals=gbm.tune$x)


# randomForest
getParamSet(btw17d.learners$classif.randomForest)
rf.param <-  makeParamSet(
  makeIntegerParam("mtry",1,10),
  makeIntegerParam("ntree",100,1000)
  
)
rf.tune <- tuneParams(learner = btw17d.learners$classif.randomForest,
                       task = btw17d.task,
                       cv5,acc,
                       par.set = rf.param,
                       control = makeTuneControlMBO(budget=50))
setHyperPars(btw17d.learners$classif.randomForest,par.vals=rf.tune$x)

# neuralnet
getParamSet(btw17d.learners$classif.nnet)
nnet.param <-  makeParamSet(
  makeIntegerParam("size",1,20)
)
nnet.tune <- tuneParams(learner = btw17d.learners$classif.nnet,
                      task = btw17d.task,
                      cv5,acc,
                      par.set = nnet.param,
                      control = makeTuneControlMBO(budget=20))
setHyperPars(btw17d.learners$classif.nnet,par.vals=nnet.tune$x)


# Benchmark on default values
btw17d.benchmark.tuned <- benchmark(learners = btw17d.learners,
                                     tasks = btw17d.task,
                                     resamplings = btw17d.resample,
                                     measures = acc)

btw17d.benchmark.direct
btw17d.benchmark.tuned
