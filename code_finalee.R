#######################################################################
#                         Project Big Data                            #
#                       HAMID Mahamat Abakar                          #
#                       KAKOGIANNAKI MARIA                            #
#                         SHAO Yucheng                                #
######################################################################

###########################
#Installation de packages #
###########################
#install.packages('VIM')
#install.packages('dplyr')
#install.packages('tidyr')
#install.packages('ggplot2')
#install.packages('readr')
#install.packages('Hmisc')
#install.packages('lattice')
#install.packages('survival')
#install.packages('Formula')
#install.packages('funModeling')
#install.packages('corrplot')
#install.packages('MASS')
#install.packages('caret')
#install.packages('epiDisplay')
#install.packages('ROCR')
#install.packages('naniar')
#install.packages('pROC')
#install.packages('MLmetrics')
#install.packages('tidyr')
#install.packages('Matrix')
#install.packages('glmnet')
#install.packages("pdp")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("vip")
#install.packages("doParallel")
#install.packages("PRROC")
#install.packages("kernlab")
#install.packages("glmnetUtils")
##########################
#Chargement des packages #
#########################
library(lattice)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(VIM)
library(Hmisc)
library(funModeling)
library(corrplot)
library(MASS)
library(lattice)
library(caret)
library(epiDisplay)
library(ROCR)
library(naniar)
library(pROC)
library(MLmetrics)
library(tidyr)
library(Matrix)
library(glmnet)
library(pdp)
library(rpart)
library(rpart.plot)
library(vip)
library(randomForest)
library(caTools)
library(doParallel)
library(foreach)
library(iterators)
library(parallel)
library(gbm)
library(PRROC)
library(kernlab)
library(glmnetUtils)

##########################
#Importation de la base  #
#########################
#setwd('C:/Users/Maria Kakogiannaki/Desktop/Big data dossier')
#setwd('D:/projet big data')
X=c('num_indiv','cible','RevolvingUtilizationOfUnsecuredLines', 'age',
    'NumberOfTime30-59DaysPastDueNotWorse', 'DebtRatio',
    'MonthlyIncome',
    'NumberOfOpenCreditLinesAndLoans',
    'NumberOfTimes90DaysLate',
    'NumberRealEstateLoansOrLines',
    'NumberOfTime60-89DaysPastDueNotWorse',
    'NumberOfDependents')

data=read.table('kaggle.txt',h=FALSE, col.names=X)
#head(data)

#################################
#Analyse des valeurs manquantes #
################################
###Analyse des valeurs manquantes par la variable cible
#data %>%
#  group_by(data$cible) %>%
#  miss_var_summary()

###Statistiques descriptives pour les valeurs manquantes de
###la variable MonthlyIncome
#data %>%
#  bind_shadow() %>%
#  group_by(cible) %>%
#  summarise_at(.vars = "MonthlyIncome",
#               .funs = c("mean", "sd", "var", "min", "max"),
#               na.rm = TRUE)

###Statistiques descriptives pour les valeurs manquantes de
###la variable NumberOfDependents
#data %>%
#  bind_shadow() %>%
#  group_by(cible) %>%
#  summarise_at(.vars = "NumberOfDependents",
#               .funs = c("mean", "sd", "var", "min", "max"),
#               na.rm = TRUE)

#Visualisation des valeurs manquantes
#vis_miss(data, sort_miss = TRUE,warn_large_data = FALSE)

#Nous remarquons la non existance de corr?lation entre les valeurs
#manquantes. En plus,pour la variable Number0fDependents nous
#avons un taux des valeurs manquantes assez faible et seulement
#pour les premi?res et derni?res observations. Nous pouvons
#considerer un problÃ¨me sur la collection des donn?es pour cette variable.

#Pour la variable MonthlyIncome, nous voyons que les valeurs
#manquantes sont pr?sent?es pour tout le longeur de cette variable.
#Pour ne pas perdre d'informations significatives nous allons
#inputer les valeurs manquantes.





#Imputation des valeurs manquantes par le median pour
#les valeurs manquantes.

data =data  %>%
  mutate(MonthlyIncome= ifelse(is.na(MonthlyIncome), median(MonthlyIncome,na.rm=TRUE),MonthlyIncome)) %>%
  mutate(NumberOfDependents= ifelse(is.na(NumberOfDependents), median(NumberOfDependents,na.rm = TRUE),NumberOfDependents))
###Vérification d'imputation.
#sum(is.na(data))

#colSums(is.na(data))
#MonthlyIncome  et  NumberOfDependents sont les variables qui
#contiennent des valeurs manquantes.

#is.data.frame(data)
attach(data)
#names(data)

#Suppression des valeurs extrêmes

#Détection des valeurs extrêmes en utilisant l'histogramme

#histogram<-function(variable){
#  ggplot(data) +
#    aes(x = variable) +
#    geom_histogram(bins = 30L, fill = "#0c4c8a") +
#    theme_minimal()
#}

#Histogramme pour chaque variable dans la base des donnÃ©es.
#summary(data)
#histogram(RevolvingUtilizationOfUnsecuredLines)
#histogram(age)
#histogram(NumberOfTime30.59DaysPastDueNotWorse)
#histogram(DebtRatio)
#histogram(MonthlyIncome)
#histogram(NumberOfOpenCreditLinesAndLoans)
#histogram(NumberOfTimes90DaysLate)
#histogram(NumberRealEstateLoansOrLines)
#histogram(NumberOfTime60.89DaysPastDueNotWorse)
#histogram(NumberOfDependents)

#Nous remarquons qu'il y a des variables avec valeurs extrêmes

#Pour vérifier nos résultats nous utilisons le box plot

#boxplot<-function(variable){
#  ggplot(data) +
#    aes(x = "", y = variable) +
#    geom_boxplot(fill = "#0c4c8a") +
#    theme_minimal()

#}

#boxplot(RevolvingUtilizationOfUnsecuredLines)
#boxplot(age)
#boxplot(NumberOfTime30.59DaysPastDueNotWorse)
#boxplot(DebtRatio)
#boxplot(MonthlyIncome)
#boxplot(NumberOfOpenCreditLinesAndLoans)
#boxplot(NumberOfTimes90DaysLate)
#boxplot(NumberRealEstateLoansOrLines)
#boxplot(NumberOfTime60.89DaysPastDueNotWorse)
#boxplot(NumberOfDependents)

#Nous remarquons que pour plusieures variables l'existance des valeurs extrêmes

#Correction des valeurs extrêmes
#Nous allons remplacer les valeurs extrêmes de chaque variable par la médiane.

#Variable : RevolvingUtilizationOfUnsecuredLines
#Il s'agit un valeur de pourcentage, donc on remplace les valeur supérieur à 100% par la médiane
data$RevolvingUtilizationOfUnsecuredLines[which(data$RevolvingUtilizationOfUnsecuredLines>1)]<- median(data$RevolvingUtilizationOfUnsecuredLines)

#Variable : age
#On remplace 0 par la médiane
data$age[which(data$age==0)]<- median(data$age)

#Variable : NumberOfTime30.59DaysPastDueNotWorse
#On remplace 96 et 98 par la médiane
data$NumberOfTime30.59DaysPastDueNotWorse[which(data$NumberOfTime30.59DaysPastDueNotWorse>90)]<- median(data$NumberOfTime30.59DaysPastDueNotWorse)

#Variable : DebtRatio
#Il s'agit un valeur de pourcentage, donc on remplace les valeur supérieur à 100% par la médiane
data$DebtRatio[which(data$DebtRatio>1)]<- median(data$DebtRatio)

#Variable : MonthlyIncome
#Il n'y a pas de valeur aberrante

#Variable : NumberOfOpenCreditLinesAndLoans
#Il n'y a pas de valeur aberrante

#Variable : NumberOfTimes90DaysLate
#On remplace 96 et 98 par la médiane
data$NumberOfTimes90DaysLate[which(data$NumberOfTimes90DaysLate>90)]<- median(data$NumberOfTimes90DaysLate)

#Variable : NumberRealEstateLoansOrLines
#Il n'y a pas de valeur aberrante

#Variable : NumberOfTime60.89DaysPastDueNotWorse
#On remplace 96 et 98 par la médiane
data$NumberOfTime60.89DaysPastDueNotWorse[which(data$NumberOfTime60.89DaysPastDueNotWorse>90)]<- median(data$NumberOfTime60.89DaysPastDueNotWorse)

#Variable : NumberOfDependents
#Il n'y a pas de valeur aberrante


#Nous supprimons la variable num_indiv puisque elle ne porte pas
#d'information pertinante à notre analyse
data = data[,-1]

#head(data)
#dim(data)

#Fréquence de la variable cible
#tab1(data$cible, sort.group = "decreasing", cum.percent = TRUE)



###########################################
#            Modélisation                #
#########################################

##########################################
#Pertitionnement d'echantillon           #
#Echantillon train(75%)                  #
#Echantillon test (25%)                  #
#Partitionnement par strarification      #
##########################################

set.seed(7)

#On convertit la variable cible en factor
data$cible=factor(data$cible)

split=sample.split(data$cible,SplitRatio = 0.75)
train=subset(data,split==T)
test=subset(data,split==F)
#Création d'un vecteur qui contient tous les input, pour les echantillons test
#et train, et un autre vecteur qui contient la variable cible
train_X=as.matrix(subset(train,select=-c(cible)))
train_Y=train$cible
test_X=as.matrix(subset(test,select=-c(cible)))
test_Y=test$cible



###########################################
#Modèle linéaire simple: Régression       #
#Logistique                              #
#########################################

#Afin d'utiliser le modèle de la regression logistique, il
#faut prendre en compte plusieurs hypothèses liées à la validation
#du modèle. L'hypothèse liée à l'association entre les variables
#explicatives est celle qui diversifie la regression logistique
#de les autres modèles. Dans notre analyse, nous allons prendre
#en compte toutes les variables fournies afin de tester les
#capacités predictives de chaque modèle à la présence des effets
#non-linéaires.

# Sélection des variables et choix du seuil tel que le nombre de défaut prévus sur l'échantillon d'apprentissage est égal au
# nombre de défaut observé
#set.seed(7)
#logit_fit<-glm(cible~.,data=train,family="binomial")
#summary(logit_fit)
##Stepwise selection
#step<-stepAIC(logit_fit,trace=FALSE)
#step$anova
#Toutes les variables sont significatives pour notre modèle.
#logit_fit=glm(cible~.,data=train,family="binomial")
#logit_probs=predict(logit_fit,train,type="response")
#predictions<-prediction(logit_probs,train$cible)
#Graphique pour la sélection du seuil optimal
#sens <- data.frame(x=unlist(performance(predictions, "sens")@x.values),
#                   y=unlist(performance(predictions, "sens")@y.values))
#spec <- data.frame(x=unlist(performance(predictions, "spec")@x.values),
#                   y=unlist(performance(predictions, "spec")@y.values))

#sens %>% ggplot(aes(x,y)) +
#  geom_line() +
#  geom_line(data=spec, aes(x,y,col="red")) +
#  scale_y_continuous(sec.axis = sec_axis(~., name = "Specificity")) +
#  labs(x='Cutoff', y="Sensitivity") +
#  theme(axis.title.y.right = element_text(colour = "red"), legend.position="none")


#confusionMatrix(data = as.factor(logit_pred),reference =as.factor(train$cible))

# Estimation et évaluation du modèle en utilisant un seuil=0.071
set.seed(7)
logit_fit=glm(cible~.,data=train,family="binomial")

logit_probs=predict(logit_fit,test,type="response")
auc_logit=auc(logit_probs,ifelse(test$cible=="1",1,0)) #????????
KS_logit=KS_Stat(logit_probs,ifelse(test$cible=="1",1,0))/100
log_logit=LogLoss(logit_probs,ifelse(test$cible=="1",1,0))

logit_pred=ifelse(logit_probs>=0.071,"1","0")
pcc_logit=Accuracy(logit_pred,test$cible)
F1_logit=F1_Score(logit_pred,test$cible)


#################################################################
####################      pénalisation      #####################
#################################################################

### 1- Régression logistique avec pénalisation Ridge

#Construction du modèle Ridge
set.seed(123)
#Le code en bas consiste à construire le modèle Ridge
#Mais il prend du temps pour le faire, on a donc sauvegardé le modèle en avance
#ainsi que vous n'ayez pas besoin de perdre du temps
#ridge_model=cv.glmnet(x=train_X, y=train_Y,family="binomial",type.measure = "auc",nfolds=10, alpha=0)

#save(ridge_model, file = "D:/projet big data/ridge_model.rda")

## on recharge le modèle ridge_model que l'on a déjà trouvé et sauvgardé
load(file = "ridge_model.rda")

plot(ridge_model)
plot(ridge_model$glmnet.fit, "lambda", label = T)
ridge_min <- ridge_model$lambda.min #0,6213
#Récupérer les coefficients estimés
coef(ridge_model, s = ridge_min)

# Prédiction (probabilité) du modèle Ridge sur l'échantillon test
ridge_prob <- predict(ridge_model,newx=test_X,s=ridge_model$lambda.1se, type="response")

# La performance du modèle Ridge
auc_ridge=AUC(ridge_prob,ifelse(test_Y=="1",1,0)) #auc_ridge=0.8374
KS_ridge=KS_Stat(ridge_prob,ifelse(test_Y=="1",1,0))/100 #KS_ridge=0.5291
log_ridge=LogLoss(ridge_prob,ifelse(test_Y=="1",1,0)) #log_ridge=0.2451

# Prédiction (classification) du modèle Ridge sur l'échantillon test
ridge_pred=predict(ridge_model,newx=test_X,s=ridge_model$lambda.1se, type="class")
pcc_ridge=Accuracy(ridge_pred,test_Y) #pcc_ridge=0.9332
F1_ridge=F1_Score(ridge_pred,test_Y) #F1_ridge=0,9654



### 2- Régression logistique avec pénalisation Lasso

#Construction du modèle Lasso
#Le code en bas consiste à construire le modèle Lasso
#Mais il prend du temps pour le faire, on a donc sauvegardé le modèle en avance
#ainsi que vous n'ayez pas besoin de perdre du temps
set.seed(123)
#lasso_model <- cv.glmnet(x=train_X, y=train_Y,family="binomial",type.measure = "auc",nfolds=10, alpha=1)
#save(lasso_model, file = "D:/projet big data/lasso_model.rda")

#on recharge le modèle lasso_model que l'on a déjà trouvé et sauvgardé
load(file = "lasso_model.rda")

plot(lasso_model)
plot(lasso_model$glmnet.fit, "lambda", label = T)
lasso_min <-lasso_model$lambda.min #0.0101
#Récupérer les coefficients estimés
coef(lasso_model, s = lasso_min)
## Lasso sélectionne 5 variables (RevolvingUtilizationOfUnsecuredLines &
## age & NumberOfTime30.59DaysPastDueNotWorse & NumberOfTimes90DaysLate &
## NumberOfTime60.89DaysPastDueNotWorse).

# Prédiction (probabilité) du modèle Lasso sur l'échantillon test
lasso_prob <- predict(lasso_model,newx=test_X,s=lasso_model$lambda.1se, type="response")

# La performance du modèle Lasso
auc_lasso=AUC(lasso_prob,ifelse(test_Y=="1",1,0)) #auc_lasso=0.8311817
KS_lasso=KS_Stat(lasso_prob,ifelse(test_Y=="1",1,0))/100 #KS_lasso=0.5201756
log_lasso=LogLoss(lasso_prob,ifelse(test_Y=="1",1,0)) #log_lasso=0.2139092

# Prédiction (classification) du modèle Lasso sur l'échantillon test
lasso_pred=predict(lasso_model,newx=test_X,s=lasso_model$lambda.1se, type="class")
pcc_lasso=Accuracy(lasso_pred,test_Y) #pcc_lasso=0.93416
F1_lasso=F1_Score(lasso_pred,test_Y) #F1_lasso=0.9659011


### 3- Régression logistique avec pénalisation Elastic-net

# Chercher la valeur de l'alpha qui permet la meilleure performance du modèle Elastic-net
# Cette étape prend beaucoup du temps, on vous conseille donc de ne pas exécuter cette partie
# et de passer directement à la prochaine partie où on va construit un modèle avec le meilleur alpha
#list.of.fits <- list()
#for (i in 0:10) {
#  fit.name <- paste0("alpha", i/10)
#  set.seed(123)
#  list.of.fits[[fit.name]] <-
#    cv.glmnet(x=train_X, y=train_Y, type.measure="auc", alpha=i/10,
#              family="binomial",nfolds=10)
#}

#results <- data.frame()
#for (i in 0:10) {
#  fit.name <- paste0("alpha", i/10)

# Prédiction (probabilité) du modèle Elastic-net sur l'échantillon test
#  probabilite <-
#    predict(list.of.fits[[fit.name]], newx=test_X,
#            s=list.of.fits[[fit.name]]$lambda.1se, type="response" )

#  auc <- AUC(probabilite,ifelse(test_Y=="1",1,0))
#  KS <- KS_Stat(probabilite,ifelse(test_Y=="1",1,0))/100
#  log <- LogLoss(probabilite,ifelse(test_Y=="1",1,0))

# Prédiction (classification) du modèle Elastic-net sur l'Ã©chantillon test
#  prediction <-
#    predict(list.of.fits[[fit.name]], newx=test_X,
#            s=list.of.fits[[fit.name]]$lambda.1se, type="class" )

#  pcc=Accuracy(prediction,test_Y) #pcc_elastic=
# F1=F1_Score(prediction,test_Y) #F1_elastic=

#  temp <- data.frame(fit.name=fit.name,alpha=i/10, auc=auc, KS=KS,log=log,pcc=pcc, F1=F1 )
#  results <- rbind(results, temp)
#}

#results
## On sélection alpha=0,4 qui permet un AUC plus grand.


#Construction du modèle Elastic-net avec le meilleur hyperparamètre
#Le code en bas consiste à construire le modèle Elastic net.
#Mais il prend du temps pour le faire, on a donc sauvegardé le modèle en avance
#ainsi que vous n'ayez pas besoin de perdre du temps
#set.seed(123)
#elastic_model <- cv.glmnet(x=train_X, y=train_Y,family="binomial",type.measure = "auc",nfolds=10, alpha=0.4)
#save(elastic_model, file = "C:/Users/Maria Kakogiannaki/Desktop/models/elastic_model.rda")

#on recharge le modèle elastic_model que l'on a déjà trouvé et sauvgardé
load(file = "elastic_model.rda")

plot(elastic_model)
plot(elastic_model$glmnet.fit, "lambda", label = T)
elastic_min <-elastic_model$lambda.min #

#Récupérer les coefficients estimés
coef(elastic_model, s = elastic_min)
## Elastic-net sélectionne 5 variables (RevolvingUtilizationOfUnsecuredLines &
## age & NumberOfTime30.59DaysPastDueNotWorse & NumberOfTimes90DaysLate &
## NumberOfTime60.89DaysPastDueNotWorse).

# Prédiction (probabilité) du modèle Elastic-net sur l'échantillon test
elastic_prob <- predict(elastic_model,newx=test_X,s=elastic_model$lambda.1se, type="response")

# La performance du modèle Elastic-net
auc_elastic=AUC(elastic_prob,ifelse(test_Y=="1",1,0)) #auc_elastic=0.8405676
KS_elastic=KS_Stat(elastic_prob,ifelse(test_Y=="1",1,0))/100 #KS_elastic=0.536344
log_elastic=LogLoss(elastic_prob,ifelse(test_Y=="1",1,0)) #log_elastic=0.212955

# Prédiction (classification) du modèle Elastic-net sur l'échantillon test
elastic_pred=predict(elastic_model,newx=test_X,s=elastic_model$lambda.1se, type="class")
pcc_elastic=Accuracy(elastic_pred,test_Y) #pcc_elastic=0.9339467
F1_elastic=F1_Score(elastic_pred,test_Y) #F1_elastic=0.9658



#################################################################
####################      Aggrégation       #####################
#################################################################

### 1- Arbre de décision

# construire un modèle d'arbre de décision sans élagage (cp=0)
#arbre0 <- rpart(formula=cible~., data=train, method  = "class",
#                control=list(cp=0, xval=10)) #Pour comparer l'erreur pour chaque valeur, rpart() effectue un Cross Validation de 10 fois (par défaut).
# voir ce modèle sans élagage
#summary(arbre0)

# Elagage de l'arbre
#plotcp(arbre0)
#abline(v = 4, lty = "dashed")
## on prends le premier point en dessous de la ligne pointillé, où (size of tree)=4,
## afin de trouver une balance entre la précision et la complexité du modèle
#arbre0$cptable # rpart cross validation results
## selon ce tableau, nsplit=4 correspond à un cp=0.00299

# Modèle de l'arbre de décision après élagage
arbre <- rpart(formula=cible~., data=train, method  = "class",
               control=list(cp=0.00299, xval=10))  #xval=nombre de cross-validations

# voir ce modèle après élagage
#summary(arbre)
#arbre
#prp(arbre,type = 2, extra = 104,
#    fallen.leaves = TRUE, main = 'Decision Tree') #c'est un autre moyen pour avoir la graphique de l'arbre
#rpart.plot(arbre)
## on a un arbre avec 7 feuilles et un fondeur à 4.

# Feature interpretation
#vip(arbre, num_features = 10, bar = FALSE)
## on constate que les variables NumberOfTimes90DaysLate, NumberOfTime60.89DaysPastDueNotWorse,
## NumberOfTime30.59DaysPastDueNotWorse, NumberOfOpenCreditLinesAndLoans
## sont les plus importantes pour la prédiction

# Prédiction du modèle d'arbre de décision sur l'échantillon test
arbre_pred <- predict(arbre,test,type = 'class')
#confusion <- table(test$cible,arbre_pred,
#                   dnn = c('Actual','Predicted'))
#confusion

arbre_prob<- predict(arbre,test,type = 'prob')
arbre_prob<- arbre_prob[,2]
# Performance du modèle d'arbre de décision sur l'échantion test
pcc_arbre=Accuracy(arbre_pred,test_Y) #pcc_arbre=0.93616
F1_arbre=F1_Score(arbre_pred,test_Y) #F1_arbre=0.9667047
auc_arbre=AUC(arbre_prob,ifelse(test_Y=="1",1,0)) #auc_arbre=0.64126473
KS_arbre=KS_Stat(arbre_prob,ifelse(test_Y=="1",1,0))/100 #KS_arbre=0.27962
log_arbre=LogLoss(arbre_prob,ifelse(test_Y=="1",1,0)) #log_arbre=0.2163324


### 2- Random Forest

train$cible<-as.factor(train$cible)
test$cible<-as.factor(test$cible)
levels(train$cible)=c("Yes","No")
levels(test$cible)=c("Yes","No")
#set.seed(7)
#ctrl <- trainControl(method = "repeatedcv",number = 5, repeats = 2, allowParallel = T,classProbs=T)

# Cette étape prend beaucoup du temps, on vous conseille donc de ne pas exécuter cette partie
# et de passer directement à la prochaine partie où on va construit un modèle avec le meilleur alpha
#rf_model=train(cible~.,data=train,method="rf",tuneGrid=expand.grid(mtry=seq(1,5,1)),trControl=ctrl,ntree=500,metric="Accuracy")
#max(rf_model$results[,"Accuracy"])#0.9360222
#rf_model$bestTune
#ntree=500 mtry=2 sont les meilleurs hyperparamètres

# Estimation et évaluation du modèle avec les meilleurs hyperparamètres
#Le code en bas consiste à construire le modèle Random Forest.
#Mais il prend du temps pour le faire, on a donc sauvegardé le modèle en avance
#ainsi que vous n'ayez pas besoin de perdre du temps
#set.seed(7)
#rf_fit=randomForest(cible~.,data=train,ntree=500,mtry=2)
#save(rf_fit, file = "C:/Users/Maria Kakogiannaki/Desktop/models/rf_fit.rda")

#on recharge le modèle elastic_model que l'on a déjà trouvé et sauvgardé
##load(file = "rf_fit.rda")

#plot(rf_fit$err.rate[, 1], type = "l", xlab = "nombre d'arbres", ylab = "erreur OOB",main="OBB error evolution-number of trees")

##rf_pred1=predict(rf_fit,test,type="prob")

##auc_rf=AUC(rf_pred1[,"Yes"],ifelse(test$cible=="Yes",1,0))#0.8374480832107714
auc_rf=0.8374480832107714
##KS_rf=KS_Stat(rf_pred1[,"Yes"],ifelse(test$cible=="Yes",1,0))/100#0.5245826
KS_rf=0.5245826
##log_rf=LogLoss(rf_pred1[,"Yes"],ifelse(test$cible=="Yes",1,0))#0.2664319
log_rf=0.2664319

#plot(roc.curve(scores.class0=rf_pred1[,"Yes"],weights.class0=test$cible=="Yes",curve=TRUE))
#plot(pr.curve(scores.class0=rf_pred1[,"Yes"],weights.class0=test$cible=="Yes",curve=TRUE))

##rf_pred2=predict(rf_fit,test,type="class")
##pcc_rf=Accuracy(rf_pred2,test$cible)#0.9368267
pcc_rf=0.9368267
##F1_rf=F1_Score(rf_pred2,test$cible)#0.9670721
F1_rf=0.9670721

#3-Gradient Boosting
#set.seed(7)
#ctrl <- trainControl(method = "repeatedcv",number = 5, repeats = 2, allowParallel = T,classProbs=T)
#sto_gbm=train(cible~.,data=train,method="gbm",trControl=ctrl,metric="ROC")
#max(sto_gbm$results[,"ROC"])#0.92
#sto_gbm$bestTune #n.trees=150 interaction.depth=3 shrinkage=0.1 n.minobsinnode=10

# Estimation et prévision
set.seed(7)
gbm_fit=gbm(as.character(ifelse(cible=="Yes",1,0))~.,
            n.trees=150,interaction.depth=3,shrinkage=0.1,n.minobsinnode=10,data=train,distribution="bernoulli")

gbm_pred1=predict(gbm_fit,test,n.trees=150,type="response")
auc_gbm=AUC(gbm_pred1,ifelse(test$cible=="Yes",1,0))#0.57988443897417
KS_gbm=KS_Stat(gbm_pred1,ifelse(test$cible=="Yes",1,0))/100#0.557171561185657
log_gbm=LogLoss(gbm_pred1,ifelse(test$cible=="Yes",1,0))#0.181909535298299

gbm_pred2=ifelse(gbm_pred1>=0.278,"Yes","No")
pcc_gbm=Accuracy(gbm_pred2,test$cible)#0.934693333333333
F1_gbm=F1_Score(gbm_pred2,test$cible)#0.0649102710958381


########################################### Présentation des résultats ########################################

# Création d'un tableau regroupant tous les modèles comparés avec les mesures de performances associés
tableau=data.frame(Methodes=c("Linear Logistic Regression","Ridge penalisation",
                              "Lasso penalisation", "Elastic-net penalisation",
                              "Decision trees",
                              "Random Forest","Gradient Boosting"),
                   AUC     =c(auc_logit,auc_ridge,auc_lasso,auc_elastic,auc_arbre,auc_rf,auc_gbm),
                   PCC     =c(pcc_logit,pcc_ridge,pcc_lasso,pcc_elastic,pcc_arbre,pcc_rf,pcc_gbm),
                   KS      =c(KS_logit,KS_ridge,KS_lasso,KS_elastic,KS_arbre,KS_rf,KS_gbm),
                   F_Score =c(F1_logit,F1_ridge,F1_lasso,F1_elastic,F1_arbre,F1_rf,F1_gbm),
                   Log_Loss=c(log_logit,log_ridge,log_lasso,log_elastic,log_arbre,log_rf,log_gbm))


