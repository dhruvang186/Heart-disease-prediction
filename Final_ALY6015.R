pacman::p_load(tidyverse, rio, party, ggplot2, dplyr, vcd, webr, psych,
               plotrix, gginference,corrplot,ggpubr,BSDA,MASS)


library(scales)

#Load dataset
library(readxl)
heart1 <- read_csv("D:/Downloads/heartyesno.csv")
heart<- read_csv("D:/Downloads/archive (8)/heart.csv")
View(heart)

heart
heart$target<-as.factor(heart$target)
levels(heart$target)

g3 <- ggplot(heart1,aes(cp,col=as.factor(target),fill=as.factor(target)))+
  geom_bar(position = "fill")+
  guides(col="none")+
  labs(fill="Heart disease",x="Chest pain")+
  ggtitle("Chest pain vs heart disease")
g3

g7 <- ggplot(heart1,aes(exang,col=as.factor(target),fill=as.factor(target)))+
  geom_bar(position = "fill")+
  guides(col="none")+
  labs(fill="Heart disease",x="Exercise induced angina")+
  ggtitle("Exercise angina vs heart disease")
g7
view(heart1)
g1 <- ggplot(heart1,aes(thalach,col=as.factor(target),fill=as.factor(target)))+
  geom_density(alpha=0.2)+
  guides(col="none")+
  labs(fill="Heart DIsease",x="Maximum heart rate achieved")+
  ggtitle("Max heart rate vs heart disease")
g1  

g4 <- ggplot(heart1,aes(x = as.factor(exang),fill=as.factor(target)))+
  geom_boxplot(position = fill)+
  labs(y="Age",x="Sex",fill="Heart Disease")+
  ggtitle("Gender vs heart disease")
g4


yes <- filter(heart, target == "yes")
no <- filter(heart, target == "no")
tbl
qpl <- t.test(yes$thalach ,no$thalach, alternative = "two.sided", conf.level = 0.95)
qpl
chi<-chisq.test(tbl)
qchisq(0.05, 90, lower.tail = F)
library(leaps)
model1 <- regsubsets(target~., data = heart, nvmax = 5)
summary(model1)
?read.csv
cor(heart$exang, heart$sex)

tbl=table(heart1$thalach, heart1$target)
tbl
chisq.test(tbl)

#can we predict the heart disease with predictors and what is the accuracy
library(caret)
set.seed(3456)
trainIndex <- createDataPartition(heart$target, p = 0.7, list = FALSE, times = 1) 
trainheart <- heart[ trainIndex,] 
testheart <- heart[-trainIndex,] 


library(corrplot)
heart.cor=cor(heart)
corPlot(heart.cor)

mod_01 <- glm(target ~ ., data = heart, family = binomial(link = "logit"))
summary(mod_01)

LogModel <- glm(target ~ oldpeak + cp + sex + thalach + exang, data=heart, 
                family=binomial
                (link = "logit"))
summary(LogModel)

prob.test_set = predict(LogModel, newdata = heart, type = "response")

predicted_clas_min1 = as.factor(ifelse(prob.test_set >= 0.5, "1", "0"))


cmheart<-confusionMatrix(predicted_clas_min1, heart$target, positive = "1")
cmheart
gplotConfusionMatrix <- function(m){
  mytitle <- paste("Accuracy", percent_format()(m$overall[1]))
  p <-
    ggplot(data = as.data.frame(cmheart$table) ,
           aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = log(Freq)), colour = "white") +
    scale_fill_gradient(low = "white", high = "lightgreen") +
    geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
    theme(legend.position = "none") +
    ggtitle(mytitle)
  return(p)
}

gplotConfusionMatrix(cmheart)

library(pROC)
ROC = roc(heart$target, prob.test_set)
X <- plot(ROC, col = "darkgreen",
          ylab = "Sensitivity = TP rate", xlab = 'specificity = FP rate')

AUC=auc(ROC)
cat("auc curve"=AUC)

#Age group affects on heart attack
levels(heart$age)
summary(heart)
summary(heart$target)
levels(heart$target) <- c("No","Yes")
levels(heart$sex) <- c("Male","Female")
view(heart$target)
heartdisease<-filter(heart, target=="Yes")
view(heartdisease)
ggplot(data = heartdisease, aes(x = age)) +
  geom_bar(color="black", fill="lightblue") + 
  labs(x="Age", y="Heart Disease", title="People having heart disease") 


levels(heart$cp) <- c("Asymptomatic", "Atypical angina", "No angina", "Typical angina")
View(heart$cp)
view(heart)
ggplot(data=heart, aes(x=cp, fill=target)) + geom_bar()

ggplot(data=heart, aes(x=fbs, fill=target)) + geom_bar()
levels(heart1$sex)<-c("Male","Female")

tbl=table(heart1$exang, heart1$target)
tbl
chisq.test(tbl)
qchisq(0.05,1,lower.tail = F)
install.packages("tigerstats")
library(tigerstats)
xtabs(~ target + sex, data= heart1)


ggplot(heart, aes(x=target, y=oldpeak)) + 
  geom_point()

yes <- filter(heart, target == "yes")
no <- filter(heart, target == "no")
test4<-chisq.test(table(heart$exang,heart$target))
test4
qchisq(0.05, 1,lower.tail = F)
test3 <- wilcox.test(x = heart$exang , y = heart$target , alternative = "two.sided",correct = F)
test3


inner <- factor(sample(letters[1:25], 100, replace = TRUE))
inout <- factor(sample(LETTERS[1:5], 25, replace = TRUE))
fr <- data.frame(inner = inner, outer = inout[as.integer(inner)])
xtabs(~ inner + outer, fr, sparse = TRUE)


xtabs(~target + exang, data = heart1)
?glm

levels(heart1$exang) <- c("Yes", "No")
view(heart1$exang)

t.test(heart$thalach, heart$target, alternative = "two.sided", conf.level = 0.95)

barplot(prop.table(data, 2))

ggplot(heart1, aes(y = target, x =exang)) + 
  geom_bar(position="fill", stat="identity")

levels(heart1$cp) <- c("Asymptomatic", "Atypical angina", "No angina", "Typical angina")
view(heart1$cp)
?view

#female vs male
female <- filter(heart, sex == "0")
male <- filter(heart, sex == "1")
view(male)

#female

modelfemale <- regsubsets(target~., data = female, nvmax = 5)
summary(modelfemale)

mod_female <- glm(target ~ ., data = female, family = binomial(link = "logit"))
summary(mod_female)

LogModelfemale <- glm(target ~ cp + exang + thal + fbs, data=female, 
                family=binomial
                (link = "logit"))
summary(LogModelfemale)



prob.test_set = predict(LogModelfemale, newdata = female, type = "response")

predicted_clas_min1 = as.factor(ifelse(prob.test_set >= 0.5, "1", "0"))

library(caret)
female$target<-as.factor(female$target)
cmfemale<-confusionMatrix(predicted_clas_min1, female$target, positive = "1")
cmfemale
gplotConfusionMatrix <- function(m){
  mytitle <- paste("Accuracy", percent_format()(m$overall[1]))
  p <-
    ggplot(data = as.data.frame(cmfemale$table) ,
           aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = log(Freq)), colour = "white") +
    scale_fill_gradient(low = "white", high = "lightgreen") +
    geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
    theme(legend.position = "none") +
    ggtitle(mytitle)
  return(p)
}

gplotConfusionMatrix(cmfemale)

library(pROC)
ROCfemale = roc(female$target, prob.test_set)
X <- plot(ROCfemale, col = "darkgreen",
          ylab = "Sensitivity = TP rate", xlab = 'specificity = FP rate')

AUCfemale=auc(ROCfemale)
cat("auc curve of female"=AUCfemale)
#male

modelmale <- regsubsets(target~., data = male, nvmax = 5)
summary(modelmale)

mod_male <- glm(target ~ ., data = male, family = binomial(link = "logit"))
summary(mod_male)

LogModelmale <- glm(target ~ thalach + cp + oldpeak + thal, data=male, 
                family=binomial
                (link = "logit"))
summary(LogModelmale)



prob.test_set = predict(LogModelmale, newdata = male, type = "response")

predicted_clas_min1 = as.factor(ifelse(prob.test_set >= 0.5, "1", "0"))

library(caret)
male$target<-as.factor(male$target)
cmmale<-confusionMatrix(predicted_clas_min1, male$target, positive = "1")
cmmale
gplotConfusionMatrix <- function(m){
  mytitle <- paste("Male Accuracy", percent_format()(m$overall[1]))
  p <-
    ggplot(data = as.data.frame(cmmale$table) ,
           aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = log(Freq)), colour = "white") +
    scale_fill_gradient(low = "white", high = "lightgreen") +
    geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
    theme(legend.position = "none") +
    ggtitle(mytitle)
  return(p)
}

gplotConfusionMatrix(cmmale)

library(pROC)
ROCmale = roc(male$target, prob.test_set)
X <- plot(ROCmale, col = "darkgreen",
          ylab = "Sensitivity = TP rate", xlab = 'specificity = FP rate')

AUCmale=auc(ROCmale)
cat("auc curve of female"=AUCmale)
                        
