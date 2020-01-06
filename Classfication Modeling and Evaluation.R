# load library
library(tidyverse)
library(readr)
library(reshape2)
library(glmnet)
install.packages("tree")

### read data 
training = read_csv("training_tallskinny.csv")
validation = read_csv("validation_tallskinny.csv")
interest_topics = read_csv("interest_topics.csv")

#ggplot(training, aes(factor(inAudience))) + geom_bar()

interest_topics['topic'] = vapply(strsplit(interest_topics$topic_name, "/", fixed = TRUE), "[", "", 2)

training <- training %>%
  inner_join(interest_topics, by = "topic_id")
training2 <- training %>%
  group_by(userID, topic) %>%
  summarize(sum_lt = sum(ltiFeatures) , sum_st = sum(stiFeatures))

# use ols to predict missing data
training2$topic = factor(training2$topic)

df <- training2[(!is.na(training2$sum_lt)) & (!is.na(training2$sum_st)),]

mod_lt <- lm(sum_lt ~ topic + sum_st, df)
summary(mod_lt)

mod_st <- lm(sum_st ~ topic + sum_lt, df)
summary(mod_st)

# use ols model to fill in missing values
missing_lt <- training2[is.na(training2$sum_lt),]
missing_lt_val <- predict(mod_lt, newdata = missing_lt)

training2[is.na(training2$sum_lt),3] = missing_lt_val
training2[is.na(training2$sum_lt),3] = 0
summary(training2$sum_lt)

missing_st <- training2[is.na(training2$sum_st),]
missing_st_val <- predict(mod_st, newdata = missing_st)

training2[is.na(training2$sum_st),4] = missing_st_val
summary(training2)

# reshape dataset 
tr_lt <- training2 %>%
  select(-sum_st)

melt_lt <- dcast(tr_lt, userID ~ topic, value.var = 'sum_lt')

tr_st <- training2 %>%
  select(-sum_lt)

melt_st <- dcast(tr_st, userID ~ topic, value.var = 'sum_st')

conversion <- unique(training[,1:2])

tr <- melt_lt %>%
  inner_join(melt_st, by = 'userID') %>%
  inner_join(conversion, by = 'userID')
# fill missing vals as 0
tr[is.na(tr)] = 0
tr = tr[,2:52]
tr$inAudience = factor(as.numeric(tr$inAudience)) # true = 1
summary(tr)

### modeling 

# logistic 
model.logistic <- glm(inAudience~., data = tr, family = 'binomial')
summary(model.logistic)

# lasso
## First lets set up the data for it
Mx<- model.matrix(inAudience ~., data=tr)
My<- tr$inAudience == 1

num.features <- ncol(Mx)
num.n <- nrow(Mx)
num.churn <- sum(My)
w <- (num.churn/num.n)*(1-(num.churn/num.n))
lambda.theory <- sqrt(w*log(num.features/0.05)/num.n)

lassoCV <- cv.glmnet(Mx,My, family="binomial")

support<- function(x, tr = 10e-6) {
  m<- rep(0, length(x))
  for (i in 1:length(x)) if( abs(x[i])> tr ) m[i]<- i
  m <- m[m>0]
  m
}
# lasso theory
model.lassoTheory <- glmnet(Mx,My, family="binomial",lambda = lambda.theory)
colnames(Mx)[support(model.lassoTheory$beta)]
# lasso se
model.lasso1se  <- glmnet(Mx,My, family="binomial",lambda = lassoCV$lambda.1se)
colnames(Mx)[support(model.lasso1se$beta)]
# null model
model.null <- glm(inAudience~1, data=tr, family="binomial")

### clean validation data 
validation <- validation %>%
  inner_join(interest_topics, by = "topic_id")
validation2 <- validation %>%
  group_by(userID, topic) %>%
  summarize(sum_lt = sum(ltiFeatures) , sum_st = sum(stiFeatures))

# use ols to predict missing data
validation2$topic = as.factor(validation2$topic)
df2 <- validation2[(!is.na(validation2$sum_lt)) & (!is.na(validation2$sum_st)),]

mod_lt2 <- lm(sum_lt ~ topic + sum_st, df2)
summary(mod_lt2)

mod_st2 <- lm(sum_st ~ topic + sum_lt, df2)
summary(mod_st2)

# use ols model to fill in missing values
missing_lt2 <- validation2[is.na(validation2$sum_lt),]
missing_lt_val2 <- predict(mod_lt2, newdata = missing_lt2)

validation2[is.na(validation2$sum_lt),3] = missing_lt_val2
validation2[is.na(validation2$sum_lt),3] = 0
summary(validation2)

missing_st2 <- validation2[is.na(validation2$sum_st),]
missing_st_val2 <- predict(mod_st2, newdata = missing_st2)

validation2[is.na(validation2$sum_st),4] = missing_st_val2
summary(validation2)

# reshape dataset 
tr_lt2 <- validation2 %>%
  select(-sum_st)

melt_lt2 <- dcast(tr_lt2, userID ~ topic, value.var = 'sum_lt')

tr_st2 <- validation2 %>%
  select(-sum_lt)

melt_st2 <- dcast(tr_st2, userID ~ topic, value.var = 'sum_st')

conversion2 <- unique(validation[,1:2])

val <- melt_lt2 %>%
  inner_join(melt_st2, by = 'userID') %>%
  inner_join(conversion2, by = 'userID')
# fill missing vals as 0
val[is.na(val)] = 0
val = val[,2:52]
val$inAudience = factor(as.numeric(val$inAudience))
summary(val)

# evaluate logistic model
FPR_TPR <- function(prediction, actual){
  
  TP <- sum((prediction)*(actual))
  FP <- sum((prediction)*(!actual))
  FN <- sum((!prediction)*(actual))
  TN <- sum((!prediction)*(!actual))
  result <- data.frame( FPR = FP / (FP + TN), TPR = TP / (TP + FN), ACC = (TP+TN)/(TP+TN+FP+FN) )
  
  return (result)
}

Mx_test<- model.matrix(inAudience ~., data=val)
My_test<- val$inAudience == 1

OOS <- data.frame(logistic = rep(NA,3),lassoTheory=rep(NA,3),lasso1se=rep(NA,3), null = rep(NA,3)) 
pred.logistic <- predict(model.logistic, newdata=val, type="response")
values <- FPR_TPR((pred.logistic >= 0.1) , My_test )
OOS$logistic[1] <- values$FPR
OOS$logistic[2]  <- values$TPR
OOS$logistic[3]  <- values$ACC

pred.lassoTheory <- predict(model.lassoTheory, newx=Mx_test, type="response")
values <- FPR_TPR((pred.lassoTheory >= 0.1) , My_test)
OOS$lassoTheory[1] <- values$FPR
OOS$lassoTheory[2]  <- values$TPR
OOS$lassoTheory[3]  <- values$ACC


pred.lasso1se <- predict(model.lasso1se, newx=Mx_test, type="response")
values <- FPR_TPR((pred.lasso1se >= 0.1) , My_test)
OOS$lasso1se[1] <- values$FPR
OOS$lasso1se[2]  <- values$TPR
OOS$lasso1se[3]  <- values$ACC

pred.null <- predict(model.null, newdata=val, type="response")
values <- FPR_TPR((pred.null >= 0.1), My_test)
OOS$null[1] <- values$FPR
OOS$null[2]  <- values$TPR
OOS$null[3]  <- values$ACC

plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(-0.4,0.4), ylim=c(0,0.05), bty="n", xlab = "False positive rate", ylab="True positive rate")
lines(c(0,1),c(0,0.2), lty=2)
text( OOS$logistic[1] , OOS$logistic[2]+0.003,labels=c("logistic"))
points( OOS$logistic[1] , OOS$logistic[2] )
text( OOS$lassoTheory[1] , OOS$lassoTheory[2]+0.003,labels=c("lasso theory"))
points( OOS$lassoTheory[1] , OOS$lassoTheory[2] )
text( OOS$lasso1se[1] , OOS$lasso1se[2]+0.003,labels=c("lasso1se"))
points( OOS$lasso1se[1] , OOS$lasso1se[2] )
text( OOS$null[1],OOS$null[2]+0.003,labels=c("null"))
points(OOS$null[1],OOS$null[2])

barplot(c(0.8684,unlist(OOS[3,])), xpd=FALSE, ylim=c(.8,1), xlab="Method",
        names = c("\n neural \n network", "logistic\n", "\n lasso \n theory", "lasso1se \n", "\n null \n"), ylab = "Accuracy")
