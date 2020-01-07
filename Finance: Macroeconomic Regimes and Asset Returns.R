###FIN647 Case 4###
######Housing Kepping######
# load library
library(tidyverse)
library(readxl)

#load data
load('data_4.RData')

######Question 1######

###Question 1.a###
## Calculate the mean monthly nominal return, standard deviation of monthly returns, and monthly Sharpe Ratio 
## for each asset. TBill = nominal risk-free rate in each regime.

assets <- dt[,6:9]

## mean monthly nominal return
output1a_mean <- matrix(NA,nrow = 5, ncol = 4)
row_names = c('Whole Sample','Low INF Low GRW', 'High INF Low GRW',  'Low INF High GRW','High INF High GRW')
output1a_mean <- data.frame(output1a_mean, row.names = row_names)
names(output1a_mean) <- names(assets)

output1a_mean[1,] = colMeans(assets)
output1a_mean[2,] = colMeans(assets[dt$INF1GRW1 == 1,])
output1a_mean[3,] = colMeans(assets[dt$INF2GRW1 == 1,])
output1a_mean[4,] = colMeans(assets[dt$INF1GRW2 == 1,])
output1a_mean[5,] = colMeans(assets[dt$INF2GRW2 == 1,])

## standard deviation of monthly returns
output1a_sd <- matrix(NA,nrow = 5, ncol = 4)
output1a_sd <- data.frame(output1a_sd, row.names = row_names)
names(output1a_sd) <- names(assets)

output1a_sd[1,] = apply(assets,2,sd)
output1a_sd[2,] = apply(assets[dt$INF1GRW1 == 1,],2,sd)
output1a_sd[3,] = apply(assets[dt$INF2GRW1 == 1,],2,sd)
output1a_sd[4,] = apply(assets[dt$INF1GRW2 == 1,],2,sd)
output1a_sd[5,] = apply(assets[dt$INF2GRW2 == 1,],2,sd)

## monthly Sharpe Ratio
output1a_SR <- matrix(NA,nrow = 5, ncol = 4)
output1a_SR <- data.frame(output1a_SR, row.names = row_names)
names(output1a_SR) <- names(assets)

output1a_SR[1,] = (output1a_mean[1,] - mean(dt$Tbill))/output1a_sd[1,]
output1a_SR[2,] = (output1a_mean[2,] - mean(dt$Tbill[dt$INF1GRW1 == 1]))/output1a_sd[2,]
output1a_SR[3,] = (output1a_mean[3,] - mean(dt$Tbill[dt$INF2GRW1 == 1]))/output1a_sd[3,]
output1a_SR[4,] = (output1a_mean[4,] - mean(dt$Tbill[dt$INF1GRW2 == 1]))/output1a_sd[4,]
output1a_SR[5,] = (output1a_mean[5,] - mean(dt$Tbill[dt$INF2GRW2 == 1]))/output1a_sd[5,]


###Question 1.b###
## Calculate the variance-covariance matrix (VCV) for the four risky assets’ nominal returns 
## (equity, bonds, gold, and commodities) using only the data on that sheet.

# whole sample
cor_whole = cor(assets)
VCV_whole = apply(assets,2,sd) %*% t(apply(assets,2,sd))*cor_whole

# INF1GRW1
cor_INF1GRW1 = cor(assets[dt$INF1GRW1 == 1,])
VCV_INF1GRW1 = apply(assets[dt$INF1GRW1 == 1,],2,sd) %*% t(apply(assets[dt$INF1GRW1 == 1,],2,sd))*cor_INF1GRW1

# INF2GRW1
cor_INF2GRW1 = cor(assets[dt$INF2GRW1 == 1,])
VCV_INF2GRW1 = apply(assets[dt$INF2GRW1 == 1,],2,sd) %*% t(apply(assets[dt$INF2GRW1 == 1,],2,sd))*cor_INF2GRW1

# INF1GRW2
cor_INF1GRW2 = cor(assets[dt$INF1GRW2 == 1,])
VCV_INF1GRW2 = apply(assets[dt$INF1GRW2 == 1,],2,sd) %*% t(apply(assets[dt$INF1GRW2 == 1,],2,sd))*cor_INF1GRW2

# INF2GRW2
cor_INF2GRW2 = cor(assets[dt$INF2GRW2 == 1,])
VCV_INF2GRW2 = apply(assets[dt$INF2GRW2 == 1,],2,sd) %*% t(apply(assets[dt$INF2GRW2 == 1,],2,sd))*cor_INF2GRW2

###Question 1.c###
# calculate the portfolio weights, expected return, expected standard deviation of returns, 
# and expected Sharpe Ratio of the maximal Sharpe Ratio portfolio.
vec=rep(1, length=4)

MSRfunction <- function(ret,rf, VCV){
  allocMSR1 = t(solve(VCV) %*% (ret-rf)) / as.numeric(t(vec) %*% solve(VCV) %*% (ret-rf))
  allocMSR = data.frame(t(append(allocMSR1, 1-sum(allocMSR1))))
  ER = as.matrix(allocMSR) %*% append(ret,rf)
  alloc.r = allocMSR[,-ncol(allocMSR)]
  Std = (as.numeric(alloc.r) %*% VCV %*% as.numeric(alloc.r)) ^ 0.5
  SR = (ER-rf) / Std
  return(cbind(allocMSR,ER,Std,SR))
}

output1c<- matrix(NA,nrow = 5, ncol = 8)
output1c <- data.frame(output1c, row.names = row_names)
names(output1c) <- c('Stocks','Bonds','Gold','Commodities','Rf','Expected Return','Standard Deviation','Sharpe Ratio')
output1c[1,] <- MSRfunction(colMeans(assets),mean(dt$Tbill),VCV_whole)
output1c[2,] <- MSRfunction(colMeans(assets[dt$INF1GRW1 == 1,]),mean(dt$Tbill[dt$INF1GRW1 == 1]),VCV_INF1GRW1)
output1c[3,] <- MSRfunction(colMeans(assets[dt$INF2GRW1 == 1,]),mean(dt$Tbill[dt$INF2GRW1 == 1]),VCV_INF2GRW1)
output1c[4,] <- MSRfunction(colMeans(assets[dt$INF1GRW2 == 1,]),mean(dt$Tbill[dt$INF1GRW2 == 1]),VCV_INF1GRW2)
output1c[5,] <- MSRfunction(colMeans(assets[dt$INF2GRW2 == 1,]),mean(dt$Tbill[dt$INF2GRW2 == 1]),VCV_INF2GRW2)

###Question 1.d###
## Repeat step c for the global minimum variance portfolio.

GMVfunction <- function(ret,rf, VCV){
  allocGMV1 = t(solve(VCV) %*% vec) / as.numeric(t(vec) %*% solve(VCV) %*% vec)
  allocGMV = data.frame(t(append(allocGMV1, 1-sum(allocGMV1))))
  ER = as.matrix(allocGMV) %*% append(ret,rf)
  alloc.r = allocGMV[,-ncol(allocGMV)]
  Std = (as.numeric(alloc.r) %*% VCV %*% as.numeric(alloc.r)) ^ 0.5
  SR = (ER-rf) / Std
  return(cbind(allocGMV,ER,Std,SR))
}

output1d <- matrix(NA,nrow = 5, ncol = 8)
output1d <- data.frame(output1d, row.names = row_names)
names(output1d) <- names(output1c)
output1d[1,] <- GMVfunction(colMeans(assets),mean(dt$Tbill),VCV_whole)
output1d[2,] <- GMVfunction(colMeans(assets[dt$INF1GRW1 == 1,]),mean(dt$Tbill[dt$INF1GRW1 == 1]),VCV_INF1GRW1)
output1d[3,] <- GMVfunction(colMeans(assets[dt$INF2GRW1 == 1,]),mean(dt$Tbill[dt$INF2GRW1 == 1]),VCV_INF2GRW1)
output1d[4,] <- GMVfunction(colMeans(assets[dt$INF1GRW2 == 1,]),mean(dt$Tbill[dt$INF1GRW2 == 1]),VCV_INF1GRW2)
output1d[5,] <- GMVfunction(colMeans(assets[dt$INF2GRW2 == 1,]),mean(dt$Tbill[dt$INF2GRW2 == 1]),VCV_INF2GRW2)

###Question 1.e###
## Repeat step c for the optimal portfolio allocations for 
## mean variance investors with risk aversion (A in the equations below) of 1.3, 2.8, 6.5, 10.5, and 16.9.

Aversion = c(1.3, 2.8, 6.5, 10.5, 16.9 )

RAfunction <- function(ret,rf, VCV){
  allcoRA1 = t(sapply(
    Aversion,
    FUN = function(x){
      t(solve(VCV) %*% (ret-rf)) / x
    }
  ))
  allocRA = cbind(allcoRA1, 1-rowSums(allcoRA1))
  ER = as.matrix(allocRA) %*% append(ret,rf)
  std = sapply(
    X = 1:nrow(allcoRA1),
    FUN = function(x){
      (allcoRA1[x, ] %*% VCV %*% (allcoRA1[x, ])) ^ 0.5
    }
  )
  SR = (ER-rf)/std
  return(cbind(allocRA,ER,std,SR))
}

row_names2 = c('A=1.3','A=2.8','A=6.5','A=10.5','A=16.9')

output1e_whole <- RAfunction(colMeans(assets),mean(dt$Tbill),VCV_whole)
output1e_whole <- data.frame(output1e_whole, row.names = row_names2)
names(output1e_whole) <- names(output1c)


output1e_INF1GRW1 <- RAfunction(colMeans(assets[dt$INF1GRW1 == 1,]),mean(dt$Tbill[dt$INF1GRW1 == 1]),VCV_INF1GRW1)
output1e_INF1GRW1 <- data.frame(output1e_INF1GRW1, row.names = row_names2)
names(output1e_INF1GRW1) <- names(output1c)

output1e_INF2GRW1 <- RAfunction(colMeans(assets[dt$INF2GRW1 == 1,]),mean(dt$Tbill[dt$INF2GRW1 == 1]),VCV_INF2GRW1)
output1e_INF2GRW1 <- data.frame(output1e_INF2GRW1, row.names = row_names2)
names(output1e_INF2GRW1) <- names(output1c)


output1e_INF1GRW2 <- RAfunction(colMeans(assets[dt$INF1GRW2 == 1,]),mean(dt$Tbill[dt$INF1GRW2 == 1]),VCV_INF1GRW2)
output1e_INF1GRW2 <- data.frame(output1e_INF1GRW2, row.names = row_names2)
names(output1e_INF1GRW2) <- names(output1c)

output1e_INF2GRW2 <- RAfunction(colMeans(assets[dt$INF2GRW2 == 1,]),mean(dt$Tbill[dt$INF2GRW2 == 1]),VCV_INF2GRW2)
output1e_INF2GRW2 <- data.frame(output1e_INF2GRW2, row.names = row_names2)
names(output1e_INF2GRW2) <- names(output1c)

######Question 2######
## Create a summary table on the “Summary” sheet which contains: 
# the mean, standard deviation, and Sharpe Ratio of each asset return calculated on 
# each sheet and the weights of the portfolio optimal for a mean-variance investor 
# with a risk aversion of 6.5 calculated on each sheet (the “6.5-Optimal Portfolio”).

output2_summary = rbind(output1e_whole[3,],output1e_INF1GRW1[3,],output1e_INF2GRW1[3,],output1e_INF1GRW2[3,],output1e_INF2GRW2[3,])
row.names(output2_summary) <- row_names

######Question 3######
## Create a set of portfolio weights by equally weighting the portfolio weights of the 6.5-Optimal Portfolio 
## calculated for each regime (not the Unconditional data set, so the weights will each be .25). 
## Call this the Static – EW Portfolio. 

weights <- output2_summary[2:5,1:5]
output3_Static.EW.Portfolio <- colMeans(weights)

## Now create 4 more sets of portfolio weights, each by weighting one regime 1/2 and the other three at 1/6, 
## naming each Tilt – “Regime” (e.g “Tilt – INF1GRW1”) based on the regime it overweights.

output3_Tilt_INF1GRW1 <- 1/2*weights[1,]+1/6*(colSums(weights[2:4,]))
output3_Tilt_INF2GRW1 <- 1/2*weights[2,]+1/6*(colSums(weights[3:4,])+weights[1,])
output3_Tilt_INF1GRW2 <- 1/2*weights[3,]+1/6*(colSums(weights[1:2,])+weights[4,])
output3_Tilt_INF2GRW2 <- 1/2*weights[4,]+1/6*(colSums(weights[1:3,]))

output3_Tilt_Regime <- rbind(output3_Tilt_INF1GRW1,output3_Tilt_INF2GRW1,output3_Tilt_INF1GRW2,output3_Tilt_INF2GRW2)

######Question 4######
## Estimate the Sharpe ratio of each 6.5-Optimal Portfolio and the Static and Tilted Portfolios 
## in each regime and in the unconditional (full) data set. Thus you will have a 10x5 table of 
## Sharpe Ratios, Unconditional in Unconditional, Unconditional in INF1GRW1, etc. 
## You may use the VCV and mean returns calculated in each regime to create these estimates.

output4 <- matrix(NA, ncol = 10, nrow = 5)
output4 <- data.frame(output4, row.names = row_names)
names(output4) <- c('A=6.5 Optimal Whole Sample','A=6.5 Optimal INF1GRW1','A=6.5 Optimal INF2GRW1',
                    'A=6.5 Optimal INF1GRW2','A=6.5 Optimal INF2GRW2','Static EW','Tilt INF1GRW1',
                    'Tilt INF2GRW1','Tilt INF1GRW2','Tilt INF2GRW2')

SRfunction <- function(w, ret, rf, VCV) {
  ER = as.matrix(w) %*% append(ret,rf)
  w1 = w[,-ncol(w)]
  Std = (as.numeric(w1) %*% VCV %*% as.numeric(w1)) ^ 0.5
  SR = (ER-rf) / Std
  return(SR)
}

weights2 <- rbind(output2_summary[1:5,1:5],output3_Static.EW.Portfolio,output3_Tilt_Regime)


#whole sample
for (i in 1:10){
  output4[1,i] <- SRfunction(weights2[i,],colMeans(assets),mean(dt$Tbill),VCV_whole)
}

# low inf low grw
for (i in 1:10){
  output4[2,i] <- SRfunction(weights2[i,],colMeans(assets[dt$INF1GRW1 == 1,]),mean(dt$Tbill[dt$INF1GRW1 == 1]),VCV_INF1GRW1)
}

# high inf low grw
for (i in 1:10){
  output4[3,i] <- SRfunction(weights2[i,],colMeans(assets[dt$INF2GRW1 == 1,]),mean(dt$Tbill[dt$INF2GRW1 == 1]),VCV_INF2GRW1)
}

# low inf high grw
for (i in 1:10){
  output4[4,i] <- SRfunction(weights2[i,],colMeans(assets[dt$INF1GRW2 == 1,]),mean(dt$Tbill[dt$INF1GRW2 == 1]),VCV_INF1GRW2)
}

# high inf high grw
for (i in 1:10){
  output4[5,i] <- SRfunction(weights2[i,],colMeans(assets[dt$INF2GRW2 == 1,]),mean(dt$Tbill[dt$INF2GRW2 == 1]),VCV_INF2GRW2)
}

######End Housekeeping######
## clean data frame,  round to 4 decimal places
output1a_mean = round(output1a_mean,4)
output1a_sd = round(output1a_sd,4)
output1a_SR = round(output1a_SR,4)
output1c = round(output1c,4)
VCV_whole = round(VCV_whole,4)
VCV_INF1GRW1 = round(VCV_INF1GRW1,4)
