###FIN647 Case 1###
######Part B######
# load library
library(tidyverse)
#install.packages("CVXR")
library(CVXR)

#load data
load('A.RData')
load('B1.RData')

### Part B1###
## EF frontier with estimated mean & stdev
# get investment universe matrix
mean = B1$mean
sd = B1$sd
cor = B1$cor
rf = B1$rf
VCV = sd %*% t(sd) *cor
 
# calculate EF matrix
nasset = 6
TargetER = seq(0.012,0.071,by=0.001)
w = Variable(nasset)
r = t(w) %*% mean
var = quad_form(w,VCV)
obj = var
alloc.frontier = matrix(0, nrow = length(TargetER), ncol = nasset)
Std.frontier = rep(0, length(TargetER))

for (i in seq_along(TargetER)){
  constraints = list(w>=0, sum(w) == 1, r==TargetER[i])
  prob = Problem(Minimize(obj), constraints)
  result = solve(prob)
  alloc.frontier[i,] = result$getValue(w)
  Std.frontier[i] = result$getValue(sqrt(var))
}
ER.frontier = TargetER

# plot frontier
plot(Std.frontier, ER.frontier 
     , type = 'l', col = 'blue', lwd = 2
     , main = 'MVF Constrained'
     , xlab = 'Portfolio Standard Deviation'
     , ylab = 'Portfolio Return'
     , xlim = c(0, 0.25), ylim = c(0, 0.08)
     , xaxt = 'n', yaxt = 'n'
)
points(x = Std.frontier[which.max(ER.frontier/Std.frontier)], 
       y = ER.frontier[which.max(ER.frontier/Std.frontier)], type = "p", col = 'red', pch = 19)
text(x = Std.frontier[which.max(ER.frontier/Std.frontier)]+0.04, 
     y = ER.frontier[which.max(ER.frontier/Std.frontier)],
     labels = 'MVE Portfolio',col = 'red')
axis(1, at=seq(0, 0.25, by = 0.05), labels = paste0(seq(0,25, by = 5),'.00%'))
axis(2, at=seq(0,0.08, by = 0.005), labels = paste(seq(0,8,by=0.5), '%'), las = 2,lwd =2)


## sensitivity test 1: increase estimated mean by 2%
mean2 = mean + 0.02
# calculate EF matrix
nasset = 6
TargetER = seq(0.012,0.071,by=0.001)
w = Variable(nasset)
r = t(w) %*% mean2
var = quad_form(w,VCV)
obj = var
alloc.frontier2 = matrix(0, nrow = length(TargetER), ncol = nasset)
Std.frontier2 = rep(0, length(TargetER))

for (i in seq_along(TargetER)){
  constraints = list(w>=0, sum(w) == 1, r==TargetER[i])
  prob = Problem(Minimize(obj), constraints)
  result = solve(prob)
  alloc.frontier2[i,] = result$getValue(w)
  Std.frontier2[i] = result$getValue(sqrt(var))
}

## sensitivity test 2: decrease mean by 2%
mean3 = mean - 0.02
# calculate EF matrix
nasset = 6
TargetER = seq(0.012,0.071,by=0.001)
w = Variable(nasset)
r = t(w) %*% mean3
var = quad_form(w,VCV)
obj = var
alloc.frontier3 = matrix(0, nrow = length(TargetER), ncol = nasset)
Std.frontier3 = rep(0, length(TargetER))

for (i in seq_along(TargetER)){
  constraints = list(w>=0, sum(w) == 1, r==TargetER[i])
  prob = Problem(Minimize(obj), constraints)
  result = solve(prob)
  alloc.frontier3[i,] = result$getValue(w)
  Std.frontier3[i] = result$getValue(sqrt(var))
}

## sensitivity test 3: increase estimated sd by 2%
sd2 = sd+0.02
VCV2 = sd2 %*% t(sd2) *cor
# calculate EF matrix
nasset = 6
TargetER = seq(0.012,0.071,by=0.001)
w = Variable(nasset)
r = t(w) %*% mean
var = quad_form(w,VCV2)
obj = var
alloc.frontier4 = matrix(0, nrow = length(TargetER), ncol = nasset)
Std.frontier4 = rep(0, length(TargetER))

for (i in seq_along(TargetER)){
  constraints = list(w>=0, sum(w) == 1, r==TargetER[i])
  prob = Problem(Minimize(obj), constraints)
  result = solve(prob)
  alloc.frontier4[i,] = result$getValue(w)
  Std.frontier4[i] = result$getValue(sqrt(var))
}

## sensitivity test 4: decrease estimated sd by 2%
sd3 = sd-0.02
VCV3 = sd3 %*% t(sd3) *cor
# calculate EF matrix
nasset = 6
TargetER = seq(0.012,0.071,by=0.001)
w = Variable(nasset)
r = t(w) %*% mean
var = quad_form(w,VCV3)
obj = var
alloc.frontier5 = matrix(0, nrow = length(TargetER), ncol = nasset)
Std.frontier5 = rep(0, length(TargetER))

for (i in seq_along(TargetER)){
  constraints = list(w>=0, sum(w) == 1, r==TargetER[i])
  prob = Problem(Minimize(obj), constraints)
  result = solve(prob)
  alloc.frontier5[i,] = result$getValue(w)
  Std.frontier5[i] = result$getValue(sqrt(var))
}

# plot frontier with sensitity test for means
plot(Std.frontier2, ER.frontier
     , type = 'l', col = 'blue', lwd = 2
     , main = 'MVF Constrained: increase estimated mean by 2%'
     , xlab = 'Portfolio Standard Deviation'
     , ylab = 'Portfolio Return'
     , xlim = c(0, 0.25), ylim = c(0, 0.08)
     , xaxt = 'n', yaxt = 'n'
)
points(x = Std.frontier2[which.max(ER.frontier/Std.frontier2)], 
       y = ER.frontier[which.max(ER.frontier/Std.frontier2)], type = "p", col = 'red', pch = 19)
text(x = Std.frontier2[which.max(ER.frontier/Std.frontier2)]+0.04, 
     y = ER.frontier[which.max(ER.frontier/Std.frontier2)],
     labels = 'MVE Portfolio',col = 'red')
axis(1, at=seq(0, 0.25, by = 0.05), labels = paste0(seq(0,25, by = 5),'.00%'))
axis(2, at=seq(0,0.08, by = 0.005), labels = paste(seq(0,8,by=0.5), '%'), las = 2,lwd =2)

plot(Std.frontier3, ER.frontier
     , type = 'l', col = 'blue', lwd = 2
     , main = 'MVF Constrained: decrease estimated mean by 2%'
     , xlab = 'Portfolio Standard Deviation'
     , ylab = 'Portfolio Return'
     , xlim = c(0, 0.25), ylim = c(0, 0.08)
     , xaxt = 'n', yaxt = 'n'
)
points(x = Std.frontier3[which.max(ER.frontier/Std.frontier3)], 
       y = ER.frontier[which.max(ER.frontier/Std.frontier3)], type = "p", col = 'red', pch = 19)
text(x = Std.frontier3[which.max(ER.frontier/Std.frontier3)]+0.04, 
     y = ER.frontier[which.max(ER.frontier/Std.frontier3)],
     labels = 'MVE Portfolio',col = 'red')
axis(1, at=seq(0, 0.25, by = 0.05), labels = paste0(seq(0,25, by = 5),'.00%'))
axis(2, at=seq(0,0.08, by = 0.005), labels = paste(seq(0,8,by=0.5), '%'), las = 2,lwd =2)

# plot frontier with sensitity test for sd

plot(Std.frontier4, ER.frontier
     , type = 'l', col = 'blue', lwd = 2
     , main = 'MVF Constrained: increase estimated standard deviation by 2%'
     , xlab = 'Portfolio Standard Deviation'
     , ylab = 'Portfolio Return'
     , xlim = c(0, 0.25), ylim = c(0, 0.08)
     , xaxt = 'n', yaxt = 'n'
)
points(x = Std.frontier4[which.max(ER.frontier/Std.frontier4)], 
       y = ER.frontier[which.max(ER.frontier/Std.frontier4)], type = "p", col = 'red', pch = 19)
text(x = Std.frontier4[which.max(ER.frontier/Std.frontier4)]+0.04, 
     y = ER.frontier[which.max(ER.frontier/Std.frontier4)],
     labels = 'MVE Portfolio',col = 'red')
axis(1, at=seq(0, 0.25, by = 0.05), labels = paste0(seq(0,25, by = 5),'.00%'))
axis(2, at=seq(0,0.08, by = 0.005), labels = paste(seq(0,8,by=0.5), '%'), las = 2,lwd =2)


plot(Std.frontier5, ER.frontier
     , type = 'l', col = 'blue', lwd = 2
     , main = 'MVF Constrained: decrease estimated standard deviation by 2%'
     , xlab = 'Portfolio Standard Deviation'
     , ylab = 'Portfolio Return'
     , xlim = c(0, 0.25), ylim = c(0, 0.08)
     , xaxt = 'n', yaxt = 'n'
)
lines(Std.frontier4, ER.frontier, col = 'green')
lines(Std.frontier, ER.frontier, col ='yellow')
points(x = Std.frontier5[which.max(ER.frontier/Std.frontier5)], 
       y = ER.frontier[which.max(ER.frontier/Std.frontier5)], type = "p", col = 'red', pch = 19)
axis(1, at=seq(0, 0.25, by = 0.05), labels = paste0(seq(0,25, by = 5),'.00%'))
axis(2, at=seq(0,0.08, by = 0.005), labels = paste(seq(0,8,by=0.5), '%'), las = 2,lwd =2)


## effecient portfolios matrix
port.Returns = c(0.015,0.03, seq(0.035,0.07, by = 0.005))*100
temp = ER.frontier*100
idx = temp %in% port.Returns
idx[c(19, 34,49)] = TRUE
port.stdev = Std.frontier[idx]*100
port.SR = port.Returns/port.stdev
port.w = alloc.frontier[idx,]
port.noneq.w = rowSums(port.w[,1:3])
port.eq.w = rowSums(port.w[,4:6])
outputB1 = round(cbind(port.Returns,port.stdev,port.SR,port.noneq.w,port.eq.w),2)
outputB1 = data.frame(outputB1)
colnames(outputB1) = c('Expected Returns', 'Standard Deviation','Sharp Ratio',
                       'Sum of Non-Equity Weights', 'Sum of Equity Weights')

### Part B2###
load('B2.RData')
s1 = r$s1
s2 = r$s2
s3 = r$s3

## scenario 1
age = seq(25,85, by=1)
ports1 = matrix(0, nrow = nrow(payment), ncol = 4)
scenario1 = data.frame(cbind(age, payment$Net.Payment,s1,ports1))
colnames(scenario1) = c('age', 'payment', '4%', '5%', '6%', '7%', 'portvalue4%','portvalue5%', 'portvalue6%', 'portvalue7%')
scenario1[1,7:10] = scenario1$payment[1]

for (i in 2:(nrow(scenario1))){
  if (scenario1$`portvalue4%`[i-1] > 0){
    scenario1$`portvalue4%`[i] = scenario1$`portvalue4%`[i-1]*(1+scenario1$`4%`[i])+scenario1$payment[i]
  } else {
    scenario1$`portvalue4%`[i] = scenario1$`portvalue4%`[i-1] + scenario1$payment[i]
  }
  if (scenario1$`portvalue5%`[i-1] > 0){
    scenario1$`portvalue5%`[i] = scenario1$`portvalue5%`[i-1]*(1+scenario1$`5%`[i])+scenario1$payment[i]
  } else {
    scenario1$`portvalue5%`[i] = scenario1$`portvalue5%`[i-1] + scenario1$payment[i]
  }
  if (scenario1$`portvalue6%`[i-1] > 0){
    scenario1$`portvalue6%`[i] = scenario1$`portvalue6%`[i-1]*(1+scenario1$`6%`[i])+scenario1$payment[i]
  } else {
    scenario1$`portvalue6%`[i] = scenario1$`portvalue6%`[i-1] + scenario1$payment[i]
  }
  if (scenario1$`portvalue7%`[i-1] > 0){
    scenario1$`portvalue7%`[i] = scenario1$`portvalue7%`[i-1]*(1+scenario1$`7%`[i])+scenario1$payment[i]
  } else {
    scenario1$`portvalue7%`[i] = scenario1$`portvalue7%`[i-1] + scenario1$payment[i]
  }
}            

temp = seq(1,61,by=1)
discount_rate = 1/(1.02)^temp
discounted_net_pay = payment$Net.Payment*discount_rate

scenario1_net = data.frame(matrix(0, nrow = 61, ncol = 4))
colnames(scenario1_net) = c('net4%', 'net5%', 'net6%', 'net7%')
for (i in 1:(nrow(scenario1))){
  scenario1_net$`net4%`[i] = scenario1$`portvalue4%`[i] + (sum(discounted_net_pay[(i+1):61]))/discount_rate[i]
  scenario1_net$`net5%`[i] = scenario1$`portvalue5%`[i] + (sum(discounted_net_pay[(i+1):61]))/discount_rate[i]
  scenario1_net$`net6%`[i] = scenario1$`portvalue6%`[i] + (sum(discounted_net_pay[(i+1):61]))/discount_rate[i]
  scenario1_net$`net7%`[i] = scenario1$`portvalue7%`[i] + (sum(discounted_net_pay[(i+1):61]))/discount_rate[i]
}
scenario1_net[61,] = scenario1[61,7:10]
  
## scenario 2
ports2 = matrix(0, nrow = nrow(payment), ncol = 4)
scenario2 = data.frame(cbind(age, payment$Net.Payment,s2,ports2))
colnames(scenario2) = c('age', 'payment', '4%', '5%', '6%', '7%', 'portvalue4%','portvalue5%', 'portvalue6%', 'portvalue7%')
scenario2[1,7:10] = scenario2$payment[1]

for (i in 2:(nrow(scenario2))){
  if (scenario2$`portvalue4%`[i-1] > 0){
    scenario2$`portvalue4%`[i] = scenario2$`portvalue4%`[i-1]*(1+scenario2$`4%`[i])+scenario2$payment[i]
  } else {
    scenario2$`portvalue4%`[i] = scenario2$`portvalue4%`[i-1] + scenario2$payment[i]
  }
  if (scenario2$`portvalue5%`[i-1] > 0){
    scenario2$`portvalue5%`[i] = scenario2$`portvalue5%`[i-1]*(1+scenario2$`5%`[i])+scenario2$payment[i]
  } else {
    scenario2$`portvalue5%`[i] = scenario2$`portvalue5%`[i-1] + scenario2$payment[i]
  }
  if (scenario2$`portvalue6%`[i-1] > 0){
    scenario2$`portvalue6%`[i] = scenario2$`portvalue6%`[i-1]*(1+scenario2$`6%`[i])+scenario2$payment[i]
  } else {
    scenario2$`portvalue6%`[i] = scenario2$`portvalue6%`[i-1] + scenario2$payment[i]
  }
  if (scenario2$`portvalue7%`[i-1] > 0){
    scenario2$`portvalue7%`[i] = scenario2$`portvalue7%`[i-1]*(1+scenario2$`7%`[i])+scenario2$payment[i]
  } else {
    scenario2$`portvalue7%`[i] = scenario2$`portvalue7%`[i-1] + scenario2$payment[i]
  }
}  

scenario2_net = data.frame(matrix(0, nrow = 61, ncol = 4))
colnames(scenario2_net) = c('net4%', 'net5%', 'net6%', 'net7%')
for (i in 1:(nrow(scenario1))){
  scenario2_net$`net4%`[i] = scenario2$`portvalue4%`[i] + (sum(discounted_net_pay[(i+1):61]))/discount_rate[i]
  scenario2_net$`net5%`[i] = scenario2$`portvalue5%`[i] + (sum(discounted_net_pay[(i+1):61]))/discount_rate[i]
  scenario2_net$`net6%`[i] = scenario2$`portvalue6%`[i] + (sum(discounted_net_pay[(i+1):61]))/discount_rate[i]
  scenario2_net$`net7%`[i] = scenario2$`portvalue7%`[i] + (sum(discounted_net_pay[(i+1):61]))/discount_rate[i]
}
scenario2_net[61,] = scenario2[61,7:10]

## scenario 3
ports3 = matrix(0, nrow = nrow(payment), ncol = 4)
scenario3 = data.frame(cbind(age, payment$Net.Payment,s3,ports3))
colnames(scenario3) = c('age', 'payment', '4%', '5%', '6%', '7%', 'portvalue4%','portvalue5%', 'portvalue6%', 'portvalue7%')
scenario3[1,7:10] = scenario3$payment[1]

for (i in 2:(nrow(scenario3))){
  if (scenario3$`portvalue4%`[i-1] > 0){
    scenario3$`portvalue4%`[i] = scenario3$`portvalue4%`[i-1]*(1+scenario3$`4%`[i])+scenario3$payment[i]
  } else {
    scenario3$`portvalue4%`[i] = scenario3$`portvalue4%`[i-1] + scenario3$payment[i]
  }
  if (scenario3$`portvalue5%`[i-1] > 0){
    scenario3$`portvalue5%`[i] = scenario3$`portvalue5%`[i-1]*(1+scenario3$`5%`[i])+scenario3$payment[i]
  } else {
    scenario3$`portvalue5%`[i] = scenario3$`portvalue5%`[i-1] + scenario3$payment[i]
  }
  if (scenario3$`portvalue6%`[i-1] > 0){
    scenario3$`portvalue6%`[i] = scenario3$`portvalue6%`[i-1]*(1+scenario3$`6%`[i])+scenario3$payment[i]
  } else {
    scenario3$`portvalue6%`[i] = scenario3$`portvalue6%`[i-1] + scenario3$payment[i]
  }
  if (scenario3$`portvalue7%`[i-1] > 0){
    scenario3$`portvalue7%`[i] = scenario3$`portvalue7%`[i-1]*(1+scenario3$`7%`[i])+scenario3$payment[i]
  } else {
    scenario3$`portvalue7%`[i] = scenario3$`portvalue7%`[i-1] + scenario3$payment[i]
  }
}  

scenario3_net = data.frame(matrix(0, nrow = 61, ncol = 4))
colnames(scenario3_net) = c('net4%', 'net5%', 'net6%', 'net7%')
for (i in 1:(nrow(scenario1))){
  scenario3_net$`net4%`[i] = scenario3$`portvalue4%`[i] + (sum(discounted_net_pay[(i+1):61]))/discount_rate[i]
  scenario3_net$`net5%`[i] = scenario3$`portvalue5%`[i] + (sum(discounted_net_pay[(i+1):61]))/discount_rate[i]
  scenario3_net$`net6%`[i] = scenario3$`portvalue6%`[i] + (sum(discounted_net_pay[(i+1):61]))/discount_rate[i]
  scenario3_net$`net7%`[i] = scenario3$`portvalue7%`[i] + (sum(discounted_net_pay[(i+1):61]))/discount_rate[i]
}
scenario3_net[61,] = scenario3[61,7:10]

## final output table 
age_list = c(55,64,75,85)
outputB2 = rbind(scenario1_net[which(age %in% age_list),], scenario2_net[which(age %in% age_list),], scenario3_net[which(age %in% age_list),])
                      
library(xlsx)
write.xlsx(outputB2, "coutputB2.xlsx")
