######FIN527Q Case2######
#####Team B21#####

# load packages & data
library(tidyverse)
load('data_2.RData')

## Analysis 1 ##
# Calculate and record the annualized mean - rf, annualized sd, and annualized SR 
# for each trading strategy, not BRK-A.
rf = dt1$RF
nts = dt1[,1:5] #new strategy
kts = dt1[,6:9] #known strategy
nts.mean = colMeans(nts-rf)*12 # annualized mean
nts.sd = apply(nts,2,sd)*(12^0.5) # annualzied standard dev
nts.sr =  nts.mean/nts.sd# annualized sharpe ratio
output1 = data.frame(rbind(nts.mean,nts.sd,nts.sr),
                     row.names = c('Annualized Mean','Annualized Standard Deviation',
                                    'Annualized Sharpe Ratio'))
output1 = round(output1,3)
## Analysis 2 ##
# Calculate and record the regression results for a one factor model where the only factor is RM-Rf
# For each strategy compute the idiosyncratic variance and market variance as 
# percentages of the total variance.
reg_capm = NULL
nts.names = colnames(nts)

for(nts.name in nts.names) {
  lm.temp = lm(unlist(nts[nts.name]-rf) ~ kts$Mkt.RF) # run linear regression
  sum = summary(lm.temp)
  ab = sum$coefficients[,'Estimate'] # get alpha & beta
  ab.se = sum$coefficients[,'Std. Error'] # get alpha & beta sd
  R = sum$r.squared
  ab.h0 = c(0,1) #test alpha different from 0 beta different from 1
  ab.tstate = (ab-ab.h0)/ab.se # t-stat for alpha & beta
  reg = c(ab, R, ab.tstate)
  reg_capm = rbind(reg_capm, reg)
}

reg_capm[1, 4:5] = NA #mkt.cap weighted computed exactly as mkt.rf
rownames(reg_capm) = nts.names
colnames(reg_capm) = c('alpha', 'beta', 'R.squared', 'tstat.alpha', 'tstat.beta')
reg_capm = data.frame(reg_capm)
reg_capm$alpha = paste0(round(reg_capm$alpha*100,3), '%')

reg_capm[,'systematic.var'] = reg_capm$R.squared # systematic variance
reg_capm[,'idiosyncratic.var'] = 1-reg_capm$R.squared # undiversifiable var
reg_capm[,-1] = round(reg_capm[,-1],3)

## Analysis 3##
# Calculate and record the regression results for a four factor model
# factors are RM-Rf, SMB, HML, and MOM
reg_factor = NULL

for(nts.name in nts.names) {
  lm.temp = lm(unlist(nts[nts.name]-rf) ~ kts$Mkt.RF + kts$SMB + kts$HML + kts$MOM) 
  sum = summary(lm.temp)
  coef = sum$coefficients[,'Estimate'] # factor coefficients
  coef.se = sum$coefficients[,'Std. Error'] # factor coefficients sd
  R = sum$r.squared
  coef.h0 = c(0,1,0,0,0) # t-test
  coef.tstate = (coef-coef.h0)/coef.se # t-stat for factors
  reg = c(coef, R, coef.tstate)
  reg_factor = rbind(reg_factor, reg)
}
reg_factor[,-1] = round(reg_factor[,-1],3)
reg_factor[1,7:11] = NA
rownames(reg_factor) = nts.names
colnames(reg_factor) = c('alpha', 'Mkt.Rf','SMB','HML','MOM','R.squared','tstat.alpha',
                         'tstat.Mkt.Rf','tstat.SMB','tstat.HML','tstat.MOM')
reg_factor = data.frame(reg_factor)
reg_factor$alpha = paste0(round(reg_factor$alpha*100,3), '%')


## Analysis 4##
# Calculate and record the regression results for a four factor model where the 
# factors are RM-Rf, SMB, HML, and MOM
reg_bkr = NULL

full = rep(TRUE, nrow(dt2))
sub1 = substr(rownames(dt2),1,4) %in% as.character(1980:2007) #subsample1  April 1980 ~ December 2007
sub2 = !sub1 #subsample2 January 2008 ~  December 2013
idx = cbind(full, sub1, sub2)

for(i in 1:ncol(idx)){
  samp = dt2[idx[,i],] #sample
  rf2 = samp[,6]
  BRK = samp[,1] #BRK-A ret
  kts2 = samp[,2:5] #factors
  msr = mean(kts2[,1])/sd(kts2[,1]+rf2) #value weighted market index SR
  sr = mean(BRK - rf2)/sd(BRK)
  lm.temp = lm(BRK - rf2 ~ kts2$Mkt.RF + kts2$SMB + kts2$HML + kts2$MOM)
  sum = summary(lm.temp)
  coef = sum$coefficients[,'Estimate'] # factor coefficients
  coef.se = sum$coefficients[,'Std. Error'] # factor coefficients sd
  R = sum$r.squared
  coef.tstate = coef/coef.se # t-stat for factors
  reg = c(msr, sr, coef, R, coef.tstate)
  reg_bkr = rbind(reg_bkr, reg)
}
rownames(reg_bkr) = c('1980-2013', '1980-2007','2008-2013')
colnames(reg_bkr) = c('Market Sharpe Ratio','BRK-A Sharpe Ratio','alpha', 
                         'Mkt.Rf','SMB','HML','MOM','R.squared','tstat.alpha',
                         'tstat.Mkt.Rf','tstat.SMB','tstat.HML','tstat.MOM')
reg_bkr = data.frame(reg_bkr)
reg_bkr$alpha = paste0(round(reg_bkr$alpha*100,3),'%')
reg_bkr[,-3] = round(reg_bkr[,-3],3)

## Analysis 5##
#c onstruct an appropriate long only benchmark for Berkshire Hathaway 
# based on any criteria you find relevant and compute the active returns 
# and the tracking error for Berkshire relative to this benchmark

# minimize tracking error of BRK-A

library(stringr)
#install.packages('Rsolnp')
library(Rsolnp)

y = dt3$BRK.A
x1 = dt3$Vanguard.S.P.500.Index.Inv..VFINX.
x2 = dt3$Vanguard.Small.Cap.Index.Inv..NAESX.
x3 = dt3$Vanguard.Value.Index.Inv..VIVAX.
ind_sub = str_sub(rownames(dt3),-4,-1) %in% as.character(2008:2013)

# equality constraints function
eq = function(b){
  return(b[2]+b[3]+b[4])
}
theta = c(0.1,0.5,0.25,0.25)
f = function(dt, sub=FALSE, Shorts = FALSE){
  if(sub){
    dt = dt[ind_sub,]
  }
  y = dt[,1]
  x1 = dt[,2]
  x2 = dt[,3]
  x3 = dt[,4]
  # define loss funciton, b = vector with parameters
  f_loss = function(b){
    p = y # portfolio 
    b = b[1] + b[2]*x1 + b[3]*x3 + b[4]*x3 # benchmark
    return(sum((b-p)^2)) # objective function: sum of diff between BRKA & investment portfolio
  }
  if(Shorts){
    m = solnp(theta, f_loss, eqfun=eq, eqB=1,LB=c(-Inf,-Inf,-Inf,-Inf))
  } else {
    m = solnp(theta, f_loss, eqfun=eq, eqB=1,LB=c(-Inf,0,0,0))
  }
  w = m$pars
  y_b = as.matrix(dt)%*%w
  mar = mean(y-y_b) # mean active return
  te = sd(y-y_b) # tracking error
  return(c(w, mar,te))
}

# long only, unit exposure
r1 = f(dt3,sub=F,Shorts = F)
# long only, unit exposure, 2008-2013 subsample
r2 = f(dt3,sub=T,Shorts = F)

r = data.frame(rbind(r1, r2))
colnames(r) = c('alpha','Large-Cap','Small-Cap','Value','Mean Active Return','Tracking Error')
rownames(r) = c('1993-2013 Long Only Unit Exposure',
                '2008-2013 Long Only Unit Exposure') 
r$alpha = paste0(round(r$alpha*100,3),'%')
r[,-1] = round(r[,-1],3)

library(openxlsx)
write.xlsx(reg_bkr, "data.xlsx")

