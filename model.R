#read the data in cvs
work <- read.csv("workorders.csv",header = T)

#print the whole dataset
day <- work$DAY
order <- work$WORKORDERS
order <- ts(order)
ts.plot(order)
order

#split the data into train and test part.
ofit <- order[1:108]
nfit <- length(ofit)
timefit <- time(ofit)
otest <- order[109:120]
ntest <- length(otest)
timetest <- time(otest)

#fit linear regression model
#k = 1
mlr.lin <- lm(ofit~timefit)
summary(mlr.lin)
plot(mlr.lin)
anova(mlr.lin)
#k = 2
tsqfit <- timefit^2/factorial(2)
mlr.quad <- lm(ofit~timefit+tsqfit)
summary(mlr.quad)
plot(mlr.quad)
anova(mlr.quad)
#k = 3
tcubfit <- timefit^3/factorial(3)
mlr.cub <- lm(ofit~timefit+tsqfit+tcubfit) 
summary(mlr.cub)
plot(mlr.cub)
anova(mlr.cub)
#plot fitted model
par(mfrow=c(2,2))
ts.plot(ofit,main="ofit") # Time Series Plot
# Plot of xfit vs mlr.lin$fitted 
plin=cbind(ofit,mlr.lin$fitted) 
ts.plot(plin,main="ofit and fit.linear") 
pquad=cbind(ofit,mlr.quad$fitted) 
ts.plot(pquad,main="ofit and fit.quadratic") 
pcub=cbind(ofit,mlr.cub$fitted) 
ts.plot(pcub,main="ofit and fitt.cubic")
#compare
sigsq.lin=anova(mlr.lin)[["Mean Sq"]][2] 
sigsq.quad=anova(mlr.quad)[["Mean Sq"]][3] 
sigsq.cub=anova(mlr.cub)[["Mean Sq"]][4]
# Akaike Information Criterion,AIC
(AIC.lin = AIC(mlr.lin)/nfit )
(AIC.quad = AIC(mlr.quad)/nfit)
(AIC.cub = AIC(mlr.cub)/nfit)
# Bayesian Information Criterion,BIC
(BIC.lin = BIC(mlr.lin)/nfit )
(BIC.quad = BIC(mlr.quad)/nfit)
(BIC.cub = BIC(mlr.cub)/nfit)
# Corrected AIC, i.e., AICc using formula
k = 1
(AICc.lin=log(sigsq.lin)+(nfit+k)/(nfit-k-2) )
k = 2
(AICc.quad=log(sigsq.quad)+(nfit+k)/(nfit-k-2))
k = 3
(AICc.cub=log(sigsq.cub)+(nfit+k)/(nfit-k-2))
#test
new <- data.frame(timefit=c(109:120))
pfore.lin <- predict(mlr.lin,new,se.fit = TRUE)
pfore.lin$fit
efore.lin=otest-pfore.lin$fit
(me.lin=mean(efore.lin) )#31.98648
(mpe.lin=100*(mean(efore.lin/otest)) )#1.470221
(mse.lin=sum(efore.lin**2)/ntest)#1599.984
(mae.lin=mean(abs(efore.lin)))#35.17683
(mape.lin=100*(mean(abs((efore.lin)/otest))))#1.620296
timefit=c(109:120)
tsqfit=timefit^2/factorial(2)
matq=matrix(c(timefit,tsqfit),ncol=2,dimnames = list(c(),c("timefit","tsqfit"))) 
matq
newnq <- data.frame(matq)
pfore.quad=predict(mlr.quad,newnq,se.fit = TRUE) 
pfore.quad$fit # point predictions
(efore.quad=otest-pfore.quad$fit)
(me.quad=mean(efore.quad))#206.1312
(mpe.quad=100*(mean(efore.quad/otest)))#9.531554
(mse.quad=sum(efore.quad**2)/ntest)#44171.91
(mae.quad=mean(abs(efore.quad)))#206.1312
(mape.quad=100*(mean(abs((efore.quad)/otest))))# 9.531554
timefit=c(109:120)
tsqfit=tfit^2/factorial(2)
tcubfit=tfit^3/factorial(3) 
matc=matrix(c(timefit,tsqfit,tcubfit),ncol=3,dimnames = list(c(),c("timefit","tsqfit","tcubfit")))
newnc=data.frame(matc)
pfore.cub=predict(mlr.cub,newnc,se.fit = TRUE)
pfore.cub$fit
efore.cub=otest-pfore.cub$fit
(me.cub=mean(efore.cub))#158.2064
(mpe.cub=100*(mean(efore.cub/otest)))#7.308646
(mse.cub=sum(efore.cub**2)/ntest)#25983.9
(mae.cub=mean(abs(efore.cub)))#158.2064
(mape.cub=100*(mean(abs((efore.cub)/otest))))#7.308646

linff=c(mlr.lin$fitted,pfore.lin$fit)
quadff=c(mlr.quad$fitted,pfore.quad$fit) 
cubff=c(mlr.cub$fitted,pfore.cub$fit)
obs=c(ofit,otest)
obslin=cbind(obs,linff) 
obsquad=cbind(obs,quadff) 
obscub=cbind(obs,cubff) 
time=c(1:length(obs))
ts.plot(obs,main="Obs. Series")
abline(v=108)
ts.plot(obslin,main="Linear Trend Model") 
abline(v=108)
ts.plot(obsquad,main="Quadratic Trend Model") 
abline(v=108)
ts.plot(obscub,main="Cubic Trend Model") 
abline(v=108)
#fit the linear model to find 1000
goal <- data.frame(timefit=c(121:300))
pfore.lin <- predict(mlr.lin,goal,se.fit = TRUE)
pfore.lin$fit
#fit the cub model to find 1000
timefit=c(121:300)
tsqfit=timefit^2/factorial(2)
tcubfit=timefit^3/factorial(3) 
matc=matrix(c(timefit,tsqfit,tcubfit),ncol=3,dimnames = list(c(),c("timefit","tsqfit","tcubfit")))
goalc=data.frame(matc)
pfore.cub=predict(mlr.cub,goalc,se.fit = TRUE)
pfore.cub$fit

#ARIMA model
workdiff1 <- diff(work$WORKORDERS[1:108])
                  
ts.plot(workdiff1)
workdiff12 <- diff(workdiff1)
ts.plot(workdiff12)


acf(workdiff12,max(50))
pacf(workdiff12,max(100))
sarima(work$WORKORDERS[1:108],0,2,1)
foredata <- sarima.for(work$WORKORDERS[1:108], 12, 0, 1, 1)
tresidual <- work$WORKORDERS[109:120]-foredata$pred
sum(tresidual)/12#ME 57.50273 55.82046
100*mean(tresidual/work$WORKORDERS[109:120])#MPE 2.643544 2.565986
mean(tresidual^2)#3842.004 3651.492
par(mfrow=c(1,1))
sarima.for(work$WORKORDERS[1:108], 200, 0, 2, 1)

#use the whole dataset to bulid ARIMA model
finaldiff1 <- diff(work$WORKORDERS)
ts.plot(finaldiff1) 
finaldiff2 <- diff(finaldiff1)
ts.plot(finaldiff2)
mean(finaldiff2)
acf(finaldiff2,max(100))
pacf(finaldiff2,max(50))
sarima(work$WORKORDERS,1,2,1)#6.104306 6.08778 6.191969
sarima.for(work$WORKORDERS, 200, 1, 2, 1)

