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
#k = 2
tsqfit <- timefit^2/factorial(2)
mlr.quad <- lm(ofit~timefit+tsqfit)
summary(mlr.quad)
plot(mlr.quad)
#k = 3











workdiff <- diff(work$WORKORDERS)
ts.plot(workdiff)
workdiff2 <- diff(workdiff)
ts.plot(workdiff2)
par(mfrow = c(2,2))
acf(workdiff2,max(50))
pacf(workdiff2,max(100))
sarima(work$WORKORDERS,0,2,1)
