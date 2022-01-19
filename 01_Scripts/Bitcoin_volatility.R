library(tidyverse)
library(xts)

data = read.csv('00_Data/BTC-USD (1).csv')
head(data)
dim(data)
df = as_tibble(data[1:1514,])
head(df)
tail(df)

dt = seq(from = as.Date('2017-11-09'), to = as.Date('2021-12-31'), by = 'days')
op.p = xts(df[ ,2], dt)
ad.p = xts(df[ ,6], dt)

summary(df)

# Calculating simple return
s.rtn = (ad.p - op.p)/op.p
# Calculating log return
ln.rtn = log(s.rtn + 1)
dt = seq(from = as.Date('2017-11-09'), to = as.Date('2021-12-31'), by = 'days')
l.rtn = xts(ln.rtn, dt)

# Plot the log return series
plot(l.rtn, main = 'BTC daily log return', ylim = c(-0.6,0.6))

# Log return series summary statistics
summary(l.rtn)
var(l.rtn)
sd(l.rtn)

## Model Building-Mean model=====================================
# ACF
# par(mfrow = c(1,2))
acf(l.rtn, lag.max = 10, main = 'ACF of log return') # check for stationary

# PACF
pacf(l.rtn, lag.max = 10, main = 'PACF of log return')

# Check for unit root
# H_0: There is a unit root
library(fUnitRoots)
adfTest(l.rtn)

# Box test H0: There is no serial correlation
Box.test(l.rtn, lag = 1, type = 'Ljung')
Box.test(l.rtn, lag = 3, type = 'Ljung')
Box.test(l.rtn, lag = 5, type = 'Ljung')
Box.test(l.rtn, lag = 6, type = 'Ljung')
Box.test(l.rtn, lag = 7, type = 'Ljung')
# After running a couple of Box.test with different lags,
# the large p-values suggest that there is no serial correlation within the series. 
# No model needed. 

# Mean model
m = arima(l.rtn, order = c(0, 0, 0))
m
tsdiag(m)

m1 = arima(l.rtn, order = c(2, 0, 2))
m1
tsdiag(m1)

m2 = arima(l.rtn, order = c(2, 0, 0))
m2
tsdiag(m2)

m.train = arima(l.rtn[1:1509], order = c(0, 0, 0))
m.train
tsdiag(m.train)
#auto.arima(l.rtn)

length(l.rtn)
time = l.rtn[1480:1514, 0]
return = l.rtn[1480:1514, 1]
plot(return, time, ylim = c(-0.1, 0.1))


p = predict(m.train, n.ahead = 5)
dt2 = seq(from = as.Date('2021-12-27'), to = as.Date('2021-12-31'), by = 'days')
pred = xts(p$pred, dt2)
se = xts(p$se, dt2)
upper = pred + 1*se
lower = pred - 1*se

lines(pred, type= 'l', col ='red') 
lines(upper, lty = 2, col = 'blue')
lines(lower, lty = 2, col = 'blue')

library(forecast)
auto.arima(l.rtn)
## Test for ARCH effect ========================================================

# GARCH(m, s)
# ACF
acf(l.rtn^2, lag.max = 10, main = 'ACF of squared residuals') # max s = 4
# PACF
pacf(l.rtn^2, lag.max = 10, main = 'PACF of squared residuals') # max m = 4

library(FinTS)
ArchTest(l.rtn)
# Arch Test H0: There is no ARCH effect
# The small p-values suggest that there is ARCH effect


## Model building-Conditional variance model =============================================== 

library(rugarch)
# Box test on standardized residuals H0: No correlation in the standardized residuals 
# Box test on standardized residuals squared H0: No more ARCH effects


# RiskMetrics
# Mu.t = 0
# IGARCH(1,1) with alph.0 = 0
spec.1 = ugarchspec(variance.model = list(model = 'iGARCH', garchOrder = c(1,1)),
                    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                    distribution.model = 'norm',
                    fixed.pars = list(omega = 0))
m1.fit = ugarchfit(spec = spec.1, data = l.rtn,
                   solver.control = list(trace = 1))
m1.fit
# Box test on standardized residuals--> Mean model is acceptable 
# Box test on standardized residuals--> Conditional variance model is acceptable
# AIC: -3.6914
# BIC: -3.6891
# plot(m1.fit)
plot(m1.fit, which = 8)
plot(m1.fit, which = 9)

# Standard GARCH(1,1)
spec.2 = ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),
                    mean.model = list(armaOrder = c(0, 0)),
                    distribution.model = 'norm')
m2.fit = ugarchfit(spec = spec.2, data = l.rtn,
                   solver.control = list(trace = 1))
m2.fit
# Box test on standardized residuals--> Mean model is not acceptable 
# Box test on standardized residuals--> Conditional variance model is acceptable
# AIC: -3.8086
# BIC: -3.7994
plot(m2.fit, which = 8)
plot(m2.fit, which = 9)

# IGARCH(1,1)
spec.3 = ugarchspec(variance.model = list(model = 'iGARCH', garchOrder = c(1,1)),
                    mean.model = list(armaOrder = c(0, 0)),
                    distribution.model = 'norm')
m3.fit = ugarchfit(spec = spec.3, data = l.rtn,
                   solver.control = list(trace = 1))
m3.fit

plot(m3.fit, which = 8)
plot(m3.fit, which = 9)

# Box test on standardized residuals--> Mean model is not acceptable 
# Box test on standardized residuals--> Conditional variance model is acceptable
# AIC: -3.8060
# BIC: -3.7991

# GJR-GARCH(1,1)
spec.4 = ugarchspec(variance.model = list(model = 'gjrGARCH', garchOrder = c(1,1)),
                    mean.model = list(armaOrder = c(0, 0)),
                    distribution.model = 'norm')
m4.fit = ugarchfit(spec = spec.4, data = l.rtn,
                   solver.control = list(trace = 1))
m4.fit

plot(m4.fit, which = 8)
plot(m4.fit, which = 9)

h = length(l.rtn)
a.h = residuals(m4.fit)[h]
a.h # a.h < 0, N_h = 1 

# Box test on standardized residuals--> Mean model is not acceptable 
# Box test on standardized residuals--> Conditional variance model is acceptable
# AIC: -3.8104
# BIC: -3.7989



## Model forecasting=============================================================
position = 1000000

#1. RiskMetrics
forc.1 = ugarchforecast(m1.fit, l.rtn, n.ahead = 1)
forc.1

#par(mfrow=c(2,2))

#par(col.main = 'white')
#plot(forc.1, which = 3)
#par(col.main = 'black')
#title(main = 'RiskMetrics Volatility Forecast', cex.main = 0.8)

# RiskMetrics One-step-ahead VaR
var.forc.1 = qnorm(0.99)*forc.1@forecast$sigmaFor
var.forc.1

dollar.var1 = var.forc.1*position
dollar.var1
#2. GARCH(1,1)
forc.2 = ugarchforecast(m2.fit, l.rtn, n.ahead = 1)
forc.2

#par(col.main = 'white')
#plot(forc.2, which = 3)
#par(col.main = 'black')
#title(main = 'GARCH(1,1) Volatility Forecast', cex.main = 0.8)

# GARCH(1,1) One-step-ahead VaR
var.forc.2 = qnorm(0.99)*forc.2@forecast$sigmaFor
var.forc.2

dollar.var2 = var.forc.2*position
dollar.var2

#3. IGARCH(1,1)
forc.3 = ugarchforecast(m3.fit, l.rtn, n.ahead = 1)
forc.3

#par(col.main = 'white')
#plot(forc.3, which = 3)
#par(col.main = 'black')
#title(main = 'iGARCH(1,1) Volatility Forecast', cex.main = 0.8)

# IGARCH(1,1) One-step-ahead VaR
var.forc.3 = qnorm(0.99)*forc.3@forecast$sigmaFor
var.forc.3

dollar.var3 = var.forc.3*position
dollar.var3

#4. GJRGARCH(1,1)
forc.4 = ugarchforecast(m4.fit, l.rtn, n.ahead = 1)
forc.4

#par(col.main = 'white')
#plot(forc.4, which = 3)
#par(col.main = 'black')
#title(main = 'gjrGARCH(1,1) Volatility Forecast', cex.main = 0.8)

# GJRGARCH(1,1) One-step-ahead VaR
var.forc.4 = qnorm(0.99)*forc.4@forecast$sigmaFor
var.forc.4

dollar.var4 = var.forc.4*position
dollar.var4


## Value at Risk (VaR) backtesting =============================================================


# RiskMetrics
length(l.rtn)
roll.1 = ugarchroll(spec = spec.1, 
                    data = l.rtn, 
                    n.start = 1264, # used the most recent 250 days data
                    refit.every = 1, # reestimate every 1 days
                    refit.window = 'moving', # used all previous data from the first calculation
                    calculate.VaR = T, # calculate forecast VaR
                    solver = 'hybrid', 
                    VaR.alpha = 0.01) # VaR tail level
report(roll.1, type = 'VaR', VaR.alpha = 0.01, conf.level = 0.95)
report(roll.1, type = 'fpm') # returns Mean squared error
# Mean absolute error
# Directional accuracy
par(mfrow = c(1,1))
show(roll.1)
# plot(roll.1)

# Acess to the VaR forecast up to 2021/08/31 forecast VS Realized
tail(roll.1@forecast$VaR)

# VaR exceedance plot 
df.roll.1 = as.data.frame(roll.1)
head(df.roll.1)
dim(df.roll.1)

var.1 = quantile(roll.1, probs = 0.01)
actual.1 = xts(df.roll.1$Realized, time(var.1))

VaRplot(alpha = 0.01, actual = actual.1, VaR = var.1)
title(main = 'RiskMetrics VaR exceedance plot', 
      xlab = 'Time', ylab = 'Return', cex.main = 0.8)

# sGARCH(1,1) with normal distribution
roll.2 = ugarchroll(spec = spec.2, 
                    data =l.rtn, 
                    n.start = 1264,
                    refit.every = 1,
                    refit.window = 'moving',
                    calculate.VaR = T,
                    solver = 'hybrid',
                    VaR.alpha = 0.01)
report(roll.2, type = 'VaR', VaR.alpha = 0.01, conf.level = 0.95)
report(roll.2, type = 'fpm')

# VaR exceedance plot 
df.roll.2 = as.data.frame(roll.2)

var.2 = quantile(roll.2, probs = 0.01)
actual.2 = xts(df.roll.2$Realized, time(var.2))

VaRplot(alpha = 0.01, actual = actual.2, VaR = var.2)
title(main = 'GARCH(1,1) VaR exceedance plot', 
      xlab = 'Time', ylab = 'Return', cex.main = 0.8)


# IGARCH(1,1) with normal distribution
roll.3 = ugarchroll(spec = spec.3, 
                    data = l.rtn, 
                    n.start = 1264,
                    refit.every = 1,
                    refit.window = 'moving',
                    calculate.VaR = T,
                    solver = 'hybrid',
                    VaR.alpha = 0.01)
report(roll.3, type = 'VaR', VaR.alpha = 0.01, conf.level = 0.95)
report(roll.3, type = 'fpm')

# VaR exceedance plot 
df.roll.3 = as.data.frame(roll.3)

var.3 = quantile(roll.3, probs = 0.01)
actual.3 = xts(df.roll.3$Realized, time(var.3))

VaRplot(alpha = 0.01, actual = actual.3, VaR = var.3)
title(main = 'IGARCH(1,1) VaR exceedance plot', 
      xlab = 'Time', ylab = 'Return', cex.main = 0.8)


# GJR-GARCH(1,1) with normal distribution
roll.4 = ugarchroll(spec = spec.4, 
                    data = l.rtn, 
                    n.start = 1264, 
                    refit.every = 1, 
                    refit.window = 'moving', 
                    calculate.VaR = T,
                    solver = 'hybrid',
                    VaR.alpha = 0.01)
report(roll.4, type = 'VaR', VaR.alpha = 0.01, conf.level = 0.95)
report(roll.4, type = 'fpm')
show(roll.4)

# VaR exceedance plot 
df.roll.4 = as.data.frame(roll.4)

var.4 = quantile(roll.4, probs = 0.01)
actual.4 = xts(df.roll.4$Realized, time(var.4))

VaRplot(alpha = 0.01, actual = actual.4, VaR = var.4)
title(main = 'GJR-GARCH(1,1) VaR exceedance plot', 
      xlab = 'Time', ylab = 'Return', cex.main = 0.8)

## Model Validation======================================

# Backtest from 2020 to 2021 

# RiskMetrics
roll.5 = ugarchroll(spec = spec.1, 
                    data = l.rtn, 
                    n.start = 784, # from 2020-01-01
                    refit.every = 1, # reestimate every 1 days
                    refit.window = 'moving', # used all previous data from the first calculation
                    calculate.VaR = T, # calculate forecast VaR
                    solver = 'hybrid', 
                    VaR.alpha = 0.01) # VaR tail level
report(roll.5, type = 'VaR', VaR.alpha = 0.01, conf.level = 0.95)
df.roll.5 = as.data.frame(roll.5)

var.5 = quantile(roll.5, probs = 0.01)
actual.5 = xts(df.roll.5$Realized, time(var.5))

par(mfrow = c(1,1))
VaRplot(alpha = 0.01, actual = actual.5, VaR = var.5)
title(main = 'RiskMetrics VaR exceedance plot', 
      xlab = 'Time', ylab = 'Return', cex.main = 0.8)

# plot(roll.5, which =4)  #generates VaRplot
# GARCH(1,1)
roll.6 = ugarchroll(spec = spec.2, 
                    data = l.rtn, 
                    n.start = 784, 
                    refit.every = 1, # reestimate everyday
                    refit.window = 'moving', # used all previous data from the first calculation
                    calculate.VaR = T, # calculate forecast VaR
                    solver = 'hybrid', 
                    VaR.alpha = 0.01) # VaR tail level
report(roll.6, type = 'VaR', VaR.alpha = 0.01, conf.level = 0.95)
df.roll.6 = as.data.frame(roll.6)

var.6 = quantile(roll.6, probs = 0.01)
actual.6 = xts(df.roll.6$Realized, time(var.6))

VaRplot(alpha = 0.01, actual = actual.6, VaR = var.6)
title(main = 'GARCH(1,1) VaR exceedance plot', 
      xlab = 'Time', ylab = 'Return', cex.main = 0.8)

# IGARCH(1,1)
roll.7 = ugarchroll(spec = spec.3, 
                    data = l.rtn, 
                    n.start = 784,  
                    refit.every = 1, # reestimate everyday
                    refit.window = 'moving', # used all previous data from the first calculation
                    calculate.VaR = T, # calculate forecast VaR
                    solver = 'hybrid', 
                    VaR.alpha = 0.01) # VaR tail level
report(roll.7, type = 'VaR', VaR.alpha = 0.01, conf.level = 0.95)
df.roll.7 = as.data.frame(roll.7)

var.7 = quantile(roll.7, probs = 0.01)
actual.7 = xts(df.roll.7$Realized, time(var.7))

VaRplot(alpha = 0.01, actual = actual.7, VaR = var.7)
title(main = 'IGARCH(1,1) VaR exceedance plot', 
      xlab = 'Time', ylab = 'Return', cex.main = 0.8)

# GJR-GARCH(1,1)
roll.8 = ugarchroll(spec = spec.4, 
                    data = l.rtn, 
                    n.start = 784, 
                    refit.every = 1, # reestimate everyday
                    refit.window = 'moving', # used all previous data from the first calculation
                    calculate.VaR = T, # calculate forecast VaR
                    solver = 'hybrid', 
                    VaR.alpha = 0.01) # VaR tail level
report(roll.8, type = 'VaR', VaR.alpha = 0.01, conf.level = 0.95)
df.roll.8 = as.data.frame(roll.8)

var.8 = quantile(roll.8, probs = 0.01)
actual.8 = xts(df.roll.8$Realized, time(var.8))

VaRplot(alpha = 0.01, actual = actual.8, VaR = var.8)
title(main = 'GJR-GARCH(1,1) VaR exceedance plot', 
      xlab = 'Time', ylab = 'Return', cex.main = 0.8)


# Check Distribution
# Comparing the normal distribution to the actual distribution
library(PerformanceAnalytics)
chart.Histogram(l.rtn, main = 'Empirical density VS Normal density', cex.main = 0.8,
                methods = c('add.normal', 'add.density'),
                colorset = c('grey', 'red', 'blue'))
title(cex.main = 0.8)
legend('topleft', legend = c('Empirical distribution', 'Normal distribution'),
       cex = 0.5, lwd = 0.5, box.lwd = 0.5, inset =0.01, col = c('red', 'blue'))

# Test for normality
chart.QQPlot(l.rtn, main = 'Normal QQ-Plot', cex.main = 0.8, cex = 0.5)
chart.QQPlot(l.rtn, distribution = 't', df = 3, cex = 0.5)
shapiro.test(ln.rtn)
jarqueberaTest(ln.rtn)


# Check skewness
skewness(l.rtn) # skewness < 0 suggests left skew

# Check kurtosis
kurtosis(l.rtn) # Kurtosis > 3 suggests heavier tail than normal distribution

# Try to improve the models by modifying the distribution model=====================================================================

# sGARCH(1,1) with skewed t-distribution
spec.2t = ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(0, 0)),
                     distribution.model = 'sstd')
m2t.fit = ugarchfit(spec = spec.2t, data = l.rtn,
                    solver.control = list(trace = 1))
m2t.fit
plot(m2t.fit, which = 8)
plot(m2t.fit, which = 9)

# plot(m2t.fit)
# Box test on standardized residuals--> Mean model not acceptable 
# Box test on standardized residuals--> Conditional variance model is accetable
# AIC: -4.1115
# BIC: -4.0977

# Backtesting
roll.2t = ugarchroll(spec = spec.2t,
                     data = l.rtn,
                     n.start = 784,
                     refit.every = 30,
                     refit.window = 'recursive',
                     calculate.VaR = T,
                     solver = 'hybrid',
                     VaR.alpha = 0.01)
report(roll.2t, type = 'VaR', VaR.alpha = 0.01, conf.level = 0.95)

# VaR exceedance plot 
df.roll.2t = as.data.frame(roll.2t)

var.2t = quantile(roll.2t, probs = 0.01)
actual.2t = xts(df.roll.2t$Realized, time(var.2t))

VaRplot(alpha = 0.01, actual = actual.2t, VaR = var.2t, 
        ylab = 'Daily log return', xlab = 'Time')
title(main = 'GARCH(1,1) VaR exceedance plot with sstd', 
      xlab = 'Time', ylab = 'Return', cex.main = 0.8)

# library(GAS)
# length(l.rtn)
# in.sample = l.rtn[1:499, ]
# out.sample = l.rtn[500:2541, ]

# forc.2t = ugarchforecast(m2t.fit, l.rtn, n.ahead = 1)
# forc.2t
# var.2t = qt(0.95, ???)
# BacktestVaR(out.sample, )

# IGARCH(1,1) with skewed t-distribution
spec.3t = ugarchspec(variance.model = list(model = 'iGARCH', garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(0, 0)),
                     distribution.model = 'sstd')
m3t.fit = ugarchfit(spec = spec.3t, data = l.rtn,
                    solver.control = list(trace = 1))
m3t.fit
# Box test on standardized residuals--> Mean model not acceptable 
# Box test on standardized residuals--> Conditional variance model is accetable
# AIC: -4.1126
# BIC: -4.1011
plot(m3t.fit, which = 8)
plot(m3t.fit, which = 9)


# Backtesting 
roll.3t = ugarchroll(spec = spec.3t,
                     data = l.rtn,
                     n.start = 784,
                     refit.every = 30,
                     refit.window = 'recursive',
                     calculate.VaR = T,
                     solver = 'hybrid',
                     VaR.alpha = 0.01)
report(roll.3t, type = 'VaR', VaR.alpha = 0.01, conf.level = 0.95)

# VaR exceedance plot 
df.roll.3t = as.data.frame(roll.3t)

var.3t = quantile(roll.3t, probs = 0.01)
actual.3t = xts(df.roll.3t$Realized, time(var.3t))

VaRplot(alpha = 0.01, actual = actual.3t, VaR = var.3t, 
        ylab = 'Daily log return', xlab = 'Time')
title(main = 'iGARCH(1,1) VaR exceedance plot with sstd', 
      xlab = 'Time', ylab = 'Return', cex.main = 0.8)


# GJR-GARCH(1,1) with skewed t-distribution
spec.4t = ugarchspec(variance.model = list(model = 'gjrGARCH', garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(0, 0)),
                     distribution.model = 'sstd')
m4t.fit = ugarchfit(spec = spec.4t, data = l.rtn,
                    solver.control = list(trace = 1))
m4t.fit
# Box test on standardized residuals--> Mean model not acceptable 
# Box test on standardized residuals--> Conditional variance model is accetable
# AIC: -4.1143
# BIC: -4.0982

plot(m4t.fit, which = 8)
plot(m4t.fit, which = 9)

# Backtesting
roll.4t = ugarchroll(spec = spec.4t,
                     data = l.rtn,
                     n.start = 784,
                     refit.every = 30,
                     refit.window = 'recursive',
                     calculate.VaR = T,
                     solver = 'hybrid',
                     VaR.alpha = 0.01)
report(roll.4t, type = 'VaR', VaR.alpha = 0.01, conf.level = 0.95)

# VaR exceedance plot 
df.roll.4t = as.data.frame(roll.4t)

var.4t = quantile(roll.4t, probs = 0.01)
actual.4t = xts(df.roll.4t$Realized, time(var.4t))

VaRplot(alpha = 0.01, actual = actual.4t, VaR = var.4t, 
        ylab = 'Daily log return', xlab = 'Time')
title(main = 'GJR-GARCH(1,1) VaR exceedance plot with sstd', 
      xlab = 'Time', ylab = 'Return', cex.main = 0.8)


# On VaR plot, there is a huge lose with -0.465
# Bitcoin plunge on 2020-03-12
which.min(l.rtn)
l.rtn[855, ]


likelihood(m4.fit)
likelihood(m4t.fit)
