k <-100
w <- 1:100
x <- w + k * rnorm(100)
y <- w + k * rnorm(100)
png("ccf.png")
plot(ccf(x,y), main="The ccf(x,y)")
dev.off()
browseURL("ccf.png")

Time <- 1:370
k <- 1
x <- sin(2 * pi * Time / 37)+k*rnorm(370)
y <- sin(2 * pi * (Time + 4) / 37)+k*rnorm(370)
png("ccf_2.png")
plot(ccf(x,y), main="The ccf(x,y)")
dev.off()
browseURL("ccf_2.png")


t <- 1:370
u <- sqrt(3)/3.14
b <- 1
x <- 1/(1+exp(-(t-u) / b))
y <- pnorm(t)
png("cdf_log_vs_normal.png")
plot(x, y, main="Normal Vs logistic", xlab='logistic', ylab="Normal")
dev.off()
browseURL("cdf_log_vs_normal.png")



www <-file.path(getwd(), 'wine.dat')
wine.dat <- read.table(www, header = T) 
attach (wine.dat)
sweetw.ts <- ts(sweetw, start = c(1980,1), freq = 12)
png("sweetw.png")
plot(sweetw.ts, xlab= "Time (months)", ylab = "sales (1000 litres)")
dev.off()
browseURL("sweetw.png")
sweetw.hw <- HoltWinters (sweetw.ts, seasonal = "mult")
print(cbind("The optimum SSE is: ", sweetw.hw$SSE))
sweetw.hw <- HoltWinters (sweetw.ts, alpha = 0.2, beta = 0.2, gamma = 0.2, seasonal = "mult")
print(cbind("The SSE for set alpha, gamma, beta=0.2 is: ", sweetw.hw$SSE))


sweetwlog.ts <- ts(log(sweetw), start = c(1980,1), freq = 12)
print(sweetwlog.ts)
png("sweetwlog.png")
plot(sweetwlog.ts, xlab= "Time (months)", ylab = "log of sales (1000 litres)")
dev.off()
browseURL("sweetwlog.png")
sweetwlog.hw <- HoltWinters (sweetwlog.ts, seasonal = "additive")
print(cbind("The optimum SSE is: ", sweetwlog.hw$SSE))
sweetwlog.hw <- HoltWinters (sweetwlog.ts, alpha = 0.2, beta = 0.2, gamma = 0.2, seasonal = "additive")
print(cbind("The SSE for set alpha, gamma, beta=0.2 is: ", sweetwlog.hw$SSE))

# print(sweetw.ts[2:length(sweetw.ts)])
lagged_sweetts <- lag(sweetw.ts, k=-1)[1:length(sweetw.ts)-1]
print(cbind("The SSE for persistent model: ", sum((lagged_sweetts-sweetw.ts[2:length(sweetw.ts)])^2)))

result = c()
for (y in c(1982:1995))
{for (m in c(1:12)) 
  {
    sweetw.ts <- ts(log(sweetw), start = c(1980,1), end=c(y,m),  freq = 12)
    sweetw.hw <- HoltWinters (sweetw.ts, seasonal = "add")
    result <- c(result, sweetw.hw$SSE)
  }
}
png("SSE.png")
plot(result,xlab="step", ylab="SSE", main="SSE from 1982-1995")
dev.off()
browseURL("SSE.png")


www <-file.path(getwd(), 'global.dat')
Global <- scan(www)
Global.ts <- ts(Global, st = c(1856, 1), end = c(2005, 12),
      fr = 12)

png("Global.png")
plot(Global.ts, col="blue", lwd=1)
dev.off()
browseURL("Global.png")

Global.annual.ts <- aggregate(Global.ts)/12

png("global_agg_annual.png")
plot(Global.annual.ts, main=" annual average Global temp", col="orange", lwd=3)
dev.off()
browseURL("global_agg_annual.png")

png("BoxplotGolobalTemp.png")
boxplot(Global.ts ~ cycle(Global.ts), names=c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul","Aug", "Sep", "Oct", "Nov", "Dec" ), main="Monthly Global Temp for 1856-2005 ",ylab = "Temp",col = "orange")
dev.off()
browseURL("BoxplotGolobalTemp.png")


png("Globaldec.png")
Global.decomp <- decompose(Global.ts, type = c("additive"), filter = NULL)
plot(Global.decomp, xlab = "Time", lwd=3)
dev.off()
browseURL("Globaldec.png")


png("Globaldec_trend_seas.png")
plot(Global.decomp$trend+Global.decomp$seasonal, xlab = "Time", lwd=3)
dev.off()
browseURL("Globaldec_trend_seas.png")

png("Globaldec_rand.png")
len <- length(Global.decomp$random)
acf(Global.decomp$random[-c(1:6,len-6+1:len)], main="ACF of residual")
dev.off()
browseURL("Globaldec_rand.png")

global.hw <- HoltWinters (Global.ts, seasonal = "additive")
global.hw ; global.hw$coef ; global.hw$SSE

png("GlobalHW.png")
plot(global.hw$fitted)
dev.off()
browseURL("GlobalHW.png")

png("GlobalHW_fitted.png")
plot(global.hw, xlim=c(1990,2000))
dev.off()
browseURL("GlobalHW_fitted.png")

global.predict <- predict(global.hw, n.ahead = 5 * 12)
png("GlobalPredict.png")
ts.plot(Global.ts, global.predict,  xlim=c(1990,2010), lty = 1:2, lwd=3, main="Predicted Temp values for 2005-2010")
dev.off()
browseURL("GlobalPredict.png")

www <-file.path(getwd(), 'motororg.dat')
Motor.dat <- read.table(www, header = T)
attach(Motor.dat)
Comp.ts <- ts(complaints, start = c(1996, 1), freq = 12)

png("MotorComp.png")
plot(Comp.ts, xlab = "Time / months", ylab = "Complaints")
dev.off()
browseURL("MotorComp.png")

target <- 18

png("MotorComp_cumsum.png")
plot(cumsum(Comp.ts - target), xlab = "Time / months", ylab = "cumsum of Complaints", main="Cumsum of complaints with target 18")
lines(cumsum(Comp.ts - target))
dev.off()
browseURL("MotorComp_cumsum.png")

www <-file.path(getwd(), 'motororg.dat')
Motor.dat <- read.table(www, header = T)
attach(Motor.dat)
Comp.ts <- ts(complaints, start = c(1996, 1), freq = 12)

png("MotorComp.png")
plot(Comp.ts, xlab = "Time / months", ylab = "Complaints")
dev.off()
browseURL("MotorComp.png")

global.hw <- HoltWinters (Comp.ts, seasonal = "additive", alpha=0.01, beta=0, gamma=0)
png("complaintmotor.png")
plot(global.hw$fitted, main="Exponential smoothing with alpha=0.01")
dev.off()
browseURL("complaintmotor.png")
err <- Comp.ts - global.hw$fitted[,1]
print(Comp.ts)
print(global.hw$fitted[,1])
print(err)

global.hw <- HoltWinters (Comp.ts, seasonal = "additive", alpha=0.99, beta=0, gamma=0)
png("complaintmotor99.png")
plot(global.hw, main="Exponential smoothing with alpha=0.99")
dev.off()
browseURL("complaintmotor99.png")
err <- Comp.ts - global.hw$fitted[,1]
print(Comp.ts)
print(global.hw$fitted[,1])
print(err)