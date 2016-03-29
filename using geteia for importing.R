require(EIAdata)
key<-("95365B2462BFD45A96D4EB0DBC60E59A")
#getCatEIA(key = key)
#getCatEIA(key=key,714755)
#getEIA(714757,key)
amount<-getEIA("PET.RBRTE.M",key)
head(amount)
amount

class(amount)
names(amount)<-c("price")
oilp<-ts(amount$price,start=c(1987,5),freq=12) #converting to ts
head(oilp)
class(oilp)
oilp

time(oilp)
oilp.july<-window(oilp,start=c(1990,1),freq=T)
?window
oilp.july
plot(oilp,ylab="Price",main="Oil Price")#plot oil price
plot(oilp.july)
abline(reg=lm(oilp~time(oilp)))
decompose(oilp)
plot(decompose(oilp)) # to plot the graph of randomness

acf(oilp)



require(forecast)

oiltrain<-window(oilp,end= 2015.99)
oiltest<-window(oilp,start=2016)
plot(forecast(oilp))# something like auto foreccast 
f1<-meanf(oiltrain,h=10) # using mean forecast
f2<-naive(oiltrain,h=10) # using naive forecast method

plot(f1) 
accuracy(f1,oiltest) #to test the forecast 

#to determine which arima moel worked
tsdisplay(oiltrain)
tsdisplay(diff(oiltrain))
fit1<-Arima(oiltrain,order = c(3,1,0))# using arima with a special model (i dont know anything about this)
fit2<-auto.arima(oiltrain) # using auto arima
fcast1<-forecast(fit1,h=10) #h represents the number of forecast
fcast2<-forecast(fit2,h=10)

plot(fcast1)
plot(fcast2)

accuracy(fcast2,oiltest)
?auto.arima
# resuming back to understanding r
cycle(oilp)
time(oilp)
m1=lm(oilp~time(oilp)+factor(cycle(oilp)))
m1
m2=lm(oilp~0+time(oilp)+factor(cycle(oilp)))#taking the time out and the season out
m2
summary(m2)
m3<-residuals(m2)
  pacf(m3)
  
  
  
  
  
# want to enter visualisation now
  