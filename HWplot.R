library(ggplot2)

library(reshape)
library(forecast)


funggcast<-function(dn,data_point=12,is.date = NULL, ts.connect = TRUE,
                              predict.geom = 'line',
                              predict.colour = '#0000FF', predict.size = NULL,
                              predict.linetype = NULL, predict.alpha = NULL,
                              predict.fill = NULL, predict.shape = NULL,
                              conf.int = TRUE,
                              conf.int.colour = '#0000FF', conf.int.linetype = 'none',
                              conf.int.fill = '#000000', conf.int.alpha = 0.3,...){ 
	require(zoo) #needed for the 'as.yearmon()' function
    fcast <- forecast.HoltWinters(HoltWinters(dn), h=data_point)
    ds <- ggplot2::fortify(fcast, is.date = is.date, ts.connect = ts.connect)
    # replace whitespace to underscore to make column name handling easie
    colnames(ds) <- sub(' ', '_', colnames(ds))
    colnames(ds)[colnames(ds) == 'Index'] <- "date"
    colnames(ds)[colnames(ds) == 'Data'] <- "observed"
    colnames(ds)[colnames(ds) == 'Fitted'] <- "fitted"
    colnames(ds)[colnames(ds) == 'Point_Forecast'] <- "forecast"
    colnames(ds)[colnames(ds) == 'Lo_95'] <- "lo95"
    colnames(ds)[colnames(ds) == 'Hi_95'] <- "hi95"
 
	# en<-max(time(fcast$mean)) #extract the max date used in the forecast
 
	# #Extract Source and Training Data
	# ds<-as.data.frame(window(dn,end=en))
	# names(ds)<-'observed'
	# ds$date<-as.Date(time(window(dn,end=en)))
 
	# #Extract the Fitted Values (need to figure out how to grab confidence intervals)
	# dfit<-as.data.frame(fcast$fitted)
	# dfit$date<-as.Date(time(fcast$fitted))
	# names(dfit)[1]<-'fitted'
 
	# ds<-merge(ds,dfit,all.x=T) #Merge fitted values with source and training data
 
	# #Exract the Forecast values and confidence intervals
	# dfcastn<-as.data.frame(fcast)
	# dfcastn$date<-as.Date(as.yearmon(row.names(dfcastn)))
	# names(dfcastn)<-c('forecast','lo80','hi80','lo95','hi95','date')
 
	# pd<-merge(ds,dfcastn,all.x=T) #final data.frame for use in ggplot
	# return(pd)
    return(ds)
 
}
HWplot<-function(ts_object,  n.ahead=4,  CI=.95,  error.ribbon='green', line.size=1){
  
#   hw_object<-HoltWinters(ts_object)
  
#   forecast<-predict(hw_object,  n.ahead=n.ahead,  prediction.interval=T,  level=CI)
  
#   #for_values<-data.frame(time=round(time(forecast),  3),  value_forecast=as.data.frame(forecast)$fit,  dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
#   for_values<-data.frame(time=as.Date(as.yearmon(time(forecast))),  value_forecast=as.data.frame(forecast)$fit,  dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
  
#   #fitted_values<-data.frame(time=round(time(hw_object$fitted),  3),  value_fitted=as.data.frame(hw_object$fitted)$xhat)
#   fitted_values<-data.frame(time=as.Date(as.yearmon(time(hw_object$fitted))),  value_fitted=as.data.frame(hw_object$fitted)$xhat)
  
#   actual_values<-data.frame(time=as.Date(as.yearmon(time(hw_object$x))),  Actual=c(hw_object$x))
  
  
#   graphset<-merge(actual_values,  fitted_values,  by='time',  all=TRUE)
#   graphset<-merge(graphset,  for_values,  all=TRUE,  by='time')
#   # replaces all not available values to 0
#   graphset[is.na(graphset$dev),  ]$dev<-0
#   View(graphset)
#   graphset$Fitted<-c(rep(NA,  NROW(graphset)-(NROW(for_values) + NROW(fitted_values))),  fitted_values$value_fitted,  for_values$value_forecast)
#   View(graphset)
  
  graphset <- funggcast(ts_object,n.ahead)
  p <- ggplot(graphset,aes(date,observed)) + 
       geom_line(color="red") + 
       geom_line(aes(y=fitted)) + 
       geom_line(color="blue") + 
       geom_line(aes(y = forecast)) + 
       geom_ribbon(aes(ymin = lo95, ymax = hi95), alpha = .25) +
       ylab("Oil Prices Values") + xlab("Years")
#   p<-ggplot(graphset.melt,  aes(x=time,  y=value)) + 
#     geom_ribbon(data=graphset, aes(x=time, y=Fitted, ymin=Fitted-dev,  ymax=Fitted + dev),  alpha=.2,  fill=error.ribbon) + 
#     geom_line(aes(colour=variable), size=line.size) + 
#     # geom_vline(x=max(actual_values$time),  lty=2) + 
#     #xlab('Time') + ylab('Value') +
#     #opts(legend.position='bottom') + 
#     scale_colour_hue('')
  return(p)
  
}

# get_model <- function(object, is.date = NULL, ts.connect = TRUE,
#                               predict.geom = 'line',
#                               predict.colour = '#0000FF', predict.size = NULL,
#                               predict.linetype = NULL, predict.alpha = NULL,
#                               predict.fill = NULL, predict.shape = NULL,
#                               conf.int = TRUE,
#                               conf.int.colour = '#0000FF', conf.int.linetype = 'none',
#                               conf.int.fill = '#000000', conf.int.alpha = 0.3,
#                               ...) {
#   plot.data <- ggplot2::fortify(object, is.date = is.date, ts.connect = ts.connect)
#   # replace whitespace to underscore to make column name handling easie
#   colnames(plot.data) <- sub(' ', '_', colnames(plot.data))
#   plot.data
# }