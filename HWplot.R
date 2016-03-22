library(ggplot2)
library(reshape)


HWplot<-function(ts_object,  n.ahead=4,  CI=.95,  error.ribbon='green', line.size=1){
  
  hw_object<-HoltWinters(ts_object)
  
  forecast<-predict(hw_object,  n.ahead=n.ahead,  prediction.interval=T,  level=CI)
  
  #for_values<-data.frame(time=round(time(forecast),  3),  value_forecast=as.data.frame(forecast)$fit,  dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
  for_values<-data.frame(time=as.Date(as.yearmon(time(forecast))),  value_forecast=as.data.frame(forecast)$fit,  dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
  
  #fitted_values<-data.frame(time=round(time(hw_object$fitted),  3),  value_fitted=as.data.frame(hw_object$fitted)$xhat)
  fitted_values<-data.frame(time=as.Date(as.yearmon(time(hw_object$fitted))),  value_fitted=as.data.frame(hw_object$fitted)$xhat)
  
  actual_values<-data.frame(time=as.Date(as.yearmon(time(hw_object$x))),  Actual=c(hw_object$x))
  
  
  graphset<-merge(actual_values,  fitted_values,  by='time',  all=TRUE)
  graphset<-merge(graphset,  for_values,  all=TRUE,  by='time')
  # replaces all not available values to 0
  graphset[is.na(graphset$dev),  ]$dev<-0
  View(graphset)
  graphset$Fitted<-c(rep(NA,  NROW(graphset)-(NROW(for_values) + NROW(fitted_values))),  fitted_values$value_fitted,  for_values$value_forecast)
  View(graphset)
  
  graphset.melt<-melt(graphset[, c('time', 'Actual', 'Fitted')], id='time')
  p<-ggplot(graphset.melt,  aes(x=time,  y=value)) + 
    geom_ribbon(data=graphset, aes(x=time, y=Fitted, ymin=Fitted-dev,  ymax=Fitted + dev),  alpha=.2,  fill=error.ribbon) + 
    geom_line(aes(colour=variable), size=line.size) + 
    # geom_vline(x=max(actual_values$time),  lty=2) + 
    #xlab('Time') + ylab('Value') + 
    ylab("Oil Prices Values") + xlab("Years") +
    #opts(legend.position='bottom') + 
    scale_colour_hue('')
  return(p)
  
}