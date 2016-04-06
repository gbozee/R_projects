library(ggplot2)

library(reshape)
library(forecast)
library(scales)

forecast_function<-function(dn,data_point=12,f_type="holt_winters"){
    require(zoo) #needed for the 'as.yearmon()' function
    if(f_type == "holt_winters"){
        fcast <- forecast.HoltWinters(HoltWinters(dn), h=data_point)
        cat("holt_winters")        
    }else{
        fcast <- forecast(auto.arima(dn), h=data_point)
        cat("arima")    
    }
    return(fcast)
}
funggcast<-function(dn,data_point=12,f_type="holt_winters",is.date = NULL, ts.connect = TRUE,
                              predict.geom = 'line',
                              predict.colour = '#0000FF', predict.size = NULL,
                              predict.linetype = NULL, predict.alpha = NULL,
                              predict.fill = NULL, predict.shape = NULL,
                              conf.int = TRUE,
                              conf.int.colour = '#0000FF', conf.int.linetype = 'none',
                              conf.int.fill = '#000000', conf.int.alpha = 0.3,...){ 
	require(zoo) #needed for the 'as.yearmon()' function
    fcast <- forecast_function(dn,data_point,f_type)
    ds <- ggplot2::fortify(fcast, is.date = is.date, ts.connect = ts.connect)
    # replace whitespace to underscore to make column name handling easie
    colnames(ds) <- sub(' ', '_', colnames(ds))
    colnames(ds)[colnames(ds) == 'Index'] <- "date"
    colnames(ds)[colnames(ds) == 'Data'] <- "observed"
    colnames(ds)[colnames(ds) == 'Fitted'] <- "fitted"
    colnames(ds)[colnames(ds) == 'Point_Forecast'] <- "forecast"
    colnames(ds)[colnames(ds) == 'Lo_95'] <- "lo95"
    colnames(ds)[colnames(ds) == 'Hi_95'] <- "hi95"
 
	return(ds)
 
}
s_var <- function(date_type){    
  if (date_type=="Monthly"){
      scale_var <- scale_x_date(breaks = "1 year", minor_breaks = "1 month", labels=date_format("%B"))
  }else if(date_type == "Weekly"){
      scale_var <- scale_x_date(breaks = "1 week",minor_breaks = "1 day", labels = date_format("%W"))
  }else{
      scale_var <- scale_x_date(breaks = "1 year", minor_breaks = "1 month", labels=date_format("%B"))
    #   scale_var <- scale_x_date(limits = c(Sys.Date() - 7, NA))
  }
  return(scale_var)
}
ArimaPlot<-function(ts_object,  n.ahead=4,  CI=.95,  error.ribbon='green', line.size=1,date_type="Monthly"){
    f <- forecast_function(ts_object,n.ahead,"arima")
    graphset <- funggcast(ts_object,data_point=n.ahead,f_type="arima")
    scale_var <- s_var(date_type)
    # graphset$date <- factor(graphset$date, levels=graphset$date[!duplicated(graphset$date)])
  p <- old_plot(graphset)
    # p <- plot_function(f)
    return(p)
}
HWplot<-function(ts_object,  n.ahead=4,  CI=.95,  error.ribbon='green', line.size=1,date_type="Monthly"){
    f <- forecast_function(ts_object,n.ahead)
  graphset <- funggcast(ts_object,n.ahead)
    scale_var <- s_var(date_type)
  p <- old_plot(graphset)
#   p <- plot_function(graphset)
  return(p)
  
}

old_plot <- function(graphset,data.color = 'blue', fit.color = 'red', forec.color = 'black',
                           lower.fill = 'darkgrey', upper.fill = 'grey'){
   p <- ggplot(graphset,aes(date,observed)) + 
       geom_line(color="red") + 
       geom_line(aes(y=fitted,colour="fitted")) + 
    #    geom_line(color="blue") + 
       geom_line(aes(y = forecast,colour="forecast")) + 
    #    geom_line(color="green") + 
       scale_color_manual('Series', values=c('observed' = "red", 'fitted' = "blue", 'forecast' = "green")) + 
       geom_ribbon(aes(ymin = lo95, ymax = hi95), alpha = .25) +
       ylab("Oil Prices Values") + xlab("Years")
       
   return(p) 
   
}
plot_function <- function(forec.obj, data.color = 'blue', fit.color = 'red', forec.color = 'black',
                           lower.fill = 'darkgrey', upper.fill = 'grey', format.date = F)
{
    serie.orig = forec.obj$x
    serie.fit = forec.obj$fitted
    pi.strings = paste(forec.obj$level, '%', sep = '')
    
     if(format.date)
        dates = as.Date(time(serie.orig))
    else
        dates = time(serie.orig)
    View(dates)
    View(serie.fit)
    serie.df = data.frame(date = dates, serie.orig = serie.orig, serie.fit = serie.fit)
    
    forec.M = cbind(forec.obj$mean, forec.obj$lower[, 1:2], forec.obj$upper[, 1:2])
    forec.df = as.data.frame(forec.M)
    colnames(forec.df) = c('forec.val', 'l0', 'l1', 'u0', 'u1')
    
    if(format.date)
        forec.df$date = as.Date(time(forec.obj$mean))
    else
        forec.df$date = time(forec.obj$mean)
    
    p = ggplot() + 
        geom_line(aes(date, serie.orig, colour = 'data'), data = serie.df) + 
        geom_line(aes(date, serie.fit, colour = 'fit'), data = serie.df) + 
        scale_y_continuous() +
        geom_ribbon(aes(x = date, ymin = l0, ymax = u0, fill = 'lower'), data = forec.df, alpha = I(0.4)) + 
        geom_ribbon(aes(x = date, ymin = l1, ymax = u1, fill = 'upper'), data = forec.df, alpha = I(0.3)) + 
        geom_line(aes(date, forec.val, colour = 'forecast'), data = forec.df) + 
        scale_color_manual('Series', values=c('data' = data.color, 'fit' = fit.color, 'forecast' = forec.color)) + 
        scale_fill_manual('P.I.', values=c('lower' = lower.fill, 'upper' = upper.fill)) +
        ylab("Oil Prices Values")
    
    if (format.date)
        p = p + scale_x_date()
    
    p
}
funggcast2<-function(dn,fcast){ 
	require(zoo) #needed for the 'as.yearmon()' function
 
	en<-max(time(fcast$mean)) #extract the max date used in the forecast
 
	#Extract Source and Training Data
	ds<-as.data.frame(window(dn,end=en))
	names(ds)<-'observed'
	ds$date<-as.Date(time(window(dn,end=en)))
 
	#Extract the Fitted Values (need to figure out how to grab confidence intervals)
	dfit<-as.data.frame(fcast$fitted)
	dfit$date<-as.Date(time(fcast$fitted))
	names(dfit)[1]<-'fitted'
 
	ds<-merge(ds,dfit,all.x=T) #Merge fitted values with source and training data
 
	#Exract the Forecast values and confidence intervals
	dfcastn<-as.data.frame(fcast)
	dfcastn$date<-as.Date(as.yearmon(row.names(dfcastn)))
	names(dfcastn)<-c('forecast','lo80','hi80','lo95','hi95','date')
 
	pd<-merge(ds,dfcastn,all.x=T) #final data.frame for use in ggplot
	return(pd)
 
}