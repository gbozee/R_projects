library(ggplot2)
library(reshape)
library(forecast)
library(scales)

forecast_function<-function(dn,data_point=12,f_type="holt_winters",arima_order=NULL,
    alpha=NULL,beta=NULL){
    require(zoo) #needed for the 'as.yearmon()' function
    if(f_type == "holt_winters"){
        fcast <- hw(dn,h=as.integer(data_point))                
    }
    if(f_type=="ses"){
        fcast <- ses(dn,h=as.integer(data_point),alpha=alpha,initial="simple")
    }
    if(f_type=="holt_linear"){
        fcast <- holt(dn,h=as.integer(data_point),alpha=alpha,
            beta=beta, initial="simple")
    }
    if(f_type == "c_arima"){
        fcast <- forecast(Arima(dn,order=arima_order), h=data_point)
    }
    if(f_type == "arima"){         
        fcast <- forecast(auto.arima(dn), h=data_point)   
    }
    cat("arima")    
    return(fcast)
}
funggcast<-function(dn,data_point=12,f_type="holt_winters",is.date = NULL, ts.connect = TRUE,
                              arima_order=NULL,alpha=NULL,beta=NULL){ 
	require(zoo) #needed for the 'as.yearmon()' function
    fcast <- forecast_function(dn,data_point,f_type,arima_order=arima_order,alpha=alpha,beta=beta)
    ds <- ggplot2::fortify(fcast, is.date = is.date, ts.connect = ts.connect)
    # replace whitespace to underscore to make column name handling easie
    colnames(ds) <- sub(' ', '_', colnames(ds))
    colnames(ds)[colnames(ds) == 'Index'] <- "date"
    colnames(ds)[colnames(ds) == 'Data'] <- "observed"
    colnames(ds)[colnames(ds) == 'Fitted'] <- "fitted"
    colnames(ds)[colnames(ds) == 'Point_Forecast'] <- "forecast"
    colnames(ds)[colnames(ds) == 'Lo_95'] <- "lo95"
    colnames(ds)[colnames(ds) == 'Hi_95'] <- "hi95"
 
	return(ds[,c(1,2,3,7,5,4,6,8)])
 
}

ArimaPlot<-function(ts_object,  n.ahead=4,date_type="Monthly",arima_order=NULL){
    graphset <- funggcast(ts_object,data_point=n.ahead,f_type="arima",arima_order=arima_order)
    p <- old_plot(graphset)
    return(p)
}
HWplot<-function(ts_object,  n.ahead=4,  date_type="Monthly",f_type="holt_winters",
    alpha=NULL,beta=NULL){    
  graphset <- funggcast(ts_object,n.ahead,f_type=f_type,alpha=alpha,beta=beta)
  
  p <- old_plot(graphset)
  return(p)
}

old_plot <- function(graphset,data.color = 'blue', fit.color = 'red', forec.color = 'black',
                           lower.fill = 'darkgrey', upper.fill = 'grey',date_type="Monthly"){
   #n_graph <- graphset  
  o <- FALSE
   if(class(graphset$date) == "numeric"){
       graphset$date <- as.Date(sapply(graphset$date,convert_from_decimal_to_date))
       o <- TRUE
   }
   p <- ggplot(graphset,aes(date,observed)) + 
       geom_line(aes(y=observed,colour="observed")) + 
       geom_line(aes(y=fitted,colour="fitted")) + 
       geom_line(aes(y = forecast,colour="forecast")) + 
       scale_color_manual('Series', values=c('observed' = "red", 'fitted' = "blue", 'forecast' = "green")) + 
       ylab("Oil Prices Values") + xlab("Years") +
      geom_ribbon(aes(ymin = lo95, ymax = hi95), alpha = .25)
     
       # This uses the scale library 
    #    scale_x_date(breaks=date_breaks("1 month"),minor_breaks=date_breaks("1 month"), 
            # labels="%W") 
    if(o == TRUE){
        p <- p + scale_x_date(breaks = "1 month", minor_breaks = "1 week", labels=date_format("%B"))
    }
   return(p) 
   
}

convert_from_decimal_to_date <- function(date_string){
    date_num <- as.numeric(date_string)
    year <- floor(date_num)
    frac <- date_num - year 
    sec_year <- unclass(ISOdate(year+1,1,1,0,0,0)) -  unclass(ISOdate(year,1,1,0,0,0))
    date <- ISOdate(year,1,1,0,0,0) + frac * sec_year
    
    final_date <- format(date, format='%Y-%m-%d')
    return(final_date)
     
}