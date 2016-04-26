library(ggplot2)

library(reshape)
library(forecast)
library(scales)

forecast_function<-function(dn,data_point=12,f_type="holt_winters",arima_order=NULL,
    alpha=NULL,beta=NULL){
    require(zoo) #needed for the 'as.yearmon()' function
    if(f_type == "holt_winters"){
        fcast <- hw(dn,h=as.integer(data_point))
        # fcast <- forecast.HoltWinters(HoltWinters(dn), h=data_point)
        cat("holt_winters")        
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
 
	return(ds)
 
}

ArimaPlot<-function(ts_object,  n.ahead=4,date_type="Monthly",arima_order=NULL){
    graphset <- funggcast(ts_object,data_point=n.ahead,f_type="arima",arima_order=arima_order)
    # graphset$date <- factor(graphset$date, levels=graphset$date[!duplicated(graphset$date)])
    p <- old_plot(graphset)
    # p <- plot_function(f)
    return(p)
}
HWplot<-function(ts_object,  n.ahead=4,  date_type="Monthly",f_type="holt_winters",
    alpha=NULL,beta=NULL){    
  graphset <- funggcast(ts_object,n.ahead,f_type=f_type,alpha=alpha,beta=beta) 
  p <- old_plot(graphset)
  return(p)
}

old_plot <- function(graphset,data.color = 'blue', fit.color = 'red', forec.color = 'black',
                           lower.fill = 'darkgrey', upper.fill = 'grey'){
   p <- ggplot(graphset,aes(date,observed)) + 
       geom_line(aes(y=observed,colour="observed")) + 
       geom_line(aes(y=fitted,colour="fitted")) + 
    #    geom_line(color="blue") + 
       geom_line(aes(y = forecast,colour="forecast")) + 
    #    geom_line(color="green") +
    #    scale_x_datetime(labels = date_format("%b")) +
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

convert_from_decimal_to_date <- function(date_string){
    date_num <- as.numeric(date_string)
    year <- floor(date_num)
    year_beginning <- as.POSIXct(paste0(year, '-01-01'))
    year_end <- as.POSIXct(paste0(year+1, '-01-01'))
    date <- year_beginning + (date_num %% 1) * (year_end - year_beginning)
    
    final_date <- format(date, format='%Y-%m-%d')
    # string_val <- as.character(date,)
    return(final_date)
    # > 
}