
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(Quandl)
library(ggfortify)
library(ggplot2)
library(reshape2)
# library(DT)
# import this file which plots graph for predicted values
source("./HWplot.R")
source("./fetch_dataset.R")
first_element_as_a_list <- function(plotter,index=1){
   
    year_data <- as.numeric(format(as.Date(as.character(plotter[index,]$date),format="%Y-%m-%d"),"%Y"))
    month_data <- as.numeric(format(as.Date(as.character(plotter[index,]$date),format="%Y-%m-%d"),"%m"))
    return(c(year_data,month_data))
}
date_from_slider <- function(vect){
        first <- as.Date(as.character(vect[1]),"%Y")
        second <- as.Date(as.character(vect[2]),"%Y")
        return(c(first,second))
    }
    
get_time_series_object <- function(plotter,observations,date_type="Monthly"){
    frequency <- switch(date_type,
        "Monthly"=12,"Weekly"=52,"Quarterly"=4,"Daily"=1)
    # get first and last data 
    start <- first_element_as_a_list(plotter)
    end <- first_element_as_a_list(plotter,length(plotter$date))
    # No end date is set when generating time series for weekly and daily dataset.
    if(date_type == "Weekly" || date_type == "Daily"){     
      ts_data <- ts(plotter$price,frequency=frequency,start=start) 
    }else{      
      ts_data <- ts(plotter$price,frequency=frequency,start=start,end=end)
    }
    return(ts_data)
}
plotGraph <- function(plotter,variableToForcast,model_type='',start_year=1980,
    observations=1,date_type="Monthly",arima_order=NULL,alpha=NULL,beta=NULL){        
    
  new_graph <- ggplot(plotter,aes_string(x=plotter$DATE,y=variableToForcast)) +
    ylab("Oil Prices Values") + xlab("Years") +geom_line(colour="blue")
  if(model_type == "linear_model"){
    f <- paste(names(plotter)[2], "~", paste(names(plotter)[3]))
    plotter$predicted <- predict(lm(f,data=plotter))    
    new_graph <- new_graph + 
        geom_line(aes(y=plotter$predicted))
  }
  if(model_type == 'holt_winters'){
    # function that predicts future values and returns a plot object
    # spent the whole night writing this function
    ts_data <- get_time_series_object(plotter,observations,date_type)    
    new_graph <- HWplot(ts_data,n.ahead=observations,date_type=date_type) 
  }
  if(model_type == "arima"){      
      ts_data <- get_time_series_object(plotter,observations,date_type)
    
      new_graph <- ArimaPlot(ts_data,n.ahead=observations,date_type=date_type)
  }
  if(model_type == "c_arima"){
    ts_data <- get_time_series_object(plotter,observations,date_type)    
      new_graph <- ArimaPlot(ts_data,n.ahead=observations,date_type=date_type,arima_order=arima_order)
  }
  if(model_type == "holt_linear" || model_type == "ses"){
    ts_data <- get_time_series_object(plotter,observations,date_type)
    new_graph <- HWplot(ts_data,n.ahead=observations,date_type=date_type,f_type=model_type,
      alpha=alpha,beta=beta)
  }
  return(new_graph)
}
convert_to_quarterly_dataframe <- function(original_dataframe){
    s_date  <- first_element_as_a_list(original_dataframe)
    l_date <- first_element_as_a_list(original_dataframe,length(original_dataframe$date))
    e_q_eilolpts <- ts(original_dataframe,frequency=4,start=s_date,end=l_date)
    q_eiolp <- data.frame(date=as.Date(as.yearmon(time(e_q_eilolpts))),
        price=as.matrix(e_q_eilolpts))
    return(q_eiolp)
    
}
require(EIAdata)

# Fetch datasets
d_eiaoilp<-read.table("./eia_daily.csv",header=TRUE,sep=",")[,c("date","price")]
w_eiaoilp<-read.table("./eia_weekly.csv",header=TRUE,sep=",")[,c("date","price")]
m_eiaoilp<-read.table("./eia_monthly.csv",header=TRUE,sep=",")[,c("date","price")]
q_eiaoilp <- convert_to_quarterly_dataframe(m_eiaoilp)
#Todo change to actual data
d_boilp<-read.table("./brent_daily.csv",header=TRUE,sep=",")[,c("date","price")]
w_boilp<-read.table("./brent_weekly.csv",header=TRUE,sep=",")[,c("date","price")]
m_boilp<-read.table("./brent_monthly.csv",header=TRUE,sep=",")[,c("date","price")]
q_boilp <- convert_to_quarterly_dataframe(m_boilp)
d_woilp<-read.table("./wt_daily.csv",header=TRUE,sep=",")[,c("date","price")]
w_woilp<-read.table("./wt_weekly.csv",header=TRUE,sep=",")[,c("date","price")]
m_woilp<-read.table("./wt_monthly.csv",header=TRUE,sep=",")[,c("date","price")]
q_woilp <- convert_to_quarterly_dataframe(m_woilp)
# get the years only with no duplicate
# unique function removes duplicates
eia_years <- unique(as.numeric(format(as.yearmon(m_eiaoilp$date),"%Y")))
# eia_years <- unique(as.numeric(format(as.Date(as.yearmon(time(m_eiaoilp$date))),"%Y")))
# bo_years <- unique(as.numeric(format(boilp$date,"%Y")))
# wo_years <- unique(as.numeric(format(woilp$date,"%Y")))
get_years_from_d_frame <- function(d_frame){
    eia_years <- unique(as.numeric(format(as.yearmon(d_frame),"%Y")))
    return(eia_years)    
}

dataSetWithDuration <- function(sourceVal,daterange="Monthly"){
  if(daterange=='Weekly'){              
    val <- switch(sourceVal,
        "eiaoilp"=w_eiaoilp,"boilp"=w_boilp,"woilp"=w_woilp)    
    }else if(daterange=='Daily'){              
        val <- switch(sourceVal,
            "eiaoilp"=d_eiaoilp,"boilp"=d_boilp,"woilp"=d_woilp)
    }else if(daterange=='Monthly'){              
        val <- switch(sourceVal,
            "eiaoilp"=m_eiaoilp,"boilp"=m_boilp,"woilp"=m_woilp)
    }else if(daterange == 'Quarterly'){
        val <- switch(sourceVal,
            "eiaoilp"=m_eiaoilp,"boilp"=m_boilp,"woilp"=m_woilp)        
     }else{
        val <- NULL
    }
    return(val)
    
}
retrieveDatasetInRange<-function(dataset,date_range){
    plotter <- dataset[which(
          as.Date(dataset$date) >= date_range[1] & 
          as.Date(dataset$date) <= date_range[2]),]
    return(plotter)
}

determine_start_and_end_range <- function(date_input,date_range_input,dataset_selected){
  if(dataset_selected == "Weekly" || dataset_selected == "Daily"){
    # We are fixing the range when either weekly or daily is selected.
    start <- date_input
    end <- date_input + (365*1) # a year addition
    return(c(start,end))
  }
  return(date_range_input)
}

shinyServer(function(input, output,session) {
  plotter <- '' # name of final dataframe to visualize
  slider_component <- NULL
  user_selected_range <- c() # vector to hold the range selected by the user
  
  # EVENT LISTENERS  
  observeEvent(input$loadDataset,{
    updateTextInput(session,"actionSelected",value= "loadDataset")
    observe({
      updateSelectInput(session,"oilPricesSource",
                        choices = c("EIA OIL" = "eiaoilp",
                                    "Brent OIL" = "boilp",
                                    "W OIL" = "woilp"))
    })
    
    plotter<-m_eiaoilp
          # Increment the top-level progress indicator
        # incProgress(1)     
  })
  observeEvent(input$oilPrices,{
    val <- dataSetWithDuration(input$oilPricesSource,input$oilPrices)
    
    if(is.null(val)){
          return(NULL)
    }       
    observe({      
        start_date = min(as.Date(val$date))
        end_date = max(as.Date(val$date))
        
        # I am alternating between a dateinput and a daterange input
        # based on if the data is daily/weekly or monthly/quarterly
        if(input$oilPrices == "Weekly" || input$oilPrices == "Daily"){
          cat("it is valid")
          updateDateInput(session,'date_range',max=as.Date(end_date)-(365*1),
            min=as.Date(start_date),value=as.Date(start_date))
        }
        else{          
          updateDateRangeInput(session, 'daterange', 
              start = start_date, end = end_date) 
        }   
      
    })
  })
  
  
  observeEvent(input$uploadDataset,{
    updateTextInput(session,"actionSelected",value= "uploadDataset")
  })
  
  observeEvent(input$displayAction,{
    if(input$actionSelected == "uploadDataset"){
        inFile <- input$fileUploaded
        if (is.null(inFile))
          return(NULL)
        plotter <-read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                            quote=input$quote)
        output$table_output <- renderTable({
          plotter
        })
        observe({
          new_options <- colnames(plotter)
          new_options <- new_options[2]
          updateCheckboxGroupInput(session,"variableToForcast",
                                   choices=new_options)
        })
    }else{
      if(is.null(input$oilPrices) || input$oilPrices == 'Select')
            return(NULL)
            
      user_selected_range <- determine_start_and_end_range(input$date_range,input$daterange,input$oilPrices)
      plotter<- dataSetWithDuration(input$oilPricesSource,input$oilPrices)
      plotter <-retrieveDatasetInRange(plotter,user_selected_range)
      if(input$oilPrices == 'Quarterly'){
          plotter <- convert_to_quarterly_dataframe(plotter)
      }
      
      rev_plotter <- plotter[order(plotter$date,decreasing = TRUE),]

      slider_component <- get_years_from_d_frame(plotter$date)      
            # Increment the top-level progress indicator
            # incProgress(1)
            # setProgress(1)     
        
      output$table_output <- renderDataTable({
        # plotter$date <- format(plotter$date,'%Y-%m-%d')
        # plotter <- datasetInput()
        rev_plotter
        # plotter
      }) 
   
      observe({
        #plotter<-datasetInput()
        new_options <- colnames(plotter)
        new_options <- new_options[2]
        if(input$oilPrices == "Daily" || input$oilPrices == "Weekly"){
          max = max(slider_component)
          step = 1/12
        }else{
          max = 2016
          step = 1
        }
        updateSliderInput(session,"yearSlider",min=min(slider_component),max=max,
                        step=step,value = c(min(slider_component),max(slider_component)))
      })
    }
  })
  observeEvent(input$visualizeAction,{
    if (is.null(plotter))
      return(NULL)

    if(input$actionSelected == "uploadDataset"){
      inFile <- input$fileUploaded
      if (is.null(inFile))
        return(NULL)
      plotter <-read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                         quote=input$quote)
    }else{
        plotter<- dataSetWithDuration(input$oilPricesSource,input$oilPrices)
        
     cat(input$yearSlider[1])
    }
    get_alpha_and_beta_particles <- function(){
      al <- NULL
      be <- NULL
      if(input$modelSelection == "holt_linear"){
        al <- input$h_alpha
        be <- input$h_beta
      }
      if(input$modelSelection == "ses"){
        al <- input$s_alpha
      }
      return(c(al,be))
    }
    output$plot_output <- renderPlot({
      # generate bins based on input$bins from ui.R
      # reduces the date to the specified slider
    #   cat(input$yearSlider)
    #   plotter <- plotter[which(plotter$date >= as.Date(as.character(input$yearSlider[1]),"%Y") & 
    #                             plotter$date <= as.Date(as.character(input$yearSlider[2]),"%Y")),]
    #   plotter <- plotter[which(
    #       as.numeric(format(as.yearmon(plotter$date),"%Y")) >= input$yearSlider[1] & 
    #       as.numeric(format(as.yearmon(plotter$date),"%Y")) <= input$yearSlider[2]),]
      d_slider <- date_from_slider(input$yearSlider)
      user_selected_range <- determine_start_and_end_range(input$date_range,input$daterange,input$oilPrices)
      
      plotter <-retrieveDatasetInRange(plotter,user_selected_range)
      plotter <- retrieveDatasetInRange(plotter,d_slider)
      plotter$DATE<-as.Date(as.character(plotter$date),format="%Y-%m-%d")
      arima_order <- c(input$first_order,input$second_order,input$third_order)
      alpha_beta <- get_alpha_and_beta_particles()
      new_graph <- plotGraph(plotter,"price",model_type = input$modelSelection,
                             start_year = input$yearSlider[1],
                             observations = input$no_of_observations,
                             date_type=input$oilPrices,
                             arima_order=arima_order,
                             alpha=alpha_beta[1],
                             beta=alpha_beta[2])
      new_graph
    })
    f_data <- function(default="funggcast",exclude_slider=FALSE){
      d_slider <- date_from_slider(input$yearSlider)
      user_selected_range <- determine_start_and_end_range(input$date_range,input$daterange,input$oilPrices)
      plotter <-retrieveDatasetInRange(plotter,user_selected_range)
      if(exclude_slider == TRUE){
         plotter <- retrieveDatasetInRange(plotter,d_slider) 
      }      
      plotter$DATE<-as.Date(as.character(plotter$date),format="%Y-%m-%d")
      ts_data <- get_time_series_object(plotter,input$no_of_observations,input$oilPrices)
      arima_order <- c(input$first_order,input$second_order,input$third_order)
      alpha_beta <- get_alpha_and_beta_particles()
      if(default == "funggcast"){ 
        predicted <- funggcast(ts_data,input$no_of_observations,input$modelSelection,arima_order=arima_order,
          alpha=alpha_beta[1], beta=alpha_beta[2])
      } 
      else{
        predicted <- forecast_function(ts_data,input$no_of_observations,input$modelSelection,arima_order=arima_order,
          alpha=alpha_beta[1], beta=alpha_beta[2])
      } 
      return(predicted)
    }
    predicted_t <- function(){
      predicted <- f_data(exclude_slider=TRUE)
      predicted <- predicted[which(
          predicted$forecast != "NA"
        ), 
      ] # limits the result to forecasted data.      
      date_column <- predicted$date
      drops <- c("date")
      predicted <- predicted[ , !(names(predicted) %in% drops)]
      the_rownames <- colnames(predicted)
      predicted <- as.data.frame(t(predicted))
      predicted<- head(predicted,4)
      if(input$oilPrices == "Daily" || input$oilPrices == "Weekly"){       
        colnames(predicted) <- sapply(date_column,convert_from_decimal_to_date) 
      } else{
        colnames(predicted) <- date_column
      }
      return(predicted)
      
    }
    output$predicted_table <- renderDataTable(
      predicted_t(),options=list())    
    output$f_summary <- renderPrint({
      f <- f_data("forecast_function")
      # print(forecast$model)
      display_data <- capture.output(summary(f$model))
      # cat(display_data)
      print(display_data)
    })
  })

})
