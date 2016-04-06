
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(Quandl)
library(ggfortify)
library(ggplot2)
# import this file which plots graph for predicted values
source("./HWplot.R")
source("./fetch_dataset.R")

plotGraph <- function(plotter,variableToForcast,model_type='',start_year=1980,
    observations=1,date_type="Monthly"){
        
    frequency <- switch(date_type,
        "Monthly"=12,"Weekly"=52,"Daily"=365)
    # get first and last data 
    year_data <- as.numeric(format(as.Date(as.character(plotter[1,]$date),format="%Y-%m-%d"),"%Y"))
    month_data <- as.numeric(format(as.Date(as.character(plotter[1,]$date),format="%Y-%m-%d"),"%m"))
    
    ts_data <- ts(plotter$price,frequency=frequency,start=c(year_data,month_data))
  theGraph <- ggplot(plotter,aes_string(x=plotter$DATE,y=variableToForcast
#   theGraph <- ggplot(plotter,aes(x=plotter$DATE,y=variableToForcast
  )) +
    ylab("Oil Prices Values") + xlab("Years") +geom_line(colour="blue")
  if(model_type == "linear_model"){
    f <- paste(names(plotter)[2], "~", paste(names(plotter)[3]))
    plotter$predicted <- predict(lm(f,data=plotter))    
    new_graph <- theGraph + 
        geom_line(aes(y=plotter$predicted))
  }
  if(model_type == 'holt_winters'){
    
    # function that predicts future values and returns a plot object
    # spent the whole night writing this function
    new_graph <- HWplot(ts_data,n.ahead=observations,date_type=date_type) 
  }
  if(model_type == "arima"){
      new_graph <- ArimaPlot(ts_data,n.ahead=observations,date_type=date_type)
  }
  if(model_type == ''){        
    new_graph <- theGraph 
  }      
  return(new_graph)
}

require(EIAdata)

# Fetch datasets
d_eiaoilp<-read.table("./eia_daily.csv",header=TRUE,sep=",")[,c("date","price")]
w_eiaoilp<-read.table("./eia_weekly.csv",header=TRUE,sep=",")[,c("date","price")]
m_eiaoilp<-read.table("./eia_monthly.csv",header=TRUE,sep=",")[,c("date","price")]
#Todo change to actual data
d_boilp<-read.table("./brent_daily.csv",header=TRUE,sep=",")[,c("date","price")]
w_boilp<-read.table("./brent_weekly.csv",header=TRUE,sep=",")[,c("date","price")]
m_boilp<-read.table("./brent_monthly.csv",header=TRUE,sep=",")[,c("date","price")]
d_woilp<-read.table("./wt_daily.csv",header=TRUE,sep=",")[,c("date","price")]
w_woilp<-read.table("./wt_weekly.csv",header=TRUE,sep=",")[,c("date","price")]
m_woilp<-read.table("./wt_monthly.csv",header=TRUE,sep=",")[,c("date","price")]


# get the years only with no duplicate
# unique function removes duplicates
eia_years <- unique(as.numeric(format(as.yearmon(m_eiaoilp$date),"%Y")))
# eia_years <- unique(as.numeric(format(as.Date(as.yearmon(time(m_eiaoilp$date))),"%Y")))
# bo_years <- unique(as.numeric(format(boilp$date,"%Y")))
# wo_years <- unique(as.numeric(format(woilp$date,"%Y")))


dataSetWithDuration <- function(sourceVal,daterange="Monthly"){
  if(daterange=='Daily'){       
    val <- switch(sourceVal,
        "eiaoilp"=d_eiaoilp,"boilp"=d_boilp,"woilp"=d_woilp)
        
    }else if(daterange=='Weekly'){              
    val <- switch(sourceVal,
        "eiaoilp"=w_eiaoilp,"boilp"=w_boilp,"woilp"=w_woilp)
    }else if(daterange=='Monthly'){              
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
shinyServer(function(input, output,session) {
  plotter <- '' # name of final dataframe to visualize
  slider_component <- NULL
  
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
        updateDateRangeInput(session, 'daterange', 
            start = start_date, end = end_date)   
      
    })
  })
  observeEvent(input$daterange,{
      cat(input$daterange[1])
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
   
      datasetInput <- reactive({
         switch(input$oilPrices,
               "Daily" = d_eiaoilp_rev,
               "Weekly" = w_eiaoilp_rev,
               "Monthly" = m_eiaoilp_rev
        ) 
      })
      plotter<- dataSetWithDuration(input$oilPricesSource,input$oilPrices)
      View(plotter)
      plotter <-retrieveDatasetInRange(plotter,input$daterange)
      rev_plotter <- plotter[order(plotter$date,decreasing = TRUE),]

      slider_component <- eia_years      
            # Increment the top-level progress indicator
            # incProgress(1)
            # setProgress(1)     
        
      output$table_output <- renderDataTable({
        # plotter$date <- format(plotter$date,'%Y-%m-%d')
        # plotter <- datasetInput()
        plotter
      }) 
   
      observe({
        #plotter<-datasetInput()
        new_options <- colnames(plotter)
        new_options <- new_options[2]
        updateSliderInput(session,"yearSlider",min=min(slider_component),max=max(slider_component),
                        step=1,value = c(min(slider_component),max(slider_component)))
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
    output$plot_output <- renderPlot({
      # generate bins based on input$bins from ui.R
      # reduces the date to the specified slider
    #   cat(input$yearSlider)
    #   plotter <- plotter[which(plotter$date >= as.Date(as.character(input$yearSlider[1]),"%Y") & 
    #                             plotter$date <= as.Date(as.character(input$yearSlider[2]),"%Y")),]
    #   plotter <- plotter[which(
    #       as.numeric(format(as.yearmon(plotter$date),"%Y")) >= input$yearSlider[1] & 
    #       as.numeric(format(as.yearmon(plotter$date),"%Y")) <= input$yearSlider[2]),]
      plotter <-retrieveDatasetInRange(plotter,input$daterange)
      plotter$DATE<-as.Date(as.character(plotter$date),format="%Y-%m-%d")
      new_graph <- plotGraph(plotter,"price",model_type = input$modelSelection,
                             start_year = input$yearSlider[1],
                             observations = input$no_of_observations,
                             date_type=input$oilPrices)
      new_graph
    })
  })

})
