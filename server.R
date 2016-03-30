
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

plotGraph <- function(plotter,variableToForcast,model_type='',start_year=1980){
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
    # convert data from data_frame to time series
    ts_data = ts(plotter$price,start=c(start_year,1),frequency = 12)
    # function that predicts future values and returns a plot object
    # spent the whole night writing this function
    new_graph <- HWplot(ts_data) 
  }
  if(model_type == "arima"){
      ts_data = ts(plotter$price,start=c(start_year,1),frequency = 12)
      new_graph <- ArimaPlot(ts_data)
  }
  if(model_type == ''){        
    new_graph <- theGraph 
  }      
  return(new_graph)
}
require(EIAdata)

# Fetch datasets
d_eiaoilp<-read.table("./daily.csv",header=TRUE,sep=",")[,c("date","price")]
w_eiaoilp<-read.table("./weekly.csv",header=TRUE,sep=",")[,c("date","price")]
m_eiaoilp<-read.table("./monthly.csv",header=TRUE,sep=",")[,c("date","price")]

# order of table. order date in decreasing order
d_eiaoilp_rev <- d_eiaoilp[order(d_eiaoilp$date,decreasing = TRUE),]
w_eiaoilp_rev <- w_eiaoilp[order(w_eiaoilp$date,decreasing = TRUE),]
m_eiaoilp_rev <- m_eiaoilp[order(m_eiaoilp$date,decreasing = TRUE),]
# boilp <- boilp[order(boilp$date,decreasing = TRUE),]
# woilp <- woilp[order(woilp$date,decreasing = TRUE),]

# get the years only with no duplicate
# unique function removes duplicates
eia_years <- unique(as.numeric(format(as.yearmon(m_eiaoilp$date),"%Y")))
# eia_years <- unique(as.numeric(format(as.Date(as.yearmon(time(m_eiaoilp$date))),"%Y")))
# bo_years <- unique(as.numeric(format(boilp$date,"%Y")))
# wo_years <- unique(as.numeric(format(woilp$date,"%Y")))


shinyServer(function(input, output,session) {
  plotter <- '' # name of final dataframe to visualize
  slider_component <- NULL
  
  # EVENT LISTENERS  
  observeEvent(input$loadDataset,{
    updateTextInput(session,"actionSelected",value= "loadDataset")
    # observe({
    #   updateSelectInput(session,"oilPrices",
    #                     choices = c("EIA OIL" = "eiaoilp",
    #                                 "Brent OIL" = "boilp",
    #                                 "W OIL" = "woilp"))
    # })
    plotter<-m_eiaoilp
          # Increment the top-level progress indicator
        # incProgress(1)     
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
    #   datasetInput <- reactive({
    #     switch(input$oilPrices,
    #            "daily" = d_eiaoilp,
    #            "weekly" = w_eiaoilp,
    #            "monthly" = m_eiaoilp
    #     )
    #   })
      datasetInput <- reactive({
         switch(input$oilPrices,
               "Daily" = d_eiaoilp_rev,
               "Weekly" = w_eiaoilp_rev,
               "Monthly" = m_eiaoilp_rev
        ) 
      })
      plotter<-datasetInput()
            # plotter<-m_eiaoilp
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
        updateCheckboxGroupInput(session,"variableToForcast",
                                 choices=new_options,selected = new_options[1])
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
     datasetInput <- reactive({
        switch(input$oilPrices,
               "Daily" = d_eiaoilp,
               "Weekly" = w_eiaoilp,
               "Monthly" = m_eiaoilp
        )
      }) 
     plotter <-datasetInput() 
     cat(input$yearSlider[1])
    }
    output$plot_output <- renderPlot({
      # generate bins based on input$bins from ui.R
      # reduces the date to the specified slider
      cat(input$yearSlider)
    #   plotter <- plotter[which(plotter$date >= as.Date(as.character(input$yearSlider[1]),"%Y") & 
    #                             plotter$date <= as.Date(as.character(input$yearSlider[2]),"%Y")),]
      plotter <- plotter[which(
          as.numeric(format(as.yearmon(plotter$date),"%Y")) >= input$yearSlider[1] & 
          as.numeric(format(as.yearmon(plotter$date),"%Y")) <= input$yearSlider[2]),]
      plotter$DATE<-as.Date(as.character(plotter$date),format="%Y-%m-%d")
      new_graph <- plotGraph(plotter,input$variableToForcast,model_type = input$modelSelection,
                             start_year = input$yearSlider[1])
      #theGraph <- ggplot(plotter,aes_string(x=plotter$date,y=input$variableToForcast
      #                                      )) +
      #  ylab("Oil Prices Values") + xlab("Years")
      #if(input$modelSelection == "linear_model"){
      #  f <- paste(names(plotter)[1], "~", paste(names(plotter)[-1]))
      #  plotter$predicted <- predict(lm(f,data=plotter))
                
      #  new_graph <- theGraph +geom_line(colour="blue") + 
      #      geom_line(aes(y=plotter$predicted))          
      #}else{        
      #  new_graph <- theGraph +geom_line(colour="blue") 
      #}      
      new_graph
    })
  })

})
