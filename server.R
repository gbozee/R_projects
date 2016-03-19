
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(Quandl)
library(ggplot2)
require(EIAdata)

# Fetch datasets
key<-("95365B2462BFD45A96D4EB0DBC60E59A")
oil<-getEIA("PET.RBRTE.M",key)
oilp<-ts(oil[,"PET.RBRTE.M"],start=c(1987,5),freq=12)
brentoilprice<-Quandl("ODA/POILBRE_USD",type = "ts")
wtioilprice<-Quandl("ODA/POILWTI_USD",type = "ts")

eiaoilp<-data.frame(price=as.matrix(oilp),date=as.Date(as.yearmon(time(oilp)))) # i need to fix this bug
boilp<-data.frame(price=as.matrix(brentoilprice),date=as.Date(as.yearmon(time(brentoilprice)))) 
woilp<-data.frame(price=as.matrix(wtioilprice),date=as.Date(as.yearmon(time(wtioilprice)))) #from ts to data.frame

# order of table. order date in decreasing order
eiaoilp <- eiaoilp[order(eiaoilp$date,decreasing = TRUE),]
boilp <- boilp[order(boilp$date,decreasing = TRUE),]
woilp <- woilp[order(woilp$date,decreasing = TRUE),]

#the names of the columns are price and date.
colnames(eiaoilp) <- c("price","date")

# get the years only with no duplicate
# unique function removes duplicates
eia_years <- unique(as.numeric(format(eiaoilp$date,"%Y")))
bo_years <- unique(as.numeric(format(boilp$date,"%Y")))
wo_years <- unique(as.numeric(format(woilp$date,"%Y")))


shinyServer(function(input, output,session) {
  plotter <- '' # name of final dataframe to visualize
  slider_component <- NULL
  
  # EVENT LISTENERS  
  observeEvent(input$loadDataset,{
    updateTextInput(session,"actionSelected",value= "loadDataset")
    observe({
      updateSelectInput(session,"oilPrices",
                        choices = c("EIA OIL" = "eiaoilp",
                                    "Brent OIL" = "boilp",
                                    "W OIL" = "woilp"))
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
          new_options <- new_options[2:length(new_options)]
          updateCheckboxGroupInput(session,"variableToForcast",
                                   choices=new_options)
        })
    }else{
      datasetInput <- reactive({
        switch(input$oilPrices,
               "eiaoilp" = eiaoilp,
               "boilp" = boilp,
               "woilp" = woilp
        )
      })
      plotter<-datasetInput()
      output$table_output <- renderTable({
        plotter$date <- format(plotter$date,'%Y-%m-%d')
        plotter
      })  
      yearSelection <- reactive({
            switch(input$oilPrices,
            "eiaoilp" = eia_years,
            "boilp" = bo_years,
            "woilp" = wo_years)
        })
      slider_component <- yearSelection()
      observe({
        #plotter<-datasetInput()
        new_options <- colnames(plotter)
        new_options <- new_options[1:length(new_options)-1]
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
        # This is where you would put the dataset options that you want to download
        switch(input$oilPrices,
               "eiaoilp" = eiaoilp,
               "boilp" = boilp,
               "woilp" = woilp
        )
      })
     plotter <- datasetInput() 
     cat(input$yearSlider[1])
    }
    output$plot_output <- renderPlot({
      # generate bins based on input$bins from ui.R
      # reduces the date to the specified slider
    #   cat(input$yearSlider)
      plotter <- plotter[which(plotter$date >= as.Date(as.character(input$yearSlider[1]),"%Y") & 
                                plotter$date <= as.Date(as.character(input$yearSlider[2]),"%Y")),]
      theGraph <- ggplot(plotter,aes_string(x=plotter$date,y=input$variableToForcast
                                            )) +
        ylab("Oil Prices Values") + xlab("Years")
      if(input$modelSelection == "linear_model"){
        f <- paste(names(plotter)[1], "~", paste(names(plotter)[-1]))
        plotter$predicted <- predict(lm(f,data=plotter))
                
        new_graph <- theGraph +geom_line(colour="blue") + 
            geom_line(aes(y=plotter$predicted))          
      }else{        
        new_graph <- theGraph +geom_line(colour="blue") 
      }      
      new_graph
    })
  })

})
