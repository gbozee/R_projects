
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(Quandl)
library(ggplot2)
require(EIAdata)

key<-("95365B2462BFD45A96D4EB0DBC60E59A")
oil<-getEIA("PET.RBRTE.M",key)
oilp<-ts(oil[,"PET.RBRTE.M"],start=c(1987,5),freq=12)
eiaoilp<-data.frame(date=as.Date(as.yearmon(time(oilp))),price=as.matrix(oilp))
colnames(eiaoilp) <- c("date","price")

brentoilprice<-Quandl("ODA/POILBRE_USD",type = "ts")
wtioilprice<-Quandl("ODA/POILWTI_USD",type = "ts")
#the names of the columns are price and date.
boilp<-data.frame(price=as.matrix(brentoilprice),date=as.Date(as.yearmon(time(brentoilprice)))) 
woilp<-data.frame(price=as.matrix(wtioilprice),date=as.Date(as.yearmon(time(wtioilprice)))) #from ts to data.frame

shinyServer(function(input, output,session) {
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
    cat("hello")
    if(input$actionSelected == "uploadDataset"){
        inFile <- input$fileUploaded
        if (is.null(inFile))
          return(NULL)
        csv_file <-read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                            quote=input$quote)
        output$table_output <- renderTable({
          csv_file
        })
        observe({
          new_options <- colnames(csv_file)
          new_options <- new_options[2:length(new_options)]
          updateCheckboxGroupInput(session,"variableToForcast",
                                   choices=new_options)
        })
    }else{
      datasetInput <- reactive({
        # This is where you would put the dataset options that you want to download
        switch(input$oilPrices,
               "eiaoilp" = eiaoilp,
               "boilp" = boilp,
               "woilp" = woilp
        )
      })
      output$table_output <- renderTable({
        u<-datasetInput()
        u$date <- format(u$date,'%Y-%m-%d')
        u
      })  
      observe({
        new_options <- colnames(datasetInput())
        new_options <- new_options[2:length(new_options)]
        updateCheckboxGroupInput(session,"variableToForcast",
                                 choices=new_options)
      })
    }
  })
  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })

})
