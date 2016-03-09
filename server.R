
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
#eiaoilp<-data.frame(date=as.Date(as.yearmon(time(oilp))),price=as.matrix(oilp)) # i need to fix this bug
eiaoilp<-data.frame(price=as.matrix(oilp),date=as.Date(as.yearmon(time(oilp)))) # i need to fix this bug
#colnames(eiaoilp) <- c("date","price")
colnames(eiaoilp) <- c("price","date")

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
  plotter <- '' # name of final dataframe to visualize
  observeEvent(input$displayAction,{
    cat("hello")
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
        # This is where you would put the dataset options that you want to download
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
      observe({
        #plotter<-datasetInput()
        new_options <- colnames(plotter)
        new_options <- new_options[1:length(new_options)-1]
        updateCheckboxGroupInput(session,"variableToForcast",
                                 choices=new_options,selected = new_options[1])
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
    }
    output$plot_output <- renderPlot({
      # generate bins based on input$bins from ui.R
      f <- paste(names(plotter)[1], "~", paste(names(plotter)[-1]))
      plotter$predicted <- predict(lm(f,data=plotter))
      theGraph <- ggplot(plotter,aes_string(x=plotter$date,y=input$variableToForcast
                                            )) +
        ylab("Oil Prices Values") + xlab("Years")
      new_graph <- theGraph +geom_line(colour="blue") + 
        geom_line(aes(y=plotter$predicted))
      
      new_graph
    })
  })

})
