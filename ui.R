
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
#library(shinyjs)

shinyUI(fluidPage(theme = "app.css", #css file to further style the page
  #useShinyjs(),
  includeScript("page_load.js"),
  # Application title
  titlePanel("Oil Estimator"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("actionSelected",label = ''),
      actionButton("loadDataset",class="btn btn-block btn-default", "Load Available Dataset"),
      actionButton("uploadDataset",class="btn btn-block btn-default","Upload Dataset"),
      
      tags$div(class="set_container hidden",
        tags$div(id="l_dataset",class="hidden",
                 tags$hr(),
                 selectInput("oilPrices","Choose a dataset:", 
                             choices = c("WTI","Brent"))
                 #actionButton("displayAction","Display Dataset as Table",class="displayAction btn-primary center-block")
        ),
      	tags$div(id="u_dataset",class="hidden",
      	    tags$hr(),
      		   fileInput('fileUploaded', 'Choose file to upload',
                         accept = c(
                           'text/csv',
                           'text/comma-separated-values',
                           'text/tab-separated-values',
                           'text/plain',
                           '.csv',
                           '.tsv')
                         ),
      	    checkboxInput('header', 'Header', TRUE),
      	    radioButtons('sep', 'Separator', c(Comma=',',Semicolon=';',Tab='\t'),',')
      	    ),
        actionButton("displayAction","Display Dataset as Table",class="displayAction btn-primary center-block")   
        
      	),
      tags$hr(),
      tags$div(id="model_section",class="hidden",
               selectInput("modelSelection","Select a model",
                           choices=c("Linear Model","Time Series")),
               checkboxGroupInput("variableToForcast","Select Variable to Forcast",
                                  choices = c("Price"="price",
                                              "Dist" = "Dist")),
               actionButton("visualizeAction","Visualize Dataset",class="btn-info center-block")
               )
    ),   
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel("tabsets",
                  tabPanel("table",
                           tags$p(),
                           tableOutput("table_output"),
                           h2("This is the first panel.")),
                  tabPanel("visualization", 
                           plotOutput('plot_output'),
                           h2("This is the second panel."),
                           tableOutput("predicted_table"))
                  )
    )
  )
))
