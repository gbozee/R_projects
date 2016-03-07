
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)

shinyUI(fluidPage(
  useShinyjs(),
  includeScript("page_load.js"),
  # Application title
  titlePanel("Oil Estimator"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      actionButton("loadDataset",class="btn btn-block btn-default", "Load Dataset"),
      actionButton("uploadDataset",class="btn btn-block btn-default","Upload Dataset"),
      
      tags$div(id="l_dataset",
               selectInput("oilPrices","Choose a dataset:", 
                                      choices = c("WTI","Brent"))
               ),
      tags$div(id="u_dataset",
      	tags$div(
      		   fileInput('fileUploaded', 'Choose file to upload',
                         accept = c(
                           'text/csv',
                           'text/comma-separated-values',
                           'text/tab-separated-values',
                           'text/plain',
                           '.csv',
                           '.tsv')
                         )
               ),
               tags$hr(),
			   checkboxInput('header', 'Header', TRUE),
			   radioButtons('sep', 'Separator', c(Comma=',',Semicolon=';',Tab='\t'),',')
      	) 
    ),   
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
