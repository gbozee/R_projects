
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
#library(shinyjs)

mycss <- "
#plot-container {
  position: relative;
}
#loading-spinner {
  position: absolute;
  left: 50%;
  top: 80%;
  z-index: -1;
  margin-top: -33px;  /* half of the spinner's height */
  margin-left: -33px; /* half of the spinner's width */
}
#plot.recalculating {
  z-index: -2;
"


shinyUI(fluidPage(theme = "app.css", #css file to further style the page
  includeScript("page_load.js"),
  
  # Application title
  titlePanel("Oil Estimator"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
        tags$head(tags$style(HTML(mycss))),
        tags$div(id="table_sidebar",
            textInput("actionSelected",label = ''),
            actionButton("loadDataset",class="btn btn-block btn-default", "Load Available Dataset"),
            actionButton("uploadDataset",class="btn btn-block btn-default","Upload Dataset"),        
            tags$div(class="set_container hidden",
                   
                tags$div(id="l_dataset",class="hidden",
                        tags$hr(),
                        selectInput("oilPricesSource","Select a data source",
                            choices= c("")),
                        # default select input before the server loads the dataset
                        selectInput("oilPrices","Choose a dataset:", 
                                    choices = c("Select","Weekly","Monthly","Quarterly")),
                        # actionButton("displayAction","Display Dataset as Table",class="displayAction btn-primary center-block")
                        dateRangeInput("daterange", "Date range:",
                            start = "2001-01-01",
                            end   = "2010-12-31")
                        # actionButton('')
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
                                choices=c("None" = "none",
                                    "Linear Model"="linear_model",
                                           "Time Series (Holt Winters)"="holt_winters",
                                           "Time Series (Arima)"="arima")),
                    selectInput("no_of_observations","Select Number of Observations to forecast",
                                        choices = c(1,3,6,9)
                        ),
                    actionButton("visualizeAction","Visualize Dataset",class="btn-info center-block")
                    )
                ),
            tags$div(id="visual_sidebar",class="hidden",
                sliderInput("yearSlider", "Years:",
                        min = 0, max = 10000, value = c(200,500), step = 10,
                        sep = "", animate=TRUE)
            )
                
    ),   
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel("tabsets",
                  tabPanel("table",
                           tags$p(),
                           h3(id="table_text"),
                           dataTableOutput("table_output"),
                           h2("This is the first panel.")),
                  tabPanel("visualization",
                    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                            # tags$div("Loading...",id="loadmessage")),
                            tags$img(src = "35.gif", id = "loading-spinner")),
                    conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                            tags$div(                             
                                plotOutput('plot_output'),
                                h2("This is the second panel."),
                                div(style = 'overflow-x: scroll', dataTableOutput("predicted_table"))   
                            ) )
      ))
                #   tabPanel("predicted_data",
                #             tableOutput("predicted_output")
                #     )
    )
  )
))
