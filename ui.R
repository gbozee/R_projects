
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
#library(shinyjs)
library(DT)

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
        actionButton("getLatestData",class="btn btn-block btn-default", "Get Latest Datasets"),
        tags$div(id="table_sidebar",
            textInput("actionSelected",label = ''),
            actionButton("loadDataset",class="btn btn-block btn-default", "Load Available Dataset"),
            actionButton("uploadDataset",class="btn btn-block btn-default","Upload Dataset"),        
            tags$div(class="set_container hidden",
                   
                tags$div(id="l_dataset",class="hidden",
                        tags$hr(),
                        selectInput("oilPricesSource","Data Source",
                            choices= c("")),
                        # default select input before the server loads the dataset
                        selectInput("oilPrices","Data Frequency", 
                                    choices = c("Select","Daily","Weekly","Monthly")),
                        # actionButton("displayAction","Display Dataset as Table",class="displayAction btn-primary center-block")
                        # dateRangeInput("daterange", "Date range:",
                        #     start = "2001-01-01",
                        #     end   = "2010-12-31")
                        conditionalPanel(
                            condition="input.oilPrices == 'Daily' || input.oilPrices == 'Weekly'",                            
                            dateInput("date_range","End Date",
                                min = Sys.Date() - (365*5), # 5 year back (default. would be changed in server.R)
                                max = Sys.Date(),
                                # value=Sys.Date()
                            )   
                        ),
                        conditionalPanel(
                            condition="input.oilPrices != 'Daily' && input.oilPrices != 'Weekly'",                            
                            dateRangeInput("daterange", "Date range:",
                                start = "2001-01-01",
                                end   = "2010-12-31")
                            
                        )
                        # actionButton('')
                        ),
                tags$div(id="u_dataset",class="hidden",
                        tags$hr(),
                        fileInput('fileUploaded', 'Choose file',
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
                actionButton("displayAction","Display Data",class="displayAction btn-primary center-block")   
                
                ),
            tags$hr(),
            tags$div(id="model_section",class="hidden",
                    selectInput("modelSelection","Model",
                                choices=c("None" = "none",
                                    "Linear Model"="linear_model",
                                           "Holt Linear"="holt_linear",
                                           "Simple Exponential Smoothing"="ses",
                                           "Auto Arima"="arima",
                                           "Custom Arima"="c_arima")),
                    conditionalPanel(
                        condition="input.modelSelection == 'holt_linear'",
                        tags$div(class="extra_params1",
                            numericInput('h_alpha',label="alpha",step=0.1,max=1,min=0.1,value=0.6),
                            numericInput('h_beta',label="beta",step=0.1,max=1,min=0.1,value=0.2)
                        )
                    ),
                    conditionalPanel(
                        condition="input.modelSelection == 'ses'",
                        tags$div(class="extra_params1 form-inline",
                            numericInput('s_alpha',label="alpha",step=0.1,max=1,min=0.1,value=0.6)                         
                        )
                    ),
                    conditionalPanel(
                        condition="input.modelSelection == 'c_arima'",
                        tags$h4("Input Arima order"),                       
                        tags$div(class="order_section form-inline",
                            numericInput("first_order",label="p",value=1,min=0,max=10),
                            numericInput("second_order",label="d",value=2,min=0,max=10),
                            numericInput("third_order",label="q",value=1,min=0,max=10))                            
                            ),
                    selectInput("no_of_observations","Forecast Period",
                                        choices = c(1,3,6,9)
                        ),
                    actionButton("visualizeAction","Visualise Data",class="btn-info center-block")
                    )
                ),
            tags$div(id="visual_sidebar",class="hidden",
                sliderInput("yearSlider", "Years:",
                        min = as.Date("2010-01-01"), max = as.Date("2016-10-01"), 
                        value = c(as.Date("2016-04-01"),as.Date("2016-03-01")), 
                        step = 1,
                        sep = "", animate=TRUE,timeFormat="%F"),
               tags$div(id="summary_display",
                tags$h4("Forecast Summary"),
                verbatimTextOutput("f_summary"),
                downloadButton('downloadData', 'Export data'),
                downloadButton('downloadData2', 'Export forecast summary')
                )
                
                
            )
                
    ),   
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel("tabsets",
                  tabPanel("Table",
                           tags$p(),
                           h3(id="table_text"),
                           dataTableOutput("table_output"),
                           h2("")),
                  tabPanel("Visualization",
                    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                            # tags$div("Loading...",id="loadmessage")),
                            tags$img(src = "35.gif", id = "loading-spinner")),
                    conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                            tags$div(                             
                                plotOutput('plot_output',
                                   brush = brushOpts(
                                    id = "plot2_brush",
                                    resetOnNew = FALSE
                                    )),
                                h2(""),
                                div(style = 'overflow-x: scroll', DT::dataTableOutput("predicted_table"))   
                            ) ) )
                  
               )
                #   tabPanel("predicted_data",
                #             tableOutput("predicted_output")
                #     )
    )
  )
))
