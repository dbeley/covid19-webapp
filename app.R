#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# source("dependencies.R")
library("tidyverse")
library("lubridate")
library("RCurl")
library("leaflet")
library("shiny")
library("shinyWidgets")
library("shinythemes")
library("plotly")

theme_set(theme_minimal() + theme(text = element_text(size = 16)))

source("data.R")

ui <- fluidPage(
    theme = shinytheme("flatly"),
    navbarPage(
        "Covidtracker",
        tabPanel(
            "Total",
            fluidRow(
                column(3,
                       h3("Evolution of total cases")),
                column(
                    2,
                    selectInput(
                        "plotType",
                        label = "Graph type",
                        choices = list(
                            "Active Cases" = 'Active.Cases',
                            "Deaths" = 'Deaths',
                            "Recovered" = 'Recovered',
                            "Confirmed" = "Confirmed"
                        ),
                        selected = 'Active.Cases'
                    )
                ),
                column(2,
                       uiOutput("dateInput")),
                column(3,
                       uiOutput("countryPicker"))
            ),
            fluidRow(plotlyOutput("plot1"))
        ),
        tabPanel(
            "New Cases",
            fluidRow(
                column(3, h3("New cases per day")),
                column(
                    2,
                    selectInput(
                        "plotTypeNewCases",
                        label = "Graph type",
                        choices = list(
                            "Active Cases" = 'Active.Cases',
                            "Deaths" = 'Deaths',
                            "Recovered" = 'Recovered',
                            "Confirmed" = "Confirmed"
                        ),
                        selected = 'Active.Cases'
                    )
                ),
                column(2,
                       uiOutput("dateInputNewCases")),
                column(3,
                       uiOutput("countryPickerNewCases"))
            ),
            fluidRow(plotlyOutput("plotNewCases"))
        ),
        tabPanel(
            "Comparison",
            fluidRow(
                column(3, h3("Compare countries evolution")),
                column(
                    2,
                    selectInput(
                        "plotType100",
                        label = "Graph type",
                        choices = list("Deaths" = 'Deaths',
                                       "Confirmed" = "Confirmed"),
                        selected = 'Deaths'
                    )
                ),
                column(2, uiOutput("sliderinput100")),
                column(3,
                       uiOutput("countryPicker100"))
            ),
            fluidRow(plotlyOutput("plot100"))
        ),
        tabPanel("Map",
                 div(
                     class = "outer",
                     leafletOutput("plotmap", height = "75vh"),
                     absolutePanel(
                         top = 40,
                         right = 10,
                         width = 300,
                         draggable = TRUE,
                         style = " opacity: 0.95",
                         wellPanel(
                             selectInput(
                                 "mapType",
                                 label = "Graph type",
                                 choices = list(
                                     "Active Cases" = 'Active.Cases',
                                     "Deaths" = 'Deaths',
                                     "Recovered" = 'Recovered',
                                     "Confirmed" = "Confirmed"
                                 ),
                                 selected = 'Active.Cases'
                             ),
                             sliderInput(
                                 "mapinputdate",
                                 label = "Date",
                                 min = min(dates),
                                 max = max(dates),
                                 value = max(dates),
                                 animate = TRUE
                             )
                         )
                     )
                 )),
        tabPanel(
            "Explore Data",
            conditionalPanel("input.tableType == 'Country'",
                column(2, h3("Data by country"))
                             ),
            conditionalPanel("input.tableType == 'Date'",
                column(2, h3("Data by date"))
                             ),
            column(
                1,
                radioButtons(
                    "tableType",
                    label = "Table type",
                    choices = list("Date" = 'Date',
                                   "Country" = "Country"),
                    selected = 'Date'
                )
            ),
            conditionalPanel(
                "input.tableType == 'Country'",
                column(4,
                       uiOutput("countryPicker2")),
                fluidRow(DT::dataTableOutput("df_pays"))
            ),
            conditionalPanel(
                "input.tableType == 'Date'",
                column(4,
                       selectInput("inputdate", "Choose a date:",
                                   choices = dates)),
                fluidRow(DT::dataTableOutput("df_jour"))
            )
        ),
        tabPanel("About",
                 mainPanel(includeMarkdown("about.md")))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    source("reactive_components.R", local = TRUE)
    output$plot1 <- renderPlotly({
        plot1()
    })
    output$plotNewCases <- renderPlotly({
        plotNewCases()
    })
    output$plot100 <- renderPlotly({
        plot100()
    })
    output$df_jour <- DT::renderDataTable({
        DT::datatable(df_jour_reactive(),
                      colnames = c("", "Country", "Active Cases", "Confirmed Cases", "Deaths", "Recovered"),
                      selection = "none",
                      options = list(pageLength = 15))
    })
    output$df_pays <- DT::renderDataTable({
        DT::datatable(df_pays_reactive(),
                      colnames = c("", "Country", "Date", "Active Cases", "Confirmed Cases", "Deaths", "Recovered"),
                      selection = "none",
                      options = list(pageLength = 15))
    })
    output$plotmap <- renderLeaflet({
        plotmap()
    })

    output$countryPickerNewCases <- renderUI({
        pickerInput(
            inputId = "countriesNewCases",
            label = "Select countries (sorted by active cases)",
            choices = countries,
            options = list(
                `actions-box` = TRUE,
                `live-search` = TRUE,
                size = 20
            ),
            multiple = T,
            selected = most_affected_countries(1)
        )
    })
    output$countryPicker <- renderUI({
        pickerInput(
            inputId = "countries",
            label = "Select countries (sorted by active cases)",
            choices = countries,
            options = list(
                `actions-box` = TRUE,
                `live-search` = TRUE,
                size = 20
            ),
            multiple = TRUE,
            selected = most_affected_countries(15)
        )
    })
    output$countryPicker100 <- renderUI({
        pickerInput(
            inputId = "countries100",
            label = "Select countries (sorted by active cases)",
            choices = countries100,
            options = list(
                `actions-box` = TRUE,
                `live-search` = TRUE,
                size = 20
            ),
            multiple = TRUE,
            selected = most_affected_countries100(10)
        )
    })
    output$sliderinput100 <- renderUI({
        sliderInput(
            "slider100",
            label = paste("Number of ", str_to_lower(input$plotType100), sep =
                              ""),
            min = 0,
            max = 1000,
            value = 100
        )
    })
    output$countryPicker2 <- renderUI({
        pickerInput(
            inputId = "countries2",
            label = "Select countries (sorted by active cases)",
            choices = countries,
            options = list(
                `actions-box` = TRUE,
                `live-search` = TRUE,
                size = 20
            ),
            multiple = FALSE,
            selected = most_affected_countries(1)
        )
    })

    output$dateInput <- renderUI({
        dateRangeInput(
            'dateRange',
            label = 'Choose a time range',
            start = Sys.Date() - weeks(4),
            end = Sys.Date(),
            min = "2020-01-01",
            max = Sys.Date(),
            format = "dd/mm/yyyy",
            startview = 'week',
            language = 'fr',
            weekstart = 1
        )
    })
    output$dateInputNewCases <- renderUI({
        dateRangeInput(
            'dateRangeNewCases',
            label = 'Choose a time range',
            start = Sys.Date() - weeks(4),
            end = Sys.Date(),
            min = "2020-01-01",
            max = Sys.Date(),
            format = "dd/mm/yyyy",
            startview = 'week',
            language = 'fr',
            weekstart = 1
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
