#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("dependencies.R")
library("shiny")
library("shinyWidgets")
library("shinythemes")
library("plotly")
source("data.R")
# dataframes df, df_a
# vector current_year, current_month, countries, date

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage(
                    "Covid-tracker",
                    tabPanel(
                        "Plot",
                        fluidRow(
                            column(
                                3,
                                # offset = '0.2',
                                # align = 'center',
                                radioButtons(
                                    "plotType",
                                    label = h4("Graph type"),
                                    choices = list(
                                        "Active Cases" = 'Active.Cases',
                                        "Deaths" = 'Deaths',
                                        "Recovered" = 'Recovered',
                                        "Confirmed" = "Confirmed"
                                    ),
                                    selected = 'Active.Cases'
                                )
                            ),
                            column(3,
                                   h4("Countries"),
                                   uiOutput("countryPicker")),
                            column(3,
                                   h4("Date"),
                                   uiOutput("dateInput"))
                        ),
                        plotlyOutput("plot1")
                    ),
                    tabPanel("Table",
                             mainPanel(
                                 h4("Table"),
                                 selectInput("inputdate", "Choose a Date:",
                                             choices = dates),
                                 DT::dataTableOutput("df_jour")
                             )),
                    tabPanel(
                        "Map",
                        fluidRow(
                            column(2,
                                   radioButtons(
                            "mapType",
                            label = h4("Graph type"),
                            choices = list(
                                "Active Cases" = 'Active.Cases',
                                "Deaths" = 'Deaths',
                                "Recovered" = 'Recovered',
                                "Confirmed" = "Confirmed"
                            ),
                            selected = 'Active.Cases'
                        )),
                        column(2,
                               selectInput("mapinputdate", "Choose a Date:",
                                    choices = dates)
                               )
                        ),
                        mainPanel(leafletOutput("plotmap", width=1200, height=600), p())
                    )
                ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    source("reactive_components.R", local = TRUE)
    output$plot1 <- renderPlotly({
        plot1()
    })
    output$plot2 <- renderPlotly({
        plot2()
    })
    output$df_jour <- DT::renderDataTable({
        df_jour_reactive()
    })
    output$plotmap <- renderLeaflet({
        plotmap()
    })

        output$countryPicker <- renderUI({
            pickerInput(
                inputId = "countries",
                label = "Select one or more",
                choices = countries,
                options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    size = 20
                ),
                multiple = TRUE,
                selected = most_active_countries(15)
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
    }

    # Run the application
    shinyApp(ui = ui, server = server)