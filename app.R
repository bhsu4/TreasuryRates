library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(lubridate)


# Define UI
ui <- fluidPage(
    list(tags$head(HTML('<link rel="icon", href="symetra-favicon.png", type="image/png" />'))),

    #Navbar structure for UI
    navbarPage(title=div(img(src="symetra-logo.png", style="margin-top: -12px; 
                             margin-left: -10px; padding-bottom: 10px", height = 50)),
               windowTitle="Treasure Hunt", theme = shinytheme("simplex"),
        tabPanel(HTML("Historical Treasury Yields"), fluid = TRUE, icon = icon("chart-line"),
            # Sidebar layout with a input and output definitions
            sidebarLayout(
                sidebarPanel(width = 3,
                   titlePanel(HTML("<h3>Choose Parameters</h3></br>")), 
                       fluidRow(column(width = 12,
                            tags$label("Select Duration"), 
                            fluidRow(
                                column(width = 4, 
                                       checkboxGroupInput(inputId = "historicalDurationA", 
                                                          label = NULL, 
                                                          choices = c("1-Month" = "X1.Mo", "6-Month" = "X6.Mo")
                                       )
                                ),
                                column(width = 4, 
                                       checkboxGroupInput(inputId = "historicalDurationB", 
                                                          label = NULL, 
                                                          choices = c("1-Year" = "X1.Yr", "5-Year" = "X5.Yr")
                                       )
                                ),
                                column(width = 4, 
                                       checkboxGroupInput(inputId = "historicalDurationC", 
                                                          label = NULL, 
                                                          choices = c("10-Year" = "X10.Yr", "30-Year" = "X30.Yr")
                                       )
                                )
                            ), hr(), textOutput("poop"),
                            sliderInput(inputId = "historicalYear",
                                        label = "Select Years",
                                        min = 2010, max = 2020,
                                        value = c(2010,2020)),
                            helpText(HTML("<p style = 'text-align: right'>Last update: 6/30/2020</p>")),
                            hr())
                       )
                ),
                mainPanel(
                    fluidRow(
                        column(width = 12, 
                            withSpinner(plotlyOutput(outputId = "historicalLP", height = 500))
                        )
                    ), hr(),
                    fluidRow(
                        column(width = 12,
                            wellPanel(fluidRow(align = "center", 
                                column(width = 3, 
                                    numericInput("input1", "Input Rate", value = 2)
                                ),
                                column(width = 3, 
                                    numericInput("input2", "Input Rate", value = 3)
                                ),
                                column(width = 3, 
                                    numericInput("input3", "Input Rate", value = 0)
                                ),
                                column(width = 3, 
                                    numericInput("input4", "Input Rate", value = 2.5)
                                )
                            ))
                        )
                    ) 
                )
            )
        ),
        tags$head(
            tags$style(type = 'text/css', 
                       HTML('.navbar-default .navbar-nav > .active >a:hover {color: #555;}
                             .navbar-default .navbar-nav > .active >a {color: #047cdc;}
                             .navbar-default .navbar-nav > .active >a:focus {color: #000}')
            )
        )
    )
)


                        
treasury_time <- function(t1, t2){
    treasury <- read.csv("TreasuryRates.csv")
    #convert to date format
    treasury$Date <- as.Date(treasury$Date, "%m/%d/%Y")
    treasury$year <- year(ymd(treasury$Date))
    treasury$month <- month(ymd(treasury$Date))
    treasury$day <- day(ymd(treasury$Date))
    #choose interval of interest
    return(treasury %>% filter(year >= t1 & year <= t2))
}

duration_names <- reactive({
    data.frame(variable = c("X1.Mo", "X2.Mo", "X3.Mo", "X6.Mo", "X1.Yr", "X2.Yr", 
                           "X3.Yr", "X5.Yr", "X7.Yr", "X10.Yr", "X20.Yr", "X30.Yr"),
               vartitle = c("1-Month", "2-Month", "3-Month", "6-Month", "1-Year", "2-Year", 
                            "3-Year", "5-Year", "7-Year", "10-Year", "20-Year", "30-Year"), 
               months = c(1, 2, 3, 6, 12, 24, 36, 60, 84, 120, 240, 360))
})

server <- function(input, output) {
    
    dur_selected <- reactive({
        x <- c(input$historicalDurationA, input$historicalDurationB, input$historicalDurationC)
    })
    
    treasury_res <- reactive({
        #get treasury time range
        treasury <- treasury_time(input$historicalYear[1], input$historicalYear[2])
        #select duration of interest
        treasury_select <- treasury %>% select(Date, X1.Mo, X6.Mo, X1.Yr, X5.Yr, X10.Yr, X20.Yr, X30.Yr) 
        treasury_select <- data.frame(Date = treasury_select$Date, 
                                      mutate_all(treasury_select[,-1], function(x) as.numeric(as.character(x))))
        #convert wide to long 
        treasury_long <- melt(treasury_select, id.vars = c("Date"))
        #get start of month treasury rates
        treasury_startmo <- treasury_long %>% mutate(year = year(Date), month = month(Date), 
                                                     day = day(Date), mon_yr = format(Date, "%Y-%m")) %>% 
                                group_by(mon_yr) %>% filter(Date == min(Date))
        return(list(treasury_long, treasury_startmo))
    })
    
    output$historicalLP <- renderPlotly({
        #get legend names 
        treasury_line <- merge(treasury_res()[[1]], duration_names(), by = "variable")
        treasury_point <- merge(treasury_res()[[2]], duration_names(), by = "variable")
        treasury_line <- treasury_line[order(treasury_line$Date),] #order to avoid line connectivity problems
        #treasury_point <- treasury_point[order(treasury_point$Date),] #order to avoid line connectivity problems
        #chosen durations
        chosen_duration <- duration_names()[duration_names()$vartitle %in% unique(treasury_line$vartitle),]
        #factorize the variables
        treasury_line$vartitle <- factor(treasury_line$vartitle, 
                                         levels = chosen_duration[order(chosen_duration$months),]$vartitle)
        

        #plotly output
        p <- plot_ly(type = 'scatter', mode = 'lines')
        for (i in chosen_duration$vartitle){
            p <- add_trace(p, data = treasury_line[treasury_line$vartitle == i,], 
                           legendgroup = ~vartitle, x = ~Date, y = ~value, 
                           type = 'scatter', mode = 'lines', 
                           color = ~vartitle, hoverinfo = 'none')
            p <- add_trace(p, data = treasury_point[treasury_point$vartitle == i,], 
                           legendgroup = ~vartitle, showlegend = F, 
                           color = ~vartitle, mode = 'markers',               
                           marker = list(symbol = "circle", size = 8), 
                           x = ~Date, y = ~value, 
                           hoverinfo = 'text', 
                           text = ~paste0('Date: ', Date, '</br></br>', 
                                          'Duration: ', vartitle, '</br>',
                                          'Rate: ', value, "%"))
        }
        p %>% layout(title = "Treasury Rates at Select Durations",
                     legend = list(orientation = "h", xanchor = "center", x = 0.5, font = list(size = 15)), 
                     font = list(size = 12), 
                     xaxis = list(title = "", size = 15, tickangle = 0), 
                     yaxis = list(title = "Treasury Rates", size = 8, tickangle = 0)) %>% 
            config(displaylogo = FALSE, modeBarButtonsToRemove = list("zoomIn2d", "zoomOut2d", "zoom2d", "autoScale2d", "resetScale2d", "select2d", 
                                                                      "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "pan2d"))
        
    })
    
    ###start of percentile computation 
    
    
    
    treasury_pct <- reactive({
        #get treasury time range
        treasury <- treasury_time(input$historicalYear[1], input$historicalYear[2])
        treasury_pct <- treasury %>% select(Date, c("X1.Mo", "X6.Mo", "X1.Yr", "X5.Yr"))
        #convert wide to long 
        treasury_long <- melt(treasury_pct, id.vars = c("Date"))
        #percentile calculation
        treasury_long <- treasury_long %>% group_by(variable) %>% mutate(PCT = ntile(value, 100))
        treasury_long <- treasury_long[order(treasury_long$Date),] #order to avoid line connectivity problems
        return(treasury_long)
    })
    

    
}

# Run the application 
shinyApp(ui = ui, server = server)
