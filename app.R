library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(lubridate)
library(plotly)
library(shinycssloaders)
library(reshape2)
library(RColorBrewer)


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
                                                          choices = c("1-Month" = "X1.Mo", "6-Month" = "X6.Mo"),
                                                          selected = "X1.Mo"
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
                            ), hr(),
                            sliderInput(inputId = "historicalYear",
                                        label = "Select Years",
                                        min = 2010, max = 2020,
                                        value = c(2010,2020)),
                            helpText(HTML("<p style = 'text-align: right'>Last update: 6/30/2020</p>")),
                            hr(), 
                            textOutput("poop2"))
                       )
                ),
                mainPanel(
                    fluidRow(
                        column(width = 12, 
                            withSpinner(plotlyOutput(outputId = "historicalLP", height = 400))
                        ), hr(),
                        column(width = 12, 
                            withSpinner(plotlyOutput(outputId = "percentileLP", height = 500))
                        )
                    ), hr()
                )
            )
        ),
        tabPanel(HTML("Percentile of Treasury Yields"), fluid = TRUE, icon = icon("percent"),
            fluidRow(
                column(width = 12,
                     fluidRow(tags$label("Input Treasury Rate for Selected Duration(s)"),
                          wellPanel(fluidRow(
                                column(width = 12, 
                                     column(width = 8, 
                                            sliderInput("percentileTime", "Select Time Interval", min = 2010, max = 2020, 
                                                        value = c(2010, 2020), step = 1), 
                                     column(width = 4, 
                                            helpText("Last Update: 6/30/2020")))
                                ),
                                column(width = 12, 
                                     column(width = 2, numericInput("inputPercentile1", label = "1-Month", value = 0)), 
                                     column(width = 2, numericInput("inputPercentile2", label = "6-Month", value = 0)), 
                                     column(width = 2, numericInput("inputPercentile3", label = "1-Year", value = 0)), 
                                     column(width = 2, numericInput("inputPercentile4", label = "5-Year", value = 0)), 
                                     column(width = 2, numericInput("inputPercentile5", label = "10-Year", value = 0)), 
                                     column(width = 2, numericInput("inputPercentile6", label = "30-Year", value = 0))
                                )
                            )
                         )
                     )
                )
            ), 
            fluidRow(
                mainPanel(
                       fluidRow(
                           column(width = 12, 
                               column(width = 4, 
                                      withSpinner(plotlyOutput("plotPercentile1", height = 400))), 
                               column(width = 4, 
                                      withSpinner(plotlyOutput("plotPercentile2", height = 400))), 
                               column(width = 4, 
                                      withSpinner(plotlyOutput("plotPercentile3", height = 400))),
                           ),
                           column(width = 12, 
                              column(width = 4, 
                                     withSpinner(plotlyOutput("plotPercentile4", height = 400))),
                              column(width = 4, 
                                     withSpinner(plotlyOutput("plotPercentile5", height = 400))), 
                              column(width = 4, 
                                     withSpinner(plotlyOutput("plotPercentile6", height = 400)))
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

server <- function(session, input, output) {
    
    dur_selected <- reactive({
        x <- c(input$historicalDurationA, input$historicalDurationB, input$historicalDurationC)
    })
    
    treasury_res <- reactive({
        #get treasury time range
        treasury <- treasury_time(input$historicalYear[1], input$historicalYear[2])
        #select duration of interest
        treasury_select <- treasury %>% select(Date, dur_selected()) 
        
        treasury_select <- mutate_all(treasury_select, function(x) as.numeric(as.character(x)))
        treasury_select$Date <- treasury$Date
        #convert wide to long 
        treasury_long <- melt(treasury_select, id.vars = c("Date"))
        #get start of month treasury rates
        treasury_startmo <- treasury_long %>% mutate(year = year(Date), month = month(Date), 
                                                     day = day(Date), mon_yr = format(Date, "%Y-%m")) %>% 
                                group_by(mon_yr) %>% filter(Date == min(Date))
        return(list(treasury_long, treasury_startmo))
    })
    
    output$historicalLP <- renderPlotly({
        
        shiny::validate(
            need(length(dur_selected()) >0, "Please Choose a Duration")
        )
        
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
        treasury_pct <- treasury %>% select(Date, dur_selected())
        #convert wide to long , drop nas
        treasury_long <- melt(treasury_pct, id.vars = c("Date"))
        treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
        #percentile calculation
        treasury_long <- treasury_long %>% group_by(variable) %>% mutate(PCT = ntile(value, 100))
        treasury_long <- treasury_long %>% arrange(value, .by_group = TRUE) #order to avoid line connectivity problems
        return(treasury_long)
    })
    
    output$percentileLP <- renderPlotly({
        
        shiny::validate(
            need(length(dur_selected()) >0, "Please Choose a Duration")
        )
        
        #get legend names 
        treasury_long2 <- treasury_pct() %>% arrange(value, .by_group = TRUE) %>% mutate(count = seq(n()))
        treasury_long2 <- cbind(treasury_long2, num = 1:nrow(treasury_long2))
        #add vartitle from merging
        treasury_long2 <- merge(treasury_long2, duration_names(), by = "variable")
        #get treasury points
        treasury_point <- treasury_long2 %>% filter(PCT %in% seq(0, 100, 1)) %>% group_by(vartitle, PCT) %>% 
                                filter(value == max(value)) %>% distinct(PCT, .keep_all = TRUE)
        #get treasury points 
        treasury_labels <- treasury_point[treasury_point$PCT %in% c(1, seq(5, 100, 5)),]
        treasury_labels <- treasury_labels %>% mutate(labels = PCT) 
        treasury_labels[which(treasury_labels$PCT == 100), ]$labels <- ""
        
        #plotting
        p <- plot_ly(type = 'scatter', mode = 'lines')
        for (i in dur_selected()){
            
            #start plotting
            p <- add_trace(p, data = treasury_long2[treasury_long2$variable == i,], 
                           legendgroup = ~vartitle, x = ~num, y = ~value, 
                           type = 'scatter', mode = 'lines', 
                           color = ~vartitle, hoverinfo = 'none')
            p <- add_trace(p, data = treasury_point[treasury_point$variable == i,], 
                           legendgroup = ~vartitle, showlegend = F, 
                           color = ~vartitle, mode = 'markers',               
                           marker = list(symbol = "circle", size = 8), 
                           x = ~num, y = ~value, 
                           hoverinfo = 'text', 
                           text = ~paste0('Percentile: ', PCT, '</br></br>', 
                                          'Duration: ', vartitle, '</br>',
                                          'Rate: ', value, "%"))

        }
        p %>% layout(title = "Treasury Rates at Select Durations",
                     legend = list(orientation = "h", xanchor = "center", x = 0.5, font = list(size = 15)), 
                     font = list(size = 12), 
                     xaxis = list(title = "", showgrid = TRUE, #showticklabels = FALSE), 
                                  tickvals = ~treasury_labels$num, ticktext = ~treasury_labels$labels, 
                                  size = 8, tickangle = 0),
                     yaxis = list(title = "Treasury Rates", size = 8, tickangle = 0))%>% 
            config(displaylogo = FALSE, modeBarButtonsToRemove = list("zoomIn2d", "zoomOut2d", "zoom2d", "autoScale2d", "resetScale2d", "select2d", 
                                                                      "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "pan2d"))
    })
    
    

    output$poop <- renderText(percentileInputs())
    
    observe({
        time1 <- input$percentileTime[1]
        time2 <- input$percentileTime[2]
        #update numeric inputs based on time year selected
        updateNumericInput(session, "inputPercentile1", label = "1-Month", 
                           min = min(as.numeric(treasury_time(time1, time2)$X1.Mo)), 
                           max = max(as.numeric(treasury_time(time1, time2)$X1.Mo)))
        updateNumericInput(session, "inputPercentile2", label = "6-Month", 
                           min = min(as.numeric(treasury_time(time1, time2)$X6.Mo)), 
                           max = max(as.numeric(treasury_time(time1, time2)$X6.Mo)))
        updateNumericInput(session, "inputPercentile3", label = "1-Year", 
                           min = min(as.numeric(treasury_time(time1, time2)$X1.Yr)), 
                           max = max(as.numeric(treasury_time(time1, time2)$X1.Yr)))
        updateNumericInput(session, "inputPercentile4", label = "5-Year", 
                           min = min(as.numeric(treasury_time(time1, time2)$X5.Yr)), 
                           max = max(as.numeric(treasury_time(time1, time2)$X5.Yr)))
        updateNumericInput(session, "inputPercentile5", label = "10-Year", 
                           min = min(as.numeric(treasury_time(time1, time2)$X10.Yr)), 
                           max = max(as.numeric(treasury_time(time1, time2)$X10.Yr)))
        updateNumericInput(session, "inputPercentile6", label = "30-Year", 
                           min = min(as.numeric(treasury_time(time1, time2)$X30.Yr)), 
                           max = max(as.numeric(treasury_time(time1, time2)$X30.Yr)))
    })
  
    

    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
