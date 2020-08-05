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

#finding percentile of interests' treasury rates 
find_seqpercentiles <- function(dat){
  #quantile points
  my_quantiles <- c(0.01, seq(0.05, 1, by = 0.05))
  ind_q <- min(which(min(dat$percentile) <= my_quantiles))
  quantile_focus <- my_quantiles[ind_q:length(my_quantiles)]
  #order the data just in case
  dat <- dat[order(dat$percentile),]
  #calculate the rate
  res_p <- c()
  for(i in quantile_focus){
    #finding percentile point
    my_k <- max(which(i >= dat$percentile))
    if(my_k == length(dat$percentile)){
      res_p = c(res_p, dat[my_k,]$value)
    }
    else{
      pdiff_total <- dat[my_k+1,]$percentile - dat[my_k,]$percentile
      pdiff <- i - dat[my_k,]$percentile
      res_p <- c(res_p, pdiff/pdiff_total*as.numeric(dat[my_k+1,]$value) + (1-pdiff/pdiff_total)*as.numeric(dat[my_k,]$value))
    }
  }
  return(data.frame(perct = quantile_focus*100, rate = res_p))
}

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
                            hr())#, 
                            #tags$label("Input Percentile for Selected Duration(s)"),
                            #wellPanel(fluidRow(align = "center", uiOutput("inputPercentile"))
                            #), hr())
                       )
                ),
                mainPanel(
                    fluidRow(
                        column(width = 12, 
                            withSpinner(plotlyOutput(outputId = "historicalLP", height = 600))
                        )
                    ), hr()
                )
            )
        ),
        tabPanel(HTML("Percentile of Treasury Yields"), fluid = TRUE, icon = icon("percent"),
            fluidRow(
                column(width = 12,
                     fluidRow(HTML("<h3>Input Percentile for Treasury Rates of Select Durations</h3>"),
                          wellPanel(fluidRow(
                                column(width = 12, 
                                     column(width = 12, 
                                            sliderInput("percentileTime", "Select Time Interval", min = 2010, max = 2020, 
                                                        value = c(2010, 2020), step = 1))
                                ),
                                column(width = 12, 
                                     column(width = 2, numericInput("inputPercentile1", label = "Input Percentile (1-Month)", 
                                                                    value = 50, min = 1, max = 100, step = 1)),
                                     column(width = 2, numericInput("inputPercentile2", label = "Input Percentile (6-Month)", 
                                                                    value = 50, min = 1, max = 100, step = 1)), 
                                     column(width = 2, numericInput("inputPercentile3", label = "Input Percentile (1-Year)", 
                                                                    value = 50, min = 1, max = 100, step = 1)), 
                                     column(width = 2, numericInput("inputPercentile4", label = "Input Percentile (5-Year)", 
                                                                    value = 50, min = 1, max = 100, step = 1)), 
                                     column(width = 2, numericInput("inputPercentile5", label = "Input Percentile (10-Year)", 
                                                                    value = 50, min = 1, max = 100, step = 1)), 
                                     column(width = 2, numericInput("inputPercentile6", label = "Input Percentile (30-Year)", 
                                                                    value = 50, min = 1, max = 100, step = 1))
                                )
                            )
                         )
                     )
                )
            ), 
            fluidRow(tags$label("Short Durations"),
                     column(width = 12, 
                         column(width = 4, 
                                withSpinner(plotlyOutput("plotPercentile1", height = 300))), 
                         column(width = 4, 
                                withSpinner(plotlyOutput("plotPercentile2", height = 300))), 
                         column(width = 4, 
                                withSpinner(plotlyOutput("plotPercentile3", height = 300))),
                     )
            ), tags$br(), hr(), tags$br(),
            fluidRow(tags$label("Long Durations"),
                     column(width = 12, 
                        column(width = 4, 
                               withSpinner(plotlyOutput("plotPercentile4", height = 300))),
                        column(width = 4, 
                               withSpinner(plotlyOutput("plotPercentile5", height = 300))), 
                        column(width = 4, 
                               withSpinner(plotlyOutput("plotPercentile6", height = 300)))
                           
                           
                    )
                )
            
        ),
        #navbarMenu("More", icon = icon("info-circle"),
            tabPanel("About", fluid = TRUE, icon = icon("info-circle"),
                fluidRow(
                  column(6,
                         HTML(paste0("<h2><b>Treasury Yield Curves</b></h2>")), hr(),
                         HTML(paste0("<h4>The daily treasury yield curve rates is directly taken from the ", 
                              a("US Department of Treasury", href = "https://home.treasury.gov/policy-issues/financing-the-government/interest-rate-statistics"), 
                              ".", HTML("</br></br>"), "The US Department of Treasury explains the treasury yield curves as", HTML("</br></br>"),
                              shinydashboardPlus::blockQuote("[A curve that] relates the yield on a security to 
                                                               its time to maturity based on the closing market bid yields 
                                                               on actively traded Treasury securities in the over-the-counter 
                                                               market. These market yields are calculated from composites of 
                                                               quotations obtained by the Federal Reserve Bank of New York."))
                         ), HTML("</br>"),
                  HTML(paste0("<h2><b>How It Impacts Us</b></h2>")), hr(),
                              HTML(paste0("<h4> hi brad </h4>"))
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
    #)
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
    
    # Insert the right number of numericinput objects into the web page
    output$inputPercentile <- renderUI({
      numeric_input_list <- lapply(1:length(dur_selected()), function(i) {
        inputname <- paste("percentileInput", i, sep="")
        numericInput(inputId = inputname, label = paste(duration_names()[duration_names()$variable == dur_selected()[i],]$vartitle),
                     value = 50,
                     min = 1, max = 100,
                     width = "60%")
      })
      
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      #do.call(tagList, numeric_input_list)
    })
    
    treasury_res <- reactive({
        #get treasury time range
        treasury <- treasury_time(input$historicalYear[1], input$historicalYear[2])
        #select duration of interest
        treasury_select <- treasury %>% dplyr::select(Date, dur_selected()) 
        
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
        treasury_pct <- treasury %>% dplyr::select(Date, dur_selected())
        #convert wide to long , drop nas
        treasury_long <- melt(treasury_pct, id.vars = c("Date"))
        treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
        #percentile calculation
        treasury_long <- treasury_long %>% group_by(variable) %>% mutate(PCT = ntile(value, 100))
        treasury_long <- treasury_long %>% arrange(value, .by_group = TRUE) #order to avoid line connectivity problems
        return(treasury_long)
    })
    
#### historical percentile tab #########
    
    output$plotPercentile1 <- renderPlotly({
        treasury <- treasury_time(input$percentileTime[1], input$percentileTime[2])
        treasury_pct <- treasury %>% dplyr::select(Date, "X1.Mo")
        #convert wide to long , drop nas
        treasury_long <- melt(treasury_pct, id.vars = c("Date"))
        treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
        treasury_long2 <- treasury_long %>% group_by(variable) %>% 
                            mutate(count = seq(n()),  percentile = ecdf(value)(value)) %>% 
                            arrange(percentile)
        #finding percentile pooint
        my_k <- max(which(input$inputPercentile1/100 >= treasury_long2$percentile))
        pdiff_total <- treasury_long2[my_k+1,]$percentile - treasury_long2[my_k,]$percentile
        pdiff <- input$inputPercentile1/100 - treasury_long2[my_k,]$percentile
        res_p <- pdiff/pdiff_total*as.numeric(treasury_long2[my_k+1,]$value) + (1-pdiff/pdiff_total)*as.numeric(treasury_long2[my_k,]$value)
        
        #get the circular points for general percentiles 
        res_ps <- find_seqpercentiles(treasury_long2)
        
        p <- plot_ly(data = treasury_long2, 
                        legendgroup = "1-Month", x = ~percentile*100, y = ~value, 
                        type = 'scatter', mode = 'lines', 
                        color = "1-Month", hoverinfo = 'none') %>% 
                add_trace(p, data = res_ps, x = ~perct, y = ~rate, 
                          showlegend = F, mode = 'markers', marker = list(symbol = 'circle', size = 8), 
                          hoverinfo= 'text', 
                          text = ~paste0('Percentile: ', perct, '</br></br>', 
                                        'Duration: 1-Month', '</br>',
                                        'Rate: ', rate, "%")) %>% 
                add_trace(p, x = input$inputPercentile1, y = res_p, 
                          showlegend = F, 
                          mode = 'markers', marker = list(symbol = 'square', size = 8, color = "#000000"), 
                          hoverinfo= 'text', text = ~paste0('Percentile: ', input$inputPercentile1, '</br></br>', 
                                                            'Duration: 1-Month', '</br>',
                                                            'Rate: ', res_p, "%"))
        
        p %>% layout(title = "1-Month Treasury Rates",
                     legend = list(orientation = "h", xanchor = "center", x = 0.5, font = list(size = 15)), 
                     font = list(size = 12), 
                     xaxis = list(title = "Percentile", showgrid = FALSE, size = 8, tickangle = 0),
                     yaxis = list(title = "Treasury Rates", size = 8, tickangle = 0))%>% 
          config(displaylogo = FALSE, modeBarButtonsToRemove = list("zoomIn2d", "zoomOut2d", "zoom2d", "autoScale2d", "resetScale2d", "select2d", 
                                                                    "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "pan2d"))
    })
    
    output$plotPercentile2 <- renderPlotly({
      treasury <- treasury_time(input$percentileTime[1], input$percentileTime[2])
      treasury_pct <- treasury %>% dplyr::select(Date, "X6.Mo")
      #convert wide to long , drop nas
      treasury_long <- melt(treasury_pct, id.vars = c("Date"))
      treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
      treasury_long2 <- treasury_long %>% group_by(variable) %>% 
                          mutate(count = seq(n()),  percentile = ecdf(value)(value)) %>% 
                          arrange(percentile)
      #finding percentile point
      my_k <- max(which(input$inputPercentile2/100 >= treasury_long2$percentile))
      pdiff_total <- treasury_long2[my_k+1,]$percentile - treasury_long2[my_k,]$percentile
      pdiff <- input$inputPercentile2/100 - treasury_long2[my_k,]$percentile
      res_p <- pdiff/pdiff_total*as.numeric(treasury_long2[my_k+1,]$value) + (1-pdiff/pdiff_total)*as.numeric(treasury_long2[my_k,]$value)
      
      #get the circular points for general percentiles 
      res_ps <- find_seqpercentiles(treasury_long2)
      
      p <- plot_ly(data = treasury_long2, 
                   legendgroup = "6-Month", x = ~percentile*100, y = ~value, 
                   type = 'scatter', mode = 'lines', 
                   color = "1-Month", hoverinfo = 'none') %>% 
        add_trace(p, data = res_ps, x = ~perct, y = ~rate, 
                  showlegend = F, mode = 'markers', marker = list(symbol = 'circle', size = 8), 
                  hoverinfo= 'text', 
                  text = ~paste0('Percentile: ', perct, '</br></br>', 
                                 'Duration: 6-Month', '</br>',
                                 'Rate: ', rate, "%")) %>% 
        add_trace(p, x = input$inputPercentile2, y = res_p, 
                  showlegend = F, 
                  mode = 'markers', marker = list(symbol = 'square', size = 8, color = "#000000"), 
                  hoverinfo= 'text', text = ~paste0('Percentile: ', input$inputPercentile2, '</br></br>', 
                                                    'Duration: 6-Month', '</br>',
                                                    'Rate: ', res_p, "%"))
      p %>% layout(title = "6-Month Treasury Rates",
                   legend = list(orientation = "h", xanchor = "center", x = 0.5, font = list(size = 15)), 
                   font = list(size = 12), 
                   xaxis = list(title = "Percentile", showgrid = FALSE, size = 8, tickangle = 0),
                   yaxis = list(title = "Treasury Rates", size = 8, tickangle = 0))%>% 
        config(displaylogo = FALSE, modeBarButtonsToRemove = list("zoomIn2d", "zoomOut2d", "zoom2d", "autoScale2d", "resetScale2d", "select2d", 
                                                                  "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "pan2d"))
    })
 
    output$plotPercentile3 <- renderPlotly({
      treasury <- treasury_time(input$percentileTime[1], input$percentileTime[2])
      treasury_pct <- treasury %>% dplyr::select(Date, "X1.Yr")
      #convert wide to long , drop nas
      treasury_long <- melt(treasury_pct, id.vars = c("Date"))
      treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
      treasury_long2 <- treasury_long %>% group_by(variable) %>% 
        mutate(count = seq(n()),  percentile = ecdf(value)(value)) %>% 
        arrange(percentile)
      #finding percentile point
      my_k <- max(which(input$inputPercentile3/100 >= treasury_long2$percentile))
      pdiff_total <- treasury_long2[my_k+1,]$percentile - treasury_long2[my_k,]$percentile
      pdiff <- input$inputPercentile3/100 - treasury_long2[my_k,]$percentile
      res_p <- pdiff/pdiff_total*as.numeric(treasury_long2[my_k+1,]$value) + (1-pdiff/pdiff_total)*as.numeric(treasury_long2[my_k,]$value)
      
      #get the circular points for general percentiles 
      res_ps <- find_seqpercentiles(treasury_long2)
      
      p <- plot_ly(data = treasury_long2, 
                   legendgroup = "1-Year", x = ~percentile*100, y = ~value, 
                   type = 'scatter', mode = 'lines', 
                   color = "1-Year", hoverinfo = 'none') %>% 
        add_trace(p, data = res_ps, x = ~perct, y = ~rate, 
                  showlegend = F, mode = 'markers', marker = list(symbol = 'circle', size = 8), 
                  hoverinfo= 'text', 
                  text = ~paste0('Percentile: ', perct, '</br></br>', 
                                 'Duration: 1-Year', '</br>',
                                 'Rate: ', rate, "%")) %>% 
        add_trace(p, x = input$inputPercentile3, y = res_p, 
                  showlegend = F, 
                  mode = 'markers', marker = list(symbol = 'square', size = 8, color = "#000000"), 
                  hoverinfo= 'text', text = ~paste0('Percentile: ', input$inputPercentile3, '</br></br>', 
                                                    'Duration: 1-Year', '</br>',
                                                    'Rate: ', res_p, "%"))
      p %>% layout(title = "1-Year Treasury Rates",
                   legend = list(orientation = "h", xanchor = "center", x = 0.5, font = list(size = 15)), 
                   font = list(size = 12), 
                   xaxis = list(title = "Percentile", showgrid = FALSE, size = 8, tickangle = 0),
                   yaxis = list(title = "Treasury Rates", size = 8, tickangle = 0))%>% 
        config(displaylogo = FALSE, modeBarButtonsToRemove = list("zoomIn2d", "zoomOut2d", "zoom2d", "autoScale2d", "resetScale2d", "select2d", 
                                                                  "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "pan2d"))
    })
    
    output$plotPercentile4 <- renderPlotly({
      treasury <- treasury_time(input$percentileTime[1], input$percentileTime[2])
      treasury_pct <- treasury %>% dplyr::select(Date, "X5.Yr")
      #convert wide to long , drop nas
      treasury_long <- melt(treasury_pct, id.vars = c("Date"))
      treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
      treasury_long2 <- treasury_long %>% group_by(variable) %>% 
        mutate(count = seq(n()),  percentile = ecdf(value)(value)) %>% 
        arrange(percentile)
      #finding percentile point
      my_k <- max(which(input$inputPercentile4/100 >= treasury_long2$percentile))
      pdiff_total <- treasury_long2[my_k+1,]$percentile - treasury_long2[my_k,]$percentile
      pdiff <- input$inputPercentile4/100 - treasury_long2[my_k,]$percentile
      res_p <- pdiff/pdiff_total*as.numeric(treasury_long2[my_k+1,]$value) + (1-pdiff/pdiff_total)*as.numeric(treasury_long2[my_k,]$value)
      
      #get the circular points for general percentiles 
      res_ps <- find_seqpercentiles(treasury_long2)
      
      p <- plot_ly(data = treasury_long2, 
                   legendgroup = "5-Year", x = ~percentile*100, y = ~value, 
                   type = 'scatter', mode = 'lines', 
                   color = "5-Year", hoverinfo = 'none') %>% 
        add_trace(p, data = res_ps, x = ~perct, y = ~rate, 
                  showlegend = F, mode = 'markers', marker = list(symbol = 'circle', size = 8), 
                  hoverinfo= 'text', 
                  text = ~paste0('Percentile: ', perct, '</br></br>', 
                                 'Duration: 5-Year', '</br>',
                                 'Rate: ', rate, "%")) %>% 
        add_trace(p, x = input$inputPercentile4, y = res_p, 
                  showlegend = F, 
                  mode = 'markers', marker = list(symbol = 'square', size = 8, color = "#000000"), 
                  hoverinfo= 'text', text = ~paste0('Percentile: ', input$inputPercentile4, '</br></br>', 
                                                    'Duration: 5-Year', '</br>',
                                                    'Rate: ', res_p, "%"))
      p %>% layout(title = "5-Year Treasury Rates",
                   legend = list(orientation = "h", xanchor = "center", x = 0.5, font = list(size = 15)), 
                   font = list(size = 12), 
                   xaxis = list(title = "Percentile", showgrid = FALSE, size = 8, tickangle = 0),
                   yaxis = list(title = "Treasury Rates", size = 8, tickangle = 0))%>% 
        config(displaylogo = FALSE, modeBarButtonsToRemove = list("zoomIn2d", "zoomOut2d", "zoom2d", "autoScale2d", "resetScale2d", "select2d", 
                                                                  "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "pan2d"))
    })
    
    output$plotPercentile5 <- renderPlotly({
      treasury <- treasury_time(input$percentileTime[1], input$percentileTime[2])
      treasury_pct <- treasury %>% dplyr::select(Date, "X10.Yr")
      #convert wide to long , drop nas
      treasury_long <- melt(treasury_pct, id.vars = c("Date"))
      treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
      treasury_long2 <- treasury_long %>% group_by(variable) %>% 
        mutate(count = seq(n()),  percentile = ecdf(value)(value)) %>% 
        arrange(percentile)
      #finding percentile point
      my_k <- max(which(input$inputPercentile5/100 >= treasury_long2$percentile))
      pdiff_total <- treasury_long2[my_k+1,]$percentile - treasury_long2[my_k,]$percentile
      pdiff <- input$inputPercentile5/100 - treasury_long2[my_k,]$percentile
      res_p <- pdiff/pdiff_total*as.numeric(treasury_long2[my_k+1,]$value) + (1-pdiff/pdiff_total)*as.numeric(treasury_long2[my_k,]$value)
      
      #get the circular points for general percentiles 
      res_ps <- find_seqpercentiles(treasury_long2)
      
      p <- plot_ly(data = treasury_long2, 
                   legendgroup = "10-Year", x = ~percentile*100, y = ~value, 
                   type = 'scatter', mode = 'lines', 
                   color = "10-Year", hoverinfo = 'none') %>% 
        add_trace(p, data = res_ps, x = ~perct, y = ~rate, 
                  showlegend = F, mode = 'markers', marker = list(symbol = 'circle', size = 8), 
                  hoverinfo= 'text', 
                  text = ~paste0('Percentile: ', perct, '</br></br>', 
                                 'Duration: 10-Year', '</br>',
                                 'Rate: ', rate, "%")) %>% 
        add_trace(p, x = input$inputPercentile5, y = res_p, 
                  showlegend = F, 
                  mode = 'markers', marker = list(symbol = 'square', size = 8, color = "#000000"), 
                  hoverinfo= 'text', text = ~paste0('Percentile: ', input$inputPercentile5, '</br></br>', 
                                                    'Duration: 10-Year', '</br>',
                                                    'Rate: ', res_p, "%"))
      p %>% layout(title = "10-Year Treasury Rates",
                   legend = list(orientation = "h", xanchor = "center", x = 0.5, font = list(size = 15)), 
                   font = list(size = 12), 
                   xaxis = list(title = "Percentile", showgrid = FALSE, size = 8, tickangle = 0),
                   yaxis = list(title = "Treasury Rates", size = 8, tickangle = 0))%>% 
        config(displaylogo = FALSE, modeBarButtonsToRemove = list("zoomIn2d", "zoomOut2d", "zoom2d", "autoScale2d", "resetScale2d", "select2d", 
                                                                  "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "pan2d"))
    })
    
    output$plotPercentile6 <- renderPlotly({
      treasury <- treasury_time(input$percentileTime[1], input$percentileTime[2])
      treasury_pct <- treasury %>% dplyr::select(Date, "X30.Yr")
      #convert wide to long , drop nas
      treasury_long <- melt(treasury_pct, id.vars = c("Date"))
      treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
      treasury_long2 <- treasury_long %>% group_by(variable) %>% 
        mutate(count = seq(n()),  percentile = ecdf(value)(value)) %>% 
        arrange(percentile)
      #finding percentile point
      my_k <- max(which(input$inputPercentile6/100 >= treasury_long2$percentile))
      pdiff_total <- treasury_long2[my_k+1,]$percentile - treasury_long2[my_k,]$percentile
      pdiff <- input$inputPercentile6/100 - treasury_long2[my_k,]$percentile
      res_p <- pdiff/pdiff_total*as.numeric(treasury_long2[my_k+1,]$value) + (1-pdiff/pdiff_total)*as.numeric(treasury_long2[my_k,]$value)
      
      #get the circular points for general percentiles 
      res_ps <- find_seqpercentiles(treasury_long2)
      
      p <- plot_ly(data = treasury_long2, 
                   legendgroup = "30-Year", x = ~percentile*100, y = ~value, 
                   type = 'scatter', mode = 'lines', 
                   color = "30-Year", hoverinfo = 'none') %>% 
        add_trace(p, data = res_ps, x = ~perct, y = ~rate, 
                  showlegend = F, mode = 'markers', marker = list(symbol = 'circle', size = 8), 
                  hoverinfo= 'text', 
                  text = ~paste0('Percentile: ', perct, '</br></br>', 
                                 'Duration: 30-Year', '</br>',
                                 'Rate: ', rate, "%")) %>% 
        add_trace(p, x = input$inputPercentile6, y = res_p, 
                  showlegend = F, 
                  mode = 'markers', marker = list(symbol = 'square', size = 8, color = "#000000"), 
                  hoverinfo= 'text', text = ~paste0('Percentile: ', input$inputPercentile6, '</br></br>', 
                                                    'Duration: 30-Year', '</br>',
                                                    'Rate: ', res_p, "%"))
      p %>% layout(title = "30-Year Treasury Rates",
                   legend = list(orientation = "h", xanchor = "center", x = 0.5, font = list(size = 15)), 
                   font = list(size = 12), 
                   xaxis = list(title = "Percentile", showgrid = FALSE, size = 8, tickangle = 0),
                   yaxis = list(title = "Treasury Rates", size = 8, tickangle = 0))%>% 
        config(displaylogo = FALSE, modeBarButtonsToRemove = list("zoomIn2d", "zoomOut2d", "zoom2d", "autoScale2d", "resetScale2d", "select2d", 
                                                                  "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "pan2d"))
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
