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

find_seqcurr <- function(current_point, dat){
  if(current_point <= as.numeric(min(dat$value))){
    perct_x = min(dat[which(dat$value == as.numeric(min(dat$value))),]$percentile)
  }
  else if(current_point >= as.numeric(max(dat$value))){
    perct_x = 1
  }
  else{
    my_k <- min(which(current_point <= dat$value))
    total_diff = as.numeric(dat[my_k,]$value)-as.numeric(dat[my_k-1,]$value)
    curr_diff <- current_point-as.numeric(dat[my_k-1,]$value)
    perct_x = curr_diff/total_diff*as.numeric(dat[my_k,]$percentile) +
                (1-curr_diff/total_diff)*as.numeric(dat[my_k-1,]$percentile)
  }
  return(data.frame(perct = perct_x, rate = current_point))
}

find_ratecurr <- function(current_point, dat){
  if(current_point/100 <= as.numeric(min(dat$percentile)))
    rate_x = min(dat[which(dat$percentile == as.numeric(min(dat$percentile))),]$value)
  else if(current_point/100 >= as.numeric(max(dat$percentile))){
    rate_x = max(dat[which(dat$percentile == as.numeric(max(dat$percentile))),]$value)
  }
  else{
    my_k <- max(which(current_point/100 >= dat$percentile))
    pdiff_total <- dat[my_k+1,]$percentile - dat[my_k,]$percentile
    pdiff <- current_point/100 - dat[my_k,]$percentile
    rate_x <- pdiff/pdiff_total*as.numeric(dat[my_k+1,]$value) + (1-pdiff/pdiff_total)*as.numeric(dat[my_k,]$value)
  }
  return(data.frame(perct = current_point, rate = as.numeric(as.character(rate_x))))
}

# Define UI
ui <- fluidPage(
    list(tags$head(HTML('<link rel="icon", href="symetra-favicon.png", type="image/png" />'))),

    #Navbar structure for UI
    navbarPage(title=div(img(src="symetra-logo.png", style="margin-top: -12px; 
                             margin-left: -10px; padding-bottom: 10px", height = 50)),
               windowTitle="Treasure Hunt", theme = shinytheme("simplex"),
        tabPanel(HTML("Input Treasury Yields"), fluid = TRUE, icon = icon("search"),
            fluidRow(
                column(width = 12,
                     fluidRow(HTML("<h3>Choose Your Inputs for Select Durations</h3>"),
                          wellPanel(fluidRow(
                                      column(width = 12, 
                                           column(width = 12, 
                                                  sliderInput("percentileTime", "Select Time Interval", min = 1990, max = 2020, 
                                                              value = c(1990, 2020), step = 1, sep = ""))
                                      )
                                    ), HTML("<br>"),
                                   
                                    
                                    fluidRow(column(width = 12,
                                          fluidRow(column(width = 9, 
                                                fluidRow( HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp <b> Input Treasury Rate of Interest (in %)</b><br></br>"),
                                                          tags$style("[type = 'number'] {font-size:12px;}"),
                                                          
                                                          column(width = 12, 
                                                                 column(width = 2, numericInput("inputRate1", label = "1-Month", 
                                                                                                value = 0.1132, min = 0, max = 5, step = 0.01)),
                                                                 column(width = 2, numericInput("inputRate2", label = "6-Month", 
                                                                                                value = 0.181, min = 0, max = 5, step = 0.01)), 
                                                                 column(width = 2, numericInput("inputRate3", label = "1-Year", 
                                                                                                value = 0.288, min = 0, max = 5, step = 0.01)), 
                                                                 column(width = 2, numericInput("inputRate4", label = "5-Year", 
                                                                                                value = 1.595, min = 0, max = 5, step = 0.01)), 
                                                                 column(width = 2, numericInput("inputRate5", label = "10-Year", 
                                                                                                value = 2.311, min = 0, max = 5, step = 0.01)), 
                                                                 column(width = 2, numericInput("inputRate6", label = "30-Year", 
                                                                                                value = 3.02, min = 0, max = 5, step = 0.01)), 
                                                          )
                                                ),  
                                                fluidRow( HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp Percentiles for Input Treasury Rates <br></br>"),
                                                          column(width = 12, 
                                                                 column(width = 2, htmlOutput("outputRate1", container = pre)),
                                                                 column(width = 2, htmlOutput("outputRate2", container = pre)),
                                                                 column(width = 2, htmlOutput("outputRate3", container = pre)),
                                                                 column(width = 2, htmlOutput("outputRate4", container = pre)),
                                                                 column(width = 2, htmlOutput("outputRate5", container = pre)),
                                                                 column(width = 2, htmlOutput("outputRate6", container = pre)),
                                                          ), 
                                                          
                                                ), style='.small-box margin: 0px;border:2px solid;border-radius:5px;padding: 10px; 
                                                          border-color:#DCDCDC; border-spacing: 2px; margin-left:50px; font-size: 12px;'), 
                                         
                                         fluidRow(column(width = 2,
                                                         fluidRow( HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp <b> Weighted Index </b><br></br>"),
                                                                   column(width = 12, tags$label(style = 'display;table-cell; vertical-align:middle; margin-left: 15px;', "Weighted Rate"),
                                                                          column(width = 12, htmlOutput("outputRateWeight", container = pre))
                                                                   )
                                                         ), HTML("</br>"),
                                                         fluidRow( tags$label(style = 'display;table-cell; vertical-align:middle; margin-left: 30px;', "Percentile at Weighted Rate"),
                                                                   column(width = 12, 
                                                                          column(width = 12, htmlOutput("outputRateWeightP", container = pre))
                                                                   )
                                                         ), style='.small-box margin: 0px;border:2px solid;border-radius:5px;padding: 10px; 
                                                                    border-color:#DCDCDC; border-spacing: 2px; margin-left:10px; font-size: 12px;'))
                                          )
                                     )
                                )
                           ) #wellpanel
                     )
                )
            ), 
            fluidRow(column(width = 12, withSpinner(plotlyOutput("plotRate1", height = 650)))), hr()
        ), 
        
        tabPanel(HTML("Input Percentile of Treasury Yields"), fluid = TRUE, icon = icon("percent"),
                 fluidRow(
                   column(width = 12,
                          fluidRow(HTML("<h3>Choose Your Inputs for Select Durations</h3>"),
                                   wellPanel(fluidRow(
                                     column(width = 12, 
                                            column(width = 12, 
                                                   sliderInput("percentileTime2", "Select Time Interval", min = 1990, max = 2020, 
                                                               value = c(1990, 2020), step = 1, sep = ""))
                                     )
                                   ), HTML("<br>"),
                                   fluidRow(column(width = 12,
                                                   fluidRow(column(width = 9,
                                                                   fluidRow(HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp <b> Input Percentiles of Interest</b><br></br>"),
                                                                            column(width = 12, 
                                                                                   column(width = 2, numericInput("inputPercentile1", label = "1-Month", 
                                                                                                                  value = 50, min = 1, max = 100, step = 1)),
                                                                                   column(width = 2, numericInput("inputPercentile2", label = "6-Month", 
                                                                                                                  value = 50, min = 1, max = 100, step = 1)), 
                                                                                   column(width = 2, numericInput("inputPercentile3", label = "1-Year", 
                                                                                                                  value = 50, min = 1, max = 100, step = 1)), 
                                                                                   column(width = 2, numericInput("inputPercentile4", label = "5-Year", 
                                                                                                                  value = 50, min = 1, max = 100, step = 1)), 
                                                                                   column(width = 2, numericInput("inputPercentile5", label = "10-Year", 
                                                                                                                  value = 50, min = 1, max = 100, step = 1)), 
                                                                                   column(width = 2, numericInput("inputPercentile6", label = "30-Year", 
                                                                                                                  value = 50, min = 1, max = 100, step = 1))
                                                                            )
                                                                   ), 
                                                                   fluidRow(HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp Rates for Input Percentiles <br></br>"),
                                                                            column(width = 12, 
                                                                                   column(width = 2, div(
                                                                                     #tags$b("Treasury Rate"),
                                                                                     htmlOutput("outputPercentile1", container = pre)
                                                                                   )),
                                                                                   column(width = 2, htmlOutput("outputPercentile2", container = pre)),
                                                                                   column(width = 2, htmlOutput("outputPercentile3", container = pre)),
                                                                                   column(width = 2, htmlOutput("outputPercentile4", container = pre)),
                                                                                   column(width = 2, htmlOutput("outputPercentile5", container = pre)),
                                                                                   column(width = 2, htmlOutput("outputPercentile6", container = pre))
                                                                            ) 
                                                                   ),style='margin-bottom:5px;border:2px solid;border-radius:5px;padding: 10px; 
                                            border-color:#DCDCDC; border-spacing: 2px;margin-left:50px; font-size:12px'), 
                                                            fluidRow(column(width = 2,
                                                                            fluidRow( HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp <b> Weighted Index </b><br></br>"),
                                                                                      column(width = 12, tags$label(style = 'display;table-cell; vertical-align:middle; margin-left: 15px;', "Weighted Percentiles"),
                                                                                             column(width = 12, htmlOutput("outputPercentileWeight", container = pre))
                                                                                      )
                                                                            ), HTML("</br>"),
                                                                            fluidRow( tags$label(style = 'display;table-cell; vertical-align:middle; margin-left: 30px;', "Index at Weighted Percentile"),
                                                                                      column(width = 12, 
                                                                                             column(width = 12, htmlOutput("outputPercentileWeightR", container = pre))
                                                                                      )
                                                                            ), style='.small-box margin: 0px;border:2px solid;border-radius:5px;padding: 10px; 
                                                      border-color:#DCDCDC; border-spacing: 2px; margin-left:10px; font-size: 12px;'))))))))),
                 fluidRow(tags$label("Short Durations"),
                          column(width = 12, 
                                 column(width = 4, 
                                        withSpinner(plotlyOutput("plotPercentile1", height = 300))), 
                                 column(width = 4, 
                                        withSpinner(plotlyOutput("plotPercentile2", height = 300))), 
                                 column(width = 4, 
                                        withSpinner(plotlyOutput("plotPercentile3", height = 300))),
                          )
                 ), hr(), 
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
        tabPanel(HTML("Historical Treasury Yields"), fluid = TRUE, icon = icon("chart-line"),
             # Sidebar layout with a input and output definitions
             sidebarLayout(
               sidebarPanel(width = 3,
                            titlePanel(HTML("<h3>Choose Parameters</h3></br>")), 
                            fluidRow(column(width = 12,
                                            tags$label("Select Duration"), 
                                            fluidRow(tags$style("#historicalDurationA { font-size:12px;height:50px;}"), 
                                                     tags$style("#historicalDurationB { font-size:12px;height:50px;}"),
                                                     tags$style("#historicalDurationC { font-size:12px;height:50px;}"),
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
                                                        min = 1990, max = 2020,
                                                        value = c(1990,2020), sep = ""),
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
                          withSpinner(plotlyOutput(outputId = "historicalLP", height = 850))
                   )
                 ), hr()
               )
             )
        ),
        #navbarMenu("More", icon = icon("info-circle"),
            tabPanel("About", fluid = TRUE, icon = icon("info-circle"),
                fluidRow(
                  column(6, 
                         HTML(paste0("<h2><b>How It Impacts Us</b></h2>")), hr(),
                         HTML(paste0("<h4> The Asset Adequacy Analysis under the U.S. Standard Valuation Law 
                                          and Valuation Manual requires the appointed actuary within an insurance 
                                          company to make an actuarial opinion on whether assets backing reserves 
                                          are adequate under 'moderately adverse conditions'. Though deterministic 
                                          scenarios such as the New York 7 (NY7) are widely used to help decide 
                                          adequacy, much has changed since the introduction of these set of scenarios. 
                                          In fact, one major caveat is the different interest rate environment today, 
                                          compared to years ago. Thus, moderately adverse scenarios defined in the 
                                          NY7 may not appropriately portray the conditions today. <br></br>
                                          In January 2016, the SOA provided a refined version of moderately adverse 
                                          scenarios under a more modern context that would reflect the current market 
                                          environment. The Modern Deterministic Scenarios for Interest Rates (MDS) 
                                          consisted of a set of 16 scenarios, with 8 low-rate and 8 high-rate scenarios 
                                          each. This would be an upgrade over the 3 pop-up, 3 pop-down, and level 
                                          scenarios in the NY7. A more detailed description of the MDS scenarios can 
                                          be found ", a("here", href = "https://www.soa.org/globalassets/assets/files/research/projects/2017-modern-deterministic-scenarios.pdf"), 
                                     ". <br></br> Despite the work being done to create new measures to aid 
                                          insurance companies in Asset Adequacy Analysis, there is little formal 
                                          definition on what is considered a moderately adverse condition. If the 
                                          company decides to hold reserves excess of a moderately adverse scenario,
                                          there is an increase in liabilities or cost to this decision. The cash flow 
                                          testing reserve should be able to cover this obligation without creating a 
                                          considerable opportunity cost to this decision. <br></br> 
                                          The tool illustrated in this web application is neither a replacement to the 
                                          testing being done, nor a representation of what a moderately adverse scenario 
                                          is. However, it is intended to provide users with a flexible interface to get
                                          a rough idea of where current treasury yields are compared to its historicals.</h4>"))
                  ),
                  column(6,
                         HTML(paste0("<h2><b>Treasury Yield Curves</b></h2>")), hr(),
                         HTML(paste0("<h4>The daily treasury yield curve rates is directly taken from the ", 
                              a("US Department of Treasury", href = "https://home.treasury.gov/policy-issues/financing-the-government/interest-rate-statistics"), 
                              ".", HTML("</br></br>"), "The US Department of Treasury explains the treasury yield curves as", HTML("</br></br>"),
                              shinydashboardPlus::blockQuote("[A curve that] relates the yield on a security to 
                                                               its time to maturity based on the closing market bid yields 
                                                               on actively traded Treasury securities in the over-the-counter 
                                                               market. These market yields are calculated from composites of 
                                                               quotations obtained by the Federal Reserve Bank of New York."), 
                              "These yields calculate the amount of money an individual earns by owning different debt instruments
                              such as U.S. treasury bills, notes, bonds, or securities sold by the U.S. Department of Treasury.
                              The yield reflects the percentage earned (interest rate) on the investment when the government is borrowing
                              the money. The methodology to derive these daily treasury yields are from quasi-cubic hermite spline functions
                              that can be found ", a("here", href = "https://home.treasury.gov/policy-issues/financing-the-government/interest-rate-statistics/treasury-yield-curve-methodology"), 
                              ".")
                         )
                  )
              ), HTML("<br></br>"),
              fluidRow(
                column(6,
                       HTML(paste0("<h2><b>About the Web Application</b></h2>")), hr(),
                       HTML(paste0("<h4> The web application was built in R and RStudio with Shiny, along with many other packages. 
                                   For questions about the code, get in touch with me on ", 
                                   a("LinkedIn", href = "https://www.linkedin.com/in/benjamin-hsu-10b33a97/", target="_blank"), " or by email at ", 
                                   a("bh2722@columbia.edu", href = "mailto:bh2722@columbia.edu", target="_blank"), "."))
                )
              )
        )),
        tags$head(
          tags$style(type = 'text/css', 
                     HTML('.navbar-default .navbar-nav > .active >a:hover {color: #555;}
                                 .navbar-default .navbar-nav > .active >a {color: #047cdc;}
                                 .navbar-default .navbar-nav > .active >a:focus {color: #047cdc}
                          .column_w_bar {border-right-color: #eb4034;
                                         border-right-width: 1px;
                                         border-right-style: solid;}')
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
    #change to numeric
    for(i in 2:13){
      treasury[,i] <- as.numeric(as.character(treasury[,i]))
    }
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
    
    curr_point <- reactive({
        treasury <- read.csv("TreasuryRates.csv")
        #convert to date format
        treasury$Date <- as.Date(treasury$Date, "%m/%d/%Y")
        current <- treasury[which.max(treasury$Date),]
        return(current)
    })
    
    weighted_treasury <- reactive({
      treasury <- treasury_time(input$percentileTime[1], input$percentileTime[2])
      treasury <- treasury[,c("Date", "X1.Mo", "X6.Mo", "X1.Yr", "X5.Yr", "X10.Yr", "X30.Yr")]
      treasury$value <- treasury$X1.Mo*(1/12)+treasury$X6.Mo*(1/2)+treasury$X1.Yr*(1)+
                              treasury$X5.Yr*(5)+treasury$X10.Yr*(10)+treasury$X30.Yr*(30)
      treasury_f <- treasury[,c("Date", "value")]
      treasury_f <- treasury_f[!is.na(treasury_f$value),] %>% arrange(value) %>% 
                          mutate(count = seq(n()),  percentile = ecdf(value)(value))
      return(treasury_f)
    })
    
    weighted_treasury2 <- reactive({
      treasury <- treasury_time(input$percentileTime2[1], input$percentileTime2[2])
      treasury <- treasury[,c("Date", "X1.Mo", "X6.Mo", "X1.Yr", "X5.Yr", "X10.Yr", "X30.Yr")]
      treasury$value <- treasury$X1.Mo*(1/12)+treasury$X6.Mo*(1/2)+treasury$X1.Yr*(1)+
        treasury$X5.Yr*(5)+treasury$X10.Yr*(10)+treasury$X30.Yr*(30)
      treasury_f <- treasury[,c("Date", "value")]
      treasury_f <- treasury_f[!is.na(treasury_f$value),] %>% arrange(value) %>% 
        mutate(count = seq(n()),  percentile = ecdf(value)(value))
      return(treasury_f)
    })
    
    output$plotRate1 <- renderPlotly({
        treasury <- treasury_time(input$percentileTime[1], input$percentileTime[2])
        treasury <- treasury[,c("Date", "X1.Mo", "X6.Mo", "X1.Yr", "X5.Yr", "X10.Yr", "X30.Yr")]
        treasury_long <- melt(treasury, id.vars = c("Date"))
        treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
        treasury_long2 <- treasury_long %>% group_by(variable) %>% arrange(value) %>% 
                               mutate(count = seq(n()),  percentile = ecdf(value)(value))
        #for each of the inputs
        res_curr1 <- find_seqcurr(input$inputRate1, treasury_long2[which(treasury_long2$variable == "X1.Mo"),])
        res_curr2 <- find_seqcurr(input$inputRate2, treasury_long2[which(treasury_long2$variable == "X6.Mo"),])
        res_curr3 <- find_seqcurr(input$inputRate3, treasury_long2[which(treasury_long2$variable == "X1.Yr"),])
        res_curr4 <- find_seqcurr(input$inputRate4, treasury_long2[which(treasury_long2$variable == "X5.Yr"),])
        res_curr5 <- find_seqcurr(input$inputRate5, treasury_long2[which(treasury_long2$variable == "X10.Yr"),])
        res_curr6 <- find_seqcurr(input$inputRate6, treasury_long2[which(treasury_long2$variable == "X30.Yr"),])
        #result of all the rates
        res_curall <- rbind(res_curr1, res_curr2, res_curr3, res_curr4, res_curr5, res_curr6)
        res_curall$name <- c("1-Month", "6-Month", "1-Year", "5-Year", "10-Year", "30-Year")
        res_curall$name <- factor(res_curall$name, levels = c("1-Month", "6-Month", "1-Year", "5-Year", "10-Year", "30-Year"))
        #find percentile for the weighted index 
        current_index <- sum(res_curall$rate*c(1/12, 1/2, 1, 5, 10, 30))
        res_indexp <- find_seqcurr(current_index, weighted_treasury())
        
        #update the output that shows percentile
        output$outputRate1 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr1$perct, 3)*100,  "</b>") })
        output$outputRate2 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr2$perct, 3)*100,  "</b>") })
        output$outputRate3 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr3$perct, 3)*100,  "</b>") })
        output$outputRate4 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr4$perct, 3)*100,  "</b>") })
        output$outputRate5 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr5$perct, 3)*100,  "</b>") })
        output$outputRate6 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr6$perct, 3)*100,  "</b>") })
        output$outputRateWeight <- renderText({ paste("<font color=\"#000000\"><b>", round(res_indexp$rate,3), "</b>") })
        output$outputRateWeightP <- renderText({ paste("<font color=\"#000000\"><b>", round(res_indexp$perct*100,3), "</b>") })
        
        ###plot the graph
        plot_ly(res_curall, x = ~name, y = ~rate, type = "scatter", mode = 'markers+lines', name = "Chosen Treasury Rates",
                hoverinfo= 'text', text = ~paste0('Rate: ', round(rate,3), "%", '</br></br>', 
                                                  'Duration: ', name, '</br>')) %>%
          add_trace(x = ~name, y = ~perct, mode = "markers+lines", name = "Percentiles of Chosen Treasury Rates", yaxis = "y2",
                    hoverinfo= 'text', line = list(dash = "dash"), text = ~paste0('Percentile: ', round(perct,3)*100, '</br></br>', 
                                                      'Duration: ', name, '</br>')) %>% 
          layout(yaxis2 = list(tickfont = list(color = 'black'), overlaying = "y", side = "right")) %>% 
          layout(title = "Input Rates for Select Durations", 
                 xaxis = list(title= ""), yaxis = list(title = "Treasury Rates"), 
                 yaxis2 = list(showline = FALSE, color = "black", overlaying = "y", side = "right", title = "Percentiles (100th)"),
                 legend = list(orientation = "h", xanchor = "center", x = 0.5, font = list(size = 15)),
                 margin = m <- list(r = 80)) %>% 
          config(displaylogo = FALSE, modeBarButtonsToRemove = list("zoomIn2d", "zoomOut2d", "zoom2d", "autoScale2d", "resetScale2d", "select2d", 
                                                                    "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "pan2d"))

    })

    output$plotPercentile1 <- renderPlotly({
        treasury <- treasury_time(input$percentileTime2[1], input$percentileTime2[2])
        treasury_pct <- treasury %>% dplyr::select(Date, "X1.Mo")
        #convert wide to long , drop nas
        treasury_long <- melt(treasury_pct, id.vars = c("Date"))
        treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
        treasury_long2 <- treasury_long %>% group_by(variable) %>% 
                            arrange(value) %>% 
                            mutate(count = seq(n()),  percentile = ecdf(value)(value)) 
        #finding percentile pooint
        curr_p <- input$inputPercentile1
        res_currp <- find_ratecurr(curr_p, treasury_long2)
        #most recent point 
        curr = as.numeric(as.character(curr_point()[,"X1.Mo"])) #current point despite chosen time range
        res_curr <- find_seqcurr(curr, treasury_long2)
        #get the circular points for general percentiles 
        res_ps <- find_seqpercentiles(treasury_long2) 
        
        output$outputPercentile1 <- renderText({ paste0("<font color=\"#000000\"><b>", round(res_currp$rate, 3), "%", "</b>") })
        
        p <- plot_ly(data = treasury_long2, 
                        legendgroup = "1-Month", x = ~percentile*100, y = ~value, 
                        type = 'scatter', mode = 'lines', 
                        color = "1-Month", hoverinfo = 'none') %>% 
                add_trace(p, data = res_ps, x = ~perct, y = ~rate, 
                          showlegend = F, mode = 'markers', marker = list(symbol = 'circle', size = 8), 
                          hoverinfo= 'text', 
                          text = ~paste0('Percentile: ', perct, '</br></br>', 
                                        'Duration: 1-Month', '</br>',
                                        'Rate: ', round(as.numeric(as.character(rate)),3), "%")) %>% 
                add_trace(p, x = res_currp$perct, y = res_currp$rate, 
                          showlegend = F, 
                          mode = 'markers', marker = list(symbol = 'circle', size = 10, color = "#aaaaaa"), 
                          hoverinfo= 'text', text = ~paste0('Percentile: ', round(res_currp$perct,3), '</br></br>', 
                                                            'Duration: 1-Month', '</br>',
                                                            'Rate: ', round(res_currp$rate, 3), "%")) %>% 
                add_trace(p, x = res_curr$perct*100, y = res_curr$rate, showlegend = F, 
                          mode = 'markers', marker = list(symbol = 'square', size = 10, color = "#000000"), 
                          hoverinfo= 'text', text = ~paste0('Percentile: ', round(res_curr$perct*100,3), '</br></br>', 
                                                            'Latest Date: ', curr_point()$Date, '</br>',
                                                            'Duration: 1-Month', '</br>',
                                                            'Rate: ', round(res_curr$rate,3), "%"))
        
        p %>% layout(title = "1-Month Treasury Rates",
                     legend = list(orientation = "h", xanchor = "center", x = 0.5, font = list(size = 15)), 
                     font = list(size = 12), 
                     xaxis = list(title = "Percentile", showgrid = FALSE, size = 8, tickangle = 0),
                     yaxis = list(title = "Treasury Rates", size = 8, tickangle = 0), 
                     annotations = list(x = res_curr$perct*100, y = res_curr$rate, text = curr_point()$Date, 
                                        xref = "x", yref = "y", showarrow = TRUE, arrowhead = 7, ax = 20, ay = -40))%>% 
          config(displaylogo = FALSE, modeBarButtonsToRemove = list("zoomIn2d", "zoomOut2d", "zoom2d", "autoScale2d", "resetScale2d", "select2d", 
                                                                    "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "pan2d"))
        
    })
    
    output$plotPercentile2 <- renderPlotly({
      treasury <- treasury_time(input$percentileTime2[1], input$percentileTime2[2])
      treasury_pct <- treasury %>% dplyr::select(Date, "X6.Mo")
      #convert wide to long , drop nas
      treasury_long <- melt(treasury_pct, id.vars = c("Date"))
      treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
      treasury_long2 <- treasury_long %>% group_by(variable) %>% arrange(value) %>% 
                          mutate(count = seq(n()),  percentile = ecdf(value)(value)) 
      #finding percentile pooint
      curr_p <- input$inputPercentile2
      res_currp <- find_ratecurr(curr_p, treasury_long2)
      #most recent point 
      curr = as.numeric(as.character(curr_point()[,"X6.Mo"])) #current point despite chosen time range
      res_curr <- find_seqcurr(curr, treasury_long2)
      #get the circular points for general percentiles 
      res_ps <- find_seqpercentiles(treasury_long2)
      
      output$outputPercentile2 <- renderText({ paste0("<font color=\"#000000\"><b>", round(res_currp$rate, 3), "%", "</b>") })
      
      p <- plot_ly(data = treasury_long2, 
                   legendgroup = "6-Month", x = ~percentile*100, y = ~value, 
                   type = 'scatter', mode = 'lines', 
                   color = "6-Month", hoverinfo = 'none') %>% 
        add_trace(p, data = res_ps, x = ~perct, y = ~rate, 
                  showlegend = F, mode = 'markers', marker = list(symbol = 'circle', size = 8), 
                  hoverinfo= 'text', 
                  text = ~paste0('Percentile: ', perct, '</br></br>', 
                                 'Duration: 6-Month', '</br>',
                                 'Rate: ', round(as.numeric(as.character(rate)),3), "%")) %>% 
        add_trace(p, x = res_currp$perct, y = res_currp$rate, 
                  showlegend = F, 
                  mode = 'markers', marker = list(symbol = 'circle', size = 10, color = "#aaaaaa"), 
                  hoverinfo= 'text', text = ~paste0('Percentile: ', round(res_currp$perct,3), '</br></br>', 
                                                    'Duration: 6-Month', '</br>',
                                                    'Rate: ', round(res_currp$rate, 3), "%")) %>% 
        add_trace(p, x = res_curr$perct*100, y = res_curr$rate, showlegend = F, 
                  mode = 'markers', marker = list(symbol = 'square', size = 10, color = "#000000"), 
                  hoverinfo= 'text', text = ~paste0('Percentile: ', round(res_curr$perct*100,3), '</br></br>', 
                                                    'Latest Date: ', curr_point()$Date, '</br>',
                                                    'Duration: 6-Month', '</br>',
                                                    'Rate: ', round(res_curr$rate,3), "%"))
      
      p %>% layout(title = "6-Month Treasury Rates",
                   legend = list(orientation = "h", xanchor = "center", x = 0.5, font = list(size = 15)), 
                   font = list(size = 12), 
                   xaxis = list(title = "Percentile", showgrid = FALSE, size = 8, tickangle = 0),
                   yaxis = list(title = "Treasury Rates", size = 8, tickangle = 0), 
                   annotations = list(x = res_curr$perct*100, y = res_curr$rate, text = curr_point()$Date, 
                                      xref = "x", yref = "y", showarrow = TRUE, arrowhead = 7, ax = 20, ay = -40)) %>% 
        config(displaylogo = FALSE, modeBarButtonsToRemove = list("zoomIn2d", "zoomOut2d", "zoom2d", "autoScale2d", "resetScale2d", "select2d", 
                                                                  "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "pan2d"))
      
    })
 
    output$plotPercentile3 <- renderPlotly({
      treasury <- treasury_time(input$percentileTime2[1], input$percentileTime2[2])
      treasury_pct <- treasury %>% dplyr::select(Date, "X1.Yr")
      #convert wide to long , drop nas
      treasury_long <- melt(treasury_pct, id.vars = c("Date"))
      treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
      treasury_long2 <- treasury_long %>% group_by(variable) %>% arrange(value) %>% 
        mutate(count = seq(n()),  percentile = ecdf(value)(value)) 
      #finding percentile pooint
      curr_p <- input$inputPercentile3
      res_currp <- find_ratecurr(curr_p, treasury_long2)
      #most recent point 
      curr = as.numeric(as.character(curr_point()[,"X1.Yr"])) #current point despite chosen time range
      res_curr <- find_seqcurr(curr, treasury_long2)
      #get the circular points for general percentiles 
      res_ps <- find_seqpercentiles(treasury_long2)
      
      output$outputPercentile3 <- renderText({ paste0("<font color=\"#000000\"><b>", round(res_currp$rate, 3), "%", "</b>") })
      
      p <- plot_ly(data = treasury_long2, 
                   legendgroup = "1-Year", x = ~percentile*100, y = ~value, 
                   type = 'scatter', mode = 'lines', 
                   color = "1-Year", hoverinfo = 'none') %>% 
        add_trace(p, data = res_ps, x = ~perct, y = ~rate, 
                  showlegend = F, mode = 'markers', marker = list(symbol = 'circle', size = 8), 
                  hoverinfo= 'text', 
                  text = ~paste0('Percentile: ', perct, '</br></br>', 
                                 'Duration: 1-Year', '</br>',
                                 'Rate: ', round(as.numeric(as.character(rate)),3), "%")) %>% 
        add_trace(p, x = res_currp$perct, y = res_currp$rate, 
                  showlegend = F, 
                  mode = 'markers', marker = list(symbol = 'circle', size = 10, color = "#aaaaaa"), 
                  hoverinfo= 'text', text = ~paste0('Percentile: ', round(res_currp$perct,3), '</br></br>', 
                                                    'Duration: 1-Year', '</br>',
                                                    'Rate: ', round(res_currp$rate, 3), "%")) %>% 
        add_trace(p, x = res_curr$perct*100, y = res_curr$rate, showlegend = F, 
                  mode = 'markers', marker = list(symbol = 'square', size = 10, color = "#000000"), 
                  hoverinfo= 'text', text = ~paste0('Percentile: ', round(res_curr$perct*100,3), '</br></br>', 
                                                    'Latest Date: ', curr_point()$Date, '</br>',
                                                    'Duration: 1-Year', '</br>',
                                                    'Rate: ', round(res_curr$rate,3), "%"))
      p %>% layout(title = "1-Year Treasury Rates",
                   legend = list(orientation = "h", xanchor = "center", x = 0.5, font = list(size = 15)), 
                   font = list(size = 12), 
                   xaxis = list(title = "Percentile", showgrid = FALSE, size = 8, tickangle = 0),
                   yaxis = list(title = "Treasury Rates", size = 8, tickangle = 0), 
                   annotations = list(x = res_curr$perct*100, y = res_curr$rate, text = curr_point()$Date, 
                                      xref = "x", yref = "y", showarrow = TRUE, arrowhead = 7, ax = 20, ay = -40))%>% 
        config(displaylogo = FALSE, modeBarButtonsToRemove = list("zoomIn2d", "zoomOut2d", "zoom2d", "autoScale2d", "resetScale2d", "select2d", 
                                                                  "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "pan2d"))
    })
    
    output$plotPercentile4 <- renderPlotly({
      treasury <- treasury_time(input$percentileTime2[1], input$percentileTime2[2])
      treasury_pct <- treasury %>% dplyr::select(Date, "X5.Yr")
      #convert wide to long , drop nas
      treasury_long <- melt(treasury_pct, id.vars = c("Date"))
      treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
      treasury_long2 <- treasury_long %>% group_by(variable) %>% arrange(value) %>% 
                            mutate(count = seq(n()),  percentile = ecdf(value)(value)) 
      #finding percentile pooint
      curr_p <- input$inputPercentile4
      res_currp <- find_ratecurr(curr_p, treasury_long2)
      #most recent point 
      curr = as.numeric(as.character(curr_point()[,"X5.Yr"])) #current point despite chosen time range
      res_curr <- find_seqcurr(curr, treasury_long2)
      #get the circular points for general percentiles 
      res_ps <- find_seqpercentiles(treasury_long2)
      
      output$outputPercentile4 <- renderText({ paste0("<font color=\"#000000\"><b>", round(res_currp$rate, 3), "%", "</b>") })
      
      p <- plot_ly(data = treasury_long2, 
                   legendgroup = "5-Year", x = ~percentile*100, y = ~value, 
                   type = 'scatter', mode = 'lines', 
                   color = "5-Year", hoverinfo = 'none') %>% 
        add_trace(p, data = res_ps, x = ~perct, y = ~rate, 
                  showlegend = F, mode = 'markers', marker = list(symbol = 'circle', size = 8), 
                  hoverinfo= 'text', 
                  text = ~paste0('Percentile: ', perct, '</br></br>', 
                                 'Duration: 5-Year', '</br>',
                                 'Rate: ', round(as.numeric(as.character(rate)),3), "%")) %>% 
        add_trace(p, x = res_currp$perct, y = res_currp$rate, 
                  showlegend = F, 
                  mode = 'markers', marker = list(symbol = 'circle', size = 10, color = "#aaaaaa"), 
                  hoverinfo= 'text', text = ~paste0('Percentile: ', round(res_currp$perct,3), '</br></br>', 
                                                    'Duration: 5-Year', '</br>',
                                                    'Rate: ', round(res_currp$rate, 3), "%")) %>% 
        add_trace(p, x = res_curr$perct*100, y = res_curr$rate, showlegend = F, 
                  mode = 'markers', marker = list(symbol = 'square', size = 10, color = "#000000"), 
                  hoverinfo= 'text', text = ~paste0('Percentile: ', round(res_curr$perct*100,3), '</br></br>', 
                                                    'Latest Date: ', curr_point()$Date, '</br>',
                                                    'Duration: 5-Year', '</br>',
                                                    'Rate: ', round(res_curr$rate,3), "%"))
      
      p %>% layout(title = "5-Year Treasury Rates",
                   legend = list(orientation = "h", xanchor = "center", x = 0.5, font = list(size = 15)), 
                   font = list(size = 12), 
                   xaxis = list(title = "Percentile", showgrid = FALSE, size = 8, tickangle = 0),
                   yaxis = list(title = "Treasury Rates", size = 8, tickangle = 0), 
                   annotations = list(x = res_curr$perct*100, y = res_curr$rate, text = curr_point()$Date, 
                                      xref = "x", yref = "y", showarrow = TRUE, arrowhead = 7, ax = 20, ay = -40))%>% 
        config(displaylogo = FALSE, modeBarButtonsToRemove = list("zoomIn2d", "zoomOut2d", "zoom2d", "autoScale2d", "resetScale2d", "select2d", 
                                                                  "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "pan2d"))
    })
    
    output$plotPercentile5 <- renderPlotly({
      treasury <- treasury_time(input$percentileTime2[1], input$percentileTime2[2])
      treasury_pct <- treasury %>% dplyr::select(Date, "X10.Yr")
      #convert wide to long , drop nas
      treasury_long <- melt(treasury_pct, id.vars = c("Date"))
      treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
      treasury_long2 <- treasury_long %>% group_by(variable) %>% arrange(value) %>% 
                           mutate(count = seq(n()),  percentile = ecdf(value)(value)) 
      #finding percentile pooint
      curr_p <- input$inputPercentile5
      res_currp <- find_ratecurr(curr_p, treasury_long2)
      #most recent point 
      curr = as.numeric(as.character(curr_point()[,"X10.Yr"])) #current point despite chosen time range
      res_curr <- find_seqcurr(curr, treasury_long2)
      #get the circular points for general percentiles 
      res_ps <- find_seqpercentiles(treasury_long2)
      
      output$outputPercentile5 <- renderText({ paste0("<font color=\"#000000\"><b>", round(res_currp$rate, 3), "%", "</b>") })
      
      p <- plot_ly(data = treasury_long2, 
                   legendgroup = "10-Year", x = ~percentile*100, y = ~value, 
                   type = 'scatter', mode = 'lines', 
                   color = "10-Year", hoverinfo = 'none') %>% 
        add_trace(p, data = res_ps, x = ~perct, y = ~rate, 
                  showlegend = F, mode = 'markers', marker = list(symbol = 'circle', size = 8), 
                  hoverinfo= 'text', 
                  text = ~paste0('Percentile: ', perct, '</br></br>', 
                                 'Duration: 10-Year', '</br>',
                                 'Rate: ', round(as.numeric(as.character(rate)),3), "%")) %>% 
        add_trace(p, x = res_currp$perct, y = res_currp$rate, 
                  showlegend = F, 
                  mode = 'markers', marker = list(symbol = 'circle', size = 10, color = "#aaaaaa"), 
                  hoverinfo= 'text', text = ~paste0('Percentile: ', round(res_currp$perct,3), '</br></br>', 
                                                    'Duration: 10-Year', '</br>',
                                                    'Rate: ', round(res_currp$rate, 3), "%")) %>% 
        add_trace(p, x = res_curr$perct*100, y = res_curr$rate, showlegend = F,  
                  mode = 'markers', marker = list(symbol = 'square', size = 10, color = "#000000"), 
                  hoverinfo= 'text', text = ~paste0('Percentile: ', round(res_curr$perct*100,3), '</br></br>', 
                                                    'Latest Date: ', curr_point()$Date, '</br>',
                                                    'Duration: 10-Year', '</br>',
                                                    'Rate: ', round(res_curr$rate,3), "%"))
      p %>% layout(title = "10-Year Treasury Rates",
                   legend = list(orientation = "h", xanchor = "center", x = 0.5, font = list(size = 15)), 
                   font = list(size = 12), 
                   xaxis = list(title = "Percentile", showgrid = FALSE, size = 8, tickangle = 0),
                   yaxis = list(title = "Treasury Rates", size = 8, tickangle = 0), 
                   annotations = list(x = res_curr$perct*100, y = res_curr$rate, text = curr_point()$Date, 
                                      xref = "x", yref = "y", showarrow = TRUE, arrowhead = 7, ax = 20, ay = -40))%>% 
        config(displaylogo = FALSE, modeBarButtonsToRemove = list("zoomIn2d", "zoomOut2d", "zoom2d", "autoScale2d", "resetScale2d", "select2d", 
                                                                  "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "pan2d"))
    })
    
    output$plotPercentile6 <- renderPlotly({
      treasury <- treasury_time(input$percentileTime2[1], input$percentileTime2[2])
      treasury_pct <- treasury %>% dplyr::select(Date, "X30.Yr")
      #convert wide to long , drop nas
      treasury_long <- melt(treasury_pct, id.vars = c("Date"))
      treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
      treasury_long2 <- treasury_long %>% group_by(variable) %>% arrange(value) %>% 
                            mutate(count = seq(n()),  percentile = ecdf(value)(value)) 
      #finding percentile pooint
      curr_p <- input$inputPercentile6
      res_currp <- find_ratecurr(curr_p, treasury_long2)
      #most recent point 
      curr = as.numeric(as.character(curr_point()[,"X30.Yr"])) #current point despite chosen time range
      res_curr <- find_seqcurr(curr, treasury_long2)
      #get the circular points for general percentiles 
      res_ps <- find_seqpercentiles(treasury_long2)
      
      output$outputPercentile6 <- renderText({ paste0("<font color=\"#000000\"><b>", round(res_currp$rate, 3), "%", "</b>") })
      
      p <- plot_ly(data = treasury_long2, 
                   legendgroup = "30-Year", x = ~percentile*100, y = ~value, 
                   type = 'scatter', mode = 'lines', 
                   color = "30-Year", hoverinfo = 'none') %>% 
        add_trace(p, data = res_ps, x = ~perct, y = ~rate, 
                  showlegend = F, mode = 'markers', marker = list(symbol = 'circle', size = 8), 
                  hoverinfo= 'text', 
                  text = ~paste0('Percentile: ', perct, '</br></br>', 
                                 'Duration: 30-Year', '</br>',
                                 'Rate: ', round(as.numeric(as.character(rate)),3), "%")) %>% 
        add_trace(p, x = res_currp$perct, y = res_currp$rate, 
                  showlegend = F, 
                  mode = 'markers', marker = list(symbol = 'circle', size = 10, color = "#aaaaaa"), 
                  hoverinfo= 'text', text = ~paste0('Percentile: ', round(res_currp$perct,3), '</br></br>', 
                                                    'Duration: 30-Year', '</br>',
                                                    'Rate: ', round(res_currp$rate, 3), "%")) %>% 
        add_trace(p, x = res_curr$perct*100, y = res_curr$rate, showlegend = F,  
                  mode = 'markers', marker = list(symbol = 'square', size = 10, color = "#000000"), 
                  hoverinfo= 'text', text = ~paste0('Percentile: ', round(res_curr$perct*100,3), '</br></br>', 
                                                    'Latest Date: ', curr_point()$Date, '</br>',
                                                    'Duration: 30-Year', '</br>',
                                                    'Rate: ', round(res_curr$rate,3), "%"))
      p %>% layout(title = "30-Year Treasury Rates",
                   legend = list(orientation = "h", xanchor = "center", x = 0.5, font = list(size = 15)), 
                   font = list(size = 12), 
                   xaxis = list(title = "Percentile", showgrid = FALSE, size = 8, tickangle = 0),
                   yaxis = list(title = "Treasury Rates", size = 8, tickangle = 0), 
                   annotations = list(x = res_curr$perct*100, y = res_curr$rate, text = curr_point()$Date, 
                                      xref = "x", yref = "y", showarrow = TRUE, arrowhead = 7, ax = 20, ay = -40))%>%  
        config(displaylogo = FALSE, modeBarButtonsToRemove = list("zoomIn2d", "zoomOut2d", "zoom2d", "autoScale2d", "resetScale2d", "select2d", 
                                                                  "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "pan2d"))
    })
    
    #find rate for weighted percentile 
    output$outputPercentileWeight <- renderText({ 
        current_indexp <- (input$inputPercentile1*(1/12)+input$inputPercentile2*(1/2)+input$inputPercentile3*(1)
                           +input$inputPercentile4*(5)+input$inputPercentile5*(10)+input$inputPercentile6*(30))/(46+7/12)
        res_index <- find_ratecurr(current_indexp, weighted_treasury2())
        paste("<font color=\"#000000\"><b>", round(res_index$perct,3), "</b>") 
    })
    
    output$outputPercentileWeightR <- renderText({ 
        current_indexp <- (input$inputPercentile1*(1/12)+input$inputPercentile2*(1/2)+input$inputPercentile3*(1)
                           +input$inputPercentile4*(5)+input$inputPercentile5*(10)+input$inputPercentile6*(30))/(46+7/12)
        res_index <- find_ratecurr(current_indexp, weighted_treasury2())
        paste("<font color=\"#000000\"><b>", round(res_index$rate,3), "</b>") 
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
