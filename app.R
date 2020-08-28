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

#parsing 
library('XML')
library('RCurl')

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
  
  tryCatch(
    # This is what I want to do...
    {
      if(current_point <= as.numeric(min(dat$value))){
        perct_x = min(dat[which(dat$value == as.numeric(min(dat$value))),]$percentile)
      }
      else if(current_point == as.numeric(max(dat$value))){
        perct_x = max(dat[which(dat$value == as.numeric(max(dat$value))),]$percentile)
      }
      else if(current_point > as.numeric(max(dat$value))){
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
    },
    # ... but if an error occurs, tell me what happened: 
    error = function(error_message) {
      message("Non-numeric input")
      return(data.frame(perct = NA, rate = NA))
    }
  )
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

find_last_percentile <- function(dat, p){
  if(p/100 < min(dat$percentile)){
    res <- dat[which(dat$percentile == min(dat$percentile)),]
  }
  else if(p/100 > max(dat$percentile)){
    res <- dat[which(dat$percentile == max(dat$percentile)),]
  }
  else{
    res <- dat[max(which(dat$percentile <= p/100)),]
  }
  return(res)
}

# Define UI
ui <- fluidPage(
    list(tags$head(HTML('<link rel="icon", href="symetra-favicon.png", type="image/png" />'))),

    #Navbar structure for UI
    navbarPage(title=div(img(src="symetra-logo.png", style="margin-top: -12px; 
                             margin-left: -10px; padding-bottom: 10px", height = 50)),
               windowTitle="Treasure Hunt", theme = shinytheme("simplex"), id = "tabs_all",
        tabPanel(HTML("Input Treasury Yields"), fluid = TRUE, icon = icon("search"), value = "inputyield",
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
                                                                                                value = 0.13, min = 0, max = 5.27, step = 0.01)),
                                                                 column(width = 2, numericInput("inputRate2", label = "3-Month", 
                                                                                                value = 0.16, min = 0, max = 8.26, step = 0.01)),
                                                                 column(width = 2, numericInput("inputRate3", label = "6-Month", 
                                                                                                value = 0.18, min = 0, max = 8.49, step = 0.01)), 
                                                                 column(width = 2, numericInput("inputRate4", label = "1-Year", 
                                                                                                value = 0.16, min = 0, max = 8.64, step = 0.01)), 
                                                                 column(width = 2, numericInput("inputRate5", label = "2-Year", 
                                                                                                value = 0.16, min = 0, max = 9.05, step = 0.01)), 
                                                                 column(width = 2, numericInput("inputRate6", label = "3-Year", 
                                                                                                value = 0.18, min = 0, max = 9.11, step = 0.01))
                                                          ), 
                                                ),  
                                                fluidRow( HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp Percentiles for Input Treasury Rates <br></br>"),
                                                          column(width = 12, 
                                                                 column(width = 2, htmlOutput("outputRate1", container = pre)),
                                                                 column(width = 2, htmlOutput("outputRate2", container = pre)),
                                                                 column(width = 2, htmlOutput("outputRate3", container = pre)),
                                                                 column(width = 2, htmlOutput("outputRate4", container = pre)),
                                                                 column(width = 2, htmlOutput("outputRate5", container = pre)),
                                                                 column(width = 2, htmlOutput("outputRate6", container = pre))
                                                          ), 
                                                ), 
                                                fluidRow( HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp <b> Input Treasury Rate of Interest (in %)</b><br></br>"),
                                                          tags$style("[type = 'number'] {font-size:12px;}"),
                                                          
                                                          column(width = 12, 
                                                                 column(width = 2, numericInput("inputRate7", label = "5-Year", 
                                                                                                value = 0.29, min = 0, max = 9.1, step = 0.01)),
                                                                 column(width = 2, numericInput("inputRate8", label = "7-Year", 
                                                                                                value = 0.49, min = 0, max = 9.12, step = 0.01)),
                                                                 column(width = 2, numericInput("inputRate9", label = "10-Year", 
                                                                                                value = 0.66, min = 0, max = 9.09, step = 0.01)), 
                                                                 column(width = 2, numericInput("inputRate10", label = "20-Year", 
                                                                                                value = 1.18, min = 0, max = 8.3, step = 0.01)), 
                                                                 column(width = 2, numericInput("inputRate11", label = "30-Year", 
                                                                                                value = 1.41, min = 0, max = 9.18, step = 0.01))
                                                          ), 
                                                ), 
                                                fluidRow( HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp Percentiles for Input Treasury Rates <br></br>"),
                                                          column(width = 12, 
                                                                 column(width = 2, htmlOutput("outputRate7", container = pre)),
                                                                 column(width = 2, htmlOutput("outputRate8", container = pre)),
                                                                 column(width = 2, htmlOutput("outputRate9", container = pre)),
                                                                 column(width = 2, htmlOutput("outputRate10", container = pre)),
                                                                 column(width = 2, htmlOutput("outputRate11", container = pre))
                                                          ), 
                                                ), 
                                                
                                                style='.small-box margin: 0px;border:2px solid;border-radius:5px;padding: 10px; 
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
                                                                                   column(width = 2, numericInput("inputPercentile2", label = "3-Month", 
                                                                                                                  value = 50, min = 1, max = 100, step = 1)), 
                                                                                   column(width = 2, numericInput("inputPercentile3", label = "6-Month", 
                                                                                                                  value = 50, min = 1, max = 100, step = 1)), 
                                                                                   column(width = 2, numericInput("inputPercentile4", label = "1-Year", 
                                                                                                                  value = 50, min = 1, max = 100, step = 1)), 
                                                                                   column(width = 2, numericInput("inputPercentile5", label = "2-Year", 
                                                                                                                  value = 50, min = 1, max = 100, step = 1)), 
                                                                                   column(width = 2, numericInput("inputPercentile6", label = "3-Year", 
                                                                                                                  value = 50, min = 1, max = 100, step = 1))
                                                                            )
                                                                   ), 
                                                                   fluidRow(HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp Rates for Input Percentiles <br></br>"),
                                                                            column(width = 12, 
                                                                                   column(width = 2, htmlOutput("outputPercentile1", container = pre)),
                                                                                   column(width = 2, htmlOutput("outputPercentile2", container = pre)),
                                                                                   column(width = 2, htmlOutput("outputPercentile3", container = pre)),
                                                                                   column(width = 2, htmlOutput("outputPercentile4", container = pre)),
                                                                                   column(width = 2, htmlOutput("outputPercentile5", container = pre)),
                                                                                   column(width = 2, htmlOutput("outputPercentile6", container = pre))
                                                                            ) 
                                                                   ),
                                                                   fluidRow(HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp <b> Input Percentiles of Interest</b><br></br>"),
                                                                           column(width = 12, 
                                                                                  column(width = 2, numericInput("inputPercentile7", label = "5-Year", 
                                                                                                                 value = 50, min = 1, max = 100, step = 1)),
                                                                                  column(width = 2, numericInput("inputPercentile8", label = "7-Year", 
                                                                                                                 value = 50, min = 1, max = 100, step = 1)),
                                                                                  column(width = 2, numericInput("inputPercentile9", label = "10-Year", 
                                                                                                                 value = 50, min = 1, max = 100, step = 1)),
                                                                                  column(width = 2, numericInput("inputPercentile10", label = "20-Year", 
                                                                                                                 value = 50, min = 1, max = 100, step = 1)),
                                                                                  column(width = 2, numericInput("inputPercentile11", label = "30-Year", 
                                                                                                                 value = 50, min = 1, max = 100, step = 1))
                                                                           )
                                                                   ),
                                                                   fluidRow(HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp Rates for Input Percentiles <br></br>"),
                                                                            column(width = 12, 
                                                                                   column(width = 2, htmlOutput("outputPercentile7", container = pre)),
                                                                                   column(width = 2, htmlOutput("outputPercentile8", container = pre)),
                                                                                   column(width = 2, htmlOutput("outputPercentile9", container = pre)),
                                                                                   column(width = 2, htmlOutput("outputPercentile10", container = pre)),
                                                                                   column(width = 2, htmlOutput("outputPercentile11", container = pre))
                                                                            ) 
                                                                   ),
                                                                   
                                                                   style='margin-bottom:5px;border:2px solid;border-radius:5px;padding: 10px; 
                                            border-color:#DCDCDC; border-spacing: 2px;margin-left:50px; font-size:12px'), 
                                                            fluidRow(column(width = 2,
                                                                            fluidRow( HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp <b> Weighted Index </b><br></br>"),
                                                                                      column(width = 12, tags$label(style = 'display;table-cell; vertical-align:middle; margin-left: 15px;', "Weighted Rate"),
                                                                                             column(width = 12, htmlOutput("outputPercentileWeight", container = pre))
                                                                                      )
                                                                            ), HTML("</br>"),
                                                                            fluidRow( tags$label(style = 'display;table-cell; vertical-align:middle; margin-left: 30px;', "Percentile at Weighted Rate"),
                                                                                      column(width = 12, 
                                                                                             column(width = 12, htmlOutput("outputPercentileWeightR", container = pre))
                                                                                      )
                                                                            ), style='.small-box margin: 0px;border:2px solid;border-radius:5px;padding: 10px; 
                                                      border-color:#DCDCDC; border-spacing: 2px; margin-left:10px; font-size: 12px;'))))))))),
                 fluidRow(column(width = 12, withSpinner(plotlyOutput("plotRate2", height = 650)))), hr(),
                 fluidRow(tags$label("Short Durations"),
                          column(width = 12, 
                                 column(width = 3, 
                                        withSpinner(plotlyOutput("plotPercentile1", height = 300))), 
                                 column(width = 3, 
                                        withSpinner(plotlyOutput("plotPercentile2", height = 300))), 
                                 column(width = 3, 
                                        withSpinner(plotlyOutput("plotPercentile3", height = 300))),
                                 column(width = 3, 
                                        withSpinner(plotlyOutput("plotPercentile4", height = 300)))
                          )
                 ), hr(), 
                 fluidRow(tags$label("Mid Durations"),
                          column(width = 12, 
                                 column(width = 3, 
                                        withSpinner(plotlyOutput("plotPercentile5", height = 300))), 
                                 column(width = 3, 
                                        withSpinner(plotlyOutput("plotPercentile6", height = 300))), 
                                 column(width = 3, 
                                        withSpinner(plotlyOutput("plotPercentile7", height = 300))),
                                 column(width = 3, 
                                        withSpinner(plotlyOutput("plotPercentile8", height = 300)))
                          )
                 ), hr(), 
                 fluidRow(tags$label("Long Durations"),
                          column(width = 12, 
                                 column(width = 4, 
                                        withSpinner(plotlyOutput("plotPercentile9", height = 300))), 
                                 column(width = 4, 
                                        withSpinner(plotlyOutput("plotPercentile10", height = 300))), 
                                 column(width = 4, 
                                        withSpinner(plotlyOutput("plotPercentile11", height = 300)))
                          )
                 )
        ),
        tabPanel(HTML("Find Weighted Index"), fluid = TRUE, icon = icon("balance-scale"), value = "weightindex",
                 fluidRow(
                   column(width = 12,
                          fluidRow(HTML("<h3>Choose Your Inputs for Select Durations</h3>"),
                                   wellPanel(fluidRow(
                                     column(width = 12, 
                                            column(width = 12, 
                                                   sliderInput("percentileTime3", "Select Time Interval", min = 1990, max = 2020, 
                                                               value = c(1990, 2020), step = 1, sep = ""))
                                     )
                                   ), HTML("<br>"),
                                   
                                   fluidRow(column(width = 12,
                                                   fluidRow(column(width = 2,
                                                                   fluidRow( HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp <b> Weighted Index </b> <br></br>"),
                                                                             column(width = 12, 
                                                                                    column(width = 12, numericInput("inputPercentileW", label = "Input Percentile", 
                                                                                                                    value = 50, min = 1, max = 100, step = 1))
                                                                             )
                                                                   ), 
                                                                   fluidRow( tags$label(style = 'display;table-cell; vertical-align:middle; margin-left: 30px;', "Weighted Rate at Percentile"),
                                                                             column(width = 12, 
                                                                                    column(width = 12, htmlOutput("outputRateW", container = pre))
                                                                             )
                                                                   ), 
                                                                   fluidRow( tags$label(style = 'display;table-cell; vertical-align:middle; margin-left: 30px;', "Date of Occurence"),
                                                                             column(width = 12, 
                                                                                    column(width = 12, htmlOutput("outputRateWDate", container = pre))
                                                                             )
                                                                   ),style='.small-box margin: 0px;border:2px solid;border-radius:5px;padding: 10px; 
                                                                    border-color:#DCDCDC; border-spacing: 2px; margin-left:50px; font-size: 12px;'),
                                                   fluidRow(column(width = 9, 
                                                                   
                                                                   fluidRow(HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp <b> Treasury Rates at Weighted Index Percentile of Interest</b><br></br>"),
                                                                            column(width = 12, 
                                                                                   column(width = 2, div(tags$label("1-Month"), htmlOutput("outputRateW1", container = pre))),
                                                                                   column(width = 2, div(tags$label("3-Month"), htmlOutput("outputRateW2", container = pre))),
                                                                                   column(width = 2, div(tags$label("6-Month"), htmlOutput("outputRateW3", container = pre))),
                                                                                   column(width = 2, div(tags$label("1-Year"), htmlOutput("outputRateW4", container = pre))),
                                                                                   column(width = 2, div(tags$label("2-Year"), htmlOutput("outputRateW5", container = pre))),
                                                                                   column(width = 2, div(tags$label("3-Year"), htmlOutput("outputRateW6", container = pre)))
                                                                   )), 
                                                                   fluidRow( HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp Percentiles at Select Duration     </br>"),
                                                                             column(width = 12, 
                                                                                   column(width = 2, htmlOutput("outputRateWP1", container = pre)),
                                                                                   column(width = 2, htmlOutput("outputRateWP2", container = pre)),
                                                                                   column(width = 2, htmlOutput("outputRateWP3", container = pre)),
                                                                                   column(width = 2, htmlOutput("outputRateWP4", container = pre)),
                                                                                   column(width = 2, htmlOutput("outputRateWP5", container = pre)),
                                                                                   column(width = 2, htmlOutput("outputRateWP6", container = pre))
                                                                   )), 
                                                                   fluidRow( HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp <b> Treasury Rates at Weighted Index Percentile of Interest </b><br></br>"),
                                                                             column(width = 12, 
                                                                                    column(width = 2, div(tags$label("5-Year"), htmlOutput("outputRateW7", container = pre))),
                                                                                    column(width = 2, div(tags$label("7-Year"), htmlOutput("outputRateW8", container = pre))),
                                                                                    column(width = 2, div(tags$label("10-Year"), htmlOutput("outputRateW9", container = pre))),
                                                                                    column(width = 2, div(tags$label("20-Year"), htmlOutput("outputRateW10", container = pre))),
                                                                                    column(width = 2, div(tags$label("30-Year"), htmlOutput("outputRateW11", container = pre)))
                                                                   )),
                                                                   fluidRow( HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp Percentiles at Select Duration  "),
                                                                             column(width = 12, 
                                                                                   column(width = 2, htmlOutput("outputRateWP7", container = pre)),
                                                                                   column(width = 2, htmlOutput("outputRateWP8", container = pre)),
                                                                                   column(width = 2, htmlOutput("outputRateWP9", container = pre)),
                                                                                   column(width = 2, htmlOutput("outputRateWP10", container = pre)),
                                                                                   column(width = 2, htmlOutput("outputRateWP11", container = pre))
                                                                   )), style='.small-box margin: 0px;border:2px solid;border-radius:5px;padding: 10px; 
                                                          border-color:#DCDCDC; border-spacing: 2px; margin-left:10px; font-size: 12px;')
                                          ))
                                   )
                                   )
                               ) #wellpanel
                          )
                   )
                 ), 
                 fluidRow(column(width = 12, withSpinner(plotlyOutput("plotRate3", height = 650)))), hr()
        ), 
        tabPanel(HTML("Historical Treasury Yields"), fluid = TRUE, icon = icon("chart-line"), value = "historicals",
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
                                                                        choices = c("1-Month" = "X1.Mo", "3-Month" = "X3.Mo", 
                                                                                    "6-Month" = "X6.Mo", "1-Year" = "X1.Yr"),
                                                                        selected = "X1.Mo"
                                                     )
                                              ),
                                              column(width = 4, 
                                                     checkboxGroupInput(inputId = "historicalDurationB", 
                                                                        label = NULL, 
                                                                        choices = c("2-Year" = "X2.Yr", "3-Year" = "X3.Yr", 
                                                                                    "5-Year" = "X5.Yr", "7-Year" = "X7.Yr")
                                                     )
                                              ),
                                              column(width = 4, 
                                                     checkboxGroupInput(inputId = "historicalDurationC", 
                                                                        label = NULL, 
                                                                        choices = c("10-Year" = "X10.Yr", "20-Year" = "X20.Yr", 
                                                                                    "30-Year" = "X30.Yr")
                                                     )
                                              )
                                            ), tags$br(), tags$br(), hr(), tags$br(),
                                            sliderInput(inputId = "historicalYear",
                                                        label = "Select Years",
                                                        min = 1990, max = year(Sys.Date()),
                                                        value = c(1990, year(Sys.Date())), sep = ""),
                                            helpText(HTML(paste0("<p style = 'text-align: right'>", "Last update: ", Sys.Date()-1, "</p>"))),
                                            tags$br(), hr(), tags$br(), 
                                            wellPanel( #if want latest date, set value null and updatedateinput
                                              dateInput(inputId = "historicalDate", "Select Date", value = Sys.Date()-1, format = "yyyy-mm-dd"),
                                              div(style="display:inline-block; width:160%; text-align: center;", actionButton(inputId = "DateRate", label = "Input to Treasury", icon = icon("plus"),
                                                           style="color: #fff; background-color: #b3b3b3; 
                                                                  border-color: #b3b3b3; padding:5px; font-size:100%;"))
                                              
                                            ), # wellPanel
                                            helpText(HTML("<p style = 'text-align: right'>Only available for no-gaps</p>")),
                                            )#, 
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
                              ".", HTML("</br></br>"),
                              "The treasury rates data from the US DOT are parsed from the provided XML format on the site. 
                               This will allow the data to be read and processed directly into the web application without needing to manually 
                               add the newest information. The packages, 'XML' and 'RCurl' used will read the content in 
                               from the URL provided, and parse the XML for the appropriate treasury rates at each duration.")
                         )
                  )
              ), HTML("<br></br>"),
              fluidRow(
                column(6, 
                       HTML(paste0("<h2><b>Data for Percentile Calculation</b></h2>")), hr(),
                       HTML(paste0("<h4> The percentile calculations will depend on the data availability of 
                                    each duration, with all major gaps being noted in the table below. 
                                    For our percentile calculations, results were based on available data 
                                    for that specific duration. However, the weighted index percentile calculation 
                                    was based on dates where there was available data for all durations. 
                                   <em> Please note that 10/11/2010 is a consistent gap amongst all durations. </em>")), HTML("<br></br>"),
                       DT::dataTableOutput("table")
                       
                
                ),
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

parsing_treasury <- reactive({
  # Save the URL of the xml file in a variable
  fileUrl<- "https://data.treasury.gov/feed.svc/DailyTreasuryYieldCurveRateData" 
  
  ###Using the rcurl package, first download, then parse
  #Get the content
  fileContent = getURL(fileUrl)
  
  #parse xml from text string
  xmlfile.fromstring = xmlParse(fileContent,asText=TRUE)
  # Use the xmlRoot-function to access the top node
  xmltop = xmlRoot(xmlfile.fromstring)
  
  bonds_dates  = xpathSApply(xmltop, "//ns:entry/ns:content//d:NEW_DATE", function(x){xmlValue(x)}, namespaces=c(ns="http://www.w3.org/2005/Atom", d="http://schemas.microsoft.com/ado/2007/08/dataservices"))
  bonds_1_month = xpathSApply(xmltop, "//ns:entry/ns:content//d:BC_1MONTH", function(x){as.numeric(xmlValue(x))}, namespaces=c(ns="http://www.w3.org/2005/Atom", d="http://schemas.microsoft.com/ado/2007/08/dataservices"))
  bonds_2_month = xpathSApply(xmltop, "//ns:entry/ns:content//d:BC_2MONTH", function(x){as.numeric(xmlValue(x))}, namespaces=c(ns="http://www.w3.org/2005/Atom", d="http://schemas.microsoft.com/ado/2007/08/dataservices"))
  bonds_3_month = xpathSApply(xmltop, "//ns:entry/ns:content//d:BC_3MONTH", function(x){as.numeric(xmlValue(x))}, namespaces=c(ns="http://www.w3.org/2005/Atom", d="http://schemas.microsoft.com/ado/2007/08/dataservices"))
  bonds_6_month = xpathSApply(xmltop, "//ns:entry/ns:content//d:BC_6MONTH", function(x){as.numeric(xmlValue(x))}, namespaces=c(ns="http://www.w3.org/2005/Atom", d="http://schemas.microsoft.com/ado/2007/08/dataservices"))
  bonds_1_year = xpathSApply(xmltop, "//ns:entry/ns:content//d:BC_1YEAR", function(x){as.numeric(xmlValue(x))}, namespaces=c(ns="http://www.w3.org/2005/Atom", d="http://schemas.microsoft.com/ado/2007/08/dataservices"))
  bonds_2_year = xpathSApply(xmltop, "//ns:entry/ns:content//d:BC_2YEAR", function(x){as.numeric(xmlValue(x))}, namespaces=c(ns="http://www.w3.org/2005/Atom", d="http://schemas.microsoft.com/ado/2007/08/dataservices"))
  bonds_3_year = xpathSApply(xmltop, "//ns:entry/ns:content//d:BC_3YEAR", function(x){as.numeric(xmlValue(x))}, namespaces=c(ns="http://www.w3.org/2005/Atom", d="http://schemas.microsoft.com/ado/2007/08/dataservices"))
  bonds_5_year = xpathSApply(xmltop, "//ns:entry/ns:content//d:BC_5YEAR", function(x){as.numeric(xmlValue(x))}, namespaces=c(ns="http://www.w3.org/2005/Atom", d="http://schemas.microsoft.com/ado/2007/08/dataservices"))
  bonds_7_year = xpathSApply(xmltop, "//ns:entry/ns:content//d:BC_7YEAR", function(x){as.numeric(xmlValue(x))}, namespaces=c(ns="http://www.w3.org/2005/Atom", d="http://schemas.microsoft.com/ado/2007/08/dataservices"))
  bonds_10_year = xpathSApply(xmltop, "//ns:entry/ns:content//d:BC_10YEAR", function(x){as.numeric(xmlValue(x))}, namespaces=c(ns="http://www.w3.org/2005/Atom", d="http://schemas.microsoft.com/ado/2007/08/dataservices"))
  bonds_20_year = xpathSApply(xmltop, "//ns:entry/ns:content//d:BC_20YEAR", function(x){as.numeric(xmlValue(x))}, namespaces=c(ns="http://www.w3.org/2005/Atom", d="http://schemas.microsoft.com/ado/2007/08/dataservices"))
  bonds_30_year = xpathSApply(xmltop, "//ns:entry/ns:content//d:BC_30YEAR", function(x){as.numeric(xmlValue(x))}, namespaces=c(ns="http://www.w3.org/2005/Atom", d="http://schemas.microsoft.com/ado/2007/08/dataservices"))
  
  bond = data.frame(Date=strptime(bonds_dates,format="%Y-%m-%dT%H:%M:%S", tz = "GMT"), 
                    X1.Mo = bonds_1_month, X2.Mo = bonds_2_month, X3.Mo = bonds_3_month, 
                    X6.Mo = bonds_6_month, X1.Yr = bonds_1_year, X2.Yr = bonds_2_year, 
                    X3.Yr = bonds_3_year, X5.Yr = bonds_5_year, X7.Yr = bonds_7_year, 
                    X10.Yr = bonds_10_year, X20.Yr = bonds_20_year, X30.Yr = bonds_30_year)
  bond_f <- bond[order(bond$Date),]
})

                        
treasury_time <- function(t1, t2){
    #choose interval of interest
    return(parsing_treasury() %>% filter(year(Date) >= t1 & year(Date) <= t2))
    #return(parsing_treasury())
}



duration_names <- reactive({
    data.frame(variable = c("X1.Mo", "X2.Mo", "X3.Mo", "X6.Mo", "X1.Yr", "X2.Yr", 
                           "X3.Yr", "X5.Yr", "X7.Yr", "X10.Yr", "X20.Yr", "X30.Yr"),
               vartitle = c("1-Month", "2-Month", "3-Month", "6-Month", "1-Year", "2-Year", 
                            "3-Year", "5-Year", "7-Year", "10-Year", "20-Year", "30-Year"), 
               months = c(1, 2, 3, 6, 12, 24, 36, 60, 84, 120, 240, 360))
})

server <- function(session, input, output) {
    
    output$table <- renderTable(data.frame(Duration = c("1 Month", "2 Month", "3 Month", "6 Month", "1 Year", 
                                                        "2 Year", "3 Year", "5 Year", "7 Year", "10 Year", 
                                                        "20 Year", "30 Year")))  
  
    
    output$table <- DT::renderDataTable({
      datatable(data.frame(Duration = c("1 Month", "2 Month", "3 Month", "6 Month", "1 Year", 
                              "2 Year", "3 Year", "5 Year", "7 Year", "10 Year", 
                              "20 Year", "30 Year"), 
                           Gaps = c("1/2/1990 - 07/31/2001", 
                                    "1/2/1990 - 10/15/2018",
                                    "12/10/2008, 12/18/2008, 12/24/2008", 
                                    "None", "None", "None", "None", "None", "None", "None",
                                    "1/2/1990 - 9/30/1993",
                                    "2/15/2002 - 2/9/2006"), 
                           Omit = c("No", "Yes", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No")), 
                class = 'cell-border stripe', 
                options = list(searching = FALSE, pageLength = 12, lengthMenu = FALSE, paging = FALSE, dom = 't', order = FALSE), rownames= FALSE) #%>% 
        #formatStyle(border = '1px solid #ddd')
    })
  
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
    
    ###action button, update date selection 
    
    observe({
      x = input$historicalYear
      # We'll use the input$controller variable multiple times, so save it as x
      # for convenience.
      treasury_date <- treasury_time(x[1], x[2])
      updateDateInput(session, "historicalDate",
                      value = treasury_date[which(treasury_date$year == x[1]),]$Date,
                      min   = treasury_date[which(treasury_date$year == x[1]),]$Date,
                      max   = treasury_date[which(treasury_date$year == x[2]),]$Date
      )
    })
    
    treasury_date <- reactive({
      treasury_date <- treasury_time(1990, 2020) #all dates available
      res <- treasury_date[which(treasury_date$Date == input$historicalDate),]
      return(res)
    })
    
    observeEvent(input$DateRate,{
      # Data
      updateNumericInput(session, "inputRate1", value = treasury_date()$X1.Mo)
      updateNumericInput(session, "inputRate2", value = treasury_date()$X3.Mo)
      updateNumericInput(session, "inputRate3", value = treasury_date()$X6.Mo)
      updateNumericInput(session, "inputRate4", value = treasury_date()$X1.Yr)
      updateNumericInput(session, "inputRate5", value = treasury_date()$X2.Yr)
      updateNumericInput(session, "inputRate6", value = treasury_date()$X3.Yr)
      updateNumericInput(session, "inputRate7", value = treasury_date()$X5.Yr)
      updateNumericInput(session, "inputRate8", value = treasury_date()$X7.Yr)
      updateNumericInput(session, "inputRate9", value = treasury_date()$X10.Yr)
      updateNumericInput(session, "inputRate10", value = treasury_date()$X20.Yr)
      updateNumericInput(session, "inputRate11", value = treasury_date()$X30.Yr)
      #jump to tab
      #newtab <- switch(input$tabs_all, "inputyield" = "historicals")
      #updateTabItems(session, "tabs_all", newtab)
      updateTabsetPanel(session, "tabs_all",
                        selected = "inputyield")
    })
    
    
    output$historicalLP <- renderPlotly({
        
        shiny::validate(
            need(length(dur_selected()) >0, "Please Choose a Duration")
        )
        print(input$historicalDate)
        print(treasury_date()$X1.Mo)
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
            p <- add_trace(p, data = treasury_line[treasury_line$Date == input$historicalDate & treasury_line$vartitle == i,], 
                           legendgroup = ~vartitle, showlegend = F, 
                           color = ~vartitle, mode = 'markers',               
                           marker = list(symbol = "circle", size = 8, color = "#000000"), 
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
        current <- parsing_treasury()[which.max(parsing_treasury()$Date),]
        return(current)
    })
    
    weighted_treasury <- reactive({
      treasury <- treasury_time(input$percentileTime[1], input$percentileTime[2])
      treasury <- treasury[,c("Date", "X1.Mo", "X3.Mo", "X6.Mo", "X1.Yr", "X2.Yr", "X3.Yr", "X5.Yr", "X7.Yr", "X10.Yr", "X20.Yr", "X30.Yr")]
      treasury$value <- treasury$X1.Mo*(1/12)+treasury$X3.Mo*(3/12)+treasury$X6.Mo*(1/2)+treasury$X1.Yr*(1)+
        treasury$X2.Yr*(2)+treasury$X3.Yr*(3)+treasury$X5.Yr*(5)+treasury$X7.Yr*(7)+treasury$X10.Yr*(10)+
        treasury$X20.Yr*(20)+treasury$X30.Yr*(30)
      treasury_f <- treasury[,c("Date", "value")]
      treasury_f2 <- treasury_f[!is.na(treasury_f$value),] 
      treasury_f3 <- treasury_f2 %>% arrange(value) %>% mutate(count = seq(n()),  percentile = ecdf(value)(value))
      return(treasury_f3)
    })
    
    weighted_treasury2 <- reactive({
      treasury <- treasury_time(input$percentileTime2[1], input$percentileTime2[2])
      treasury <- treasury[,c("Date", "X1.Mo", "X3.Mo", "X6.Mo", "X1.Yr", "X2.Yr", "X3.Yr", "X5.Yr", "X7.Yr", "X10.Yr", "X20.Yr", "X30.Yr")]
      treasury$value <- treasury$X1.Mo*(1/12)+treasury$X3.Mo*(3/12)+treasury$X6.Mo*(1/2)+treasury$X1.Yr*(1)+
                          treasury$X2.Yr*(2)+treasury$X3.Yr*(3)+treasury$X5.Yr*(5)+treasury$X7.Yr*(7)+treasury$X10.Yr*(10)+
                          treasury$X20.Yr*(20)+treasury$X30.Yr*(30)
      treasury_f <- treasury[,c("Date", "value")]
      treasury_f2 <- treasury_f[!is.na(treasury_f$value),] 
      treasury_f3 <- treasury_f2 %>% arrange(value) %>% mutate(count = seq(n()),  percentile = ecdf(value)(value))
      return(treasury_f3)
    })
    
    #treasury_dump <- reactive({
    #  treasury <- treasury_time(input$percentileTime[1], input$percentileTime[2])
    #  treasury <- treasury[,c("Date", "X1.Mo", "X3.Mo", "X6.Mo", "X1.Yr", "X2.Yr", "X3.Yr", "X5.Yr", "X7.Yr", "X10.Yr", "X20.Yr", "X30.Yr")]
    #  treasury_long <- melt(treasury, id.vars = c("Date"))
    #  treasury_long2 <- treasury_long[!is.na(treasury_long$value),] %>% group_by(variable) %>% arrange(value) %>% 
    #    mutate(count = seq(n()),  percentile = ecdf(value)(value)) 
    #  return(treasury_long2)
    #})
    
    #output$outputRate1 <- renderText({ 
    #  
    #  res_curr1 <- find_seqcurr(input$inputRate1, treasury_dump()[which(treasury_dump()$variable == "X1.Mo"),])
    ##  paste("<font color=\"#000000\"><b>", round(res_curr1$perct, 3)*100,  "</b>") 
    #  
    #})
    
    
    
    output$plotRate1 <- renderPlotly({
        treasury <- treasury_time(input$percentileTime[1], input$percentileTime[2])
        treasury <- treasury[,c("Date", "X1.Mo", "X3.Mo", "X6.Mo", "X1.Yr", "X2.Yr", "X3.Yr", "X5.Yr", "X7.Yr", "X10.Yr", "X20.Yr", "X30.Yr")]
        treasury_long <- melt(treasury, id.vars = c("Date"))
        treasury_long2 <- treasury_long[!is.na(treasury_long$value),] %>% group_by(variable) %>% arrange(value) %>% 
                               mutate(count = seq(n()),  percentile = ecdf(value)(value))
        #for each of the inputs
        res_curr1 <- find_seqcurr(input$inputRate1, treasury_long2[which(treasury_long2$variable == "X1.Mo"),])
        res_curr2 <- find_seqcurr(input$inputRate2, treasury_long2[which(treasury_long2$variable == "X3.Mo"),])
        res_curr3 <- find_seqcurr(input$inputRate3, treasury_long2[which(treasury_long2$variable == "X6.Mo"),])
        res_curr4 <- find_seqcurr(input$inputRate4, treasury_long2[which(treasury_long2$variable == "X1.Yr"),])
        res_curr5 <- find_seqcurr(input$inputRate5, treasury_long2[which(treasury_long2$variable == "X2.Yr"),])
        res_curr6 <- find_seqcurr(input$inputRate6, treasury_long2[which(treasury_long2$variable == "X3.Yr"),])
        res_curr7 <- find_seqcurr(input$inputRate7, treasury_long2[which(treasury_long2$variable == "X5.Yr"),])
        res_curr8 <- find_seqcurr(input$inputRate8, treasury_long2[which(treasury_long2$variable == "X7.Yr"),])
        res_curr9 <- find_seqcurr(input$inputRate9, treasury_long2[which(treasury_long2$variable == "X10.Yr"),])
        res_curr10 <- find_seqcurr(input$inputRate10, treasury_long2[which(treasury_long2$variable == "X20.Yr"),])
        res_curr11 <- find_seqcurr(input$inputRate11, treasury_long2[which(treasury_long2$variable == "X30.Yr"),])
        #result of all the rates
        res_curall <- rbind(res_curr1, res_curr2, res_curr3, res_curr4, res_curr5, res_curr6, res_curr7, res_curr8, 
                            res_curr9, res_curr10, res_curr11)
        res_curall$name <- c("1-Month", "3-Month", "6-Month", "1-Year", "2-Year", "3-Year", "5-Year", "7-Year", "10-Year", "20-Year", "30-Year")
        res_curall$name <- factor(res_curall$name, levels = c("1-Month", "3-Month", "6-Month", "1-Year", "2-Year", "3-Year", 
                                                              "5-Year", "7-Year", "10-Year", "20-Year", "30-Year"))
        #find percentile for the weighted index 
        current_index <- sum(res_curall$rate*c(1/12, 3/12, 1/2, 1, 2, 3, 5, 7, 10, 20, 30))
        res_indexp <- find_seqcurr(current_index, weighted_treasury())
        
        #update the output that shows percentile
        output$outputRate1 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr1$perct, 3)*100,  "</b>") })
        output$outputRate2 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr2$perct, 3)*100,  "</b>") })
        output$outputRate3 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr3$perct, 3)*100,  "</b>") })
        output$outputRate4 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr4$perct, 3)*100,  "</b>") })
        output$outputRate5 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr5$perct, 3)*100,  "</b>") })
        output$outputRate6 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr6$perct, 3)*100,  "</b>") })
        output$outputRate7 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr7$perct, 3)*100,  "</b>") })
        output$outputRate8 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr8$perct, 3)*100,  "</b>") })
        output$outputRate9 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr9$perct, 3)*100,  "</b>") })
        output$outputRate10 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr10$perct, 3)*100,  "</b>") })
        output$outputRate11 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr11$perct, 3)*100,  "</b>") })
        
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
    
    output$plotRate2 <- renderPlotly({
      treasury <- treasury_time(input$percentileTime2[1], input$percentileTime2[2])
      treasury <- treasury[,c("Date", "X1.Mo", "X3.Mo", "X6.Mo", "X1.Yr", "X2.Yr", "X3.Yr", "X5.Yr", "X7.Yr", "X10.Yr", "X20.Yr", "X30.Yr")]
      treasury_long <- melt(treasury, id.vars = c("Date"))
      treasury_long2 <- treasury_long[!is.na(treasury_long$value),] %>% group_by(variable) %>% arrange(value) %>% 
        mutate(count = seq(n()),  percentile = ecdf(value)(value))
      #for each of the inputs
      res_currp1 <- find_ratecurr(input$inputPercentile1, treasury_long2[which(treasury_long2$variable == "X1.Mo"),])
      res_currp2 <- find_ratecurr(input$inputPercentile2, treasury_long2[which(treasury_long2$variable == "X3.Mo"),])
      res_currp3 <- find_ratecurr(input$inputPercentile3, treasury_long2[which(treasury_long2$variable == "X6.Mo"),])
      res_currp4 <- find_ratecurr(input$inputPercentile4, treasury_long2[which(treasury_long2$variable == "X1.Yr"),])
      res_currp5 <- find_ratecurr(input$inputPercentile5, treasury_long2[which(treasury_long2$variable == "X2.Yr"),])
      res_currp6 <- find_ratecurr(input$inputPercentile6, treasury_long2[which(treasury_long2$variable == "X3.Yr"),])
      res_currp7 <- find_ratecurr(input$inputPercentile7, treasury_long2[which(treasury_long2$variable == "X5.Yr"),])
      res_currp8 <- find_ratecurr(input$inputPercentile8, treasury_long2[which(treasury_long2$variable == "X7.Yr"),])
      res_currp9 <- find_ratecurr(input$inputPercentile9, treasury_long2[which(treasury_long2$variable == "X10.Yr"),])
      res_currp10 <- find_ratecurr(input$inputPercentile10, treasury_long2[which(treasury_long2$variable == "X20.Yr"),])
      res_currp11 <- find_ratecurr(input$inputPercentile11, treasury_long2[which(treasury_long2$variable == "X30.Yr"),])
      
      res_currprate <- res_currp1$rate*(1/12)+res_currp2$rate*(3/12)+res_currp3$rate*(6/12)+res_currp4$rate*(1)+res_currp5$rate*(2)+
                          res_currp6$rate*(3)+res_currp7$rate*(5)+res_currp8$rate*(7)+res_currp9$rate*(10)+res_currp10$rate*(20)+res_currp11$rate*(30)
      
      output$outputPercentileWeight <- renderText({ paste("<font color=\"#000000\"><b>", round(res_currprate, 3), "</b>") })
      output$outputPercentileWeightR <- renderText({ 
                                              res_index <- find_seqcurr(res_currprate, weighted_treasury2())
                                              paste("<font color=\"#000000\"><b>", round(res_index$perct*100,3), "</b>") })
      #result of all the rates
      res_curpall <- rbind(res_currp1, res_currp2, res_currp3, res_currp4, res_currp5, res_currp6, res_currp7, res_currp8, 
                          res_currp9, res_currp10, res_currp11)
      res_curpall$name <- c("1-Month", "3-Month", "6-Month", "1-Year", "2-Year", "3-Year", "5-Year", "7-Year", "10-Year", "20-Year", "30-Year")
      res_curpall$name <- factor(res_curpall$name, levels = c("1-Month", "3-Month", "6-Month", "1-Year", "2-Year", "3-Year", 
                                                            "5-Year", "7-Year", "10-Year", "20-Year", "30-Year"))
      
      ###plot the graph
      plot_ly(res_curpall, x = ~name, y = ~rate, type = "scatter", mode = 'markers+lines', name = "Chosen Treasury Rates",
              hoverinfo= 'text', text = ~paste0('Rate: ', round(rate,3), "%", '</br></br>', 
                                                'Duration: ', name, '</br>')) %>%
        add_trace(x = ~name, y = ~perct, mode = "markers+lines", name = "Percentiles of Chosen Treasury Rates", yaxis = "y2",
                  hoverinfo= 'text', line = list(dash = "dash"), text = ~paste0('Percentile: ', round(perct,3), '</br></br>', 
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
      treasury_pct <- treasury %>% dplyr::select(Date, "X3.Mo")
      #convert wide to long , drop nas
      treasury_long <- melt(treasury_pct, id.vars = c("Date"))
      treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
      treasury_long2 <- treasury_long %>% group_by(variable) %>% arrange(value) %>% 
        mutate(count = seq(n()),  percentile = ecdf(value)(value)) 
      #finding percentile pooint
      curr_p <- input$inputPercentile2
      res_currp <- find_ratecurr(curr_p, treasury_long2)
      #most recent point 
      curr = as.numeric(as.character(curr_point()[,"X3.Mo"])) #current point despite chosen time range
      res_curr <- find_seqcurr(curr, treasury_long2)
      #get the circular points for general percentiles 
      res_ps <- find_seqpercentiles(treasury_long2)
      
      output$outputPercentile2 <- renderText({ paste0("<font color=\"#000000\"><b>", round(res_currp$rate, 3), "%", "</b>") })
      
      p <- plot_ly(data = treasury_long2, 
                   legendgroup = "3-Month", x = ~percentile*100, y = ~value, 
                   type = 'scatter', mode = 'lines', 
                   color = "3-Month", hoverinfo = 'none') %>% 
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
                                                    'Duration: 3-Month', '</br>',
                                                    'Rate: ', round(res_currp$rate, 3), "%")) %>% 
        add_trace(p, x = res_curr$perct*100, y = res_curr$rate, showlegend = F, 
                  mode = 'markers', marker = list(symbol = 'square', size = 10, color = "#000000"), 
                  hoverinfo= 'text', text = ~paste0('Percentile: ', round(res_curr$perct*100,3), '</br></br>', 
                                                    'Latest Date: ', curr_point()$Date, '</br>',
                                                    'Duration: 3-Month', '</br>',
                                                    'Rate: ', round(res_curr$rate,3), "%"))
      
      p %>% layout(title = "3-Month Treasury Rates",
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
      treasury_pct <- treasury %>% dplyr::select(Date, "X6.Mo")
      #convert wide to long , drop nas
      treasury_long <- melt(treasury_pct, id.vars = c("Date"))
      treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
      treasury_long2 <- treasury_long %>% group_by(variable) %>% arrange(value) %>% 
                          mutate(count = seq(n()),  percentile = ecdf(value)(value)) 
      #finding percentile pooint
      curr_p <- input$inputPercentile3
      res_currp <- find_ratecurr(curr_p, treasury_long2)
      #most recent point 
      curr = as.numeric(as.character(curr_point()[,"X6.Mo"])) #current point despite chosen time range
      res_curr <- find_seqcurr(curr, treasury_long2)
      #get the circular points for general percentiles 
      res_ps <- find_seqpercentiles(treasury_long2)
      
      output$outputPercentile3 <- renderText({ paste0("<font color=\"#000000\"><b>", round(res_currp$rate, 3), "%", "</b>") })
      
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
 
    output$plotPercentile4 <- renderPlotly({
      treasury <- treasury_time(input$percentileTime2[1], input$percentileTime2[2])
      treasury_pct <- treasury %>% dplyr::select(Date, "X1.Yr")
      #convert wide to long , drop nas
      treasury_long <- melt(treasury_pct, id.vars = c("Date"))
      treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
      treasury_long2 <- treasury_long %>% group_by(variable) %>% arrange(value) %>% 
        mutate(count = seq(n()),  percentile = ecdf(value)(value)) 
      #finding percentile pooint
      curr_p <- input$inputPercentile4
      res_currp <- find_ratecurr(curr_p, treasury_long2)
      #most recent point 
      curr = as.numeric(as.character(curr_point()[,"X1.Yr"])) #current point despite chosen time range
      res_curr <- find_seqcurr(curr, treasury_long2)
      #get the circular points for general percentiles 
      res_ps <- find_seqpercentiles(treasury_long2)
      
      output$outputPercentile4 <- renderText({ paste0("<font color=\"#000000\"><b>", round(res_currp$rate, 3), "%", "</b>") })
      
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
    
    output$plotPercentile5 <- renderPlotly({
      treasury <- treasury_time(input$percentileTime2[1], input$percentileTime2[2])
      treasury_pct <- treasury %>% dplyr::select(Date, "X2.Yr")
      #convert wide to long , drop nas
      treasury_long <- melt(treasury_pct, id.vars = c("Date"))
      treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
      treasury_long2 <- treasury_long %>% group_by(variable) %>% arrange(value) %>% 
        mutate(count = seq(n()),  percentile = ecdf(value)(value)) 
      #finding percentile pooint
      curr_p <- input$inputPercentile5
      res_currp <- find_ratecurr(curr_p, treasury_long2)
      #most recent point 
      curr = as.numeric(as.character(curr_point()[,"X2.Yr"])) #current point despite chosen time range
      res_curr <- find_seqcurr(curr, treasury_long2)
      #get the circular points for general percentiles 
      res_ps <- find_seqpercentiles(treasury_long2)
      
      output$outputPercentile5 <- renderText({ paste0("<font color=\"#000000\"><b>", round(res_currp$rate, 3), "%", "</b>") })
      
      p <- plot_ly(data = treasury_long2, 
                   legendgroup = "1-Year", x = ~percentile*100, y = ~value, 
                   type = 'scatter', mode = 'lines', 
                   color = "2-Year", hoverinfo = 'none') %>% 
        add_trace(p, data = res_ps, x = ~perct, y = ~rate, 
                  showlegend = F, mode = 'markers', marker = list(symbol = 'circle', size = 8), 
                  hoverinfo= 'text', 
                  text = ~paste0('Percentile: ', perct, '</br></br>', 
                                 'Duration: 2-Year', '</br>',
                                 'Rate: ', round(as.numeric(as.character(rate)),3), "%")) %>% 
        add_trace(p, x = res_currp$perct, y = res_currp$rate, 
                  showlegend = F, 
                  mode = 'markers', marker = list(symbol = 'circle', size = 10, color = "#aaaaaa"), 
                  hoverinfo= 'text', text = ~paste0('Percentile: ', round(res_currp$perct,3), '</br></br>', 
                                                    'Duration: 2-Year', '</br>',
                                                    'Rate: ', round(res_currp$rate, 3), "%")) %>% 
        add_trace(p, x = res_curr$perct*100, y = res_curr$rate, showlegend = F, 
                  mode = 'markers', marker = list(symbol = 'square', size = 10, color = "#000000"), 
                  hoverinfo= 'text', text = ~paste0('Percentile: ', round(res_curr$perct*100,3), '</br></br>', 
                                                    'Latest Date: ', curr_point()$Date, '</br>',
                                                    'Duration: 2-Year', '</br>',
                                                    'Rate: ', round(res_curr$rate,3), "%"))
      p %>% layout(title = "2-Year Treasury Rates",
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
      treasury_pct <- treasury %>% dplyr::select(Date, "X3.Yr")
      #convert wide to long , drop nas
      treasury_long <- melt(treasury_pct, id.vars = c("Date"))
      treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
      treasury_long2 <- treasury_long %>% group_by(variable) %>% arrange(value) %>% 
        mutate(count = seq(n()),  percentile = ecdf(value)(value)) 
      #finding percentile pooint
      curr_p <- input$inputPercentile6
      res_currp <- find_ratecurr(curr_p, treasury_long2)
      #most recent point 
      curr = as.numeric(as.character(curr_point()[,"X3.Yr"])) #current point despite chosen time range
      res_curr <- find_seqcurr(curr, treasury_long2)
      #get the circular points for general percentiles 
      res_ps <- find_seqpercentiles(treasury_long2)
      
      output$outputPercentile6 <- renderText({ paste0("<font color=\"#000000\"><b>", round(res_currp$rate, 3), "%", "</b>") })
      
      p <- plot_ly(data = treasury_long2, 
                   legendgroup = "3-Year", x = ~percentile*100, y = ~value, 
                   type = 'scatter', mode = 'lines', 
                   color = "3-Year", hoverinfo = 'none') %>% 
        add_trace(p, data = res_ps, x = ~perct, y = ~rate, 
                  showlegend = F, mode = 'markers', marker = list(symbol = 'circle', size = 8), 
                  hoverinfo= 'text', 
                  text = ~paste0('Percentile: ', perct, '</br></br>', 
                                 'Duration: 3-Year', '</br>',
                                 'Rate: ', round(as.numeric(as.character(rate)),3), "%")) %>% 
        add_trace(p, x = res_currp$perct, y = res_currp$rate, 
                  showlegend = F, 
                  mode = 'markers', marker = list(symbol = 'circle', size = 10, color = "#aaaaaa"), 
                  hoverinfo= 'text', text = ~paste0('Percentile: ', round(res_currp$perct,3), '</br></br>', 
                                                    'Duration: 3-Year', '</br>',
                                                    'Rate: ', round(res_currp$rate, 3), "%")) %>% 
        add_trace(p, x = res_curr$perct*100, y = res_curr$rate, showlegend = F, 
                  mode = 'markers', marker = list(symbol = 'square', size = 10, color = "#000000"), 
                  hoverinfo= 'text', text = ~paste0('Percentile: ', round(res_curr$perct*100,3), '</br></br>', 
                                                    'Latest Date: ', curr_point()$Date, '</br>',
                                                    'Duration: 2-Year', '</br>',
                                                    'Rate: ', round(res_curr$rate,3), "%"))
      p %>% layout(title = "3-Year Treasury Rates",
                   legend = list(orientation = "h", xanchor = "center", x = 0.5, font = list(size = 15)), 
                   font = list(size = 12), 
                   xaxis = list(title = "Percentile", showgrid = FALSE, size = 8, tickangle = 0),
                   yaxis = list(title = "Treasury Rates", size = 8, tickangle = 0), 
                   annotations = list(x = res_curr$perct*100, y = res_curr$rate, text = curr_point()$Date, 
                                      xref = "x", yref = "y", showarrow = TRUE, arrowhead = 7, ax = 20, ay = -40))%>% 
        config(displaylogo = FALSE, modeBarButtonsToRemove = list("zoomIn2d", "zoomOut2d", "zoom2d", "autoScale2d", "resetScale2d", "select2d", 
                                                                  "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "pan2d"))
    })
    
    output$plotPercentile7 <- renderPlotly({
      treasury <- treasury_time(input$percentileTime2[1], input$percentileTime2[2])
      treasury_pct <- treasury %>% dplyr::select(Date, "X5.Yr")
      #convert wide to long , drop nas
      treasury_long <- melt(treasury_pct, id.vars = c("Date"))
      treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
      treasury_long2 <- treasury_long %>% group_by(variable) %>% arrange(value) %>% 
                            mutate(count = seq(n()),  percentile = ecdf(value)(value)) 
      #finding percentile pooint
      curr_p <- input$inputPercentile7
      res_currp <- find_ratecurr(curr_p, treasury_long2)
      #most recent point 
      curr = as.numeric(as.character(curr_point()[,"X5.Yr"])) #current point despite chosen time range
      res_curr <- find_seqcurr(curr, treasury_long2)
      #get the circular points for general percentiles 
      res_ps <- find_seqpercentiles(treasury_long2)
      
      output$outputPercentile7 <- renderText({ paste0("<font color=\"#000000\"><b>", round(res_currp$rate, 3), "%", "</b>") })
      
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
    
    output$plotPercentile8 <- renderPlotly({
      treasury <- treasury_time(input$percentileTime2[1], input$percentileTime2[2])
      treasury_pct <- treasury %>% dplyr::select(Date, "X7.Yr")
      #convert wide to long , drop nas
      treasury_long <- melt(treasury_pct, id.vars = c("Date"))
      treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
      treasury_long2 <- treasury_long %>% group_by(variable) %>% arrange(value) %>% 
        mutate(count = seq(n()),  percentile = ecdf(value)(value)) 
      #finding percentile pooint
      curr_p <- input$inputPercentile8
      res_currp <- find_ratecurr(curr_p, treasury_long2)
      #most recent point 
      curr = as.numeric(as.character(curr_point()[,"X7.Yr"])) #current point despite chosen time range
      res_curr <- find_seqcurr(curr, treasury_long2)
      #get the circular points for general percentiles 
      res_ps <- find_seqpercentiles(treasury_long2)
      
      output$outputPercentile8 <- renderText({ paste0("<font color=\"#000000\"><b>", round(res_currp$rate, 3), "%", "</b>") })
      
      p <- plot_ly(data = treasury_long2, 
                   legendgroup = "7-Year", x = ~percentile*100, y = ~value, 
                   type = 'scatter', mode = 'lines', 
                   color = "7-Year", hoverinfo = 'none') %>% 
        add_trace(p, data = res_ps, x = ~perct, y = ~rate, 
                  showlegend = F, mode = 'markers', marker = list(symbol = 'circle', size = 8), 
                  hoverinfo= 'text', 
                  text = ~paste0('Percentile: ', perct, '</br></br>', 
                                 'Duration: 7-Year', '</br>',
                                 'Rate: ', round(as.numeric(as.character(rate)),3), "%")) %>% 
        add_trace(p, x = res_currp$perct, y = res_currp$rate, 
                  showlegend = F, 
                  mode = 'markers', marker = list(symbol = 'circle', size = 10, color = "#aaaaaa"), 
                  hoverinfo= 'text', text = ~paste0('Percentile: ', round(res_currp$perct,3), '</br></br>', 
                                                    'Duration: 7-Year', '</br>',
                                                    'Rate: ', round(res_currp$rate, 3), "%")) %>% 
        add_trace(p, x = res_curr$perct*100, y = res_curr$rate, showlegend = F, 
                  mode = 'markers', marker = list(symbol = 'square', size = 10, color = "#000000"), 
                  hoverinfo= 'text', text = ~paste0('Percentile: ', round(res_curr$perct*100,3), '</br></br>', 
                                                    'Latest Date: ', curr_point()$Date, '</br>',
                                                    'Duration: 7-Year', '</br>',
                                                    'Rate: ', round(res_curr$rate,3), "%"))
      
      p %>% layout(title = "7-Year Treasury Rates",
                   legend = list(orientation = "h", xanchor = "center", x = 0.5, font = list(size = 15)), 
                   font = list(size = 12), 
                   xaxis = list(title = "Percentile", showgrid = FALSE, size = 8, tickangle = 0),
                   yaxis = list(title = "Treasury Rates", size = 8, tickangle = 0), 
                   annotations = list(x = res_curr$perct*100, y = res_curr$rate, text = curr_point()$Date, 
                                      xref = "x", yref = "y", showarrow = TRUE, arrowhead = 7, ax = 20, ay = -40))%>% 
        config(displaylogo = FALSE, modeBarButtonsToRemove = list("zoomIn2d", "zoomOut2d", "zoom2d", "autoScale2d", "resetScale2d", "select2d", 
                                                                  "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "pan2d"))
    })
    
    output$plotPercentile9 <- renderPlotly({
      treasury <- treasury_time(input$percentileTime2[1], input$percentileTime2[2])
      treasury_pct <- treasury %>% dplyr::select(Date, "X10.Yr")
      #convert wide to long , drop nas
      treasury_long <- melt(treasury_pct, id.vars = c("Date"))
      treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
      treasury_long2 <- treasury_long %>% group_by(variable) %>% arrange(value) %>% 
                           mutate(count = seq(n()),  percentile = ecdf(value)(value)) 
      #finding percentile pooint
      curr_p <- input$inputPercentile9
      res_currp <- find_ratecurr(curr_p, treasury_long2)
      #most recent point 
      curr = as.numeric(as.character(curr_point()[,"X10.Yr"])) #current point despite chosen time range
      res_curr <- find_seqcurr(curr, treasury_long2)
      #get the circular points for general percentiles 
      res_ps <- find_seqpercentiles(treasury_long2)
      
      output$outputPercentile9 <- renderText({ paste0("<font color=\"#000000\"><b>", round(res_currp$rate, 3), "%", "</b>") })
      
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
    
    output$plotPercentile10 <- renderPlotly({
      treasury <- treasury_time(input$percentileTime2[1], input$percentileTime2[2])
      treasury_pct <- treasury %>% dplyr::select(Date, "X20.Yr")
      #convert wide to long , drop nas
      treasury_long <- melt(treasury_pct, id.vars = c("Date"))
      treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
      treasury_long2 <- treasury_long %>% group_by(variable) %>% arrange(value) %>% 
        mutate(count = seq(n()),  percentile = ecdf(value)(value)) 
      #finding percentile pooint
      curr_p <- input$inputPercentile10
      res_currp <- find_ratecurr(curr_p, treasury_long2)
      #most recent point 
      curr = as.numeric(as.character(curr_point()[,"X10.Yr"])) #current point despite chosen time range
      res_curr <- find_seqcurr(curr, treasury_long2)
      #get the circular points for general percentiles 
      res_ps <- find_seqpercentiles(treasury_long2)
      
      output$outputPercentile10 <- renderText({ paste0("<font color=\"#000000\"><b>", round(res_currp$rate, 3), "%", "</b>") })
      
      p <- plot_ly(data = treasury_long2, 
                   legendgroup = "10-Year", x = ~percentile*100, y = ~value, 
                   type = 'scatter', mode = 'lines', 
                   color = "20-Year", hoverinfo = 'none') %>% 
        add_trace(p, data = res_ps, x = ~perct, y = ~rate, 
                  showlegend = F, mode = 'markers', marker = list(symbol = 'circle', size = 8), 
                  hoverinfo= 'text', 
                  text = ~paste0('Percentile: ', perct, '</br></br>', 
                                 'Duration: 20-Year', '</br>',
                                 'Rate: ', round(as.numeric(as.character(rate)),3), "%")) %>% 
        add_trace(p, x = res_currp$perct, y = res_currp$rate, 
                  showlegend = F, 
                  mode = 'markers', marker = list(symbol = 'circle', size = 10, color = "#aaaaaa"), 
                  hoverinfo= 'text', text = ~paste0('Percentile: ', round(res_currp$perct,3), '</br></br>', 
                                                    'Duration: 20-Year', '</br>',
                                                    'Rate: ', round(res_currp$rate, 3), "%")) %>% 
        add_trace(p, x = res_curr$perct*100, y = res_curr$rate, showlegend = F,  
                  mode = 'markers', marker = list(symbol = 'square', size = 10, color = "#000000"), 
                  hoverinfo= 'text', text = ~paste0('Percentile: ', round(res_curr$perct*100,3), '</br></br>', 
                                                    'Latest Date: ', curr_point()$Date, '</br>',
                                                    'Duration: 20-Year', '</br>',
                                                    'Rate: ', round(res_curr$rate,3), "%"))
      p %>% layout(title = "20-Year Treasury Rates",
                   legend = list(orientation = "h", xanchor = "center", x = 0.5, font = list(size = 15)), 
                   font = list(size = 12), 
                   xaxis = list(title = "Percentile", showgrid = FALSE, size = 8, tickangle = 0),
                   yaxis = list(title = "Treasury Rates", size = 8, tickangle = 0), 
                   annotations = list(x = res_curr$perct*100, y = res_curr$rate, text = curr_point()$Date, 
                                      xref = "x", yref = "y", showarrow = TRUE, arrowhead = 7, ax = 20, ay = -40))%>% 
        config(displaylogo = FALSE, modeBarButtonsToRemove = list("zoomIn2d", "zoomOut2d", "zoom2d", "autoScale2d", "resetScale2d", "select2d", 
                                                                  "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "pan2d"))
    })
    
    output$plotPercentile11 <- renderPlotly({
      treasury <- treasury_time(input$percentileTime2[1], input$percentileTime2[2])
      treasury_pct <- treasury %>% dplyr::select(Date, "X30.Yr")
      #convert wide to long , drop nas
      treasury_long <- melt(treasury_pct, id.vars = c("Date"))
      treasury_long <- treasury_long[!is.na(as.numeric(as.character(treasury_long$value))),]
      treasury_long2 <- treasury_long %>% group_by(variable) %>% arrange(value) %>% 
                            mutate(count = seq(n()),  percentile = ecdf(value)(value)) 
      #finding percentile pooint
      curr_p <- input$inputPercentile11
      res_currp <- find_ratecurr(curr_p, treasury_long2)
      #most recent point 
      curr = as.numeric(as.character(curr_point()[,"X30.Yr"])) #current point despite chosen time range
      res_curr <- find_seqcurr(curr, treasury_long2)
      #get the circular points for general percentiles 
      res_ps <- find_seqpercentiles(treasury_long2)
      
      output$outputPercentile11 <- renderText({ paste0("<font color=\"#000000\"><b>", round(res_currp$rate, 3), "%", "</b>") })
      
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
    
    
    
    ###### weighted rate tab ############3
    weighted_treasury3 <- reactive({
          treasury <- treasury_time(input$percentileTime3[1], input$percentileTime3[2])
          treasury <- treasury[,c("Date", "X1.Mo", "X3.Mo", "X6.Mo", "X1.Yr", "X2.Yr", "X3.Yr", "X5.Yr", "X7.Yr", "X10.Yr", "X20.Yr", "X30.Yr")]
          treasury$value <- treasury$X1.Mo*(1/12)+treasury$X3.Mo*(3/12)+treasury$X6.Mo*(1/2)+treasury$X1.Yr*(1)+
            treasury$X2.Yr*(2)+treasury$X3.Yr*(3)+treasury$X5.Yr*(5)+treasury$X7.Yr*(7)+treasury$X10.Yr*(10)+
            treasury$X20.Yr*(20)+treasury$X30.Yr*(30)
          treasury_f2 <- treasury[!is.na(treasury$value),] 
          treasury_f3 <- treasury_f2 %>% arrange(value) %>% mutate(count = seq(n()), percentile = ecdf(value)(value))
          return(treasury_f3)
    })
    
    output$plotRate3 <- renderPlotly({
      
      print(weighted_treasury3()$percentile)

      #res index is the data of interest we found from percentile input
      res_index <- find_last_percentile(weighted_treasury3(), input$inputPercentileW)
      
      #create data in order to find individual duration percentiles
      treasury <- treasury_time(input$percentileTime3[1], input$percentileTime3[2])
      treasury <- treasury[,c("Date", "X1.Mo", "X3.Mo", "X6.Mo", "X1.Yr", "X2.Yr", "X3.Yr", "X5.Yr", "X7.Yr", "X10.Yr", "X20.Yr", "X30.Yr")]
      treasury_long <- melt(treasury, id.vars = c("Date"))
      treasury_long2 <- treasury_long[!is.na(treasury_long$value),] %>% group_by(variable) %>% 
        arrange(value) %>% mutate(count = seq(n()),  percentile = ecdf(value)(value))
      #for each of the inputs
      res_curr1 <- find_seqcurr(res_index$X1.Mo, treasury_long2[which(treasury_long2$variable == "X1.Mo"),])
      res_curr2 <- find_seqcurr(res_index$X3.Mo, treasury_long2[which(treasury_long2$variable == "X3.Mo"),])
      res_curr3 <- find_seqcurr(res_index$X6.Mo, treasury_long2[which(treasury_long2$variable == "X6.Mo"),])
      res_curr4 <- find_seqcurr(res_index$X1.Yr, treasury_long2[which(treasury_long2$variable == "X1.Yr"),])
      res_curr5 <- find_seqcurr(res_index$X2.Yr, treasury_long2[which(treasury_long2$variable == "X2.Yr"),])
      res_curr6 <- find_seqcurr(res_index$X3.Yr, treasury_long2[which(treasury_long2$variable == "X3.Yr"),])
      res_curr7 <- find_seqcurr(res_index$X5.Yr, treasury_long2[which(treasury_long2$variable == "X5.Yr"),])
      res_curr8 <- find_seqcurr(res_index$X7.Yr, treasury_long2[which(treasury_long2$variable == "X7.Yr"),])
      res_curr9 <- find_seqcurr(res_index$X10.Yr, treasury_long2[which(treasury_long2$variable == "X10.Yr"),])
      res_curr10 <- find_seqcurr(res_index$X20.Yr, treasury_long2[which(treasury_long2$variable == "X20.Yr"),])
      res_curr11 <- find_seqcurr(res_index$X30.Yr, treasury_long2[which(treasury_long2$variable == "X30.Yr"),])
      #result of all the rates
      res_curall <- rbind(res_curr1, res_curr2, res_curr3, res_curr4, res_curr5, res_curr6, res_curr7, res_curr8, 
                          res_curr9, res_curr10, res_curr11)
      res_curall$name <- c("1-Month", "3-Month", "6-Month", "1-Year", "2-Year", "3-Year", "5-Year", "7-Year", "10-Year", "20-Year", "30-Year")
      res_curall$name <- factor(res_curall$name, levels = c("1-Month", "3-Month", "6-Month", "1-Year", "2-Year", "3-Year", 
                                                            "5-Year", "7-Year", "10-Year", "20-Year", "30-Year"))
      
      #output for weighted index
      output$outputRateW <- renderText({ paste("<font color=\"#000000\"><b>", round(res_index$value, 3),  "</b>") })
      output$outputRateWDate <- renderText({ paste("<font color=\"#000000\"><b>", res_index$Date,  "</b>") })
      
      #output for rates at weighted index
      output$outputRateW1 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_index$X1.Mo, 3),  "</b>") })
      output$outputRateW2 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_index$X3.Mo, 3),  "</b>") })
      output$outputRateW3 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_index$X6.Mo, 3),  "</b>") })
      output$outputRateW4 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_index$X1.Yr, 3),  "</b>") })
      output$outputRateW5 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_index$X2.Yr, 3),  "</b>") })
      output$outputRateW6 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_index$X3.Yr, 3),  "</b>") })
      output$outputRateW7 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_index$X5.Yr, 3),  "</b>") })
      output$outputRateW8 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_index$X7.Yr, 3),  "</b>") })
      output$outputRateW9 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_index$X10.Yr, 3),  "</b>") })
      output$outputRateW10 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_index$X20.Yr, 3),  "</b>") })
      output$outputRateW11 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_index$X30.Yr, 3),  "</b>") })
      
      #output for percentiles calculated -- this depends on the chosen time range
      output$outputRateWP1 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr1$perct, 3)*100,  "</b>") })
      output$outputRateWP2 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr2$perct, 3)*100,  "</b>") })
      output$outputRateWP3 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr3$perct, 3)*100,  "</b>") })
      output$outputRateWP4 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr4$perct, 3)*100,  "</b>") })
      output$outputRateWP5 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr5$perct, 3)*100,  "</b>") })
      output$outputRateWP6 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr6$perct, 3)*100,  "</b>") })
      output$outputRateWP7 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr7$perct, 3)*100,  "</b>") })
      output$outputRateWP8 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr8$perct, 3)*100,  "</b>") })
      output$outputRateWP9 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr9$perct, 3)*100,  "</b>") })
      output$outputRateWP10 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr10$perct, 3)*100,  "</b>") })
      output$outputRateWP11 <- renderText({ paste("<font color=\"#000000\"><b>", round(res_curr11$perct, 3)*100,  "</b>") })
      
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
    
}

# Run the application 
shinyApp(ui = ui, server = server)
