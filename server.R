library(lubridate)
library(quantmod)
library(tidyverse)
library(highcharter)
library(tidyquant)
library(timetk)
library(shinyWidgets)
library(shiny)
library(fredr)
library(here)
library(shiny)
library(httr)
library(jsonlite)
source('FSquery.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  timeSeries_stock <- reactive({
    
    ticker <- paste0(toupper(input$ticker), '-US')
    
    days <- input$end_sp_val -input$start_sp_val
    
    
    # time series for the 'up' half of the presentation screen
    metricNameUp <- ifelse(input$ticker == "P/E", paste("FF_PE(ANN_R,", input$start_up,",", input$end_up, ",D)", sep = ""),
                          ifelse(input$ticker == "P/E Est", paste("FE_VALUATION(PE,MEAN,ANN_ROLL,+1,", input$start_up,",", input$end_up, ",D,'')", sep=""),
                                 ifelse(input$ticker == "P/B", paste("FF_PBK(ANN_R,", input$start_up,",", input$end_up, ",D)", sep=""),
                                        ifelse(input$ticker == "P/CF", paste("FF_PCF(ANN_R,", input$start_up,",", input$end_up, ",D)", sep=""),
                                               ifelse(input$ticker == "EV/Sales", paste("FF_ENTRPR_VAL_SALES(ANN_R,", input$start_up,",", input$end_up, ",D)", sep=""),
                                                      ifelse(input$ticker == "EV/EBITDA", paste("FF_ENTRPR_VAL_EBITDA_OPER(ANN_R,", input$start_up,",", input$end_up, ",D)", sep=""),   
                                                             NULL))))))
    timeSeriesUp <-  FSquery(ticker, metricNameUp) %>%
      drop_na() %>%
      select(1,3) %>%
      rename(Close = 2)
    
    timeSeriesUp <- xts(timeSeriesUp[,2], order.by = timeSeriesUp$date)
    
    # time series for the 'down' half of the presentation screen
    metricNameDown <- ifelse(input$ticker == "P/E", paste("FF_PE(ANN_R,", input$start_down,",", input$end_down, ",D)", sep = ""),
                           ifelse(input$ticker == "P/E Est", paste("FE_VALUATION(PE,MEAN,ANN_ROLL,+1,", input$start_down,",", input$end_down, ",D,'')", sep=""),
                                  ifelse(input$ticker == "P/B", paste("FF_PBK(ANN_R,", input$start_down,",", input$end_down, ",D)", sep=""),
                                         ifelse(input$ticker == "P/CF", paste("FF_PCF(ANN_R,", input$start_down,",", input$end_down, ",D)", sep=""),
                                                ifelse(input$ticker == "EV/Sales", paste("FF_ENTRPR_VAL_SALES(ANN_R,", input$start_down, ",", input$end_down, ",D)", sep=""),
                                                       ifelse(input$ticker == "EV/EBITDA", paste("FF_ENTRPR_VAL_EBITDA_OPER(ANN_R,", input$start_down,",", input$end_down, ",D)", sep=""),   
                                                              NULL))))))
    timeSeriesDown <-  FSquery(ticker, metricNameDown) %>%
      drop_na() %>%
      select(1,3) %>%
      rename(Close = 2)
    
    timeSeriesDown <- xts(timeSeriesDown[,2], order.by = timeSeriesDown$date)
  })
  
  
  output$plot_up <- renderPlot({
    
    # question: why are these ifelse statements needed here in the plot function? there is not anther query required....
    metricName2 <- ifelse(input$ticker == "P/E", paste("FF_PE(ANN_R,", input$start_down, input$end_down, ",D)", sep = ""),
                          ifelse(input$ticker == "P/E Est", paste("FE_VALUATION(PE,MEAN,ANN_ROLL,+1,", input$start_down, input$end_down, ",D,'')", sep=""),
                                 ifelse(input$ticker == "P/B", paste("FF_PBK(ANN_R,", input$start_down, input$end_down, ",D)", sep=""),
                                        ifelse(input$ticker == "P/CF", paste("FF_PCF(ANN_R,", input$start_down, input$end_down, ",D)", sep=""),
                                               ifelse(input$ticker == "EV/Sales", paste("FF_ENTRPR_VAL_SALES(ANN_R,", input$start_down, input$end_down, ",D)", sep=""),
                                                      ifelse(input$ticker == "EV/EBITDA", paste("FF_ENTRPR_VAL_EBITDA_OPER(ANN_R,", input$start_down, input$end_down, ",D)", sep=""),   
                                                             NULL))))))
    
    # below needs to be updated to reflect currrent sturcture of the app....
    timeSeries <- timeSeries_sp_val()
    
    
    
    #ToDo: there are two plots required here: up and down.  Instead of text on the charts, we need a table published above each of the charts.
    plot <- function(){
      
      chartSeries(timeSeries,
                  name = metricName,
                  type="line",
                  theme=chartTheme('white'))
      ### Text
      
      max <- round(max(timeSeries$Close),1)
      min <- round(min(timeSeries$Close),1)
      mean <- round(mean(timeSeries$Close),1)
      
      
      max <- paste("Max.:", max)  
      min <- paste("Min.:", min)
      mean <- paste("Avg.:", mean)
      #######################################################
      
      timeSeries <- as.data.frame(timeSeries)
      timeSeries$date <- as_date(row.names(timeSeries))
      
      max_date <- max(timeSeries$date)
      min_date <- min(timeSeries$date)
      
      ### Difference selected period
      
      one <- subset(timeSeries, timeSeries$date == max_date | timeSeries$date == min_date)
      
      period <- max_date - min_date
      period
      
      change <- (one$Close[2] - one$Close[1]) / one$Close[1]
      change <- round(change*100,2)
      
      change <- paste("Period Change:", change, "%")
      
      ############################################################################
      ## one year
      
      max_date <- max(timeSeries$date)
      one_year <- max_date -365
      
      one_year <- subset(timeSeries, timeSeries$date >=  one_year)
      
      one_year <- subset(one_year, one_year$date == min(one_year$date) | one_year$date == max(one_year$date))
      
      change_1year <- (one_year$Close[2] - one_year$Close[1])/one_year$Close[1]
      change_1year*100
      
      if(input$end_sp_val - input$start_sp_val >= 365){
        change_1year <-  paste("1Y Change:", round(change_1year*100,2), "%")
      }else{change_1year <-  paste("1Y Change:", "NA")}
      
      text <- paste(max, min, mean, change, change_1year, sep = "     ")
      
      
      mtext(text, side=1, outer=FALSE, cex = 0.75)
      
    }
    
    plot()
    
    
  })
  
  
  
})
