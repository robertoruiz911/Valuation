library(lubridate)
library(bizdays)
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
library(padr)
library(chron)
source('FSquery.R')
cal <-  create.calendar(name = "mycal", weekdays=c("saturday", "sunday"))


shinyServer(function(input, output) {
  
  
  timeSeries_stock_up <- reactive({
    
    ticker <- paste0(toupper(input$ticker), '-US')
    
    days <- input$end_up -input$start_up
    
    
    # time series for the 'up' half of the presentation screen - testing of queries need to be done here....
    metricNameUp <- ifelse(input$upside == "P/E", list(paste('P_PRICE(', input$end_up, ',', input$start_up, ',D,USD)', sep = ""),
                                                       paste('FF_EPS(ANN_R,', input$end_up, ',', input$start_up, ',D,,USD)', sep = "")),
                           ifelse(input$upside == "P/E Est", paste("FE_VALUATION(PE,MEAN,ANN_ROLL,+1,", input$end_up,",", input$start_up, ",D,'')", sep=""),
                                  ifelse(input$upside == "P/B", list(paste('P_PRICE(', input$end_up, ',', input$start_up, ',D,USD)', sep = ""),
                                                                     paste('FF_BPS(ANN_R,', input$end_up, ',', input$start_up, ',D,,USD)', sep = "")),
                                         ifelse(input$upside == "P/CF", list(paste('P_PRICE(', input$end_up, ',', input$start_up, ',D,USD)', sep = ""),
                                                                             paste('FF_OPER_PS_NET_CF(ANN_R,', input$end_up, ',', input$start_up, ',D,,USD)', sep = "")),
                                                ifelse(input$upside == "EV/Sales", paste("FF_ENTRPR_VAL_SALES_DAILY(", input$end_up,",", input$start_up, ",D,,,)", sep=""),
                                                       ifelse(input$upside == "EV/EBITDA", paste("FF_ENTRPR_VAL_EBITDA_OPER_DAILY(", input$end_up,",", input$start_up, ",D,,,)", sep=""),   
                                                              NULL))))))
    if(input$upside == 'P/E') {
      timeSeriesUp <- ratioQuery(ticker, metricNameUp, input$start_up, input$end_up)
    } else if (input$upside == 'P/B') {
      timeSeriesUp <- ratioQuery(ticker, metricNameUp, input$start_up, input$end_up)
    } else if (input$upside == 'P/CF') {
      timeSeriesUp <- ratioQuery(ticker, metricNameUp, input$start_up, input$end_up)
    } else {
      timeSeriesUp <-  FSquery(ticker, metricNameUp, input$start_up, input$end_up)
    }

    timeSeriesUp <- xts(timeSeriesUp[,2], order.by = timeSeriesUp$date)
    timeSeriesUp 
    
  })
  
  
  
  timeSeries_stock_down <- reactive({
    
    ticker <- paste0(toupper(input$ticker), '-US')
    
    days <- input$end_down -input$start_down
    
    
    
    # time series for the 'down' half of the presentation screen
    metricNameDown <- ifelse(input$downside == "P/E", paste("FF_PE(ANN_R,", input$start_down,",", input$end_down, ",D)", sep = ""),
                             ifelse(input$downside == "P/E Est", paste("FE_VALUATION(PE,MEAN,ANN_ROLL,+1,", input$start_down,",", input$end_down, ",D,'')", sep=""),
                                    ifelse(input$downside == "P/B", paste("FF_PBK(ANN_R,", input$start_down,",", input$end_down, ",D)", sep=""),
                                           ifelse(input$downside == "P/CF", paste("FF_PCF(ANN_R,", input$start_down,",", input$end_down, ",D)", sep=""),
                                                  ifelse(input$downside == "EV/Sales", paste("FF_ENTRPR_VAL_SALES(ANN_R,", input$start_down, ",", input$end_down, ",D)", sep=""),
                                                         ifelse(input$downside == "EV/EBITDA", paste("FF_ENTRPR_VAL_EBITDA_OPER(ANN_R,", input$start_down,",", input$end_down, ",D)", sep=""),   
                                                                NULL))))))
    timeSeriesDown <-  FSquery(ticker, metricNameDown) %>%
      drop_na() %>%
      select(1,3) %>%
      rename(Close = 2)
    
    timeSeriesDown <- xts(timeSeriesDown[,2], order.by = timeSeriesDown$date)
    timeSeriesDown 
  })
  
  
  output$plot_up <- renderPlot({
    
    # question: why are these ifelse statements needed here in the plot function? there is not anther query required....
    ## Answer: This is used only for the title of the chart
    metricNameUp <- ifelse(input$upside == "P/E", paste("FF_PE(ANN_R,", input$start_up,",", input$end_up, ",D)", sep = ""),
                           ifelse(input$upside == "P/E Est", paste("FE_VALUATION(PE,MEAN,ANN_ROLL,+1,", input$start_up,",", input$end_up, ",D,'')", sep=""),
                                  ifelse(input$upside == "P/B", paste("FF_PBK(ANN_R,", input$start_up,",", input$end_up, ",D)", sep=""),
                                         ifelse(input$upside == "P/CF", paste("FF_PCF(ANN_R,", input$start_up,",", input$end_up, ",D)", sep=""),
                                                ifelse(input$upside == "EV/Sales", paste("FF_ENTRPR_VAL_SALES(ANN_R,", input$start_up,",", input$end_up, ",D)", sep=""),
                                                       ifelse(input$upside == "EV/EBITDA", paste("FF_ENTRPR_VAL_EBITDA_OPER(ANN_R,", input$start_up,",", input$end_up, ",D)", sep=""),   
                                                              NULL))))))
    # below needs to be updated to reflect current structure of the app....
    timeSeries <- timeSeries_stock_up()
    
    
    
    #ToDo: there are two plots required here: up and down.  Instead of text on the charts, we need a table published above each of the charts.
    plot <- function(){
      
      chartSeries(timeSeries,
                  name = metricNameUp,
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
  
  
  
  output$plot_down <- renderPlot({
    
    # question: why are these ifelse statements needed here in the plot function? there is not anther query required....
    ## Answer: This is used only for the title of the chart
    metricNameDown <- ifelse(input$downside == "P/E", paste("FF_PE(ANN_R,", input$start_down,",", input$end_down, ",D)", sep = ""),
                             ifelse(input$downside == "P/E Est", paste("FE_VALUATION(PE,MEAN,ANN_ROLL,+1,", input$start_down,",", input$end_down, ",D,'')", sep=""),
                                    ifelse(input$downside == "P/B", paste("FF_PBK(ANN_R,", input$start_down,",", input$end_down, ",D)", sep=""),
                                           ifelse(input$downside == "P/CF", paste("FF_PCF(ANN_R,", input$start_down,",", input$end_down, ",D)", sep=""),
                                                  ifelse(input$downside == "EV/Sales", paste("FF_ENTRPR_VAL_SALES(ANN_R,", input$start_down, ",", input$end_down, ",D)", sep=""),
                                                         ifelse(input$downside == "EV/EBITDA", paste("FF_ENTRPR_VAL_EBITDA_OPER(ANN_R,", input$start_down,",", input$end_down, ",D)", sep=""),   
                                                                NULL))))))
    
    # below needs to be updated to reflect current structure of the app....
    timeSeries <- timeSeries_stock_down()
    
    
    
    #ToDo: there are two plots required here: up and down.  Instead of text on the charts, we need a table published above each of the charts.
    plot <- function(){
      
      chartSeries(timeSeries,
                  name = metricNameDown,
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
