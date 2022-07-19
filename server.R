#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  
  timeSeries_stock <- reactive({
    
    
    days <- input$end_sp_val -input$start_sp_val
    
    metricName2 <- ifelse(input$ticker == "P/E", paste("FF_PE(ANN_R,", input$start_up, input$start_down, ",D)", sep = ""),
                          ifelse(input$ticker == "P/E Est", paste("FE_VALUATION(PE,MEAN,ANN_ROLL,+1,", input$start_up, input$start_down, ",D,'')", sep=""),
                                 ifelse(input$ticker == "P/B", paste("FF_PBK(ANN_R,", input$start_up, input$start_down, ",D)", sep=""),
                                        ifelse(input$ticker == "P/CF", paste("FF_PCF(ANN_R,", input$start_up, input$start_down, ",D)", sep=""),
                                               ifelse(input$ticker == "EV/Sales", paste("FF_ENTRPR_VAL_SALES(ANN_R,", input$start_up, input$start_down, ",D)", sep=""),
                                                      ifelse(input$ticker == "EV/EBITDA", paste("FF_ENTRPR_VAL_EBITDA_OPER(ANN_R,", input$start_up, input$start_down, ",D)", sep=""),   
                                               NULL))))))
    
    
    

      timeSeries <-  FSQuery("SP500", metricName2) %>%
        drop_na() %>%
        select(1,3) %>%
        rename(Close = 2)

    
    
    
    
    timeSeries <- xts(timeSeries[,2], order.by = timeSeries$date)
    
    
  })
  
  
  output$plot_up <- renderPlot({
    
    
    metricName2 <- ifelse(input$ticker == "P/E", paste("FF_PE(ANN_R,", input$start_up, input$start_down, ",D)", sep = ""),
                          ifelse(input$ticker == "P/E Est", paste("FE_VALUATION(PE,MEAN,ANN_ROLL,+1,", input$start_up, input$start_down, ",D,'')", sep=""),
                                 ifelse(input$ticker == "P/B", paste("FF_PBK(ANN_R,", input$start_up, input$start_down, ",D)", sep=""),
                                        ifelse(input$ticker == "P/CF", paste("FF_PCF(ANN_R,", input$start_up, input$start_down, ",D)", sep=""),
                                               ifelse(input$ticker == "EV/Sales", paste("FF_ENTRPR_VAL_SALES(ANN_R,", input$start_up, input$start_down, ",D)", sep=""),
                                                      ifelse(input$ticker == "EV/EBITDA", paste("FF_ENTRPR_VAL_EBITDA_OPER(ANN_R,", input$start_up, input$start_down, ",D)", sep=""),   
                                                             NULL))))))
    
    timeSeries <- timeSeries_sp_val()
    
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
