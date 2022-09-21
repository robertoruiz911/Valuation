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
library(DT)

library(PerformanceAnalytics)
#library(chron)
source('FSquery.R')



shinyServer(function(input, output) {
  
  
  
  
  
  timeSeries_stock_up <- eventReactive(input$Calculate,{
    
    ticker <- paste0(toupper(input$ticker), '-US')
    
    days <- input$end_up - input$start_up
    
    
    # time series for the 'up' half of the presentation screen - testing of queries need to be done here....
    if (input$upside == 'P/E') {
        metricNameUp <- list(paste('P_PRICE(', input$end_up, ',', input$start_up, ',D,USD)', sep = ""),
                             paste('FF_EPS(ANN_R,', input$end_up, ',', input$start_up, ',D,,USD)', sep = ""))
    } else if (input$upside == 'P/E NTM') {
        metricNameUp <- paste("FE_VALUATION(PE,MEAN,ANN_ROLL,+1,", input$end_up,",", input$start_up, ",D,'')", sep="")
    } else if (input$upside == 'P/E NTM vs S&P500') { 
        metricNameUp <- paste("FE_VALUATION(PE,MEAN,ANN_ROLL,+1,", input$end_up,",", input$start_up, ",D,'')", sep="")
    } else if (input$upside == 'P/B') {
        metricNameUp <- list(paste('P_PRICE(', input$end_up, ',', input$start_up, ',D,USD)', sep = ""),
                           paste('FF_BPS(ANN_R,', input$end_up, ',', input$start_up, ',D,,USD)', sep = ""))
    } else if (input$upside == 'P/CF') {
        metricNameUp <- list(paste('P_PRICE(', input$end_up, ',', input$start_up, ',D,USD)', sep = ""),
                             paste('FF_OPER_PS_NET_CF(ANN_R,', input$end_up, ',', input$start_up, ',D,,USD)', sep = ""))
    } else if (input$upside == 'EV/Sales') {
        metricNameUp <- paste("FF_ENTRPR_VAL_SALES_DAILY(", input$end_up,",", input$start_up, ",D,,,)", sep="")
    } else if (input$upside == 'EV/EBITDA') {
        metricNameUp <- paste("FF_ENTRPR_VAL_EBITDA_OPER_DAILY(", input$end_up,",", input$start_up, ",D,,,)", sep="")
    } else {#(input$upside == 'EV/EBITDA vs S&P500') {
        metricNameUp <- paste("FF_ENTRPR_VAL_EBITDA_OPER_DAILY(", input$end_up,",", input$start_up, ",D,,,)", sep="")
    } #else {
      #  metricNameUp <- paste("FF_DIV_YLD(ANN,", input$end_up,",", input$start_up, ",D)", sep="")
    #}
    
    
    
    if(input$upside == 'P/E NTM vs S&P500' || input$upside == 'EV/EBITDA vs S&P500') {
      if(input$upside == 'P/E NTM vs S&P500') {
        timeSeriesUp <-  relativeQuery(ticker, metricNameUp, input$start_up, input$end_up)
      } else {
        numerator <- FSquery(ticker, toString(metricNameUp), input$start_up, input$end_up)
        formula <- paste("FMA_EVAL_EBITDA(NTMA,", input$end_up,",", input$start_up, ",,)", sep="")
        denominator <- FSquery('SP50', formula , input$start_up, input$end_up)
        timeSeriesUp <- merge(numerator, denominator,by='date' , all = TRUE)
        timeSeriesUp <- timeSeriesUp[order(timeSeriesUp$date),]
        colnames(timeSeriesUp)[2] <- 'numerator'
        colnames(timeSeriesUp)[3] <- 'denominator'
        timeSeriesUp <- timeSeriesUp %>% fill(denominator)
        timeSeriesUp <- timeSeriesUp[complete.cases(timeSeriesUp), ]
        timeSeriesUp$value <- timeSeriesUp$numerator / timeSeriesUp$denominator
        timeSeriesUp <-  timeSeriesUp %>%
          drop_na() %>%
          select(1,4) %>%
          rename(Close = 2)
      }
    } else if(input$upside == 'P/E' || input$upside == 'P/B' ||  input$upside == 'P/CF') {
      timeSeriesUp <- ratioQuery(ticker, metricNameUp, input$start_up, input$end_up)
    } else {
      timeSeriesUp <-  FSquery(ticker, metricNameUp, input$start_up, input$end_up)
    }

    timeSeriesUp <- xts(timeSeriesUp[,2], order.by = timeSeriesUp$date)
    colnames(timeSeriesUp)[1] <- 'Close'
    timeSeriesUp 

  })
  
  
  
  timeSeries_stock_down <- eventReactive(input$Calculate, {

    ticker <- paste0(toupper(input$ticker), '-US')

    days <- input$end_down - input$start_down


    # time series for the 'down' half of the presentation screen
    if (input$downside == 'P/E') {
        metricNameDown <- list(paste('P_PRICE(', input$end_down, ',', input$start_down, ',D,USD)', sep = ""),
                               paste('FF_EPS(ANN_R,', input$end_down, ',', input$start_down, ',D,,USD)', sep = ""))
    } else if (input$downside == 'P/E NTM') {
        metricNameDown <- paste("FE_VALUATION(PE,MEAN,ANN_ROLL,+1,", input$end_down,",", input$start_down, ",D,'')", sep="")
    } else if (input$downside == 'P/E NTM vs S&P500') { 
        metricNameDown <- paste("FE_VALUATION(PE,MEAN,ANN_ROLL,+1,", input$end_down,",", input$start_down, ",D,'')", sep="")
    } else if (input$downside == 'P/B') {
        metricNameDown <- list(paste('P_PRICE(', input$end_down, ',', input$start_down, ',D,USD)', sep = ""),
                               paste('FF_BPS(ANN_R,', input$end_down, ',', input$start_down, ',D,,USD)', sep = ""))
    } else if (input$downside == 'P/CF') {
        metricNameDown <- list(paste('P_PRICE(', input$end_down, ',', input$start_down, ',D,USD)', sep = ""),
                               paste('FF_OPER_PS_NET_CF(ANN_R,', input$end_down, ',', input$start_down, ',D,,USD)', sep = ""))
    } else if (input$downside == 'EV/Sales') {
        metricNameDown <- paste("FF_ENTRPR_VAL_SALES_DAILY(", input$end_down,",", input$start_down, ",D,,,)", sep="")
    } else if (input$downside == 'EV/EBITDA') {
        metricNameDown <- paste("FF_ENTRPR_VAL_EBITDA_OPER_DAILY(", input$end_down,",", input$start_down, ",D,,,)", sep="")
    } else { 
        metricNameDown <- paste("FF_ENTRPR_VAL_EBITDA_OPER_DAILY(", input$end_down,",", input$start_down, ",D,,,)", sep="")
    }
    
    
    if(input$downside == 'P/E NTM vs S&P500' || input$downside == 'EV/EBITDA vs S&P500') {
      if(input$downside == 'P/E NTM vs S&P500') {  
        timeSeriesDown <-  relativeQuery(ticker, metricNameDown, input$start_down, input$end_down)
      } else {
        numerator <- FSquery(ticker, toString(metricNameDown), input$start_down, input$end_down)
        formula <- paste("FMA_EVAL_EBITDA(NTMA,", input$end_down,",", input$start_down, ",,)", sep="")
        denominator <- FSquery('SP50', formula , input$start_down, input$end_down)
        timeSeriesDown <- merge(numerator, denominator,by='date' , all = TRUE)
        timeSeriesDown <- timeSeriesDown[order(timeSeriesDown$date),]
        colnames(timeSeriesDown)[2] <- 'numerator'
        colnames(timeSeriesDown)[3] <- 'denominator'
        timeSeriesDown <- timeSeriesDown %>% fill(denominator)
        timeSeriesDown <- timeSeriesDown[complete.cases(timeSeriesDown), ]
        timeSeriesDown$value <- timeSeriesDown$numerator / timeSeriesDown$denominator
        timeSeriesDown <-  timeSeriesDown %>%
          drop_na() %>%
          select(1,4) %>%
          rename(Close = 2)
      }
      
      
      
    } else if(input$downside == 'P/E' || input$downside == 'P/B' ||  input$downside == 'P/CF') {
      timeSeriesDown <- ratioQuery(ticker, metricNameDown, input$start_down, input$end_down)
    } else {
      timeSeriesDown <-  FSquery(ticker, metricNameDown, input$start_down, input$end_down)
    }

    timeSeriesDown <- xts(timeSeriesDown[,2], order.by = timeSeriesDown$date)
    colnames(timeSeriesDown)[1] <- 'Close'
    timeSeriesDown
  })
  
  
  
  #### Until here should be the eventReactive
  
  
  # plots the up chart
  output$plot_up <- renderPlot({
    ticker <- paste0(toupper(input$ticker), '-US')
    name <- getName(ticker)
    
    # question: why are these ifelse statements needed here in the plot function? there is not anther query required....
    ## Answer: This is used only for the title of the chart
    chartTitleUp <- ifelse(input$upside == "P/E", paste0("P/E", ' - ', name),
                           ifelse(input$upside == "P/E NTM", paste0("P/E NTM", ' - ', name),
                                  ifelse(input$upside == "P/E NTM vs S&P500", paste0("P/E NTM vs S&P500", ' - ', name),
                                        ifelse(input$upside == "P/B", paste0("P/B", ' - ', name),
                                               ifelse(input$upside == "P/CF", paste0("P/CF", ' - ', name),
                                                      ifelse(input$upside == "EV/Sales", paste0("EV/Sales", ' - ', name),
                                                             ifelse(input$upside == "EV/EBITDA", paste0("EV/EBITDA", ' - ', name),
                                                                    ifelse(input$upside == "EV/EBITDA vs S&P500", paste0("EV/EBITDA vs S&P500", ' - ', name),
                                                                           NULL))))))))
    # below needs to be updated to reflect current structure of the app....
    timeSeriesUp <- timeSeries_stock_up()
    
    
    
    #ToDo: there are two plots required here: up and down.  Instead of text on the charts, we need a table published above each of the charts.
    plot <- function(){
      
      if( input$upside == 'P/E' || input$upside == 'P/E NTM' || input$upside == 'P/E NTM vs S&P500' || input$upside == 'P/B' || input$upside == 'P/CF' ) {
        chartSeries(timeSeriesUp,
                    name = chartTitleUp,
                    type="line",
                    theme=chartTheme('white'),
                    TA = c(addBBands(n = 20, sd = 2, ma = "SMA", draw = 'bands', on = -1)))
      } else {
        chartSeries(timeSeriesUp,
                    name = chartTitleUp,
                    type="line",
                    theme=chartTheme('white'))
      }
      ### Text
      
      maxUp <- round(max(timeSeriesUp$Close),1)
      minUp <- round(min(timeSeriesUp$Close),1)
      meanUp <- round(mean(timeSeriesUp$Close),1)
      
      
      maxUp <- paste("Max.:", maxUp)  
      minUp <- paste("Min.:", minUp)
      meanUp <- paste("Avg.:", meanUp)
      #######################################################
      
      timeSeriesUp <- as.data.frame(timeSeriesUp)
      timeSeriesUp$date <- as_date(row.names(timeSeriesUp))
      
      max_dateUp <- max(timeSeriesUp$date)
      min_dateUp <- min(timeSeriesUp$date)
      
      ### Difference selected period
      
      one <- subset(timeSeriesUp, timeSeriesUp$date == max_dateUp | timeSeriesUp$date == min_dateUp)
      
      period <- max_dateUp - min_dateUp
      period
      
      change <- (one$Close[2] - one$Close[1]) / one$Close[1]
      change <- round(change*100,2)
      
      change <- paste("Period Change:", change, "%")
      
      ############################################################################
      ## one year
      
      max_dateUp <- max(timeSeriesUp$date)
      one_year <- max_dateUp -365
       
      one_year <- subset(timeSeriesUp, timeSeriesUp$date >=  one_year)
       
      one_year <- subset(one_year, one_year$date == min(one_year$date) | one_year$date == max(one_year$date))
       
      change_1year <- (one_year$Close[2] - one_year$Close[1])/one_year$Close[1]
      change_1year*100
      
      timeInterval <- input$end_up - input$start_up
      timeInterval <- as.numeric(timeInterval)
       
      if(timeInterval >= 365) {
        change_1year <-  paste("1Y Change:", round(change_1year*100,2), "%")
      } else {
        change_1year <-  paste("1Y Change:", "NA")}
       
      text <- paste(maxUp, minUp, meanUp, change, change_1year, sep = "     ")
       
       
      mtext(text, side=1, outer=FALSE, cex = 0.75)
      
    }
    
    
    
    plot()
    
    
  })
  
  # Presents SD statistics
  output$table_upside <- renderDataTable({
    ticker <- paste0(toupper(input$ticker), '-US')
    
    df <- timeSeries_stock_up()
    latest <- tail(df$Close,1)
    latest <- as.numeric(latest)
    stat <- c('Last', 'Min', '-2SD', '-1SD', 'Mean', '+1SD', '+2SD', 'Max')
    val <- as.data.frame(t(c(round(latest,1),
                             round(min(df),1),
                             round(mean(df)-2*sd(df),1),
                             round(mean(df)-1*sd(df),1),
                             round(mean(df),1),
                             round(mean(df)+1*sd(df),1),
                             round(mean(df)+2*sd(df),1),
                             round(max(df),1)
                             )),
                         row.names = 'Multiple')
    #get current price, place in last
    # price <- FSquery(ticker, formula)
    price <- getPrice(ticker)
    multiple <- price / latest
    prices <- as.data.frame(t(c(round(price,2),
                                round(min(df)*multiple,2),
                                round((mean(df)-2*sd(df))*multiple,2),
                                round((mean(df)-1*sd(df))*multiple,2),
                                round(mean(df)*multiple,2),
                                round((mean(df)+1*sd(df))*multiple,2),
                                round((mean(df)+2*sd(df))*multiple,2),
                                round(max(df)*multiple,2)
                              )),
                            row.names = 'Implied Price')
                            
    val <- rbind(val, prices)
    colnames(val) <- stat
    datatable(val, filter = 'none',
              options = list(dom = 't',
                             columnDefs = list(list(className = 'dt-center',
                                               targets = 0:6))))
  })
  
  # Presents backtest results
  output$backtest_upside <- renderDataTable({
    ticker <- paste0(toupper(input$ticker), '-US')
    trailPeriods <- 20
    df <- timeSeries_stock_up()
    #df <- df[, c('date', 'Close')]

    avg <- mean(df)
    trigger1 <- avg - sd(df)
    trigger2 <- avg - 2*sd(df)

    temp <- data.frame(date = index(df), coredata(df))

    #df <- as.data.frame(df)
    temp <- temp %>%
      select(date, Close) %>%
      mutate(tma = rollmean(Close, k = trailPeriods, fill = NA, align = "right"))

    # examine -1SD
    triggerDates1 <- filter(temp, tma < trigger1 & tma > trigger2)

    triggerDates1 <- triggerDates1 %>%
      mutate(under1SD=ifelse(date - lag(date, 1) < 4,1,0))
    #triggerDates1[is.na(triggerDates1)] = 0
    triggerDates1 <- triggerDates1[complete.cases(triggerDates1), ]

    changeDates1 <- triggerDates1 %>%  filter(under1SD==1 & lag(under1SD,1)==0)
    dates1 <- changeDates1$date
    
    # add potential 1st date if it is under 1SD
    if(length(dates1)>0) {
      firstDay1 <- head(triggerDates1, 1)
      if(firstDay1$under1SD == 1) {
        dates1 <- append(firstDay1$date, dates1)
      }
    }
      
    # examine -2SD...
    triggerDates2 <- filter(temp, tma < trigger2)
    triggerDates2 <- triggerDates2 %>%
      mutate(under2SD=ifelse(date - lag(date, 1) < 4,1,0))
    #triggerDates2[is.na(triggerDates2)] = 0
    triggerDates2 <- triggerDates2[complete.cases(triggerDates2), ]
    
    changeDates2 <- triggerDates2 %>%  filter(under2SD==1 & lag(under2SD,1)==0)
    dates2 <- changeDates2$date

    # add potential 1st date if it is under 2SD
    if(length(dates2)>0) {
      firstDay2 <- head(triggerDates2, 1)
      if(firstDay2$under2SD == 1) {
        dates2 <- append(firstDay2$date, dates2)
      }
    }
    
    
    
    SD1header <- data.frame(start='-1SD',end='',
                            Year1='', SectorYr1='',
                            Year2='', SectorYr2='',
                            Year3='', SectorYr3='')
    SD1data <- getBacktest(df, dates1, ticker)
    SD1data$start <- as.character(SD1data$start)
    SD1data$end <- as.character(SD1data$end)
    SD1data <- rbind(SD1header, SD1data)
    
    
  
    SD2header <- data.frame(start='-2SD',end='',
                            Year1='', SectorYr1='',
                            Year2='', SectorYr2='',
                            Year3='', SectorYr3='')
    SD2data <- getBacktest(df, dates2, ticker)
    if(nrow(SD2data)>0) {
      SD2data$start <- as.character(SD2data$start)
      SD2data$end <- as.character(SD2data$end)
      SD2data <- rbind(SD2header, SD2data)
      }

    
 
    val <- rbind(SD1data, SD2data)
    val <- as.data.frame(val)
    datatable(val, filter = 'none',
              options = list(dom = 't',
              
              columnDefs = list(list(className = 'dt-center',
              targets = 0:6))))

  })


  
  

  # plots the down chart
  output$plot_down <- renderPlot({
    ticker <- paste0(toupper(input$ticker), '-US')
    name <- getName(ticker)
    # question: why are these ifelse statements needed here in the plot function? there is not anther query required....
    ## Answer: This is used only for the title of the chart
    chartTitleDown <- ifelse(input$downside == "P/E", paste0("P/E", ' - ', name),
                             ifelse(input$downside == "P/E NTM", paste0("P/E NTM", ' - ', name),
                                    ifelse(input$downside == "P/E NTM vs S&P500", paste0("P/E NTM vs S&P500", ' - ', name),
                                            ifelse(input$downside == "P/B", paste0("P/B", ' - ', name),
                                                   ifelse(input$downside == "P/CF", paste0("P/CF", ' - ', name),
                                                          ifelse(input$downside == "EV/Sales", paste0("EV/Sales", ' - ', name),
                                                                 ifelse(input$downside == "EV/EBITDA", paste0("EV/EBITDA", ' - ', name),
                                                                        ifelse(input$upside == "EV/EBITDA vs S&P500", paste0("EV/EBITDA vs S&P500", ' - ', name), 
                                                                          NULL))))))))

    # below needs to be updated to reflect current structure of the app....
    timeSeriesDown <- timeSeries_stock_down()



    #ToDo: there are two plots required here: up and down.  Instead of text on the charts, we need a table published above each of the charts.
    plot <- function(){
      
      if( input$downside == 'P/E' || input$downside == 'P/E NTM' || input$upside == 'P/E NTM vs S&P500' || input$downside == 'P/B' || input$downside == 'P/CF' ) {
        chartSeries(timeSeriesDown,
                    name = chartTitleDown,
                    type="line",
                    theme=chartTheme('white'),
                    TA = c(addBBands(n = 20, sd = 2, ma = "SMA", draw = 'bands', on = -1)))
      } else {
        chartSeries(timeSeriesDown,
                    name = chartTitleDown,
                    type="line",
                    theme=chartTheme('white'))
      }
      
      ### Text

      maxDown <- round(max(timeSeriesDown$Close),1)
      minDown <- round(min(timeSeriesDown$Close),1)
      meanDown <- round(mean(timeSeriesDown$Close),1)


      maxDown <- paste("Max.:", maxDown)
      minDown <- paste("Min.:", minDown)
      meanDown <- paste("Avg.:", meanDown)
      #######################################################

      timeSeriesDown <- as.data.frame(timeSeriesDown)
      timeSeriesDown$date <- as_date(row.names(timeSeriesDown))

      max_dateDown <- max(timeSeriesDown$date)
      min_dateDown <- min(timeSeriesDown$date)

      ### Difference selected period

      one <- subset(timeSeriesDown, timeSeriesDown$date == max_dateDown | timeSeriesDown$date == min_dateDown)

      period <- max_dateDown - min_dateDown
      period

      change <- (one$Close[2] - one$Close[1]) / one$Close[1]
      change <- round(change*100,2)

      change <- paste("Period Change:", change, "%")

      ############################################################################
      ## one year

      max_dateDown <- max(timeSeriesDown$date)
      one_year <- max_dateDown -365

      one_year <- subset(timeSeriesDown, timeSeriesDown$date >=  one_year)

      one_year <- subset(one_year, one_year$date == min(one_year$date) | one_year$date == max(one_year$date))

      change_1year <- (one_year$Close[2] - one_year$Close[1])/one_year$Close[1]
      change_1year*100
      
      timeInterval <- input$end_down - input$start_down
      timeInterval <- as.numeric(timeInterval)
      
      if(timeInterval >= 365) {
        change_1year <-  paste("1Y Change:", round(change_1year*100,2), "%")
      } else {change_1year <-  paste("1Y Change:", "NA")}

      text <- paste(maxDown, minDown, meanDown, change, change_1year, sep = "     ")


      mtext(text, side=1, outer=FALSE, cex = 0.75)

    }

    plot()
  })
  
  output$table_downside <- renderDataTable({
    ticker <- paste0(toupper(input$ticker), '-US')
    
    df <- timeSeries_stock_down()
    latest <- tail(df$Close,1)
    latest <- as.numeric(latest)
    stat <- c('Last', 'Min', '-2SD', '-1SD', 'Mean', '+1SD', '+2SD', 'Max')
    val <- as.data.frame(t(c(round(latest,1),
                             round(min(df),1),
                             round(mean(df)-2*sd(df),1),
                             round(mean(df)-1*sd(df),1),
                             round(mean(df),1),
                             round(mean(df)+1*sd(df),1),
                             round(mean(df)+2*sd(df),1),
                             round(max(df),1)
                             )),
                         row.names = 'Multiple')
    #get current price, place in last
    # price <- FSquery(ticker, formula)
    price <- getPrice(ticker)
    multiple <- price / latest
    prices <- as.data.frame(t(c(round(price,2),
                                round(min(df)*multiple,2),
                                round((mean(df)-2*sd(df))*multiple,2),
                                round((mean(df)-1*sd(df))*multiple,2),
                                round(mean(df)*multiple,2),
                                round((mean(df)+1*sd(df))*multiple,2),
                                round((mean(df)+2*sd(df))*multiple,2),
                                round(max(df)*multiple,2)
                                )),
                            row.names = 'Implied Price')
    val <- rbind(val, prices)
    
    
    colnames(val) <- stat
    datatable(val, filter = 'none',
              options = list(dom = 't',
                             columnDefs = list(list(className = 'dt-center',
                                                    targets = 0:6))))
  })
  
  output$backtest_downside <- renderDataTable({
    ticker <- paste0(toupper(input$ticker), '-US')
    trailPeriods <- 20
    df <- timeSeries_stock_down()
    #df <- df[, c('date', 'Close')]

    avg <- mean(df)
    trigger1 <- avg - sd(df)
    trigger2 <- avg - 2*sd(df)

    temp <- data.frame(date = index(df), coredata(df))

    #df <- as.data.frame(df)
    temp <- temp %>%
      select(date, Close) %>%
      mutate(tma = rollmean(Close, k = trailPeriods, fill = NA, align = "right"))

    # examine -1SD
    triggerDates1 <- filter(temp, tma < trigger1 & tma > trigger2)

    triggerDates1 <- triggerDates1 %>%
      mutate(under1SD=ifelse(date - lag(date, 1) < 4,1,0))
    #triggerDates1[is.na(triggerDates1)] = 0
    triggerDates1 <- triggerDates1[complete.cases(triggerDates1), ]

    changeDates1 <- triggerDates1 %>%  filter(under1SD==1 & lag(under1SD,1)==0)
    dates1 <- changeDates1$date

    # add potential 1st date if it is under 1SD
    if(length(dates1)>0) {
      firstDay1 <- head(triggerDates1, 1)
      if(firstDay1$under1SD == 1) {
        dates1 <- append(firstDay1$date, dates1)
      }
    }
    
    # examine -2SD
    triggerDates2 <- filter(temp, tma < trigger2)
    triggerDates2 <- triggerDates2 %>%
      mutate(under2SD=ifelse(date - lag(date, 1) < 4,1,0))
    #triggerDates2[is.na(triggerDates2)] = 0
    triggerDates2 <- triggerDates2[complete.cases(triggerDates2), ]
    
    changeDates2 <- triggerDates2 %>%  filter(under2SD==1 & lag(under2SD,1)==0)
    dates2 <- changeDates2$date

    # add potential 1st date if it is under 2SD
    if(length(dates2)>0) {
      firstDay2 <- head(triggerDates2, 1)
      if(firstDay2$under2SD == 1) {
        dates2 <- append(firstDay2$date, dates2)
      }
    }
    
    SD1header <- data.frame(start='-1SD',end='',
                            Year1='', SectorYr1='',
                            Year2='', SectorYr2='',
                            Year3='', SectorYr3='')
    SD1data <- getBacktest(df, dates1, ticker)
    SD1data$start <- as.character(SD1data$start)
    SD1data$end <- as.character(SD1data$end)
    SD1data <- rbind(SD1header, SD1data)
    
    SD2header <- data.frame(start='-2SD',end='',
                            Year1='', SectorYr1='',
                            Year2='', SectorYr2='',
                            Year3='', SectorYr3='')
    SD2data <- getBacktest(df, dates2, ticker)
    if(nrow(SD2data)>0) {
      SD2data$start <- as.character(SD2data$start)
      SD2data$end <- as.character(SD2data$end)
      SD2data <- rbind(SD2header, SD2data)
    }
    
    
    val <- rbind(SD1data, SD2data)
    val <- as.data.frame(val)
    datatable(val, filter = 'none',
              options = list(dom = 't',
                             columnDefs = list(list(className = 'dt-center',
                                                    targets = 0:6))))


   })
  
})
