
library(shiny)
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

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Valuation"),
  
  tabsetPanel(
    
    
    tabPanel("Stock",
             sidebarLayout(
               
               sidebarPanel(
                 
                 textInput("ticker", "Ticker", value = 'ROK'),
                 
                 awesomeRadio(
                   inputId = "upside",
                   label = "UPSIDE",
                   choices = c("P/E" = 'P/E' ,
                               "P/E NTM" = "P/E NTM",
                               "P/E NTM vs S&P500" = "P/E NTM vs S&P500",
                               "P/B" = "P/B",
                               "P/CF"= "P/CF",
                               "EV/Sales" = "EV/Sales" ,
                               "EV/EBITDA" = "EV/EBITDA",
                               "EV/EBITDA vs S&P500" = "EV/EBITDA vs S&P500"
                   ),
                   selected = "P/E",
                   status = "warning"
                 ),
                 dateInput("start_up",
                           "Start Date",
                           Sys.Date() - years(10),
                           format = "yyyy-mm-dd"
                 ),
                 dateInput("end_up",
                           "End Date",
                           Sys.Date(),
                           format = "yyyy-mm-dd"
                 )
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                tabsetPanel(
                  tabPanel(
                    title = "Plot",
                    plotOutput("plot_up")
                  ),
                  tabPanel(
                    title = "Backtest",
                    dataTableOutput('table_upside'),
                    dataTableOutput('backtest_upside')
                  )
                )
               )
             ),
             
             
             sidebarPanel(
               
               awesomeRadio(
                 inputId = "downside",
                 label = "DOWNSIDE",
                 choices = c("P/E" = 'P/E' ,
                             "P/E NTM" = "P/E NTM",
                             "P/E NTM vs S&P500" = "P/E NTM vs S&P500",
                             "P/B" = "P/B",
                             "P/CF"= "P/CF",
                             "EV/Sales" = "EV/Sales" ,
                             "EV/EBITDA" = "EV/EBITDA",
                             "EV/EBITDA vs S&P500" = "EV/EBITDA vs S&P500"
                 ),
                 selected = "P/E",
                 status = "warning"
               ),
               dateInput("start_down",
                         "Start Date",
                         Sys.Date() - years(10),
                         format = "yyyy-mm-dd"
               ),
               dateInput("end_down",
                         "End Date",
                         Sys.Date(),
                         format = "yyyy-mm-dd"
               )
             ),
             
             # ToDo: there need to be two plots here....one for upside, one for downside with
             # related data presentations....
             # Show a plot of the generated distribution
             #
             
             mainPanel(
              tabsetPanel(  
                tabPanel(
                  title = 'Plot',
                  plotOutput("plot_down")
                ),
                tabPanel(
                  title = 'Backtest',
                  dataTableOutput('table_downside')
                )
              )
             )
             
    ),
    
    tabPanel("Index")
  )
)
)
