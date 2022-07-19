#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
    titlePanel("Stock Valuation"),

    tabsetPanel(
    # Sidebar with a slider input for number of bins
      
      tabPanel("Stock",
    sidebarLayout(
      
       sidebarPanel(
          
            textInput("ticker", "Ticker"),
            
            awesomeRadio(
                     inputId = "upside",
                     label = "UPSIDE",
                     choices = c("P/E" = 'P/E' ,
                                 "P/E Est" = "P/E Est",
                                 "P/B" = "P/B",
                                 "P/CF"= "P/CF",
                                 "EV/Sales" = "EV/Sales" ,
                                 "EV/EBITDA" = "EV/EBITDA"
                                 ),
                     selected = "P/B",
                     status = "warning"
                   ),
            dateInput("start_up",
                      "Start Date",
                      Sys.Date() - years(1),
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
            plotOutput("plot_up")
        )
    ),
    
   
      sidebarPanel(
        
        awesomeRadio(
          inputId = "downside",
          label = "DOWNSIDE",
          choices = c("P/B" = 'P/B' ,
                      "EV/Sales" = "EV/Sales" ,
                      "EV/EBITDA" = "EV/EBITDA",
                      "P/CF"= "P/CF"),
          selected = "P/B",
          status = "warning"
        ),
        dateInput("start_down",
                  "Start Date",
                  Sys.Date() - years(1),
                  format = "yyyy-mm-dd"
        ),
        dateInput("end_down",
                  "End Date",
                  Sys.Date(),
                  format = "yyyy-mm-dd"
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("plot2")
      )
    
),

tabPanel("Index")
)
)
)
