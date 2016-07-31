#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(rCharts)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Loan Repayment Estimator"),
  sidebarLayout(
    sidebarPanel(
        numericInput("loanAmount", "Loan Amount ($)", 25000, min = 500, 
                     max = 100000, step = 100
                         ),
        numericInput("loanTerm", "Loan Term (months)", 12, min = 1, 
                     max = 120, step = 1
        ),
        numericInput("rate",
                   "Annual Rate of Interest (%):",
                   min = 0.25,
                   max = 10,
                   value = 3,
                   step = 0.01),
        dateInput("startDate",
                  "Loan Start Date", value = date(), min = date(), max = NULL,
                  format = "yyyy-mm-dd", startview = "month", weekstart = 0, 
                  language = "en")
        
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
            tabsetPanel(
                    tabPanel("Summary",
                             br(),
                             em("Monthly installment for loan repayment"),
                             strong(textOutput("monthlyPayment")),
                             br(),
                             em("Total interest payment for loan term:"),
                             strong(textOutput("totalInterest")),
                             plotOutput("plot")),
                    tabPanel("Payment Schedule",
                             dataTableOutput("paySchedule")),
                    tabPanel("User Documentation",
                             includeHTML("UserDoc.html"))
            )
           
    )
  )
 )
)
