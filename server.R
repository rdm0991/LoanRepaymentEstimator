#
# This is the server logic of a Shiny web application "Loan Repayment Estimator"
#    Monthly loan repayment installment is computed using the full monthly
#    amortization algorithm.  Amortization schedule for the specified loan 
#    amount, annual rate of interest and the loan term is generated and served
#    to the client.  Also, the total interest paid at the end of loan term and 
#    relative percentage of principal and interest payment as of end of loan is 
#        also served.
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)

calcMonthlyPayment <- function(params){
        (params$loanAmount * (params$rate/(12*100)))/( 1 - (1 + params$rate/(12*100))^(- params$loanTerm))
}
       
calcTotalInterest <- function(params){
        (params$loanTerm * calcMonthlyPayment(params)) - params$loanAmount 
}

calcPaySchedule <- function(params){
        mp <- calcMonthlyPayment(params)
        psDF <- createScheduleDF(params)
        for(i in 1:params$loanTerm){
                d <- ymd(params$startDate)
                r <- params$rate/(12 * 100)
                if (i>1)
                {
                        psDF$payDate[i] <- ymd(psDF$payDate[i-1]) %m+% months(1)
                        psDF$Interest[i] <- r * psDF$Balance[i-1]
                        psDF$Principal[i] <- mp - psDF$Interest[i]
                        psDF$Balance[i] <- ((1 + r) ^ i) * params$loanAmount - (((1 + r) ^ i) - 1) * mp/(r)
                        psDF$TotalInterest[i] <- psDF$TotalInterest[i-1] + psDF$Interest[i]
                }
                else{
                        
                       
                        psDF$Interest[i] <- r * params$loanAmount
                        psDF$TotalInterest[i] <- psDF$Interest[i]
                        psDF$Principal[i] <- mp - psDF$Interest[i]
                        psDF$Balance[i] <- params$loanAmount - psDF$Principal[i]
                }
                
                psDF$Payment[i] <- mp
               
               
        }
        # format currency to two digits
        psDF[,-1] <-  round(psDF[,-1], 2)
        
        for(i in 1:params$loanTerm){

              psDF$Balance[i] <-  paste("$", formatC(psDF$Balance[i], format="f", digits=2, big.mark =","))
              psDF$Interest[i] <-  paste("$", formatC(psDF$Interest[i], format="f", digits=2, big.mark =","))
              psDF$Principal[i] <-  paste("$", formatC(psDF$Principal[i], format="f", digits=2, big.mark =","))
              psDF$TotalInterest[i] <-  paste("$", formatC(psDF$TotalInterest[i], format="f", digits=2, big.mark =","))
              psDF$Payment[i] <-  paste("$", formatC(psDF$Payment[i], format="f", digits=2, big.mark =","))
        }
        psDF       
}

createScheduleDF <- function(params){
        n <- params$loanTerm
        df <- data.frame(payDate = as.Date(rep(1,n), origin = params$startDate), Payment=numeric(n),
Principal=numeric(n), Interest=numeric(n), TotalInterest = numeric(n), Balance = numeric(n))
       
        df
}

shinyServer(function(input, output) {
        output$monthlyPayment <- reactive({
                mp <- calcMonthlyPayment(input) 
                paste("$",formatC(round(mp,2),  format="f", digits=2))
          
        })
        
        output$totalInterest <- reactive({
                ti <- calcTotalInterest(input)
                paste("$", formatC(round(ti, 2), format="f", digits=2))
        })
        
       output$plot <- renderPlot({
                ti <- calcTotalInterest(input)
                slices <- c(ti, ti + input$loanAmount)
                lbls <- c("Interest", "Principal")
                pct <- round(slices/sum(slices)*100)
                lbls <- paste(lbls, pct) # add percents to labels 
                lbls <- paste(lbls,"%",sep="") # ad % to labels 
                pie(slices,labels = lbls, col=rainbow(length(lbls)),
                    main="Interest & Principal Payments as percentage of total payment")      
               
       })

       scheduleDat <- reactive({

                df <- calcPaySchedule(input)
                df

       })

       output$paySchedule <- renderDataTable({
               scheduleDat()

       })
      
       
  })
