# GST 
library(tidyverse)
library(shiny)
library(DT)

calculations <- function(earnings,tax,price,depreciation,years){
  
  full_table <- tibble()
  
  for(i in 1:years){
    
    # default tax 
    tax_default <- earnings*tax/100
    
    # remove from initial income
    earnings_reduces_on_asset <- price*depreciation/100
    
    # new income
    new_earnings <- earnings - earnings_reduces_on_asset
    
    # tax to be paid on the new income  
    tax_to_be_paid <- new_earnings*30/100
    
    # tax saved due to reduced new income  
    tax_saved <- tax_default - tax_to_be_paid
    
    full_table <- full_table %>% 
      rbind(
        tibble(year = i, 
               price = price, 
               depr = earnings_reduces_on_asset,
               income = new_earnings,
               default_tax = tax_default,
               new_tax = tax_to_be_paid,
               saved = tax_saved)
      )
    
    
    # setting price for new year 
    price <- price*(100-depreciation)/100
  }
  full_table
} 


ui <- fluidPage(
  fluidRow(
    column(4, 
           numericInput("earning", "Earnings", value = 100)
           ),
    column(4, 
           numericInput("tax", "Tax Rate %", value = 30)
           ),
    column(4, 
           sliderInput("years", "No. of years", min = 1, max = 100, value = 10))
  ),
  fluidRow(
    column(6,
           numericInput("cost_price", "Cost Price of Article", value = 100)
           ),
    column(6, 
           numericInput("depreciation", "Depreciation %", value = 20)
           )
  ),
  fluidRow(
    column(6, 
           plotOutput("plot")
           ),
    column(6, 
           DT::dataTableOutput("table")
           )
  )
  
)

server <- function(input, output, session) {
  
  calculate <- reactive({
    
  input$earning -> earnings
  input$tax -> tax
  input$cost_price -> price
  input$depreciation -> depreciation
  input$years -> years
  
  calculations(earnings,tax,price, depreciation,years) %>% 
    mutate_if(is.numeric, round,2)
  
  })
  
  output$plot <- renderPlot({
    calculate() -> raw_data
    
    raw_data %>% pull(saved) %>% sum(na.rm = T) -> total_saved
    raw_data %>% 
      mutate(year = year %>% as.factor()) %>% 
      ggplot(aes(year, depr, group = 1)) +
      geom_line() +
      labs(title = paste0("Total Money Saved ",total_saved)) +
      theme_bw()
  })
  
  output$table <- renderDataTable({
    results <- DT::datatable(calculate(), 
                             options = list(
                               pageLength = 100
                             )) %>% 
      formatStyle(columns = c(-1))
  })
  
}

shinyApp(ui, server)
