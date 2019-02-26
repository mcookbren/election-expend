#ESM 244 Shiny App
#By: Meghan Cook
#2016 Election Expenditure
#Data Source: Federal Election Commission

library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage("2016 Election Expenditures",
                 tabPanel("Presidential Election"),
                 tabPanel("Congressional Elections"),
                 tabPanel("Donor Profiles"),
                 tabPanel("Data Summary")
                 )


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'red', border = 'orange')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

