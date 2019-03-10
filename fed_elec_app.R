#ESM 244 Shiny App
#By: Meghan Cook
#2016 Election Expenditure
#Data Source: Federal Election Commission

library(shiny)
library(RColorBrewer)

# Define UI for application that draws a histogram
ui <- navbarPage("2016 Election Expenditures",
                 tabPanel("Presidential Election"),
                 sidebarLayout(
                   sidebarPanel(
                     
                     #input candidate
                     selectInput("candidate", 
                                 "Select Presidential Candidate:", 
                                 choices = c(Trump ="Trump", Clinton = "Clinton")
                     ),
                     
                     #input support or opose
                     radioButtons("support", 
                       "Was Donation to Support or Oppose?", 
                       choices = c(Support ="Support", Oppose = "Oppose")
                     )
                     ), #close sidebar panel
                   
                   mainPanel(
                     
                     plotOutput("elections")
                     
                   )
                     
                     

                 )
                 )


# Define server logic required to draw a barplot
server <- function(input, output) {
   
   output$elections <- renderPlot({
     
     exp_sum_t10 <- exp_sum %>% 
       filter(can_nam == input$candidate) %>% 
       filter(sup_opp == input$support) %>% 
       arrange(desc(sum_exp)) %>% 
       slice(1:10)
     
     ggplot(data=exp_sum_t10, aes(x=spe_nam, y=sum_exp, fill=spe_nam)) +
       geom_bar(stat='identity') +
       theme(axis.text.x=element_blank()) +
       ggtitle("Top 10 Largest Donors") +
       xlab("Donor Name") +
       ylab("Total Amount Donated (USD)") +
       scale_fill_brewer(palette="Dark2")
  
   })
   
   
   
   
   
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

