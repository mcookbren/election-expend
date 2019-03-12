#ESM 244 Shiny App
#By: Meghan Cook
#2016 Election Expenditure
#Data Source: Federal Election Commission

library(shiny)
library(RColorBrewer)

# Define UI for application
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
                     
                     plotOutput("p_elections")
                     
                   )
                     
                 ), #close sidebarLayout
                 
                 tabPanel("Congressional Elections"),
                 sidebarLayout(
                   sidebarPanel(
                     
                     #input election office (house or senate)
                     radioButtons("election", 
                                 "Select Election Office:", 
                                 choices = c(S ="Senate", H = "House")
                     ),
                     
                     #input state
                     selectInput("state",
                                 "Select Election State:",
                                 choices = can_off_sta,
                                 selectize = TRUE
                     ),
                     
                     #input party
                     selectInput("party", 
                                 "Select Candidate Party Affiliation:", 
                                 choices = can_par_aff
                     ),
                     
                     #input candidate name
                     selectInput("c_candidate", 
                                 "Select Candidate Name:", 
                                 choices = can_nam
                     ),
                     
                     #input donation was to support or oppose
                     radioButtons("c_support", 
                                 "Was Donation to Support or Oppose?", 
                                 choices = c(Support ="Support", Oppose = "Oppose") 
                     )            
                     
                   )#close sidebar panel
                 ),
                 
                 mainPanel(
                   
                   plotOutput("c_elections")
                   
                 )
                 
                 
                 )


# Define server logic required to draw barplots
server <- function(input, output) {
   
  #output for presidential election tab
   output$p_elections <- renderPlot({
     
     #filter by candidate and if donation was in support or opposition
     #Show top 10 donors
     exp_sum_t10 <- exp_sum %>% 
       filter(can_nam == input$candidate) %>% 
       filter(sup_opp == input$support) %>% 
       arrange(desc(sum_exp)) %>% 
       slice(1:10)
     
     #Plot top 10 donors for either candidate, by support or opposition
     ggplot(data=exp_sum_t10, aes(x=spe_nam, y=sum_exp, fill=spe_nam)) +
       geom_bar(stat='identity') +
       theme(axis.text.x=element_blank()) +
       ggtitle("Top 10 Largest Donors") +
       xlab("Donor Name") +
       ylab("Total Amount Donated (USD)") +
       scale_fill_brewer(palette="Dark2")
  
   })
   
   #output for congressional election tab
   output$c_elections <- renderPlot({
     
     #filter by office, state, party, candidate, and if donation was to support or oppose
     #Show only 10 largest donors for a candidate
     cexp_sum_t10 <- con_df %>% 
       filter(can_off == input$election) %>%
       filter(can_off_sta == input$state) %>% 
       filter(can_par_aff == input$party) %>% 
       filter(can_nam == input$c_candidate) %>% 
       filter(sup_opp == input$c_support) %>% 
       arrange(desc(sum_exp)) %>% 
       slice(1:10)
     
     #plot top donors for a given candidate
     ggplot(data=cexp_sum_t10, aes(x=spe_nam, y=sum_exp, fill=spe_nam)) +
       geom_bar(stat='identity') +
       theme(axis.text.x=element_blank()) +
       ggtitle("Top Donors By Congressional Candidate") +
       xlab("Donor Name") +
       ylab("Total Amount Donated (USD)") +
       scale_fill_brewer(palette="Dark2")
     
   })
   
   
   
   
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

