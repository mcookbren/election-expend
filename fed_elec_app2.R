#ESM 244 Shiny App
#By: Meghan Cook
#2016 Election Expenditure
#Data Source: U.S. Federal Election Commission

library(shiny)
library(RColorBrewer)
library(tidyverse)
library(readr)

df <- read_csv("2016_pres.csv")

exp_sum <- df %>% 
  group_by(can_nam, sup_opp, spe_nam) %>% 
  summarise(sum_exp = sum(exp_amo))

con_df <- read_csv("2016_congr.csv")

contab_df <- con_df %>%
  filter(can_par_aff == "Dem" | can_par_aff == "Rep") %>% 
  group_by(spe_nam, can_par_aff, sup_opp) %>% 
  summarise(sum_exp = sum(exp_amo))

# Define UI for application
ui <- navbarPage("Independent Expenditures in the 2016 Election",
                 tabPanel("Presidential Election",
                 titlePanel("Top Donors For or Against a Presidential Candidate"),
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
                     
                     plotOutput(outputId = "p_elections")
                     
                   )
                     
                 ) #close sidebarLayout
), #closes tabPanel
                 
                 tabPanel("Congressional Donors",
                 titlePanel("Top Donors For or Against a Party in the Congressional Election"),
                 sidebarLayout(
                   sidebarPanel(
                     
                     #input candidate
                     radioButtons("party", 
                                 "Select Political Party:", 
                                 choices = c(Republican = "Rep", Democratic = "Dem")
                     ),
                     
                     #input support or opose
                     radioButtons("support_c", 
                                  "Was Donation to Support or Oppose?", 
                                  choices = c(Support = "Support", Oppose = "Oppose")
                     )
                   ), #close sidebar panel
                 
                 mainPanel(
                   
                   plotOutput(outputId = "c_elections")
                   
                 )
                 
                 
                 ) #close sidebarLayout
  ) #closes tabPanel
) #closes NavbarPage



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
       scale_fill_brewer(palette="Set3")
  
   })
   
   #output for congressional election tab
   output$c_elections <- renderPlot({
   
     #filter by party and if donation was in support or opposition
     #Show top 10 donors
     con_sum <- contab_df %>% 
       filter(can_par_aff == input$party) %>% 
       filter(sup_opp == input$support_c) %>% 
       arrange(desc(sum_exp)) %>% 
       slice(1:10)
     
     #Plot top 10 donors by party and support/opposition
     ggplot(data=con_sum, aes(x=spe_nam, y=sum_exp, fill=spe_nam)) +
       geom_bar(stat='identity') +
       theme(axis.text.x=element_blank()) +
       ggtitle("Top 10 Largest Donors") +
       xlab("Donor Name") +
       ylab("Total Amount Donated (USD)") +
       scale_fill_brewer(palette="Set3")
     
   })
   
   
   
   
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

