#ESM 244 Shiny App
#By: Meghan Cook
#2016 Election Expenditure
#Data Source: U.S. Federal Election Commission

library(shiny)
library(RColorBrewer)
library(tidyverse)
library(readr)
library(shinythemes)
library(scales)

df <- read_csv("2016_pres.csv")

exp_sum <- df %>% 
  group_by(can_nam, sup_opp, spe_nam) %>% 
  summarise(sum_exp = sum(exp_amo))

con_df <- read_csv("2016_congr.csv")

options(scipen=10000000)

# Define UI for application
ui <- navbarPage("Independent Expenditures in the 2016 Election",
                 theme = shinytheme("yeti"),
                 tabPanel("App Summary",
                   fluidPage(
                     
                     mainPanel(
                       h3("Who Spent What on the 2016 Elections?"),
                       br(),
                       h4("This application is based on data provided by the U.S. Federal Election Commission regarding independent election expenditures during the United States 2016 Federal Election. This includes the presidential and all congressional races."),
                       h4("Independent election expenditures are monetary donations made to support or oppose a candidate’s election campaign. These donations are made by private entities, which can be individuals, groups, political committees, corporations, or unions. Campaign finance can be incredibly influential on election results."),
                       h4("Political Action Committees, or PACs, are a controversial type of political organization that pools the donations of many donors for independent election expenditures. While PAC donations are publicly disclosed, PACs are not legally required to disclose their donors."),
                       h4("This creates an avenue for wealthy private individuals and corporations to anonymously donate to political campaigns. Critics argue that this provides them with undue influence over political elections and that this threatens the Democratic election process."),
                       br(),
                       br(),
                       h5("Created by Meghan Cook")
                       )
                     
                   )#close fluidPage
                   
                 ),#close tabPanel
                 
                 
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
                 
                 tabPanel("Congressional Elections",
                 titlePanel("Top Donors in the Congressional Election"),
                 sidebarLayout(
                   sidebarPanel(
                     
                     #input candidate
                     radioButtons("office", 
                                 "Select Political Office:", 
                                 choices = c(Senate = "S", House = "H")
                     )
                    
                   ), #close sidebar panel
                 
                 mainPanel(
                   
                   plotOutput(outputId = "con_donor")
                   
                 )
                 
                 
                 ) #close sidebarLayout
  ), #closes tabPanel

              tabPanel("State Election Spending",
              titlePanel("Total Spending for Top 10 Most Expensive States"),
              sidebarLayout(
                sidebarPanel(
             
             #input state
             selectInput("state", 
                          "Select From Top Spending States:", 
                          choices = c(Florida = "FL", D.C. = "DC", Illinois = "IL", Indiana = "IN", S.Carolina = "SC", Iowa = "IA", Ohio = "OH", Wisconsin = "WI", Nevada = "NV", Texas = "TX")
             )
             
           ), #close sidebar panel
           
           mainPanel(
             
             plotOutput(outputId = "state_sum")
             
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
       theme(axis.text.x=element_blank(), panel.background = element_blank(),axis.line = element_line(colour = "black")) +
       ggtitle("Top 10 Largest Donors") +
       xlab("Donor Name") +
       ylab("Total Amount Donated (USD)") +
       scale_fill_brewer(palette="Set3", name="Donor Name") +
       scale_y_continuous(labels = comma)
  
   })
   
   #output for congressional election tab
   output$con_donor <- renderPlot({
   
     #filter by congressional office, sum top donors for either 
     con_exp_sum <- con_df %>% 
       filter(can_off != "P") %>%
       filter(can_off == input$office) %>% 
       group_by(spe_nam) %>% 
       summarise(sum_exp = sum(exp_amo)) %>% 
       arrange(desc(sum_exp)) %>% 
       slice(1:10)
     
     #Plot top 10 donors by party and support/opposition
     ggplot(data=con_exp_sum, aes(x=spe_nam, y=sum_exp, fill=spe_nam)) +
       geom_bar(stat='identity') +
       theme(axis.text.x=element_blank(), panel.background = element_blank(),axis.line = element_line(colour = "black")) +
       ggtitle("Top 10 Largest Donors") +
       xlab("Donor Name") +
       ylab("Total Amount Donated (USD)") +
       scale_fill_brewer(palette="Set3", name="Donor Name") +
       scale_y_continuous(labels = comma)
       
       
   })
   
   #output for congressional election tab
   output$state_sum <- renderPlot({
     
     #filter by congressional office, sum top donors for either 
     state_sum <- df %>% 
       filter(can_off_sta != "US") %>%
       filter(can_off_sta == input$state) %>% 
       group_by(can_par_aff, sup_opp) %>% 
       summarise(sum_exp = sum(exp_amo))
     
     #Plot top 10 donors by party and support/opposition
     ggplot(data=state_sum, aes(x=can_par_aff, y=sum_exp, fill=sup_opp)) +
       geom_bar(stat='identity') +
       theme(panel.background = element_blank(),axis.line = element_line(colour = "black")) +
       ggtitle("Total Donations by Party and Support") +
       xlab("Party Affiliation") +
       ylab("Total Amount Donated (USD)") +
       scale_fill_brewer(palette="Set3", name="Donation Made to:") +
       scale_y_continuous(labels = comma)
     
     
   })   
   
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

