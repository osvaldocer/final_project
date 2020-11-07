# https://github.com/osvaldocer/final_project.git
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(shinythemes)

data <- read_rds("steam_data_set.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
    shinytheme("cerulean"),
    navbarPage("Steam Data Final Project", 
               tabPanel("Analysis",
                        titlePanel("Steam Example Graphs"),
                        
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput("games",
                                            "Number of games:",
                                            min = 1,
                                            max = 50,
                                            value = 10)
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                                plotOutput("distPlot1"),
                                br(),
                                plotOutput("distPlot2"),
                                br(),
                                plotOutput("distPlot3"),
                                br(), 
                                plotOutput("distPlot4")
                                
                            )
                        )
                        
                        ),
               tabPanel("About",
                        h1("About the data"),
                        p("This data is from the gaming platform Steam. More 
                          specifically, I intend to use existing Steam data to 
                          complete my final project. Currently I am considering 
                          some sort of comparative analysis, as several 
                          variables are contained within the data which would 
                          serve to create clear visuals. Additionally, I can 
                          include data from Steam users as well to cross 
                          reference and perhaps analyze my regions in the US, or
                          other demographic information. As such I am still 
                          searching for other potential data sets to include in 
                          my final project. Further, I will take the time in the
                          coming week to analyze and clean up my data to remove 
                          the abnormalities presenting themselves in the current
                          version of my graph."),
                        br(),
                        h2("About this app"),
                        br(), 
                        p("My repo can be found at", 
                          a("https://github.com/osvaldocer/steam_data", 
                            href = "https://github.com/osvaldocer/steam_data"))
                        
               )),

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot1 <- renderPlot({
        data %>%
            arrange(desc(metacritic)) %>%
            slice(1:10) %>%
            ggplot(aes(x = query_name, y = metacritic)) +
            geom_col() +
            coord_flip() +
            labs(title = "A Breakdown of Games by Metacritic Score", 
                 x = "Metacritic Score", y = "Title of Game") +
            theme_minimal()
    })
    
    output$distPlot2 <- renderPlot({
        data %>%
            arrange(desc(metacritic)) %>%
            slice(1:input$games) %>%
            ggplot(aes(x = query_name, y = metacritic)) +
            geom_col() +
            coord_flip() +
            labs(title = "A Breakdown of Games by Metacritic Score", 
                 x = "Metacritic Score", y = "Title of Game") +
            theme_classic()
    })
    
    output$distPlot3 <- renderPlot({
        data %>%
            arrange(desc(steam_spy_owners)) %>%
            slice(1:input$games) %>%
            ggplot(aes(x = query_name, y = steam_spy_owners)) +
            geom_col() +
            labs(title = "Steam Games by Amount of Owners", x = "Game Title", 
                 y = "# of Owners") +
            theme_dark()
    })
    
    output$distPlot4 <- renderPlot({
        data %>%
            filter(metacritic > 0) %>%
            filter(steam_spy_players_estimate > 0) %>%
            filter(steam_spy_players_estimate < 4000000) %>%
            ggplot(aes(x = metacritic, y = steam_spy_players_estimate)) +
            geom_jitter() +
            labs(title = "Estimated Players by Metacritic Scores", 
                 x = "Metacritic Score", 
                 y = "Estimated Playerbase", 
                 subtitle = "Do higher scores mean more players?") +
            theme_light()
    })
}

# Run the application 
shinyApp(ui = ui, server = server) 
