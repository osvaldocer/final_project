library(tidyverse)
library(shiny)
library(shinythemes)
library(gt)
library(gtsummary)
library(broom.mixed)
library(rstanarm)

data <- read_rds("steam_data_set.rds")
anxiety <- read_rds("anxiety.rds")
overwatch <- read_rds("overwatch.rds")

ui <- fluidPage(theme = shinytheme("united"), 
    navbarPage("An Analysis of Gaming, Gender, and Psychology",
               tabPanel("Data",
               tabsetPanel(
                   tabPanel("Analysis", 
                        titlePanel("Psychology and Gaming"),
                        h3("Anxiety and Gaming"),
                        br(),
                        p("In this study, participants were evaluated using 
                          various criteria, such as GAD 
                          (General Anxiety Disorder), SWL 
                          (Satisfaction with Life), and SPIN 
                          (Social Phobia Inventory). Below, the graph plots the 
                          hours spent gaming based on the self-reported effect 
                          of a range of anxiety-related questions on work and 
                          home relations"),
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput("participants", 
                                            "Number of participants:", 
                                            min = 1, 
                                            max = 9995,
                                            value = 100)
                                ),
                            mainPanel(
                                plotOutput("distPlot1")
                                )
                            ), 
                        sidebarLayout(p("This plot displays the average hours 
                                        spent gaming, based on the total score 
                                        on the GAD scale. This scale is created 
                                        by asking participants questions related
                                        to anxiety, such as feelings of 
                                        nervousness or trouble relaxing in the 
                                        two weeks prior to the survey. The 
                                        scores reflect different levels of 
                                        anxiety in an individual, with higher 
                                        scores being increased anxiety."), 
                                      mainPanel(
                                          plotOutput("distPlot2")
                                          )
                                      )
                        ), 
                   tabPanel("Model")
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
                        h2("UPDATE"), 
                        p("As time has gone on, this project has significantly 
                          evolved from my initial interests. While initially, I 
                          had proposed this project as one that would 
                          incorporate Steam data in analysis. However, upon 
                          reflection, I have instead honed in on the 
                          intersections of gender and psychology in various 
                          aspects of gaming. More particularly, I intend on 
                          investigating how behavioral characteristics impact 
                          gaming and life outside of gaming, whether through 
                          character selection, or other such levels of 
                          analysis."),
                        br(),
                        h2("About Me"), 
                        br(),
                        p("My name is Osvaldo Cervantes and I am a sophomore at 
                        Harvard College concentrating in Government. This is my 
                          first foray into data science, and I enjoy analyzing 
                          the data behind government processes."),
                        h2("About this app"),
                        br(), 
                        p("My repo can be found at", 
                          a("https://github.com/osvaldocer/gaming_psychology", 
                            href = "https://github.com/osvaldocer/gaming_psychology")
                          )
                        )
               )
    )


server <- function(input, output) {

    output$distPlot1 <- renderPlot({
        anxiety %>%
            slice(1:input$participants) %>%
            ggplot(aes(x = gade, y = hours)) +
            geom_col() +
            labs(title = "Number of Hours by GAD Effects on Work", 
                 x = "Self-Reported Impact of Anxiety on Workplace Performance", 
                 y = "Hours") +
            theme_classic()
    })
    
    output$distPlot2 <- renderPlot({
        anxiety %>%
            group_by(gad_t) %>%
            mutate(avg_hours = mean(hours)) %>%
            ggplot(aes(x = gad_t, y = avg_hours)) +
            geom_point() +
            labs(title = "Average Hours Spent Gaming by Anxiety Score", 
                 x = "Score on GAD Scale", 
                 y = "Average Hours Played") +
            theme_bw()
    })
}

shinyApp(ui = ui, server = server) 
