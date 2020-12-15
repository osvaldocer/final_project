library(tidyverse)
library(shiny)
library(shinythemes)
library(gt)
library(gtsummary)
library(broom.mixed)
library(rstanarm)

# While anxiety and overwatch are being implemented, data is a remnant of my 
# previous topic

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
                        p("As demonstrated by this graph, when set to the 
                          maximum participant numbers of 9995, The hours played 
                          are highest cumulatively under the 'Not difficult at 
                          all' and 'somewhat difficult' columns. Alternatively 
                          participants who rated their anxiety's effects in 
                          their life as 'Very Difficult' or 'Extremely 
                          difficult' had a significantly smaller amount of hours
                          played. This suggests that those who suffer from 
                          anxiety are typically less likely to be able to play 
                          videogames regularly, as their time potentially 
                          becomes consumed by other aspects of their lives."),
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput("participants", 
                                            "Number of participants:", 
                                            min = 500, 
                                            max = 9995,
                                            value = 5000)
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
                                        scores being increased anxiety. As 
                                        presented in this scatter plot, there 
                                        seems to be a general upward trend 
                                        related to the GAD, or Generalized 
                                        Anxiety Disorder. That is to say, in 
                                        general, as the total score of one's 
                                        GAD goes up, so too do average hours 
                                        played. While one may think this
                                        contradictory to the previous plot, 
                                        which seems to establish that there are 
                                        less hours when one self-describes their
                                        experiences as incredibly stressful, 
                                        this is simply a visualization of 
                                        understanding totals versus average 
                                        values. On average, those with 
                                        increased stress play for more hours. 
                                        However, in general, there are also less
                                        people under the descriptors of 
                                        'Extremely difficult' and 
                                        'Very difficult.' As a result, this 
                                        analysis can only establish that those 
                                        with higher anxiety play for longer 
                                        hours."),
                                      mainPanel(
                                          plotOutput("distPlot2")
                                          )
                                      )
                        ), 
                   tabPanel("Model", 
                            titlePanel("Analyzing Hours Played to Anxiety"), 
                            p("this is my analysis of the model", 
                              gt_output("model_table"), 
                              titlePanel("additional model"), 
                              p("additional analysis"))
                            )
               )
               ),
               tabPanel("About",
                        h1("About the data"),
                        p("This data is from two separate studies related to 
                          mental health and gaming in one way or another. One 
                          study, titled ‘Character choice in online gaming,’ was
                          performed in 2018 and focused on the videogame 
                          Overwatch, in which several different playable 
                          characters are available to the player, tailor-made 
                          for specific roles in game. As these character 
                          selections are vital to overall team composition and 
                          cohesiveness, this study sought to map the playstyle 
                          of individuals and find links to character selection 
                          and psychological evaluations."),
                        p("The Overwatch dataset can be found", 
                          a("here", 
                            href = "https://data.mendeley.com/datasets/f4zdfsx88z/2")),
                        p("The other study, entitled ‘Social Context and Gaming 
                          Motives Predict Mental Health Better than Time Played:
                          An Exploratory Regression Analysis with Over 13,000 
                          Video Game Players,’ conducted an extensive study of 
                          gaming habits through the lens of various 
                          psychological examinations. In particular, three 
                          metrics were used in the creation of this dataset—the 
                          Generalized Anxiety Disorder, Satisfaction with Life 
                          Scale, and Social Phobia Inventory. Other auxiliary 
                          metrics were also collected, including place of 
                          residence."), 
                        p("This dataset can be found", 
                          a("here", 
                            href = "https://www.kaggle.com/divyansh22/online-gaming-anxiety-data")),
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
               ))


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
    
    output$model_table1 <- render_gt({
        fit_model <- stan_glm(data = anxiety, 
                              hours ~ gender + gade + gad_t, 
                              family = gaussian, 
                              refresh = 0)
        
        tbl_regression(fit_model) %>%
            as_gt() %>%
            tab_header(title = "Regression of Hours Spent Gaming", 
                       subtitle = "The Link Between Hours Played, Gender, and 
                       Anxiety") %>%
            tab_source_note("Source: Study Published by Marian Sauter and Dejan 
                            Draschkow")
    })
    
    output$model_table2 <- render_gt({
        fit_model <- stan_glm(data = anxiety, 
                              hours ~ gender + gade + gad_t, 
                              family = gaussian, 
                              refresh = 0)
        
        tbl_regression(fit_model) %>%
            as_gt() %>%
            tab_header(title = "Regression of Hours Spent Gaming", 
                       subtitle = "The Link Between Hours Played, Gender, and 
                       Anxiety") %>%
            tab_source_note("Source: Study Published by Marian Sauter and Dejan 
                            Draschkow")
    })
}

shinyApp(ui = ui, server = server) 
