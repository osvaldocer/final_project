library(tidyverse)
library(shiny)
library(shinythemes)
library(gt)
library(gtsummary)
library(rstanarm)
library(broom.mixed)

# My final mistake and final correction, broom.mixed was integral in publishing 
# this correctly, as without it my published shinyapp lacked the necessary 
# models.

# Overall, this project was honestly my most difficult final of this semester. 
# I feel I definitely struggled with synthesizing the lessons gleaned from 
# earlier in the semester all together. However, what I could accomplish, I feel
# was pretty informative and effective.

anxiety <- read_rds("anxiety.rds")
overwatch <- read_rds("overwatch.rds")

# These two datasets were my focus for this project. Both are described 
# extensively in the About portion of my project.

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
                            
                            # there was definitely difficulty for me with 
                            # finding what exactly to make interactive. As part 
                            # of my process, I considered adding an interactive 
                            # map, as the anxiety dataset does contain countries
                            # of origin for its participants. However, the 
                            # complex nature coupled with limited time due to 
                            # last minute changes limited this scope of the 
                            # project.
                            
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
                            p("This regression model depicts the links between 
                              the dependent variable, hours, and the various 
                              independent variables. In particular, gender, the 
                              self-reported GAD variable, and cumulative GAD 
                              score are all used. For starters, the variable 
                              gender seems to be statistically significant. 
                              Between Male and Female, Males experience a median
                              of 3.3 hours increase in their playtime. This 
                              seems consistent with the broader data and 
                              societal understandings of who exactly plays 
                              videogames. As for the self-reported GAD 
                              responses, Extremely difficult presents an 
                              insignificant amount of individuals. For Very 
                              difficult, there is a median of negative 1.7 hour 
                              impact to playtime, and this trend continues to 
                              increase as the self-reported values reach the 
                              least impactful anxiety, at Not difficult at all. 
                              This portion of the model further reinforced what 
                              was established in the analysis of the second plot
                              in the Analysis tab. That is to say, this model 
                              seems to confirm conclusions made for the second 
                              graph, in which those who suffer from increased 
                              anxiety play for extended hours."),
                            
                            # creating these two models was particularly 
                            # interesting for me. Previously, we had been 
                            # dictated what variables to use for our regression 
                            # models, in everything from our psets to the exams. 
                            # Thus, sifting through these datasets was difficult
                            # at first, particularly with my old steam dataset 
                            # no longer present in this shinyapp. I hope in the 
                            # future I delve into additional ways of visualizing
                            # these models, as we had extensive practice with 
                            # posterior distribution models and normal 
                            # distributions.
                            
                              gt_output("model_table1"), 
                              titlePanel("Analysis of Character Choice in 
                                         Videogames with Psychological 
                                         Characteristics"), 
                              p("This regression model departs from the previous
                                model as well as the previous tab by concerning 
                                itself with character choice in videogames. 
                                Using the choice_dictation variable, which 
                                describes the willingness to adhere to group 
                                composition by choosing with team input, as the 
                                dependent variable, other factors like gender or
                                various psychological charactersitics formed the
                                basis of the independent variables. We begin by 
                                acknowledging that, under gendered terms, a Male
                                is a median of -0.53 on the scale, meaning he is
                                less likely to adhere to team cohesion than a 
                                Female when choosing their characters. Further 
                                analysis could prove to find that gendered 
                                interactions in the real world truly transition 
                                to virtual, generally anonymous mediums such as 
                                social media or online gaming. Additionally, 
                                extraversion holds a positive coefficient of 
                                0.85, suggesting that extraverts are more likely
                                to be teamplayers, seeking to choose their 
                                characters based on team input. Neuroticism, 
                                while a negative value of 0.08, is spread out 
                                through a large range, as demonstrated by the 
                                95% confidence interval being between -2 and 
                                1.8. Such a range between negative and positive 
                                values suggests neuroticism may not play as much
                                of a factor in whether one does or does not 
                                consider their team when they choose their 
                                character. Finally, conscienctiousness and 
                                agreeableness are 2.2 and 5.8 respectively, 
                                therefore establishing strong positive relation 
                                correlation with these two behaviors and 
                                character dictation. Thus, it seems that in 
                                general, folks who hold traits valuable to 
                                team-based exercises do see them translated 
                                into online mediums, where accountability is 
                                significantly reduced."), 
                            gt_output("model_table2")
                            )
               )
               ),
               tabPanel("About",
                        h1("About the data"),
                        
                        # removed from this final version of the shinyapp is the
                        # work from my original version, as I had explored how 
                        # to incorporate steam data into my project. Thus, my 
                        # new about section varies in content from my original 
                        # submitted as a milestone weeks prior.
                        
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
               )
    )

# The server portion involved extensive research to see how to implement plots 
# and tables into this section of the shinyapp. My particular difficulty arose 
# from the table format of stan_glm. Our regressions prior had not included use 
# of tables as much, and so finding how to implement them through 
# tbl_regression() functions. Additionally, the implementation of an interactive
# portion was honestly fascinating, as I managed to introduce sliders to these 
# functions.

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
        set.seed(1)
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
        set.seed(1)
        fit_1 <- stan_glm(data = overwatch, 
                          choice_dictation ~ gender + Extraversion + Neuroticism
                          + Conscienctiousness + Agreeableness, 
                          family = gaussian(), 
                          refresh = 0)
        
        tbl_regression(fit_1) %>%
            as_gt() %>%
            tab_header(title = "Regression of Character Choice in Overwatch", 
                       subtitle = "The Link Between Character Choices and 
                       Differing Behavioral Characteristics") %>%
            tab_source_note("Source: 'Character choice in online gaming' by 
            Duncan Hodges and Oliver Buckley")
    })
}

shinyApp(ui = ui, server = server)

# All in all, I enjoyed my time in this course and with the process of creating 
# this shinyapp. I hope to employ at least some of these skills I've learned in 
# my future classes and future career.
# To whomever is grading this, Happy Holidays!

