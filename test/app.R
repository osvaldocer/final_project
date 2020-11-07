#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# example app for inserting/removing a tab
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            actionButton("add", "Add 'Dynamic' tab"),
            actionButton("remove", "Remove 'Foo' tab")
        ),
        mainPanel(
            tabsetPanel(id = "tabs",
                        tabPanel("Hello", "This is the hello tab"),
                        tabPanel("Foo", "This is the foo tab"),
                        tabPanel("Bar", "This is the bar tab")
            )
        )
    )
)
server <- function(input, output, session) {
    observeEvent(input$add, {
        insertTab(inputId = "tabs",
                  tabPanel("Dynamic", "This a dynamically-added tab"),
                  target = "Bar"
        )
    })
    observeEvent(input$remove, {
        removeTab(inputId = "tabs", target = "Foo")
    })
}

shinyApp(ui, server)