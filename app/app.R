# ML Definitions Quiz
library(shinythemes)
library(tidyverse)
library(shiny)

definitions <- read_csv("../data/definitions.csv")
## Set images resource path
addResourcePath("images", "images")

ui <- fluidPage(theme = shinytheme("cerulean"),
                
    includeCSS("css/styles.css"),

    # Application title
    titlePanel("Machine Learning Definitions"),

    sidebarLayout(
        sidebarPanel(
            a(href = "https://oaiti.org", target = "_blank", img(src = "images/oaiti_transparent.png", width = "135")),
            h3("Welcome to the Machine Learning Glossary Quiz!"), 
            actionButton("start", "Start Quiz", icon = icon("play-circle", lib = "font-awesome")),
            hr(),
            a(href = "https://developers.google.com/machine-learning/glossary/", "Source")
        ),

        # Show a definition
        mainPanel(
            hr(),
            h4(textOutput("definition")),
            hr(),
            br(),
            textInput("term",
                     "What term does the above text define?"
                     ),
            actionButton("submit", "Submit", icon = icon("", lib = "font-awesome"))
            ),
        )
)

server <- function(input, output) {
    # Reactive vals to keep track of qs asked
    user <- reactiveValues(if_finish_quiz = NULL, user_response = NULL)
    track <- reactiveValues(score = 0, qs_no = 1, qs_asked = NULL)
    
    observeEvent(input$start, {
        #initialize values
        user$if_finish_quiz <- NULL
        track$score <- 0
        track$qs_no <- 1
        qs_asked <- NULL
    })
    
    observeEvent(input$submit, {
        user$user_response <- as.numeric(input$term)
        track$score <- track$score + 1
        track$qs_no <- track$qs_no + 1
        if(track$qs_no == 15) {
            user$if_finish_quiz <- TRUE
        }
    })
    
    # Next Definition Selection
    
    # Definition Display
    output$definition <- renderText({
        if(is.null(user$if_finish_quiz)){
            definitions[track$qs_no,]$Defination_Text
        } else
            "Let's see how you performed!"
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
