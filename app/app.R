# ML Definitions Quiz
library(shinythemes)
library(tidyverse)
library(shiny)
library(tidytext)
library(httr)

definitions <- read_csv("../data/definitions.csv")
## Set images resource path
addResourcePath("images", "images")
load("../data/answers_result.RData")

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

        # Show a term
        mainPanel(
            hr(),
            h4(textOutput("term")),
            hr(),
            br(),
            conditionalPanel(condition = "input.start",
                textAreaInput("termdef",
                          "What is the definition of the above term?", value = NULL
                          ),
                actionButton("submit", "Submit", icon = icon("", lib = "font-awesome")),
                br(), hr(),
                h4("Answers"),
                tableOutput("answer"),
                textOutput("prevscore")
                )
            )
        )
)

server <- function(session, input, output) {
    # Reactive vals to keep track of qs asked
    user <- reactiveValues(if_finish_quiz = NULL, user_response = NULL)
    track <- reactiveValues(score = 0, qs_no = NULL, qs_asked = NULL)
    
    observeEvent(input$start, {
        #initialize values
        user$if_finish_quiz <- NULL
        track$score <- 0
        track$qs_no <- sample(c(1:nrow(definitions)), size = 1)
        track$qs_asked <- 0
    })
    
    observeEvent(input$submit, {
        # Scoring - Compare input term to Definition Text in data
        if(length(str_split(str_trim(input$termdef), pattern = " ", simplify = TRUE)) > 1) {
            user$user_response <- tolower(as.character(input$termdef))
            # response words
            user_raw <- POST("http://104.198.98.159:5000/encode", body = list(text = user$user_response), encode = "json")
            user_result <- fromJSON(content(user_raw, "text"))
            
            # definition words encoded in answers_result
            #answers_sample <- sample(1:379, size = 50)
            scores <- as.numeric(cosineSimilarity(user_result, answers_result))
            cutoff <- quantile(scores, .95)
            cat(cutoff)
            correct <- scores[track$qs_no] > cutoff || which.max(scores) == track$qs_no
            cat(correct)
            which.max(scores)
            if(correct) {
                track$score <- track$score + 1
                cat(track$score)
            }
                
            # stop if 15 questions asked
            if(track$qs_asked == 2) {
                user$if_finish_quiz <- TRUE
            }
            # sampling from unasked questions for finding the next question
            track$qs_no <- sample(setdiff(1:380, as.numeric(track$qs_asked)), size = 1)
            track$qs_asked <- track$qs_asked + 1
            
            updateTextAreaInput(session, value = "", inputId = "termdef")
        } else { # if 0 or only 1 word entered
            updateTextAreaInput(session, value = "", inputId = "termdef")
        }
    })
    
    # Next Term Selection
    # Term Display
    output$term <- renderText({
        if(is.null(user$if_finish_quiz)){
            str_to_title(definitions[track$qs_no,]$Header)
        } else {
            "Let's see how you performed! \n Total Score:"
        }
    })
    
    output$answer <- renderTable({
        if(is.null(user$if_finish_quiz)){
            definitions[track$qs_no,] %>%
                mutate(`Your Answer` = input$termdef) %>%
                select(Definition = Defination_Text, `Your Answer`)
        }
    })
    
    output$prevscore <- renderText({
        if(!is.null(user$if_finish_quiz)) {
            track$score
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
