# ML Definitions Quiz
library(shinythemes)
library(tidyverse)
library(shiny)
library(tidytext)
library(httr)
library(jsonlite)
library(wordVectors)

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
            br(), br(),
            hr(),
            sliderInput("n_qs", label = "Select the number of questions", min = 0, max = 30, value = 5),
            hr(),
            a(href = "https://developers.google.com/machine-learning/glossary/", "Source")
        ),

        # Show a term
        mainPanel(
            hr(),
            h4(textOutput("term")),
            conditionalPanel(
                condition = "input.start",
                hr(),
                br(),
                textAreaInput("termdef",
                          "What is the definition of the above term?", value = NULL
                          ),
                actionButton("submit", "Submit", icon = icon("", lib = "font-awesome")),
                br(), hr(),
                textOutput("prevscore"),
                tableOutput("answers")
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
        track$qs_asked <- NULL
        user$user_response <- NULL
    })
    
    observeEvent(input$submit, {
        # Scoring - Compare input term to Definition Text in data
        if(length(str_split(str_trim(input$termdef), pattern = " ", simplify = TRUE)) > 0) {
            response <- tolower(as.character(input$termdef))
            # response words
            user_raw <- POST("http://104.198.98.159:5000/encode", body = list(text = response), encode = "json")
            user_result <- fromJSON(content(user_raw, "text"))
            
            # definition words encoded in answers_result
            scores <- as.numeric(cosineSimilarity(user_result, answers_result))
            cutoff <- quantile(scores, .95)
            cat(cutoff)
            correct <- scores[track$qs_no] > cutoff || which.max(scores) == track$qs_no
            cat(correct)
            which.max(scores)
            if(correct) {
                track$score <- track$score + 1
            }
                
            # keep track of asked
            track$qs_asked <- c(track$qs_asked, track$qs_no) 
            user$user_response <- c(user$user_response, input$termdef)
            
            # stop if 15 questions asked
            if(length(track$qs_asked) == input$n_qs) {
                user$if_finish_quiz <- TRUE
            }
            # sampling from unasked questions for finding the next question
            track$qs_no <- sample(setdiff(1:380, as.numeric(track$qs_asked)), size = 1)
            
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
            "Let's see how you performed! \n "
        }
    })
    
    output$answers <- renderTable({
        if(!is.null(user$if_finish_quiz)){
            definitions[track$qs_asked,] %>%
                mutate(`Your Answers` = user$user_response) %>%
                select(Term = Header, Definitions = Defination_Text, `Your Answers`)
        }
    })
    
    output$prevscore <- renderText({
        if(!is.null(user$if_finish_quiz)) {
            paste("Total Right Answers:", track$score, "\n Percent: ", track$score/input$n_qs*100, "%")
        } else {
            paste("Current Score =", track$score)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
