# ML Definitions Quiz
library(shinythemes)
library(tidyverse)
library(shiny)
library(tidytext)
library(httr)
library(jsonlite)
library(wordVectors)
## Read data
definitions <- read_csv("data/definitions.csv")
## Set images resource path
addResourcePath("images", "images")

## Load BERT vectors for definitions   
load("data/answers_result.RData")
## Initialize BERT similarity function
get_bert_scores <- function(response) {
    # response words
    user_raw <- POST("http://104.198.98.159:5000/encode", body = list(text = response), encode = "json")
    user_result <- fromJSON(content(user_raw, "text"))
    
    # definition words encoded in answers_result
    similarities <- as.numeric(cosineSimilarity(user_result, answers_result))
    return(similarities)
}

###### Load WordVectors 
load("data/word2vec_model2.RData")
## Parse All definitions words 
alldefinition_words <- definitions %>%
    unnest_tokens(word, Defination_Text, token = "tweets") %>%
    anti_join(stop_words) %>%
    mutate(word = SnowballC::wordStem(word, language = "english")) %>%
    add_count(Header, word, sort = TRUE) %>%
    bind_tf_idf(word, Header, n)
# Get definitions vectors
definitions_vector <- alldefinition_words %>%
    split(.$Header) %>%
    map(~ word2vec_model2[[.$word, average = TRUE]])
## Initialize similarity and exact word matching function
get_word_score <- function(response, qno) {
    # response words
    answords <- tibble(response = as.character(response), 
                       term = definitions[qno,]$Header) %>%
        unnest_tokens(word, response, token = "tweets") %>%
        anti_join(stop_words) %>%
        mutate(word = SnowballC::wordStem(word, language = "english"))
    # definition words
    defwords <- alldefinition_words %>%
        filter(Header == definitions[qno,]$Header)
    matchdf <- defwords %>%
        inner_join(select(answords, term, word), by = "word" ) 
    avg_words <- (length(unique(answords$word)) + length(unique(defwords$word)))/2
    # score = no. of matching words/avg length of definition
    score_exactmatch <- length(unique(matchdf$word))/avg_words  # toCorrect: will penalize when true definition is very long
    # word2vec scoring - response vector compared to all definitions vectors created earlier
    response_vector <- word2vec_model2[[answords$word, average = TRUE]] 
    # get cosine similarity to all definition vectors
    similarities <- map_dbl(definitions_vector, ~cosineSimilarity(response_vector, .))
    return(list(similarities = similarities, score_exactmatch = score_exactmatch))
}

###### Shiny App 
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
            strong(h3(textOutput("term"))),
            conditionalPanel(
                condition = "input.start",
                hr(),
                br(),
                textAreaInput("termdef",
                          "What is the definition of the above term?", value = NULL
                          ),
                actionButton("submit", "Submit", icon = icon("", lib = "font-awesome")),
                br(), hr(),
                uiOutput("prevscore"),
                tableOutput("answers")
                )
            )
        )
)

server <- function(session, input, output) {
    # Reactive vals to keep track of qs asked
    user <- reactiveValues(if_finish_quiz = NULL, user_response = NULL)
    track <- reactiveValues(score = 0, bert = NULL, wordvec = NULL, qs_no = NULL, qs_asked = NULL)
    
    observeEvent(input$start, {
        #initialize values
        user$if_finish_quiz <- NULL
        track$score <- 0
        track$qs_no <- sample(c(1:nrow(definitions)), size = 1)
        track$qs_asked <- NULL
        track$bert <- NULL
        track$wordvec <- NULL
        user$user_response <- NULL
    })
    
    observeEvent(input$submit, {
        # Scoring - Compare input term to Definition Text in data
        if(length(str_split(str_trim(input$termdef), pattern = " ", simplify = TRUE)) > 0) {
            response <- tolower(as.character(input$termdef))
            # Get BERT similarities 
            similarities_bert <- get_bert_scores(response)
            cutoff_bert <- quantile(similarities_bert, .95, na.rm = TRUE)
            percentile_bert <- ecdf(similarities_bert)
            correct_bert <- similarities_bert[track$qs_no] > cutoff_bert || which.max(similarities_bert) == track$qs_no
            result_bert <- ifelse(correct_bert, "Correct", "Incorrect")
            # Final score uses only bert #TODO
            if(correct_bert) {
                track$score <- track$score + 1
            }
            
            # Get word and wordvec similarities and score  
            word_scores <- get_word_score(response, track$qs_no)
            cutoff_word <- quantile(word_scores$similarities, na.rm = TRUE, 0.95)
            percentile_word <- ecdf(word_scores$similarities)
            correct_word <- word_scores$similarities[track$qs_no] > cutoff_word || which.max(word_scores$similarities) == track$qs_no
            result_word <- ifelse(correct_word, "Correct", "Incorrect")
            
            # Keep track of asked
            track$qs_asked <- c(track$qs_asked, track$qs_no) 
            user$user_response <- c(user$user_response, input$termdef)
            track$bert <- c(track$bert,
                            paste("Similarity:", round(similarities_bert[track$qs_no], digits = 2), ", Percentile:",
                                     round(percentile_bert(similarities_bert[track$qs_no])*100, digits = 2), "% ", result_bert)) 
            track$wordvec <- c(track$wordvec,
                               paste("Similarity:", round(word_scores$similarities[track$qs_no], digits = 2), ", Percentile:",
                                     round(percentile_word(word_scores$similarities[track$qs_no])*100, digits = 2), "% ", result_word))
            
            # Stop if set questions asked
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
                mutate(`Your Answers` = user$user_response, BERT = track$bert, WORD2VEC = track$wordvec) %>%
                select(Term = Header, Definitions = Defination_Text, `Your Answers`, BERT, WORD2VEC)
        }
    })
    
    output$prevscore <- renderUI({
        if(!is.null(user$if_finish_quiz)) {
            list(
                h3("According to BERT"),
                h4("Total Right Answers:", track$score),
                h4("Percent: ", track$score/input$n_qs*100, "%")
            )
        } else {
            list(h4("Current Score =", track$score))
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
