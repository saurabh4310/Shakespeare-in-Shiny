library(shiny)
library(tidytext)
library(tidyverse)
library(wordcloud)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)


#Books list
books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

#Function to get frequency of words
getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  
  return(text)
}



ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    sidebarPanel(
      selectInput("book","Choose a book",choices = books),
      checkboxInput("stopwords","Stop Words",value=TRUE),
      actionButton("rerun","Rerun App"),
      hr(),
      h3("Word Cloud Settings"),
      sliderInput("max_words","Max # of words:",min = 10,max = 200,value = 100,step = 20),
      sliderInput("size_large_word","Size of largest words:",min = 1,max = 8,value = 4,step = 1),
      sliderInput("size_small_word","Size of smallest words:",min = 0.1,max = 4,value = 0.5,step = 0.4),
      hr(),
      h3("Word Count Settings"),
      sliderInput("min_word_chart","Minimum words for Counts Chart:",min = 10,max = 100,value = 25,step = 9),
      sliderInput("word_size_chart","Word size for Counts Chart:",min = 8,max = 30,value = 14,step = 3)
      
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Word Clouds",
                 plotOutput("cloud",height = "600px")),
        tabPanel("Word Counts",
                 plotOutput("freq",height = "600px"))
      )
    )
  )
  
)

server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  freq <- eventReactive(
    input$rerun,{withProgress({
      setProgress(message = "Processing corpus...")
      getFreq(input$book,input$stopwords) })
    })
  
  #plotting cloud
  output$cloud <- renderPlot({v <- freq()
  pal <- brewer.pal(8,"Dark2")
  
  v %>% 
    with(
      wordcloud(
        word, 
        n, 
        scale = c(input$size_large_word, input$size_small_word),
        random.order = FALSE, 
        max.words = input$max_words, 
        colors=pal))})
  
  #plotting count
  output$freq <- renderPlot({
    p <- freq()
    p %>% filter(n>input$min_word_chart) %>% ggplot(aes(reorder(word,n),n))+geom_bar(stat='identity')+theme(text = element_text(size = input$word_size_chart),axis.title=element_blank(),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
}

shinyApp(ui = ui, server = server)
