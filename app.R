library(RCurl)
library(shiny)
library(shinythemes)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(tm)
cocktails <- read.csv(text = getURL("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv"))

# Define UI for application that draws a histogram
ui <- fluidPage( theme = shinytheme("superhero"),
                 titlePanel("Let's try Cocktails"),
                 navbarPage(
                   ">>>>>",
                   tabPanel("Overall Exploration",
                            sidebarPanel(
                              "INPUT",
                              radioButtons("butt","Most used ingredients in cocktail type:",
                                           choices = c("Alcoholic cocktail","Other cocktails"),
                                           select="Other cocktails"),
                            ),
                            mainPanel(
                              plotOutput("plot")
                            )
                            
                   ),
                   tabPanel("Indivial Exploration",
                            sidebarPanel(
                              selectInput("name","Drink name",
                                          choices=unique(cocktails$drink),
                                          selected = "747"),
                              selectInput("req","Requirements",choices=c("Status","Ingredients","None"),
                                          selected = "None"),
                              conditionalPanel('input.req=="Status"',
                                               checkboxGroupInput("chrac","Characteristics of the cocktail",
                                                                  choices = names(cocktails)[c(5,6,8)],
                                                                  selected=names(cocktails)[5])),
                              conditionalPanel('input.req =="Ingredients"',
                                               selectInput("op","Need measure of the ingredients",choices=c("Yes","No"),
                                                           selected = "No")
                              )               
                              
                            ),
                            mainPanel(
                              dataTableOutput("t1")
                            )
                   ),
                   tabPanel("My_Drink",
                            sidebarPanel(
                              selectInput("name2","Drink name",
                                          choices=unique(cocktails$drink),
                                          selected = "747"),
                              radioButtons("pic","Picture of the cocktail",choices = c("Yes","No"),
                                           selected = "No"),
                              conditionalPanel(condition='input.pic=="Yes"',
                                               p("Enjoy the music", strong("and your drink"))),
                            ),
                            mainPanel(
                              conditionalPanel(condition='input.pic=="Yes"',htmlOutput("picture")),
                              
                              conditionalPanel(condition='input.pic=="No"',
                                               p("Enjoy the music", strong("BarTender is preparing your drink")))
                            )
                   )
                   
                 )
                 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot<-renderPlot({
    if(input$butt=="Alcoholic cocktail"){
      data<-cocktails %>%
        filter(alcoholic=="Alcoholic") %>%
        select(ingredient)
      text <- data$ingredient
      docs <- Corpus(VectorSource(text))
      dtm <- TermDocumentMatrix(docs)
      matrix <- as.matrix(dtm)
      words <- sort(rowSums(matrix),decreasing=TRUE)
      df <- data.frame(word = names(words),freq=words)
      
      set.seed(1234) # for reproducibility
      wordcloud(words = df$word, freq = df$freq, min.freq = 1,max.words=200,random.order=FALSE,rot.per=0.35,
                colors=brewer.pal(8, "Dark2"))
    } else {
      data<-cocktails %>%
        filter(alcoholic!="Alcoholic") %>%
        select(ingredient)
      text <- data$ingredient
      docs <- Corpus(VectorSource(text))
      dtm <- TermDocumentMatrix(docs)
      matrix <- as.matrix(dtm)
      words <- sort(rowSums(matrix),decreasing=TRUE)
      df <- data.frame(word = names(words),freq=words)
      
      set.seed(1234) # for reproducibility
      wordcloud(words = df$word, freq = df$freq, min.freq = 1,max.words=200,random.order=FALSE,rot.per=0.35,
                colors=brewer.pal(8, "Dark2"))
      
    }
  })
  output$picture<-renderText({
    im<-cocktails %>%
      filter(drink==input$name2) %>%
      select(drink_thumb) %>%
      unique()
    src = im$drink_thumb
    c('<img src="',src,'">')})
  
  output$t1<-renderDataTable({
    if (input$req=="Ingredients" & input$op=="No"){
      cocktails %>%
        filter(drink==input$name) %>%
        select(ingredient)
    }
    else if(input$req=="Ingredients" & input$op=="Yes"){
      cocktails %>%
        filter(drink==input$name) %>%
        select(ingredient,measure)
    } else if(input$req=="Status" & length(input$chrac)==1 & input$chrac[1]=='alcoholic'){
      cocktails %>%
        filter(drink==input$name) %>%
        select(alcoholic) %>%
        unique()
    } else if(input$req=="Status" & length(input$chrac)==1 & input$chrac[1]=='category'){
      cocktails %>%
        filter(drink==input$name) %>%
        select(category) %>%
        unique()
    }
    else if(input$req=="Status" & length(input$chrac)==1 & input$chrac[1]=='glass'){
      cocktails %>%
        filter(drink==input$name) %>%
        select(glass) %>%
        unique()
    } else if(input$req=="Status" & length(input$chrac)==2 & input$chrac[1]=="alcoholic" & input$chrac[2]=="category"){
      cocktails %>%
        filter(drink==input$name) %>%
        select(alcoholic,category) %>%
        unique()
    } else if(input$req=="Status" & length(input$chrac)==2 & input$chrac[1]=="category" &
              input$chrac[2]=="glass"){
      cocktails %>%
        filter(drink==input$name) %>%
        select(category,glass) %>%
        unique()
    } else if(input$req=="Status" & length(input$chrac)==2 & input$chrac[1]=="alcoholic" &
              input$chrac[2]=="glass"){
      cocktails %>%
        filter(drink==input$name) %>%
        select(alcoholic,glass) %>%
        unique()
    } else if(input$req=="Status" & length(input$chrac)==3 & input$chrac[1]=="alcoholic" & input$chrac[2]=="category" &
              input$chrac[3]=="glass"){
      cocktails %>%
        filter(drink==input$name) %>%
        select(alcoholic,category,glass) %>%
        unique()
    }
  }) 
  
}

# Run the application
shinyApp(ui = ui, server = server)