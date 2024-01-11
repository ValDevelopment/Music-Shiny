#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(ggwordcloud)

final_data <- read_csv("https://raw.githubusercontent.com/ValDevelopment/musicData/main/topSongsLyrics1950_2019.csv")
final_data <- final_data |>
  rename(`Rank This Year` = rank, `Artist Name` = artist, `Song Name` = song, Lyrics = lyrics)
years <- unique(final_data[1])
final_data <- final_data |> arrange(`Artist Name`)



ui <- fluidPage(
  tabsetPanel(
    
    # Opening panel containing info about the app
    tabPanel("App Overview",
             verbatimTextOutput("introText")
             
    ),  
    
    # Panel where you filter by year
    tabPanel("Search by Year",
             selectInput("yearSelect", "Select Year", choices = years),
             radioButtons("lyrics", "Display Lyrics?",
                          c("No" = "hide",
                            "Yes" = "show")),
             actionButton("year", "Submit"),
             tableOutput("yearTable")
             
    ),  
    
    # Panel where you filter by Artist
    tabPanel("Search by Artist",
             selectInput("artistSelect", "Select Artist", choices = unique(final_data$`Artist Name`)),
             radioButtons("lyricsArt", "Display Lyrics?",
                          c("No" = "hide",
                            "Yes" = "show")),
             
             actionButton("artist", "Submit"),
             tableOutput("artistTable"),
             tableOutput("artistInfo"),
             verbatimTextOutput("textText"),
             plotOutput("wordsPlot")
    ),
    
    # Panel where you can see individual Artist rank distributions
    tabPanel("Artist Rank Distribution",
             selectInput("artistPlot", "Select Artist", choices = unique(final_data$`Artist Name`)),
             actionButton("plot", "Submit"),
             plotOutput("artistPlotOut")
             
    
             
             
    )
             
             
             
             
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$introText <- renderText(paste("This app provides the user with an ability to:",
                                 "1) Search Top 10 artists on the US charts in each year from 1950 to 2019 in the `Search by Year` tab,",
                                 "2) Find out about specific appearances of each artist and their infographics* from the Top 10's of all years" , 
                                 "in the 'Search by Artist' tab, and",
                                 "3) See the vizualization of each artist's rank distributions in the 'Artist Rank Distribution' tab"," ",
                                 "Data taken from: https://www.kaggle.com/datasets/stefancomanita/top-us-songs-from-1950-to-2019-w-lyrics", " ",
                                 "*infographics include: number of a given artist's appearance, their average rank, their average lines per song,",
                                 "and 30 of the most popular words from their songs shown as a word cloud.",sep = "\n"))
  
  # Events for each submission button
  yearEvent <- eventReactive(input$year ,{
    
    include <- switch(input$lyrics,
                      hide = c(1, 5),
                      show = c(1), c(1, 5))
    
    year <- final_data|>
      filter(year == input$yearSelect) |>
      select(-include) |>
      arrange(`Rank This Year`) 
    year
    
  })
  
  artistEvent <- eventReactive(input$artist ,{
    
    
    include <- switch(input$lyricsArt,
                      hide = c(3, 5),
                      show = c(3), c(3, 5))
    
    artist <- final_data|>
      filter(`Artist Name` == input$artistSelect) |>
      select(-include)|>
      rename(Year = year, Rank = `Rank This Year`)
    
    
    artist
    
  })
  
  infoEvent <- eventReactive(input$artist ,{
    
    info <- final_data|>
      filter(`Artist Name` == input$artistSelect) 
    
    Appearances <- length(info$`Artist Name`)
    `Average Rank` <- mean(info$`Rank This Year`)
    `Average Lines Per Song`  <- paste(mean(str_count(info$Lyrics, "\\|") + 1), " Lines")
    
    tibble(Appearances, `Average Rank`, `Average Lines Per Song`)
    
  })
  
  rankEvent <- eventReactive(input$plot, {
    
    art <- final_data|>
      filter(`Artist Name` == input$artistPlot) |>
      rename(Rank=`Rank This Year`)
    ggplot(art, aes( x = Rank)) +
    geom_histogram(bins= 10, binwidth = 0.5, fill = "purple") +
      xlim(0, 11) +
      ylim(0, 5) +
      scale_x_continuous(breaks = seq(1,10,1),limits = c(0,11),expand=c(0,0)) +
      
      theme(panel.background = element_blank())
    
  })
  
  wordEvent <- eventReactive(input$artist, {
    
    dataLyr <- final_data|>filter(`Artist Name` == input$artistSelect)
    
    lyrics <- str_replace_all(dataLyr$Lyrics, "\\|", " ")
    lyrics <- gsub('[().,?!]',' ',lyrics)
    lyrics <- str_remove_all(lyrics, "\\d|\\d+")
    
    lyrics_word <- strsplit(paste(lyrics, collapse = " "), ' ')[[1]]
    
    tib <- tibble(lyrics_word)|> 
      rename(Word = lyrics_word) |>
      group_by(Word) |>
      summarise(Count = n()) |>
      arrange(desc(Count))
    
    # There was an attempt to create a slider/input for user to choose how many top words to see, 
    # but it has proven to greatly impact performace of the app if the number was too high. 
    
    # Thus, the app strictly displays only top 30 words. 
    
    set.seed(123)
    tib|>
      top_n(30) |>
    ggplot() + 
      geom_text_wordcloud_area(aes(label = Word, size = 10)) +
      scale_size_area(max_size = 15)+
      
      theme(panel.background = element_blank())
    
  })
  
  # Panel outputs
  output$yearTable <- renderTable({
    
    yearEvent()
    
  })
  
  output$artistTable <- renderTable({
    artistEvent()
    
  })
  
  output$artistInfo <- renderTable({
    
    infoEvent()
    
  })
  
  output$artistPlotOut <- renderPlot({
    rankEvent()
  })
  
  output$wordsPlot <- renderPlot({
    
    wordEvent()
  })
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
