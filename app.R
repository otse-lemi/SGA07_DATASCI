library(shiny)
library(rtweet)
library(dplyr)
library(glue)
library(reactable)
library(purrr)

make_url_html <- function(url) {
    if(length(url) < 2) {
        if(!is.na(url)) {
            as.character(glue("<a title = {url} target = '_new' href = '{url}'>{url}</a>") )
        } else {
            ""
        }
    } else {
        paste0(purrr::map_chr(url, ~ paste0("<a title = '", .x, "' target = '_new' href = '", .x, "'>", .x, "</a>", collapse = ", ")), collapse = ", ")
    }
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Application title
    titlePanel("Covid Relational Environment. Hello, You're Welcome!"),
  
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(h4("Information Niche"),
                    textInput("name","Enter your name", ""),
                    textInput("email","Enter your email", ""),
                    radioButtons("update","How often do you want to be updated?", list("Daily", "Weekly", "Biweekly", "Monthly"), ""),
                    sliderInput("slide", "Select the number of bins", min = 5, max = 30, value = 10, step = 5),
                    selectInput("statenames", "Select the state", c("Benin", "Ibadan", "Kano", "Lagos", "California", "Nigeria", "USA"), selected = "Lagos", selectize = FALSE),
            numericInput("num_tweets_to_download",
                          "Number of tweets to download:",
                          min = 100,
                          max = 15000,
                          value = 200,
                          step = 100),
            textInput("hashtag_to_search",
                      "Hashtag to search:",
                      value = "#covid19"),
            dateRangeInput("date_picker", label = "Select dates:", start = "2020-01-27", end = "2020-01-30"),
            actionButton("get_data", "Get Data", class = "btn-primary"),
            br(),br(),
            downloadButton("download_data", "Download")
        ),
    
        # Show a plot of the generated distribution
        mainPanel(h4("Output Niche"),
            textOutput("yourname"),
            textOutput("youremail"),
            textOutput("yourupdate"),
            textOutput("yourslide"),
            textOutput("state"),
          
        
            reactableOutput("tweet_table")
          
        )
      
    )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$yourname<- renderText(input$name)
  output$youremail<- renderText(input$email)
  output$yourupdate<- renderText(input$update)
  output$yourslide<- renderText(input$slide)
  output$state<- renderText(input$statenames)
  
  tweet_df <- eventReactive(input$get_data, {
      search_tweets(input$hashtag_to_search, n = input$num_tweets_to_download, include_rts = FALSE)
    
  })
    
  tweet_table_data <- reactive({
      req(tweet_df())
      tweet_df() %>%
          select(user_id, status_id, created_at, screen_name, text, favorite_count, retweet_count,urls_expanded_url) %>%
          mutate(
              Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>"),
              URLs = purrr::map_chr(urls_expanded_url, make_url_html)
          )%>%
          select(DateTime = created_at, User = screen_name, Tweet, Likes = favorite_count, RTs = retweet_count, URLs = urls_expanded_url)
    
    
    })
  
output$tweet_table <- renderReactable({
        reactable::reactable(tweet_table_data(), 
                             filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                             showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200), 
                             columns = list(
                                 DateTime = colDef(defaultSortOrder = "asc"),
                                 User = colDef(defaultSortOrder = "asc"),
                                 Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                                 Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
                                 RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE)),
                                 URLs = colDef(html = TRUE)
                            )
        )
    })
  

output$download_data <- downloadHandler(
    filename = function() {
        paste(input$hashtag_to_search, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
        write.csv(tweet_table_data(), file, row.names = FALSE)
    }
    
)
  
} 

# Run the application 
shinyApp(ui = ui, server = server)
