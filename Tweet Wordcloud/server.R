library(shiny)
library(RCurl)
library(twitteR)
library("ROAuth")
library("twitteR")
library("wordcloud")
library("tm")
library(RColorBrewer)
load("twitteR_credentials")
api_key = "Enter API Key here" 
api_secret = "Enter API Secret here" 
access_token = "Access Token" 
access_token_secret = "Token Secret" 
setup_twitter_oauth(api_key,api_secret,access_token,
                    access_token_secret)

clean.text <- function(some_txt)
{
  some_txt = gsub("&amp", "", some_txt)
  
  some_txt = gsub("(RT|via)((?:\b\\W*@\\w+)+)", "", some_txt)
  
  some_txt = gsub("@\\w+", "", some_txt)
  
  some_txt = gsub("[[:punct:]]", "", some_txt)
  
  some_txt = gsub("[[:digit:]]", "", some_txt)
  
  some_txt = gsub("http\\w+", "", some_txt)
  
  some_txt = gsub("[ t]{2,}", "", some_txt)
  
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  
  # define "tolower error handling" function
  
  try.tolower = function(x)
    
  {
    
    y = NA
    
    try_error = tryCatch(tolower(x), error=function(e) e)
    
    if (!inherits(try_error, "error"))
      
      y = tolower(x)
    
    return(y)
    
  }
  
  some_txt = sapply(some_txt, try.tolower)
  
  some_txt = some_txt[some_txt != ""]
  
  names(some_txt) = NULL
  
  return(some_txt)
  
}
# registerTwitterOAuth(twitCred)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  
  output$Wordcloud <- renderPlot({
    input$MakeWordcloud
    isolate({
      search_tweets <- searchTwitter(input$searchTerm, n=input$numberOfTweets,lang="en")
      #search_tweets <- searchTwitter(input$searchTerm, n=input$numberOfTweets,lang="en")
      r_stats_text <- sapply(search_tweets , function(x) x$getText())
      convTweets <- iconv(r_stats_text, to = "utf-8",sub="")
      tweets <- (convTweets[!is.na(convTweets)])
      df7 <- do.call("rbind", lapply(search_tweets, as.data.frame))
      clean_text = clean.text(r_stats_text)
      #create corpus
      #r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))
      r_stats_text_corpus <- Corpus(VectorSource(clean_text))
      
      #clean up
      
      tdm = TermDocumentMatrix(r_stats_text_corpus,
                               control = list(removePunctuation = TRUE,
                                              stopwords = c(tolower(input$searchTerm),stopwords("english")),
                                              removeNumbers = TRUE, tolower = TRUE))
      m = as.matrix(tdm)
      # get word counts in decreasing order
      word_freqs = sort(rowSums(m), decreasing=TRUE) 
      # create a data frame with words and their frequencies
      dm = data.frame(word=names(word_freqs), freq=word_freqs)
     
      
    
    
    })
    print(wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2")))
  })
  
})