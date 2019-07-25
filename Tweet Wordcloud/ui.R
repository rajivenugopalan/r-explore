library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Twitter"),
  
  # Sidebar with controls to provide a caption, select a dataset,
  # and specify the number of observations to view. Note that
  # changes made to the caption in the textInput control are
  # updated in the output area immediately as you type
  sidebarLayout(
    sidebarPanel(
      textInput("searchTerm", "Enter Search Term:", "Workiva"),
      
      numericInput("numberOfTweets", "Number of Tweets to view:", 100),      
      conditionalPanel( 
        condition = "input.numberOfTweets >=0 & input.numberOfTweets<=1000",
        actionButton("MakeWordcloud","Draw Wordcloud")
      )
    ),
    
    
    # Show the caption, a summary of the dataset and an HTML 
    # table with the requested number of observations
    mainPanel(
      h3(textOutput("searchTerm", container = span)),
      
      plotOutput("Wordcloud") ,
      
      tableOutput("view")
      
    )
    
   
  )
))
