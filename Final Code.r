#packages required in the code 
# clear the workspace
rm(list = ls())

require(devtools)

#library for the shiny application
library(shiny)
library(memoise)

#library for text claeaning
library(tm)

#library for differenct plots
library(ggplot2)
library(rmarkdown)
library(wordcloud2)
library(dplyr)
library(plotly)

#library for sentiment analysis
library(sentimentr)

#library for data extraction from twitter
library(twitteR)
library(base64enc)
library(RCurl)
library(ROAuth)

#library for different shiny themes
library(shinythemes)

#function for extracting data from twitter and writing it into csv file 
getcsv <- memoise(function(keyword){
  #some important credentials required for twitter data extraction
  api_key <- "SdeKYrH1Oq0SaENHLpWPJ7rxp"
  api_secret <- "wxpUncoiS4wMgaBsjtkzCtNVyB4qaSmIQcKls75mMLDSXnLld4"
  api_access_token <- "790572498148397057-bn4fDloA59hSh4aymrd7bMj1MrHpk6k"
  api_access_token_secret <- "T1A2xinKWgkAAfcAeGpWfsuEs5enOnljGVbzrAjczaWBv"
  
  #setup the connection from twitter account
  setup_twitter_oauth(api_key,api_secret,api_access_token,api_access_token_secret)
  
  #search the twitter using "keyword"
  tweet <- searchTwitter(keyword,n=1000,lang = 'en')
  
  #creates the empty vector`
  l = length(tweet)
  tweeter <- vector(mode = "character", length = l)
  #bind the list of the data frames into one data frame 
  df <- do.call("rbind", lapply(tweet, as.data.frame))
  write.csv(df, file = "D:/demo.csv")
})


#function for the wordclouds and different plot
getTermMatrix <- memoise(function(keyword) {
  #read csv file  
  extract_tw=read.csv("extract_tw.csv")
  tweeter=as.vector(extract_tw$text)
  # Prepare the above text for sentiment analysis
  # Remove @RT 
  tweeter = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweeter)
  # Then remove all "@people"
  tweeter = gsub("@\\w+", "", tweeter)
  
  
  # build a corpus, and specify the source to be character vectors
  myCorpus <- Corpus(VectorSource(tweeter))
  # remove URLs
  removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
  myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
  # convert to lower case
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  # remove anything other than English letters or space
  removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
  myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
  # remove stopwords
  myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),
                   "use", "see", "used", "via", "amp","pl","th","u",keyword,"eduaubdedubufeduaubcedubuaeeduaubcedubub","oyo")
  myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
  # remove extra whitespace
  myCorpus <- tm_map(myCorpus, stripWhitespace)
  # keep a copy for stem completion later
  myCorpusCopy <- myCorpus
  myCorpus <- Corpus(VectorSource(myCorpus))
  
  #creation of term document matrix
  tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(4, Inf)))
  m = as.matrix(tdm)
  
  #creation of frequency matrix
  term.freq <- rowSums(as.matrix(tdm))
  term.freq <- subset(term.freq, term.freq >= 100)
  
  #sorting the term document matrix
  sort(rowSums(m), decreasing = TRUE)
})







#function for sentimental analysis
getTermMatrixSentiment <- memoise(function() {
  #reading the csv 
  tw=read.csv("extract_tw.csv")
  tweeter = tw$text
  
  #creating date vector 
  dates=as.vector(tw$created)
  
  #changing date into factor in specific format
  dates <- as.factor(strftime(dates, format="%d %m %y"))
  
  #text cleaning  
  tweeter = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweeter)
  # Then remove all "@people"
  tweeter = gsub("@\\w+", "", tweeter)
  # Then remove all the punctuation
  tweeter = gsub("[[:punct:]]", "", tweeter)
  # Then remove numbers, we need only text for analytics
  tweeter = gsub("[[:digit:]]", "", tweeter)
  # the remove html links, which are not required for sentiment analysis
  tweeter = gsub("https\\w+","", tweeter)
  # finally, we remove unnecessary spaces (white spaces, tabs etc)
  tweeter = gsub("[ \t]{2,}", "", tweeter)
  
  
  #function for converting all the text into lower case
  catch.error = function(x)
  {
    # let us create a missing value for test purpose
    y = NA
    # try to catch that error (NA) we just created
    catch_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(catch_error, "error"))
      y = tolower(x)
    # check result if error exists, otherwise the function works fine.
    return(y)
  }
  
  
  
  # Now we will transform all the words in lower case using catch.error function we just created above and with sapply function
  tweeter = sapply(tweeter, catch.error)
  
  # Also we will remove NAs, if any exists, from bjp_txt (the collected and refined text in analysis)
  tweeter = tweeter[!is.na(tweeter)]
  #to remove the emoticons 
  tweeter = iconv(tweeter, "latin1", "ASCII", sub="")
  tweeter = gsub("RSVP", "", tweeter)
  tweeter= gsub("oyo","",tweeter)
  #to remove first and last spaces from the text 
  tweeter <- trimws(tweeter, which = c("both", "left", "right"))
  
  # sentiment analysis
  #applying sentiment function to get sentiments of tweets
  sentiments <- sentiment(tweeter)
  #saving tweets with sentimetn in data frame
  sentiment_matrix=as.data.frame(sentiments)
  senti_matrix=table(sentiments$sentiment)
  sentiment_table=aggregate(sentiment_matrix$sentiment , by=list(Category=sentiment_matrix$element_id), FUN=sum)
  names(sentiment_table )=c("tweet","sentiment")
  #creating seperate sets for pos, neg and neutral tweets
  positive_tw <- subset(sentiment_table, sentiment>0)
  negetive_tw <- subset(sentiment_table, sentiment<0)
  neutral_tw <- subset(sentiment_table, sentiment==0)
  tweetfeed=data.frame(sentiment_table$tweet,tweeter,dates)
  names(tweetfeed)=c("id","tweet","date")
  set.seed(1)
  positive_tweets <- merge(tweetfeed, positive_tw, by.x = "id", by.y = "tweet")
 #getting number of positive tweets on each day
   pos=positive_tweets %>% group_by(date) %>%summarise(no_rows = length(date))
  negetive_tweets <- merge(tweetfeed, negetive_tw, by.x = "id", by.y = "tweet")
  neg=negetive_tweets %>% group_by(date) %>%summarise(no_rows = length(date))
  
  #creating zz which includes no. of tweets on each day
  zz <- merge(pos, neg, by.x = "date", by.y = "date",all = TRUE)
  zz[is.na(zz)] <- 0
  pos=data.frame(zz$date,zz$no_rows.x)
  names(pos)=c("date","no_rows")
  neg=data.frame(zz$date,zz$no_rows.y)
  names(neg)=c("date","no_rows")
  #ordering pos and neg
  pos1=pos[order(-pos[,2]),]
  neg1=neg[order(-neg[,2]),]
  #getting most positive and negetive day
  max_pos_date=pos1[1,1]
  #getting tweets of most pos and neg days
  pos_max_tweet=subset(positive_tweets,date==max_pos_date)
  pos_max_tweet=pos_max_tweet[,2]
  #
  max_neg_date=neg1[1,1]
  neg_max_tweet=subset(negetive_tweets,date==max_neg_date)
  neg_max_tweet=neg_max_tweet[,2]
  
  neutral_tweets <- merge(tweetfeed, neutral_tw, by.x = "id", by.y = "tweet")
  
  sentiment_count=data.frame(c("positive tweets","negetive tweets","neutral tweets"),c(nrow(positive_tweets),nrow(negetive_tweets),nrow(neutral_tweets)))
  names(sentiment_count)=c("Sentiment","Count")
  #most pos wordcloud
  myCorpus_pos <- Corpus(VectorSource(pos_max_tweet))
  tdm_pos <- TermDocumentMatrix(myCorpus_pos,control = list(wordLengths = c(1, Inf)))
  term.freq_pos <- rowSums(as.matrix(tdm_pos))
  freq_term_pos <- data.frame(term = names(term.freq_pos), freq = term.freq_pos)
  m_pos <- as.matrix(tdm_pos)
  word.freq_pos <- sort(rowSums(m_pos), decreasing = T)
  #most neg wordcloud
  myCorpus_neg <- Corpus(VectorSource(neg_max_tweet))
  tdm_neg <- TermDocumentMatrix(myCorpus_neg,control = list(wordLengths = c(1, Inf)))
  term.freq_neg <- rowSums(as.matrix(tdm_neg))
  freq_term_neg <- data.frame(term = names(term.freq_neg), freq = term.freq_neg)
  m_neg <- as.matrix(tdm_neg)
  word.freq_neg <- sort(rowSums(m_neg), decreasing = T)
  
  
  return(list(sentiment_count,positive_tweets,negetive_tweets,pos,neg,freq_term_pos,freq_term_neg))
})















Logged = FALSE;

#login page for the GUI purpose
ui1 <- function(){
  #GUI code for making frontend
  tagList(
    div(id = "login",
        wellPanel(textInput("userName", "Username"),
                  passwordInput("passwd", "Password"),
                  br(),actionButton("Login", "Log in"))),
    tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}


#main page GUI of the application 
ui2 <- function(){tagList(shinyUI(fluidPage(
  
  navbarPage("Social Media Analytics App",theme = shinythemes::shinytheme("flatly"),
             #first tab panel for the main page 
             tabPanel( "Main Page",sidebarLayout(sidebarPanel(textInput("selection", "Keyword"), actionButton("update", "Search"),width =3),
                                                 mainPanel(htmlOutput("picture"))
             )),
             #second tab panel for the wordcloud page
             tabPanel(
               
               "WordCloud"
               #,textInput("selection", "Keyword"), actionButton("update", "Search")
               ,sidebarLayout(
                 
                 # Sidebar with a slider and selection inputs
                 sidebarPanel( 
                   #for slider input     
                   sliderInput("freq",
                               "Minimum Frequency:",
                               min = 1,  max = 50, value = 15),
                   sliderInput("max",
                               "Maximum Number of Words:",
                               min = 1,  max = 300,  value = 100)
                 ),
                 
                 # Show Word Cloud
                 mainPanel(
                   #for output of wordcloud2
                   wordcloud2Output(outputId="plot")
                 ))),
             
             #third tabpanel sentiment plots 
             tabPanel( "Plot",sidebarLayout(
               
               # Sidebar with a slider and selection inputs
               sidebarPanel(radioButtons("m", "Select Plots",
                                         list("Frequency plot"='f',"Polarity Pie Chart"='p',"Line graph for tweets"='l'))),
               
               # Show Word Cloud
               mainPanel(
                 #for output of the plotly plots
                 plotlyOutput("plot1")
               ))),
             # fourth tab for sentiment word clouds 
             tabPanel( " Sentiment Word Clouds",sidebarLayout(
               
               # Sidebar with a slider and selection inputs
               sidebarPanel(selectInput("wordcloud", "Wordcloud Type : ",
                                        c("Wordcloud at peak points"='p',"Combined Word Clouds"='c')),radioButtons("s", "Select Plots",
                                                                                                                   list("Wordcloud Of Positive Tweets"='b',"Wordcloud of Negative Tweets"='c'))),
               
               # Show Word Cloud
               mainPanel(
                 wordcloud2Output("plot5"),
                 #This line is  for removing unwanted box displaying in the shiny app 
                 tags$head(
                   #tags$style(HTML('div#wcLabel {display: none;}'))
                 )
               )))
  ) 
  
)))}

#for complete execution of ui1 and ui2 
ui = (htmlOutput("page"))




#backend of the application
server = (function(input, output,session) {
  # word cloud image in the main page 
  output$picture <-
    renderText({
      c(
        '<img src="',
        "https://hr.blr.com/images/news/social-media-word-cloud-click-here.jpg",
        '" width =800 , height =500>'
      )
    })
  
  #verifying the credentials for the login page 
  USER <- reactiveValues(Logged = Logged)
  #csv file containing the usernames and passwords 
  database<- read.csv("database.csv")
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          for(i in 1:nrow(database)){ if(database[i,"username"] == Username)
          { if(database[which(database["username"] == Username),"password"] == Password)
          { USER$Logged <- TRUE
          
          break
          
          }
          }
            
          }
        } 
      }
    }    
  })
  
  observe({
    if (USER$Logged == FALSE) {
      
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",ui1())))
      })
    }
    #condition if the login credentials are correct
    if (USER$Logged == TRUE) 
    { title = paste("Welcome  ",input$userName," !")
    
    output$page <- renderUI({
      
      div(class="outer",do.call(navbarPage,c(inverse=TRUE,title = title,ui2())))
    })
    }
  })
  
  #calling the function getTermMatrix after clicking on the search button
  terms <- eventReactive(input$update,{
    # Change when the "update" button is pressed...
    
    
    # ...but not for anything else
    isolate({
      
      withProgress({
        setProgress(message = "Processing corpus...")
        #getcsv(input$selection)
        getTermMatrix(input$selection)
        
      })
    })
  })
  
  
  #calling the function getTermMatrixSentiment after clicking on the search button
  terms1<- eventReactive(input$update,{
    # Change when the "update" button is pressed...
    
    
    # ...but not for anything else
    isolate({
      
      withProgress({
        setProgress(message = "Processing corpus...")
        #getcsv(input$selection)
        getTermMatrixSentiment()
        
      })
    })
  })
  
  
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  # functions for sentiment wordclouds 
  plotoutput3<- function(){v<- terms1()
  #for combined wordclouds
  if(input$wordcloud == 'c'){
    #for wordcloud of positive tweets
    if(input$s == 'b'){v<- v[[2]]
    
    myCorpus <- Corpus(VectorSource(v$tweet))
    tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(1, Inf)))
    term.freq <- rowSums(as.matrix(tdm))
    #term.freq <- subset(term.freq, term.freq >= 25)
    freq_term <- data.frame(term = names(term.freq), freq = term.freq)
    m <- as.matrix(tdm)
    word.freq <- sort(rowSums(m), decreasing = T)
    
    wordcloud2(data=freq_term, minRotation = 0,maxRotation = 0)}
    #for negative word cloud  
    else if(input$s == 'c'){v<- v[[3]]
    myCorpus <- Corpus(VectorSource(v$tweet))
    tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(1, Inf)))
    term.freq <- rowSums(as.matrix(tdm))
    #term.freq <- subset(term.freq, term.freq >= 25)
    freq_term <- data.frame(term = names(term.freq), freq = term.freq)
    m <- as.matrix(tdm)
    word.freq <- sort(rowSums(m), decreasing = T)
    
    
    wordcloud2(data=freq_term, minRotation = 0,maxRotation = 0)}
    
  }
  #wordclouds at the peak points
  else if(input$wordcloud == 'p'){
    #for positive wordcloud
    if(input$s == 'b'){
      wordcloud2(data=v[[6]], minRotation = 0,maxRotation = 0)
    }
    #for negative wordcloud
    else if(input$s == 'c'){
      wordcloud2(data=v[[7]],size = 0.4, minRotation = 0,maxRotation = 0)
    }
  }
  }
  
  
  
  
  #for the sentiment analysis
  output$plot5 <- renderWordcloud2({
    plotoutput3()  
    
  })
  
  
  #for combined wordcloud with slider input  
  output$plot <- renderWordcloud2({
    v <- terms()
    k<-1
    
    data1 <- data.frame(names(v),v)
    data1 <- data1[1:input$max,]
    
    
    for(i in 1:nrow(data1)){
      if(data1[i,2] >= input$freq)
      {
        k = k+1;
      }
    }
    
    data1 <- data1[1:k-1,]
    
    wordcloud2(data = data1,size =0.5,minRotation = 0,maxRotation = 0)
    
  })
  
  
  
  
  
  
  #for plotting frequency,pie and line charts of the sentiments and tweets 
  output$plot1 <- renderPlotly({
    #frequency plot of the combined tweets 
    if(input$m == 'f'){
      v<- terms()
      v <- subset(v, v >= 25)
      
      #term <- data.frame(term = names(v), freq = v)
      plot_ly(
        x = names(v),
        y = v,
        name = "Frequency Plot",
        type = "bar"
      )
      
      
    }
    #pie chart for the polarity of the tweets
    else if(input$m == 'p'){
      v<- terms1()
      v <- v[[1]]
      lbls = c("Positive","Negative","Neutral")
      pct <- round(v$Count/sum(v$Count)*100)
      lbls <- paste(lbls, pct) # add percents to labels 
      lbls <- paste(lbls,"%",sep="") # ad % to labels  
      plot_ly(v,labels = lbls,values = v$Count, type = 'pie') %>%
        layout(title = 'Polarity Chart for sentiments',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) 
      #pie(v$Count,labels=lbls,col=rainbow(length(lbls)),
      #            main="Pie Chart of Polarity ",density = 100,border = TRUE,cex =2.5,cex.main=2.5)
    }
    #trend line of positive and negative tweets according to date
    else if(input$m == 'l'){
      v<- terms1()
      data <- data.frame(v[[4]]$date,v[[4]]$no_rows,v[[5]]$no_rows)
      plot_ly(data, x = ~v[[4]]$date, y = ~v[[4]]$no_rows, name = 'Positive Tweets', type = 'scatter', mode = 'lines+markers') %>%
        add_trace(y = ~v[[5]]$no_rows, name = 'Negative Tweets', mode = 'lines+markers') %>%
        layout(title = "Distribution of tweets",
               xaxis = list(title = "Date"),
               yaxis = list(title = "No. of Tweets")
        )
      
    }
    
    
    
    
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
})

#command for running the combined application
runApp(list(ui = ui, server = server))

