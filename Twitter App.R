#install.packages('twitteR')      Uncomment and install packages if they are not installed
#install.packages('ROAuth')       on your machine
#install.packages('httr')
#install.packages('tm')
#install.packages('wordcloud')
#install.packages('RColorBrewer')
#install.packages('shinydashboard')
#install.packages('shiny')

library(twitteR)
library(ROAuth)
library(httr)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(shinydashboard)
library(shiny)

api_key <- "KC2eefll3gv39QHwC8yeLrzfo"                                  #Gives R access to my twitter
api_secret <- "Yn5fU1evgQFPadb8dGfr2qNrtQTLNqnew0Wtw9WFj8Rg94KDLN"
access_token <- "4292583014-XOD5JM66nuB05CeOdQYzBeh7wR7lmFZxuTOIhhk"
access_token_secret <- "nGWyrv0sM8LLytXWkMHvPfVMSzfPsKlYgGnSEou29pgzF"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

yay = scan('positive_opinion_lexicon.txt',                              #Import lists of positive and 
           what='character', comment.char=';')                          #negative words for sentiment 
boo = scan('negative_opinion_lexicon.txt',                              #association, these files must be
           what='character', comment.char=';')                          #on your local machine

yay = as.data.frame(yay)                                                #Combine positive and negative
yay$score = 'positive'                                                  #into one list and adds whether 
colnames(yay) = c('word', 'score')                                      #word is positive or negative
boo = as.data.frame(boo)
boo$score = 'negative'
colnames(boo) = c('word', 'score')
sentiment = rbind(yay,boo)

ui <- dashboardPage(
  dashboardHeader(title = "Twitter App"),                               #Defines the user interface and
  dashboardSidebar(textInput("keyword", "Keyword", "Football"),         #starting values for the app
                   textInput("sample", "Sample Size of Tweets", 300),
                   submitButton("Update")),
  dashboardBody(fluidRow(
    box(plotOutput("wordcloud"),title = "Wordcloud"),
    box(plotOutput("pie"), title = "Sentiment Mentions")
  ))
)
server <- function(input, output) {    
  
  adjust1 = function(){
    tweets = searchTwitter(input$keyword, n=input$sample, lang = 'en')  #Searches twitter for the specified
    text = sapply(tweets, function(x) x$getText())                      #amount of tweets with a given keyword
                                                                        #and returns the text
    text2 = unlist(strsplit(text,split=", "))                           #Delists the text
    text3 = grep("text2", iconv(text2,"latin1", "ASCII", sub="text2"))  #Removes special characters 9=(%,&,@,etc)
    text4 = text2[-text3]
    text = paste(text4,collaps=",")                                     #Then relists the text
    
    corpus = Corpus(VectorSource(text))                                 #Takes text and changes it into a corpus
    
    toSpace = content_transformer( function(x, pattern) gsub(pattern," ",x) )
    
    corpus_clean = tm_map( corpus, toSpace, "https*")                   #Removes web links from the tweet text
    
    tdm = TermDocumentMatrix(corpus_clean,                             
                             control = list(removePunctuation = TRUE,              #Transforms Corpus into a TDM,
                                            stopwords = c(stopwords("english")),   #removing stop words (common words
                                            removeNumbers = TRUE, tolower = TRUE)) #that arent interesting), numbers
                                                                                   #and puts everything in lower case
    m = as.matrix(tdm)                                                  #Transforms TDM into matrix
  }
  
  adjust2 = function(){

    word_freqs = sort(rowSums(adjust1()), decreasing=TRUE)              #Calculates the frequency of words in the TDM
    dm = data.frame(word=names(word_freqs), freq=word_freqs)            #Transforms frequencies into a dataframe
    dm = dm[!dm$word == tolower(input$keyword),]                        #and then generates a word cloud
    wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
    }
  output$wordcloud = renderPlot(
    adjust2()
  )
  adjust3 = function(){

    word_freqs = sort(rowSums(adjust1()), decreasing=TRUE) 

    dm = data.frame(word=names(word_freqs), freq=word_freqs)          #Again, calculates frequency and generates a DF
    dm = dm[!dm$word == tolower(input$keyword),]
    
    final = join(dm, sentiment, by = 'word')                          #Joins sentiment text to frequency dataframe
    is.na(final$score) = 0                                            #assigns sentiment score of 0 (arbitrary
    final$score[is.na(final$score)]=0                                 #placeholder) for words not in either list
    positive = sum(final$freq[final$score == 'positive'])             #Sums up the frequency of positive and 
    negative = sum(final$freq[final$score == 'negative'])             #negative words
    
    pie3D(c(negative,positive),labels=c(paste('Negative-',negative, sep = ""),   #Generates Pie graph from frequencies
                                        paste("Positive-",positive, sep = "")),
          explode=0.05,
          main="Sentiment Mentions")
  }
  output$pie = renderPlot(
    adjust3()
  )
}
shinyApp(ui, server)                        #Deploys the shiny app
