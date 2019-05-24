library(twitteR)
library(ROAuth)
library(ggplot2)
library(stringr)
library(plyr)

reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "http://api.twitter.com/oauth/access_token"
authURL <- "http://api.twitter.com/oauth/authorize"
api_key <- "Enter API key here"
api_secret <- "Enter API secret key here"
access_token <- "Enter access token key here"
access_token_secret <- "Enter secret access token key here"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

posText <- read.delim("positive-words.txt", header=FALSE, stringsAsFactors=FALSE)
posText <- posText$V1
posText <- unlist(lapply(posText, function(x) { str_split(x, "\n") }))
negText <- read.delim("negative-words.txt", header=FALSE, stringsAsFactors=FALSE)
negText <- negText$V1
negText <- unlist(lapply(negText, function(x) { str_split(x, "\n") }))

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of positive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  # create a simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits?
                   sentence = gsub('\\d+', '', sentence)
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}

woeid <-23424848

current_trends  <-  getTrends(woeid)

trending_topic=current_trends$name[1]


tweets=searchTwitter(trending_topic,n=1000)
tweets_txt = sapply(tweets, function(t) t$getText() )
no_of_tweets=length(tweets_txt)
trending <- c(tweets_txt)

scores = score.sentiment(trending, pos.words,neg.words , .progress='text')

scores$trending = factor(rep(c(trending_topic), no_of_tweets))
scores$positive <- as.numeric(scores$score >0)
scores$negative <- as.numeric(scores$score >0)
scores$neutral <- as.numeric(scores$score==0)

trend <- subset(scores, scores$trending==trending_topic)

trend$polarity <- ifelse(trend$score >0,"Positive",ifelse(trend$score < 0,"Negative",ifelse(trend$score==0,"Neutral",0)))

qplot(factor(polarity), data=trend , geom="bar", fill=factor(polarity))+xlab("Polarity Categories") + ylab("Number of Tweets") + ggtitle("Sentimental Analysis of Trending Topic",trending_topic)
