
# #######################
# Project 3 
# Code part
# #######################


# We install out libraries
library(tidyverse)
library(curl)
#install.packages("tm") # if not already installed
library(tm)

load_csv_from_url <- function(url_path)
{
  tmp <- tempfile()
  curl_download(url_path, tmp)
  read_csv(tmp)
}


# RAW URL from data site moved to github
jobspikr_url = 'https://raw.githubusercontent.com/quaere1verum/sps_public/master/data607-001/assignments/project3/data_scientist_united_states_job_postings_jobspikr.csv'

jobspikr_small_url = 'https://raw.githubusercontent.com/quaere1verum/sps_public/master/data_scientist_42928_20211010_1633890523765196_1.csv'




jobspikr_data <- load_csv_from_url(jobspikr_small_url)

# Looks like job description has a lot of things mixed in it. Requirememnts, disclaimers, company culture descriptions
# character array 
job_description_data <- jobspikr_data[['job_description']]





# test project 3 string parsing...

#data <- "hello , hllo?"

data<- job_description_data

#put the data into a corpus for text processing
text_corpus <- (VectorSource(data))
text_corpus <- Corpus(text_corpus)
summary(text_corpus)

# idx 5 assume to be present, but it aint
#idx<-1
#to see the text and examine the corpus
text_corpus[[5]]$content
for (i in 1:5) print (text_corpus[[i]]$content)



##Tokenization: Split a text into single word terms called "unigrams" 
text_corpus_clean<-Boost_tokenizer(text_corpus)
text_corpus_clean[[1]]$content




#Example in R: by using tm package
#Normalization: lowercase the words and remove punctuation and numbers
text_corpus_clean<-tm_map(text_corpus , content_transformer(tolower))
text_corpus_clean <- tm_map(text_corpus_clean, removePunctuation)
text_corpus_clean <- tm_map(text_corpus_clean, removeNumbers)
text_corpus_clean <- tm_map(text_corpus_clean, removeWords, c("the", "and", stopwords("english")))
text_corpus_clean <- tm_map(text_corpus_clean, stripWhitespace)




##Remove stopwords and custom stopwords
#text_corpus_clean <- c(stopwords('english'), "a", "b") gi
stop_words <- c(stopwords('english'), "a", "b") 

##Remove more stop words
myStopwords <- c() # some set defined by myself based on particular data
myStopwords <- setdiff(myStopwords, c("d", "e")) 
#text_corpus_clean <- tm_map(myCorpus, removeWords, myStopwords)
text_corpus_clean <- tm_map(text_corpus_clean, removeWords, stop_words)

# requires new library... install.packages('SnowballC') I think it aims to keep stem words only.
#text_corpus_clean <- tm_map(text_corpus_clean, stemDocument, language = "english")
#writeLines(head(strwrap(text_corpus_clean[[2]]), 15))

# adding more words to remove
stop_words <- c("science", "will", "work")
text_corpus_clean <- tm_map(text_corpus_clean, removeWords, stop_words)


tdm <- TermDocumentMatrix(text_corpus_clean) #or 
dtm <- DocumentTermMatrix(text_corpus_clean, control = list(wordLengths = c(4, Inf)))
inspect(tdm)

# TODO: care about bounds ... can't be anything...

#inspect part of the term-document matrix
inspect(tdm[1:10, 1:50])

#inspect(review_dtm[500:505, 500:505])


#Frequent terms that occur between 30 and 50 times in the corpus
frequent_terms <- findFreqTerms (tdm,30,50) 

#findFreqTerms (tdm,200, 1000) 
#[1] "ability"     "analysis"    "analytics"   "business"    "models"      "python"      "scientist"   "statistical" "techniques" 
#[10] "tools"       "using"       "years"       "development" "job"         "knowledge"   "learning"    "machine"     "skills"     
#[19] "solutions"   "model"       "required"    "team"        "technical"  

# looks like the text book's cover.. good for presentation, but we care more about the frames for the coding part
install.packages("wordcloud") 
library(wordcloud)
freq = data.frame(sort(colSums(as.matrix(dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))





#Word Frequency
#install.packages("knitr")
library(knitr) 
# Sum all columns(words) to get frequency
words_frequency <- colSums(as.matrix(tdm)) 
# create sort order (descending) for matrix
ord <- order(words_frequency, decreasing=TRUE)

# get the top 20 words by frequency of appeearance
words_frequency[head(ord, 20)] %>% 
  kable()



# TODO: figure out if this would be cool to have
#findAssocs(dtm, "word",corlimit=0.80)
#install.packages('proxy')
#require('proxy')
#dis=dissimilarity(tdm, method="cosine")

#visualize the dissimilarity results by printing part of the big matrix
#as.matrix(dis)[1:20, 1:20]
#visualize the dissimilarity results as a heatmap
#heatmap(as.matrix(dis)[1:20, 1:20])



library(wordcloud)
library(qdap)
library(RColorBrewer)
library(RWeka)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm.bigram = TermDocumentMatrix(text_corpus_clean, control = list(tokenize = BigramTokenizer))

##Extract the frequency of each bigram and analyse the twenty most frequent ones.
freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)

#visualize the wordcloud   
wordcloud(freq.df$word,freq.df$freq,max.words=100,random.order = F )

#visualize the top 15 bigrams
library(ggplot2)
ggplot(head(freq.df,15), aes(reorder(word,freq), freq)) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent bigrams")
