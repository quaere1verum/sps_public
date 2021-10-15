
# #######################
# Project 3 
# Code part
# Use data to answer the question, "Which are the most valued data science skills?"
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


get_inferred_skills <- function(jobspikr_data)
{
  inferred_skills <- jobspikr_data[['inferred_skills']]
  inferred_skills <- strsplit(inferred_skills, "\\|")
  skills <- c()
  for(inferred_skill in inferred_skills){
    skills <-c(skills, inferred_skill)
  }
  # remove duplicates
  return(skills[!duplicated(skills)])
}

inferred_skills <- get_inferred_skills(jobspikr_data)


# will use this as uniq_id from the frame for companies
#uniq_id
#d578cbc1ebd47ee77eba9e981f3c2582
# uniq_id is not good since there can be multiple job postings for the company
library(digest)
#a<-digest("key_a", algo='xxhash32')
#[1] "4da5b0f8"
#companies_table <- jobspikr_data %>% select(digest(jobspikr_data[[company_name]], algo='xxhash32'), company_name)

#tmp_frame <- jobspikr_data %>% mutate(companyid=lapply(jobspikr_data$company_name, function(x) {digest(x, algo="md5", serialize = F)}))
tmp_frame <- jobspikr_data %>% mutate(companyid=unlist(lapply(company_name, function(x) {digest(x, algo="md5", serialize = F)})   ))

companies <- tmp_frame %>% select(companyid, company_name)

# table ready for insertion
companies_table <- distinct(companies, companyid, company_name)

# trying tmp tables
dbWriteTable(con,"myTempTable", companies_table)
dbExecute(con,"insert into companies(companyid, company_name) select companyid, company_name from myTempTable")
dbExecute(con,"drop table if exists myTempTable")
dbExecute(con,"commit;")

# companies data is your dataframe with the same schema as companies table defined under sql
insert_into_companies_table<- function(companies_data)
{
  dbExecute(con,"drop table if exists myTempTable")
  dbWriteTable(con,"myTempTable", companies_data)
  dbExecute(con,"insert into companies(companyid, company_name) select companyid, company_name from myTempTable")
  dbExecute(con,"drop table if exists myTempTable")
  dbExecute(con,"commit;")
}

# same thing, schema needs to be the same
insert_into_skill_types_table <- function(skill_data)
{
  dbExecute(con,"drop table if exists myTempTable")
  dbWriteTable(con,"myTempTable", skill_data)
  dbExecute(con,"insert into skill_types(skill_id, skill_name) select skill_id, skill_name from myTempTable")
  dbExecute(con,"drop table if exists myTempTable")
  dbExecute(con,"commit;")
}

# same thing, schema needs to be the same
insert_into_skill_rankings_table <- function(skill_rankings_data)
{
  dbExecute(con,"drop table if exists myTempTable")
  dbWriteTable(con,"myTempTable", skill_rankings_data)
  dbExecute(con,"insert into skill_rankings(skill_frequency, companyid, skill_id) select skill_frequency, companyid, skill_id from myTempTable")
  dbExecute(con,"drop table if exists myTempTable")
  dbExecute(con,"commit;")
}



##### Skill Set should be in Database
test<-strsplit(jobspikr_data$inferred_skills, split = "\\|")
skillset <-data.frame(skill=character())
s <-length(test)

for (i in 1:s){
  for (j in 1:lengths(test[i])){
    rows<-data.frame(skill=test[[i]][j])
    skillset <-rbind(skillset,rows)
  }  
}

word_freq<- skillset %>% group_by(skill)%>%   summarise(wfreq=n()) 

# drop skill names that are NA doesn't make sense
word_freq <- word_freq %>% drop_na() 
tmp_frame <- word_freq %>% mutate(skill_id=unlist(lapply(skill, function(x) {digest(x, algo="md5", serialize = F)})), skill_name=skill)

# table ready for insertion
skill_data <- tmp_frame %>% select(skill_id, skill_name)
insert_into_skill_types_table(skill_data)

#############





## RETRIEVAL OF GRAPHS BASED ON FREQUENCY STARTS...


# Looks like job description has a lot of things mixed in it. Requirememnts, disclaimers, company culture descriptions
# character array 
job_description_data <- jobspikr_data[['job_description']]

#data <- "hello , hllo?"
data<- job_description_data
data <- jobspikr_data[['inferred_skills']]

# what happens if we pass data without | and instead splitted data
inferred_skills <- jobspikr_data[['inferred_skills']]
inferred_skills <- strsplit(inferred_skills, "\\|")
data <- inferred_skills

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
#text_corpus_clean[[1]]$content




#Example in R: by using tm package
#Normalization: lowercase the words and remove punctuation and numbers
text_corpus_clean<-tm_map(text_corpus , content_transformer(tolower))
text_corpus_clean <- tm_map(text_corpus_clean, removePunctuation)
text_corpus_clean <- tm_map(text_corpus_clean, removeNumbers)
#text_corpus_clean <- tm_map(text_corpus_clean, removeWords, c("the", "and", stopwords("english")))
text_corpus_clean <- tm_map(text_corpus_clean, stripWhitespace)




##Remove stopwords and custom stopwords
#text_corpus_clean <- c(stopwords('english'), "a", "b") gi
stop_words <- c(stopwords('english'), "a", "b") 

##Remove more stop words
#myStopwords <- c() # some set defined by myself based on particular data
#myStopwords <- setdiff(myStopwords, c("d", "e")) 
#text_corpus_clean <- tm_map(myCorpus, removeWords, myStopwords)
text_corpus_clean <- tm_map(text_corpus_clean, removeWords, stop_words)

# requires new library... install.packages('SnowballC') I think it aims to keep stem words only.
#text_corpus_clean <- tm_map(text_corpus_clean, stemDocument, language = "english")
#writeLines(head(strwrap(text_corpus_clean[[2]]), 15))

# adding more words to remove
#stop_words <- c("science", "will", "work")
#text_corpus_clean <- tm_map(text_corpus_clean, removeWords, stop_words)


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
#install.packages("wordcloud") 
library(wordcloud)
freq = data.frame(sort(colSums(as.matrix(dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))





#Word Frequency
#install.packages("knitr")
#library(knitr) 
# Sum all columns(words) to get frequency
#words_frequency <- colSums(as.matrix(tdm)) 
# create sort order (descending) for matrix
#ord <- order(words_frequency, decreasing=TRUE)

# get the top 20 words by frequency of appeearance
#words_frequency[head(ord, 20)] %>% 
#  kable()



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
ggplot(head(freq.df, 15), aes(reorder(word,freq), freq)) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent bigrams")



 
 some  <- skill_data[['skill_name']]
 
 # skill hash mapped to list of companies
 #install.packages("hash")
 #library(tidyverse)
 #library(hash)
 skill_company_map <- hash()
 #skill_company_map[[skill]]<- c(state_uscf_prerating[3], list(opp_ids))
 
 for (skill in some)
 {
   # if jobspikr_data[['inferred_skills']]
   whatevs<- jobspikr_data %>% mutate(has_pat=grepl(skill, jobspikr_data[['inferred_skills']], fixed=TRUE))
   gracce <- whatevs %>% select(has_pat, inferred_skills, company_name)
   pattern_present <- gracce %>% filter(has_pat==TRUE) 
   companies <- pattern_present[["company_name"]]
   
   companies <- companies[!duplicated(companies)]
   skill_company_map[[skill]] <- companies
 }


skill_rankings_frame = tibble() 
for (key in names(skill_company_map)) {
  companies <- skill_company_map[[key]]
  
  skill_tmp <- skill_data %>% filter(skill_name==key)
  freq_tmp <- word_freq %>%filter(skill==key)
  
  tmp_frame <- tibble(company_name=companies, skill=key, skill_id=skill_tmp[['skill_id']], skill_frequency=freq_tmp[['wfreq']])
  tmp_frame <- tmp_frame %>% mutate(companyid=unlist(lapply(company_name, function(x) {digest(x, algo="md5", serialize = F)})   ))
  skill_rankings_frame <- rbind(tmp_frame, skill_rankings_frame)
}

skill_rankings_table <- skill_rankings_frame %>% select(skill_frequency, companyid, skill_id)
skill_rankings_table

insert_into_skill_rankings_table(skill_rankings_table)