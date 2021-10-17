
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
library(digest)
#install.packages("hash")
library(hash)
#install.packages("wordcloud") 
library(wordcloud)


load_csv_from_url <- function(url_path)
{
  tmp <- tempfile()
  curl_download(url_path, tmp)
  read_csv(tmp)
}


# RAW URL from data site moved to github
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




#################################################################################
# project3 frame insertion into SQL piece


# Assumptions:
# ------------
# * Windows Machine
# * MySQL Workbench 8.0
#   - there exists a user: 'root' &  server: 'localhost' with password=''. No password required.
# * RStudio installed

# Instructions:
# -------------
# 0.  git clone *.git or retrieve raw files from https://github.com/quaere1verum/sps...
# 1.  Start MySQL Workbench
#     a.) If your MySQL service does not start automatically when Windows starts, press Windows key and search for "Services", press enter.
#         Search for MySQL80 and start the service.
#     b.) Open the file project3.sql and run it. 
# 2.  Start RStudio
#     a.) Open and run the project3_code.r script
#################################################################################

############### write to DB functions ###############

# install required packages and load libraries
#install.packages("DBI")
library(DBI)

#install.packages("RMySQL")
library(RMySQL)

#install.packages("tidyverse")
library(tidyverse)


# create connection to DB 
con <- dbConnect(MySQL(), user='root', dbname='project3', host='localhost')

# set global local_infile=true;
dbSendQuery(con, "set global local_infile=true")

#companies <- dbSendQuery(con, "select * from companies")
#data <- fetch(companies, n=-1)
#data
#companies <- dbSendQuery(con, "describe companies")
#data <- fetch(companies, n=-1)
#data
#skill_types <- dbSendQuery(con, "select * from skill_types")
#data <- fetch(skill_types, n=-1)
#data



# create skill rankings func
create_skill_rankings_table<- function()
{
    dbExecute(con,"drop table if exists skill_rankings")
    dbExecute(con,"commit;")
    skill_rankings_create_sql <- 'CREATE TABLE `skill_rankings`
    (   `skill_frequency` int NOT NULL,
        `companyid` varchar(100) NOT NULL,
        `skill_id` varchar(100) NOT NULL,
         primary key( companyid, skill_id),
         foreign key(companyid) references companies(companyid),
    	  foreign key (skill_id)  references skill_types(skill_id) )'
    dbSendQuery(con, skill_rankings_create_sql)
}



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
############### write to DB functions END  ###############


# get Company data from frame and insert into table
tmp_frame <- jobspikr_data %>% mutate(companyid=unlist(lapply(company_name, function(x) {digest(x, algo="md5", serialize = F)})   ))
companies <- tmp_frame %>% select(companyid, company_name)
# table ready for insertion
companies_table <- distinct(companies, companyid, company_name)
insert_into_companies_table(companies_table)



##### Grab Trang's skill data and insert into DB ############### 
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
############### skill table ends ############### 





### RETRIEVAL OF GRAPHS BASED ON FREQUENCY STARTS... ###
# Insert any corpus to data. Right now we pass Trang's word_freq
data <- word_freq


#put the data into a corpus for text processing
text_corpus <- (VectorSource(data))
text_corpus <- Corpus(text_corpus)


##Tokenization: Split a text into single word terms called "unigrams" 
text_corpus_clean<-Boost_tokenizer(text_corpus)

#Example in R: by using tm package
#Normalization: lowercase the words and remove punctuation and numbers
text_corpus_clean<-tm_map(text_corpus , content_transformer(tolower))
text_corpus_clean <- tm_map(text_corpus_clean, removePunctuation)
text_corpus_clean <- tm_map(text_corpus_clean, removeNumbers)
text_corpus_clean <- tm_map(text_corpus_clean, stripWhitespace)


##Remove stopwords and custom stopwords
stop_words <- c(stopwords('english'), "a", "b") 
text_corpus_clean <- tm_map(text_corpus_clean, removeWords, stop_words)

tdm <- TermDocumentMatrix(text_corpus_clean) #or 
dtm <- DocumentTermMatrix(text_corpus_clean, control = list(wordLengths = c(4, Inf)))


# looks like the text book's cover.. good for presentation, but we care more about the frames for the coding part
freq = data.frame(sort(colSums(as.matrix(dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))


#library(qdap)
#library(RColorBrewer)
#library(RWeka)

# Bigram Bars
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm.bigram = TermDocumentMatrix(text_corpus_clean, control = list(tokenize = BigramTokenizer))

##Extract the frequency of each bigram and analyse the twenty most frequent ones.
freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)


#visualize the top 15 bigrams
library(ggplot2)
ggplot(head(freq.df, 15), aes(reorder(word,freq), freq)) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent bigrams")
### RETRIEVAL OF GRAPHS BASED ON FREQUENCY ENDS  ... ###




#############################################################
#### Skill Rankings DB Insertion ####
 skill_names  <- skill_data[['skill_name']]
 # skill hash mapped to list of companies
 skill_company_map <- hash()
 
 
 for (skill in skill_names)
 {
   
   tmp_frame <- jobspikr_data %>% mutate(has_pat=grepl(skill, jobspikr_data[['inferred_skills']], fixed=TRUE))
   tmp_frame <- tmp_frame %>% select(has_pat, inferred_skills, company_name)
   pattern_present <- tmp_frame %>% filter(has_pat==TRUE)  # has pattern 
   companies <- pattern_present[["company_name"]]
   
   companies <- companies[!duplicated(companies)]
   skill_company_map[[skill]] <- companies
 }

# create skill rankings frame
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

# insert into DB 
create_skill_rankings_table()
insert_into_skill_rankings_table(skill_rankings_table)

