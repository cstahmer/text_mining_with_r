# Script: topicmodels.R
# 
# A script written and distributed as a teaching
# aid for demonstrating how to perform Latent
# Dirichlet Allocation (LDA) Topic Modeling of
# a text cropus. Experienced programmers will see
# many ways that this code could be made more
# efficient and elegant in terms of both processing
# speed and memory management.  The code as
# presented is designed to allow a novice coder
# to follow the logic of the script as intuitively
# as possible. With this in mind, the following
# conventions are used throught the code:
#
# (1) In order to facilitate distinguishing between
# variables, functions, and objects all variables in 
# the code begin with the "var_" prefix.
#
# (2) In order to facilitate distinguishing between
# variables, functions, and objects all objects in 
# the code begin with the "obj_" prefix.  In this 
# context and object is any data representation or 
# collection that is in a non-standard data format. 
# For example, the tm package creates their own 
# special class of simple triplex matrix called a 
# Corpus.
#
# (3) Locally defined functions begin with the 
# function_ prefix
#
# Copyright Carl G. Stahmer - 2018
# Director of Data Digital Scholarship - UC Davis Library
# Associate Director for Humanities - UC Davis Data Science Initiative
# Associate Director - English Broadside Ballad Archive
#
# This work is licensed under a Creative Commons 
# Attribution-ShareAlike 4.0 International License.
#
# see http://creativecommons.org/licenses/by-sa/4.0/

# execute the blow lines if your R environment does
# not already include the required packages.
# install.packages("NLP")
# install.packages("tm")
# install.packages("hunspell")
# install.packages("sylly")
# install.packages("sylly.en")
# install.packages("koRpus")
# install.packages("koRpus.lang.en")
# install.packages("textstem")
# install.packages("modeltools")
# install.packages("topicmodels")

# Load Required Libraries
library(NLP)
library(tm)
library(hunspell)
library(sylly)
library(sylly.en)
library(koRpus)
library(koRpus.lang.en)
library(textstem)
library(topicmodels)

###################################
#         configuration           #
###################################

# set the path to corpus
var_textFilePath = "/Users/cstahmer/workspaces/rstudio_workspace/text_mining_with_r/data/ballads"
#var_textFilePath = "/Users/cstahmer/workspaces/rstudio_workspace/text_mining_with_r/data/movie_reviews"

# set path to save outpus
var_out_path = "~/Desktop/textmining_workshop/"

# blacklist words
var_blacklist = c("the", "to", "a", "an")

# lda number of topics
var_number_of_topics <- 50

# lda burnin
var_burnin <- 10

# lda  gibbs sampler iterations
var_gibbs_sampler_iterations <- 100


var_thin <- 50

var_seed <-list(2003,5,63,100001,765)

var_nstart <- 5

var_best <- TRUE

# configure the number of results to 
# view/save for the "top view" outputs
var_top_results_view <- 25


###################################
#      function declarations      #
###################################

# simple function to replace a pattern with a blank space in a string
function_toSpace <- content_transformer(function(x, pattern) { 
  return (gsub(pattern, " ", x))
})

# check all words in a string against
# dictionary and remove all non-word tokens
function_dictionaryClean <- content_transformer(function(x) {
  
  # get a list of all non-word tokens in the blob.  This can take a long
  # time, so the code below includes two lines of code.  To run in production
  # uncomment the line that uses the hunspell library.  To run during testing
  # and learning comment out the hunspell line and uncomment the line that
  # assigns a random list of non-words to the var_nonWord vector.
  #var_nonWords <- hunspell(x, dict = dictionary("en_US"))
  var_nonWords <- c("circle", "like", "another", "new", "ebooks")
  
  # create gsub options
  var_gsubChoices <- paste(var_nonWords, collapse="|")
  
  # return the transformation
  return (gsub(var_gsubChoices, " ", x))
})

###################################
#        Operational Code         #
###################################

# get list of filenames for the corpus files
var_filenames <- list.files(var_textFilePath,pattern="*.txt", full.names=TRUE)

# load the file contents into a list where each
# list item is a character vector containing the 
# complete text from a file in the corpus
var_fileContents <- lapply(var_filenames, readLines, warn=FALSE)

# insure utf8 encoding
var_newfileContents <- lapply(var_fileContents, function(x) enc2utf8(x))

# if desired, remove all non-words
#var_wordFileContents <- lapply(var_fileContents, function_dictionaryClean)

# load the files into a tm Corpus object
obj_corpus <- Corpus(VectorSource(var_newfileContents))

# inspect a particular document in the Corpus
writeLines(as.character(obj_corpus[[3]]))

# normalize case
obj_corpus <-tm_map(obj_corpus,content_transformer(tolower))

# remove potentially problematic symbols
obj_corpus <- tm_map(obj_corpus, function_toSpace, "-")
obj_corpus <- tm_map(obj_corpus, function_toSpace, "’")
obj_corpus <- tm_map(obj_corpus, function_toSpace, "‘")
obj_corpus <- tm_map(obj_corpus, function_toSpace, "•")
obj_corpus <- tm_map(obj_corpus, function_toSpace, "”")
obj_corpus <- tm_map(obj_corpus, function_toSpace, "“")

# remove punctuation
obj_corpus <- tm_map(obj_corpus, removePunctuation)

# remove digits
obj_corpus <- tm_map(obj_corpus, removeNumbers)

# remove stopwords
obj_corpus <- tm_map(obj_corpus, removeWords, stopwords("english"))

# remove non-words if desired
obj_corpus <- tm_map(obj_corpus, function_dictionaryClean)

# remove words from blacklist
obj_corpus <- tm_map(obj_corpus, removeWords, var_blacklist)

# remove whitespace
obj_corpus <- tm_map(obj_corpus, stripWhitespace)

# possibly stem or lemmatize.  Typically lemmatization is the
# best option, but it depends on research problem.
#obj_corpus <- tm_map(obj_corpus, stemDocument)
obj_corpus <- tm_map(obj_corpus, lemmatize_strings)

# create document-term matrix
obj_dtm <- DocumentTermMatrix(obj_corpus)

# convert rownames to filenames
rownames(obj_dtm) <- list.files(var_textFilePath,pattern="*.txt", full.names=FALSE)

#################
# fit the model #
#################

#Run LDA using Gibbs sampling
obj_lda_out <-LDA(obj_dtm, var_number_of_topics, method="Gibbs", control=list(nstart=var_nstart, seed = var_seed, best=var_best, burnin = var_burnin, iter = var_gibbs_sampler_iterations, thin=var_thin))


########################
# save lda fit results #
########################

# get a system time stamp
var_date_time <- as.character(Sys.time())

# format the timestamp to remove spaces
var_datetime_segments <- unlist(strsplit(var_date_time, " "))
var_time_segments <- unlist(strsplit(as.character(var_datetime_segments[2]), ":"))
var_formatted_time <- paste(var_time_segments[1], "h", 
                            var_time_segments[2], "m", 
                            var_time_segments[3], "s",
                            sep="",
                            collapse="")
var_formatted_date_time <- paste(var_datetime_segments[1], "_", var_formatted_time, sep="", collapse="")

# construct a base file output name for writing results
var_output_filename_base <- paste(var_out_path, 
                             "lda_gibbs_k-", var_number_of_topics, 
                             "_i-", var_gibbs_sampler_iterations, 
                             "_burn-", var_burnin,
                             "_", var_formatted_date_time, 
                             sep="", collapse="")

# get ordered list of the most likely topcics per document
obj_lda_out.topics <- as.data.frame(topics(obj_lda_out, var_top_results_view))

# write out ordered list of the most likely topics per document
write.csv(obj_lda_out.topics,file=paste(var_output_filename_base,"_top_topics_per_doc.csv", sep="", collapse=""))

# get ordered list of top words per topic
obj_lda_out.terms <- as.matrix(terms(obj_lda_out, var_top_results_view))

# write out ordered list of top words per topic
write.csv(obj_lda_out.terms,file=paste(var_output_filename_base, "_top_words_per_topic.csv", sep="", collapse=""))

# get probabilities associated with each topic assignment
var_topic_probabilities <- as.data.frame(obj_lda_out@gamma)

# write out probabilities associated with each topic assignment
write.csv(var_topic_probabilities,file=paste(var_output_filename_base, "_doc_topics.csv", sep="", collapse=""))

# get probabilities associated with each word to topic assignment
var_topic_word_probabilities <- as.data.frame(obj_lda_out@beta)

# set the rownames to the actual words
colnames(var_topic_word_probabilities) <- as.vector(obj_lda_out@terms)

# write out probabilities associated with each word to topic assignment
write.csv(var_topic_word_probabilities,file=paste(var_output_filename_base, "_topics_words.csv", sep="", collapse=""))


