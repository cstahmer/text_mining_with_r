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
# install.packages("ldatuning")
# install.packages("topicmodels")

# Load Required Libraries
library("NLP")
library("tm")
library("hunspell")
library("sylly")
library("sylly.en")
library("koRpus")
library("koRpus.lang.en")
library("textstem")
library("ldatuning")
library("topicmodels")

###################################
#         configuration           #
###################################

# set the path to corpus
var_textFilePath = "/Users/cstahmer/workspaces/rstudio_workspace/text_mining_with_r/data/movie_reviews/"

# set path to save output
var_outPath = "~/Desktop/textmining_workshop/"

# blacklist words
var_blacklist = c("httpwwwgutenbergorg", "sletter")

# number of cores to use in processing
var_cores = 2	

# number of topics to begin testing
var_start_topic_number = 20	

# number of topics to end testing
var_end_topic_number = 300	

# N=number of topics by which to increment tests
var_test_by = 5		# Number of topics to increment by

# define random seed for consistency
var_lda_control = list(seed = 77)

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

function_console_log = function(msg, indent = 0) {
  # Update console with output status
  
  # Indent by pasting tabs to start of message after Sys.time()
  tab = ifelse(indent==0, " ", paste0(rep("\t", indent), collapse = ""))
  
  # Paste full message together and print
  full_msg = sprintf("%s%s%s\n", Sys.time(), tab, msg)
  cat(full_msg)
}

###################################
#        Operational Code         #
###################################

# output start messages
function_console_log(paste("%s Creating DTM..."))
function_console_log(paste("%s Creating DTM...", Sys.time()), indent = 1)

# get list of filenames for the corpus files
var_filenames <- list.files(var_textFilePath,pattern="*.txt", full.names=TRUE)

# load the file contents into a list where each
# list item is a character vector containing the 
# complete text from a file in the corpus
var_fileContents <- lapply(var_filenames, readLines, warn=FALSE)

# if desired, remove all non-words
#var_wordFileContents <- lapply(var_fileContents, function_dictionaryClean)

# load the files into a tm Corpus object
obj_corpus <- Corpus(VectorSource(var_fileContents))

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

# output a console message
function_console_log("Finished creation of DTM", indent = 1)

# Run LDA tuning, use 4 methods to fit model
function_console_log("Running ldatuning...", indent = 1)
obj_result =
	ldatuning::FindTopicsNumber(
		dtm = obj_dtm,
		topics = seq(var_start_topic_number, var_end_topic_number, var_test_by),
		metrics = c('Griffiths2004', 'CaoJuan2009',
					'Arun2010', 'Deveaud2014'),
		control = var_lda_control,
		mc.cores = var_cores,
		verbose = TRUE)

function_console_log("DONE", indent = 1)


# Save as RDS object 
function_console_log("Saving as RDS object...", indent = 1)
var_formatted_datetime = gsub("-|:", "", Sys.time())
var_formatted_datetime = gsub(" ", "_", var_formatted_datetime)
var_outfile <- paste(var_outPath, "lda_tuning_result_", var_formatted_datetime, "_", var_start_topic_number, "-", var_end_topic_number, "_", var_test_by, ".rds" , sep="", collapse=NULL)
saveRDS(obj_result, file = var_outfile)
function_console_log("DONE", indent = 1)


# End
function_console_log("PROGRAM COMPLETED")

# plot the results
ldatuning::FindTopicsNumber_plot(obj_result)

