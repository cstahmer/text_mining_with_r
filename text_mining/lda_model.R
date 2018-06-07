# Script: lda_models.R
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
# install.packages("data.table")
# install.packages("koRpus")
# install.packages("koRpus.lang.en")

install.packages("colorspace")
install.packages("utf8")
install.packages("RColorBrewer")
install.packages("dichromat")
install.packages("munsell")
install.packages("labeling")
install.packages("viridisLite")
install.packages("bindr")
install.packages("assertthat")
install.packages("crayon")
install.packages("cli")
install.packages("pillar")
install.packages("tibble")
install.packages("purrr")
install.packages("zoo")
install.packages("dtt")
install.packages("bindrcpp")
install.packages("pkgconfig")
install.packages("plogr")
install.packages("dplyr")
install.packages("glue")
install.packages("stringi")
install.packages("tidyselect")
install.packages("tidyr")
install.packages("extrafontdb")
install.packages("Rttf2pt1")
install.packages("statnet.common")
install.packages("plyr")
install.packages("scales")
install.packages("RcppEigen")
install.packages("gtable")
install.packages("stringr")
install.packages("reshape2")
install.packages("lazyeval")
install.packages("reticulate")
install.packages("ISOcodes")
install.packages("assertthat")
install.packages("bindrcpp")
install.packages("textshape")
install.packages("syuzhet")
install.packages("extrafont")
install.packages("network")
install.packages("sna")
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("RcppParallel")
install.packages("RSpectra")
install.packages("fastmatch")
install.packages("XML")
install.packages("yaml")
install.packages("lubridate")
install.packages("spacyr")
install.packages("stopwords")
install.packages("RcppArmadillo")
install.packages("english")
install.packages("mgsub")
install.packages("qdapRegex")
install.packages("lexicon")
install.packages("SnowballC")
install.packages("quanteda")
install.packages("textclean")
# install.packages("textstem")
# install.packages("lda")
# devtools::install_github("cpsievert/LDAvisData")

# Load Required Libraries
library(NLP)
library(tm)
library(hunspell)
library(sylly)
library(sylly.en)
library(data.table)
library(koRpus)
library(koRpus.lang.en)
library(textstem)
library(lda)

###################################
#         configuration           #
###################################

# set the path to corpus
var_textFilePath = "/Users/cstahmer/workspaces/rstudio_workspace/text_mining_with_r/data/plainText/"
#var_textFilePath = "/Users/cstahmer/workspaces/rstudio_workspace/text_mining_with_r/data/movie_reviews/"

# set path to save outpus
var_outPath = "~/Desktop/textmining_workshop/"

# blacklist words
var_blacklist = c("httpwwwgutenbergorg", "sletter")

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

# check all words in a string against
# dictionary and remove all non-word tokens
function_token_count <- function(x) {
  var_return_counts <- c()
  for (i in 1:length(x)) {
    var_corpus_item <- unlist(strsplit(x[i]$content, " "))
    var_return_counts[i] <- length(var_corpus_item)
  }
  return(var_return_counts)
}

# get the total term frequencies across the entire corpus
# from a tm Term Document Matrix (x)
function_get_total_frequencies <- function(x) {
  var_num_document_rows <- x$nrow
  var_num_terms <- x$ncol
  #var_counts_datframe <- data.frame(matrix(ncol = var_num_terms, nrow = 0))
  
  for (i in 1:var_num_document_rows) {
    var_counts_datframe <- data.frame(x[[i]])
    #var_counts_datframe <- rbind(var_counts_datframe, var_local_counts_datframe)
    #print(x[[i]])
  }
  
  return(var_counts_datframe)
}

# get a vector of documents
function_corpus_vector <- function(x) {
  var_return_vector <- c()
  for (i in 1:length(x)) {
    var_return_vector[i] <- x[i]$content
  }
  return(var_return_vector)
}

###################################
#        Operational Code         #
###################################

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

# inspect a particular document in the Corpus
writeLines(as.character(obj_corpus[[1]]))

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

# setup some variables for the LDA parameters
var_number_of_documents <- nrow(obj_dtm) # number of documents
var_number_of_words <- ncol(obj_dtm) # number of unique words in the vocabulary
var_document_token_counts <- function_token_count(obj_corpus)  # number of tokens per document [312, 288, 170, 436, 291, ...]
var_corpus_token_count <- sum(var_document_token_counts) # total number of tokens in the data (546,827)
var_documents <- as.list(function_corpus_vector(obj_corpus)) # corpus documents as vector
var_vocabulary <- obj_dtm$dimnames$Terms

var_corpus_word_frequencies <- function_get_total_frequencies(obj_dtm)



##########################
# set the LDA parameters #
# and fit the model      #
##########################

# number of topics
var_number_of_topics <- 20

# number of iterations of Gibs sampler
var_gibbs_sampler_iterations <- 5000

# doc-topic distribution. 
# number between 0 and 1.
# lower is more diffuse.
var_prior_alpha <- 0.02

# topic-term distribution.
# number between 0 and 1.
# lower is more diffuse.
var_prior_eta <- 0.2

# a list of initial topic assignments 
# for words. It should be in the same 
# format as the assignments field of 
# the return value. If this field is 
# NULL, then the sampler will be 
# initialized with random assignments.
var_initial_assignments <- NULL

# A scalar integer indicating the 
# number of Gibbs sweeps to consider 
# as burn-in (i.e., throw away). Note 
# that burnin iterations do NOT count 
# towards num.iterations
var_burnin = 0

# use the r set.seed function to
# set the random number generator seed
# to allow more consistent results on
# repated runs. This isn't required
# in production
set.seed(357)

# get the start system time
var_sys_time_start <- Sys.time()

# fit the model
obj_fitted_lda_model <- lda.collapsed.gibbs.sampler(documents = var_documents, 
                                                    K = var_number_of_topics, 
                                                    vocab = var_vocabulary, 
                                                    num.iterations = var_gibbs_sampler_iterations, 
                                                    alpha = var_prior_alpha, 
                                                    eta = var_prior_eta, 
                                                    initial = var_initial_assignments, 
                                                    burnin = var_burnin,
                                                    compute.log.likelihood = TRUE)
