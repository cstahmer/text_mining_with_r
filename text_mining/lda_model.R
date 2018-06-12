# Script: lda_model.R
# 
# A script written and distributed as a teaching
# aid for demonstrating how to perform Latent
# Dirichlet Allocation (LDA) Topic Modeling of
# a text cropus. The code presents a visualization
# in the form of an interactive, html page that
# allows users to examine the fit of an LDA model.
# Comparing the visualizations of multiple models
# fit with different parameters allows users to
# find the appropriate model fit. An example of the
# scripts output can be found at:
#
# http://digitalscholarslab.org/ldavis_example
#
# While this script uses the parallels package
# to improve perforance, where it is possible,
# Experienced programmers will see many ways that 
# this code could be made more efficient and 
# elegant in terms of both processing speed and 
# memory management.  The code as presented is 
# designed to allow a novice coder to follow the 
# logic of the script as intuitively as possible. 
# With this in mind, the following conventions 
# are used throughout the code:
#
# (1) In order to facilitate distinguishing between
# variables, functions, and objects all variables in 
# the code begin with the "var_" prefix.
#
# (2) In order to facilitate distinguishing between
# variables, functions, and objects, all objects in 
# the code begin with the "obj_" prefix.  In this 
# context an object is any data representation or 
# collection that is in a non-standard data format. 
# For example, the tm package creates their own 
# special class of simple triplex matrix called a 
# Corpus.
#
# (3) Locally defined functions begin with the 
# function_ prefix
#
# Copyright Carl G. Stahmer - 2018
# Director of Data and Digital Scholarship - UC Davis Library
# Associate Director for Humanities - UC Davis Data Science Initiative
# Associate Director - English Broadside Ballad Archive
#
# This work is licensed under a Creative Commons 
# Attribution-ShareAlike 4.0 International License.
#
# see http://creativecommons.org/licenses/by-sa/4.0/

# execute the blow lines if your R environment does
# not already include the required packages.  Note
# that this list includes all package referenced in
# this script as well as their dependencies.
# install.packages("parallel")
# install.packages("NLP")
# install.packages("tm")
# install.packages("hunspell")
# install.packages("sylly")
# install.packages("sylly.en")
# install.packages("data.table")
# install.packages("koRpus")
# install.packages("koRpus.lang.en")
<<<<<<< HEAD
# install.packages("colorspace")
# install.packages("utf8")
# install.packages("RColorBrewer")
# install.packages("dichromat")
# install.packages("munsell")
# install.packages("labeling")
# install.packages("viridisLite")
# install.packages("bindr")
# install.packages("assertthat")
# install.packages("crayon")
# install.packages("cli")
# install.packages("pillar")
# install.packages("tibble")
# install.packages("purrr")
# install.packages("zoo")
# install.packages("dtt")
# install.packages("bindrcpp")
# install.packages("pkgconfig")
# install.packages("plogr")
# install.packages("dplyr")
# install.packages("glue")
# install.packages("stringi")
# install.packages("tidyselect")
# install.packages("tidyr")
# install.packages("extrafontdb")
# install.packages("Rttf2pt1")
# install.packages("statnet.common")
# install.packages("plyr")
# install.packages("scales")
# install.packages("RcppEigen")
# install.packages("gtable")
# install.packages("stringr")
# install.packages("reshape2")
# install.packages("lazyeval")
# install.packages("reticulate")
# install.packages("ISOcodes")
# install.packages("assertthat")
# install.packages("bindrcpp")
# install.packages("textshape")
# install.packages("syuzhet")
# install.packages("extrafont")
# install.packages("network")
# install.packages("sna")
# install.packages("ggplot2")
# install.packages("ggrepel")
# install.packages("RcppParallel")
# install.packages("RSpectra")
# install.packages("fastmatch")
# install.packages("XML")
# install.packages("yaml")
# install.packages("lubridate")
# install.packages("spacyr")
# install.packages("stopwords")
# install.packages("RcppArmadillo")
# install.packages("english")
# install.packages("mgsub")
# install.packages("qdapRegex")
# install.packages("lexicon")
# install.packages("SnowballC")
# install.packages("quanteda")
# install.packages("textclean")
# install.packages("textstem")
# install.packages("lda")
# install.packages("LDAvis")
# install.packages("servr")
# install.packages("jsonlite")

# Load Required Libraries
library(parallel)
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
library(LDAvis)
library(servr)

###################################
#         configuration           #
###################################

# set the path to corpus
var_textFilePath = "/Users/cstahmer/workspaces/rstudio_workspace/text_mining_with_r/data/movie_reviews"

# set path to save the html outpus
var_out_path <- "~/Desktop/"

# blacklist words
var_blacklist = c("httpwwwgutenbergorg", "sletter")

# number of topics
var_number_of_topics <- 20

# number of iterations of Gibs sampler
var_gibbs_sampler_iterations <- 5000

# doc-topic distribution. 
# number between 0 and 1.
# lower is more diffuse.
var_prior_alpha <- 0.5

# topic-term distribution.
# number between 0 and 1.
# lower is more diffuse.
var_prior_eta <- 0.5

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

# set TRUE/FALSE wheter you want to automatically open
# a browser and show viz in addition to saving it to disk.
var_open_browser <- TRUE

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
  for (i in 1:var_num_document_rows) {
    var_counts_datframe <- data.frame(x[[i]])
  }
  
  return(var_counts_datframe)
}

# create a dtm as list as needed by LDA package.
function_dtm_to_list <- function(var_obj_dtm, var_filelist) {
  var_document_list <- list()
  for (i in 1: length(var_filelist)) {
    var_file <- var_filelist[i]
    var_tokenlist <- unlist(strsplit(var_obj_dtm[i]$content, split = ' '))
    var_document_list[[var_file]] <- var_tokenlist
  }
  return(var_document_list)
}

# convert a document list to the data format required for
# the lda package to process
function_make_lda_required_structure <- function(var_document_list, var_match_vocab) {
  var_index <- match(var_document_list, var_match_vocab)
  var_index <- var_index[!is.na(var_index)]
  rbind(as.integer(var_index - 1), as.integer(rep(1, length(var_index))))
}

###################################
#        Operational Code         #
###################################

# calculate the number of cores available
# on your system to run this process
var_num_cores <- detectCores() - 1

# setup a parallel cluster so that we can run lapply in
# parallel using parLapply to speed things up when we can.
# To use all cores on your system, use the caclculated
# number of cores from above (stored in var_num_cores)
# If you're on a shared system, be polite and leave
# some cores for others on the system
var_parallel_cluster <- makeCluster(var_num_cores)

# get list of filenames for the corpus files
var_filenames <- list.files(var_textFilePath,pattern="*.txt", full.names=TRUE)

# load the file contents into a list where each
# list item is a character vector containing the 
# complete text from a file in the corpus
var_fileContents <- parLapply(var_parallel_cluster, var_filenames, readLines, warn=FALSE)

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
rownames(obj_dtm) <- var_filenames

# get a list of unique words in the corpus (the vocabulary)
var_vocabulary <- obj_dtm$dimnames$Terms

# convert the cleaned corpus into a list object.  This is necessary
# because the tm::corpus data type is a unique data strcuture that
# can't be directly acted upon using R native techiques
var_documents <- function_dtm_to_list(obj_corpus, var_filenames) # corpus documents as simple list

# here we setup a special structured list item that is structured
# specfically for LDAViz, which needs the data formatted a particular way
var_lda_document_List <- parLapply(var_parallel_cluster, var_documents, function_make_lda_required_structure, var_vocabulary) 

###############
# run the LDA #
###############

# get the start system time
var_sys_time_start_lda <- Sys.time()

# fit the model
obj_fitted_lda_model <- lda.collapsed.gibbs.sampler(documents = var_lda_document_List, 
                                                    K = var_number_of_topics, 
                                                    vocab = var_vocabulary, 
                                                    num.iterations = var_gibbs_sampler_iterations, 
                                                    alpha = var_prior_alpha, 
                                                    eta = var_prior_eta, 
                                                    initial = var_initial_assignments, 
                                                    burnin = var_burnin,
                                                    compute.log.likelihood = TRUE)

# get end system time
var_sys_time_end_lda <- Sys.time()

##############
# run LDAvis #
##############

# calculate a vector containing the total token/word count for each document
var_document_token_counts <- function_token_count(obj_corpus)  

# calculate a vector containing the total freqeuncy count for every word in the vocabulary
var_corpus_word_frequencies <- as.integer(colSums(as.matrix(obj_dtm))) 

# calculate a matrix from the LDA output (structured as required by ldavis package), 
# with each row containing the probability distribution over topics for a document, 
# with as many rows as there are documents in the corpus, and as many columns as 
# there are topics in the model. In the Blei LDA article this 
# coincides with the M replicate of the LDA plate model, or the matrix
# Theta which is the collection of topic vectors.
var_theta <- t(apply(obj_fitted_lda_model$document_sums + var_prior_alpha, 2, function(x) x/sum(x)))

# construct a matrix from the LDA output  (structured as required by ldavis
# package), where each row contains the distribution over terms for a topic, 
# with as many rows as there are topics in the model, and as many columns as 
# there are terms in the vocabulary.  In the Blei LDA article this 
# coincides with the N replicate of the LDA plate model, or the matrix
# Z which is the collection of topic vectors.
var_phi <- t(apply(t(obj_fitted_lda_model$topics) + var_prior_eta, 2, function(x) x/sum(x)))

# call the LDAvis createJSON functio to create a JSON data
# representation that is needed for the visualization package
obj_json <- createJSON(
  phi = var_phi, 
  theta = var_theta, 
  doc.length = var_document_token_counts, 
  vocab = var_vocabulary, 
  term.frequency = var_corpus_word_frequencies,
  cluster = var_parallel_cluster
)

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

# construct an output directory name 
# where the viz will be written
var_output_filename <- paste(var_out_path, 
                             "ldavis_k-", var_number_of_topics, 
                             "_i-", var_gibbs_sampler_iterations, 
                             "_a-", var_prior_alpha,
                             "_b-",var_prior_eta, 
                             "_burn-", var_burnin,
                             "_", var_formatted_date_time, 
                             sep="", collapse="")

# create the viz files and save to disk
serVis(obj_json, out.dir = var_output_filename, open.browser = var_open_browser)
