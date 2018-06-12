# Script: kwic_2.R
# 
# A script written and distributed as a teaching
# aid for demonstrating how to perform corpus
# stemming and analysis in R.  The script loads files from 
# a designated directory into a working corpus and performs 
# keyword in context (KWIC) analysis across the corpus based
# on a configured word of interest.
#
# Because the code is designed for teaching, it
# aims for step by step clarity rather than code
# efficiency.  Experienced programmers will see
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
# (2) In order to facilitate distinguishing a
# a variable's type or class, all variables are
# names using a _suffix that identifies the 
# variable type.
#
# (3) In order to facilitate distinguishing between
# variables, functions, and objects all objects in 
# the code begin with the "obj_" prefix.
#
# (4) Locally defined functions begin with the 
# function_ prefix
#
# Copyright Carl G. Stahmer - 2016
# Director of Digital Scholarship - UC Davis Library
# Associate Director for Humanities - Data Science Initiative
# Associate Director - English Broadside Ballad Archive
#
# Portions of this code are based on Matt Jockers'
# Introduction to text analysis with R:
#
# Jockers, M. (2014). 
# _Text Analysis with R for Students of Literature_
# Quantitative Methods in the Humanities and Social …. 
# doi:10.1007/978-3-319-03164-4
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
# install.packages("quanteda")


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
library(quanteda)


###################################
#         configuration           #
###################################


# set the path to corpus
var_textFilePath = "/Users/cstahmer/workspaces/rstudio_workspace/text_mining_with_r/data/plainText"
#var_textFilePath = "/Users/cstahmer/workspaces/rstudio_workspace/text_mining_with_r/data/movie_reviews"

# set path to save outpus
var_out_path = "~/Desktop/textmining_workshop/"

# blacklist words
var_blacklist = c("httpwwwgutenbergorg", "sletter")

# Define a word of interest that you want to plot.
var_wordofinterest_str = "whale"

# Define the range before and after that you want to examine.
var_range_int = 3

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

# if desired, remove all non-words
#var_wordFileContents <- lapply(var_fileContents, function_dictionaryClean)

# load the files into a tm Corpus object
obj_corpus <- VCorpus(VectorSource(var_fileContents))

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

# load a quanteda corpus
obj_quantCorp <- corpus(obj_corpus)

# make a kwik object
obj_thisquick <- kwic(quantCorp, var_wordofinterest_str, window = var_range_int, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE)

# combine the before and after word lists into a single list
var_worlists <- c(obj_thisquick[,4], obj_thisquick[,6])

# collapse into a single blob of text
var_text_blob <- paste(var_worlists, collapse = ' ')

# convert blob to list of words
var_wordList_cleaned = unlist(strsplit(var_text_blob, split = ' '))

# calculate total word count
var_totalWords <- length(var_wordList_cleaned)

# setup a frequency table
obj_frequencies <- table(var_wordList_cleaned)

# sort the frequency table
obj_sortedFrequencies <- sort(obj_frequencies , decreasing=T)

# convert to relative frequencies
obj_sortedRelativeFrequencies <- 100*(obj_sortedFrequencies/var_totalWords)

# run the command below in your console to find the 
# relative frequency of any word
# obj_sortedFrequenciesRel_table["the"]

# plot the actual word frequencies
var_numToPlot_integer <- 50
plot(obj_sortedFrequencies[1:var_numToPlot_integer], main="Word Frequency", type="b", xlab="Top Words", ylab="Frequency", xaxt ="n") 
axis(1, 1:var_numToPlot_integer, labels=names(obj_sortedFrequencies [1:var_numToPlot_integer]))

# plot relative word frequencies
var_numToPlot_integer <- 50
plot(obj_sortedRelativeFrequencies[1:var_numToPlot_integer], main="Relative Word Frequency", type="b", xlab="Top Words", ylab="number of occurrances / total words", xaxt ="n") 
axis(1, 1:var_numToPlot_integer, labels=names(obj_sortedRelativeFrequencies [1:var_numToPlot_integer]))

# save the results to a csv file
write.csv(obj_sortedFrequencies, file = var_outputFile)

# save the results to a csv file
write.csv(obj_sortedRelativeFrequencies, file = var_outputFile)