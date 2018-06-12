# Script: word_frequency.R
# A script written and distributed as a teaching
# aid for demonstrating how to perform word
# frequency analysis.  
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
# (2) In order to facilitate distinguishing between
# variables, functions, and objects all objects in 
# the code begin with the "obj_" prefix.
#
# (3) Locally defined functions begin with the 
# function_ prefix
#
# Copyright Carl G. Stahmer - 2018
# Director of Data and Digital Scholarship - UC Davis Library
# Associate Director for Humanities - UC Davis Data Science Initiative
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

# install.packages("NLP")
# install.packages("tm")
# install.packages("hunspell")
# install.packages("zipfR")

library(NLP)
library(tm)
library(hunspell)
library(zipfR)

###################################
#         configuration           #
###################################

# identify the text file to analyze
var_textFile = "/Users/cstahmer/workspaces/rstudio_workspace/text_mining_with_r/data/cleanText/melville.txt"

# identify any words you want removed from
# the corpus prior to analysis
var_word_blacklist = c("orphan", 
                       "children",
                       "search",
                       "missing")

# identify a file location to save frequency
# analysis
var_outputFile <-"/Users/cstahmer/Desktop/textmining_workshop/word_freq.csv"

###################################
#        Operational Code         #
###################################

# load the file to be analized into a character vector.
# The resulting vector will have as many elements as
# lines in the file with the contents of each line
# contained in a character vector.
var_textLines <- readLines(var_textFile)

# collapse the vector of lines into a single element
var_textBlob <- paste(var_textLines, collapse = " ")

# change to lowercase
var_textBlob_lowercase <- tolower(var_textBlob)

# remove stopwords
var_textBlob_unStopped <- removeWords(var_textBlob_lowercase, stopwords('english'))

# use the below line to remove any other tokens based on blacklist
var_textBlob_blacklisted <- removeWords(var_textBlob_unStopped, var_word_blacklist)

# remove punctuation
var_textBlob_noPunctuation <- removePunctuation(var_textBlob_blacklisted)

# remove numeric elements
var_textBlob_nonNumeric <- gsub("\\d+", "", var_textBlob_noPunctuation)

# collapse multiple spaces
var_textBlob_collapseWhitespace <- gsub("\\s+", " ", var_textBlob_nonNumeric)

# trim leading and trailing spaces
var_textBlob_trimmed <- trimws(var_textBlob_collapseWhitespace)

# convert blob to list of words
var_wordlist = unlist(strsplit(var_textBlob_trimmed, split = ' '))

# get a list of all non-word tokens in the blob.  This can taka a long
# time, so the code below includes two lines of code.  To run in production
# uncomment the line that uses the hunspell library.  To run during testing
# and learning comment out the hunspell line and uncomment the line that
# assigns a random list of non-words to the var_nonWord vector.
#var_nonWords <- hunspell(var_textBlob_trimmed, dict = dictionary("en_US"))
var_nonWords <- c("circle", "like", "another", "ixion")

# get a list of all elements in the list of words that
# aren't a real word
var_nonwordList <- match(var_wordlist, var_nonWords)

# subset the original list of words to include only those
# words that don't match the non-word list
var_wordList_cleaned <- var_wordlist[is.na(var_nonwordList)]

# collapse the vector back into a text blob
var_texBlob_cleaned <- paste(var_wordList_cleaned, collapse=" ")

########################################################
# At this point you have a nicely cleaned text blob    # 
# is all lower case, has stop words, stand-alone       #
# digits, a user-defined list of other words,          #
# and any non-word (as compared to dictionary) tokens, #
# and any extra spaces removed.                        #
########################################################

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

# load a tm corpus to plot zipf's curve for the entire corpus
obj_docs <- VCorpus(VectorSource(c(var_texBlob_cleaned)))
obj_docTermMatrix <- DocumentTermMatrix(obj_docs)
Zipf_plot(obj_docTermMatrix)

# identify the word of interest
var_wordOfInterest = "whale"

# calcuate how many of the totals words are
# the word of interest
var_seedHits <- length(var_wordList_cleaned[which(var_wordList_cleaned == var_wordOfInterest)])

# calcuate the percentage frequency of the
# word of interest
var_wiPercent <- 100 * (var_seedHits / var_totalWords)

print(paste("The word ", var_wordOfInterest, " comprises ", var_wiPercent, " of the text."))

# save the results to a csv file
write.csv(obj_sortedFrequencies, file = var_outputFile)

