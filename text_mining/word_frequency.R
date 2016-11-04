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
#install.packages("tm")

library(tm)

###################################
#         configuration           #
###################################

# set working directory
setwd("~/Documents/rstudio_workspace/digitalmethods/text_mining/")

# identify the text file to analyze
var_textFile_character = "data/cleanText/melville.txt"

# identify the word of interest
var_WordOfInterest_character = "whale"

# identify a file location to save frequency
# analysis
var_outputFile_character <-"/Users/cstahmer/textmining/word_freq2.csv"

###################################
#        Operational Code         #
###################################

# load the file to be analized into a character vector.
# The resulting vector will have as many elements as
# lines in the file with the contents of each line
# contained in a character vector.
var_textLines_vector <- readLines(var_textFile_character)

# collapse the vector of lines into a single element
var_textBlob_character <- paste(var_textLines_vector, collapse = " ")

# change to lowercase
var_lowercase_vector <- tolower(var_textBlob_character)

# remove stopwords
var_unStopped_vector <- removeWords(var_lowercase_vector, stopwords('english'))

# use the below line to remove any other tokens
var_unStopped_vector <- removeWords(var_unStopped_vector, "s")

# convert to a list of words
var_words_list <- strsplit(var_unStopped_vector, "\\W")

# convert back into a vector
var_text_word_vector <- unlist(var_words_list)

# find any elements that are blank
var_notBlank_vector  <-  which(var_text_word_vector!="")

# create a new vector tha thas no empty elements
var_cleanedWordList_vector <-  var_text_word_vector[var_notBlank_vector]

# calcuate how many of the totals words are
# the wor of interest
var_seedHits_integer <- length(var_cleanedWordList_vector[which(var_cleanedWordList_vector==var_WordOfInterest_character)])

# calculate total word count
var_totalWords_integer <- length(var_cleanedWordList_vector)

# calcuate the percentage frequency of the
# word of interest
var_wiPercent_integer <- 100 * (var_seedHits_integer / var_totalWords_integer)

print(paste("The word ", var_WordOfInterest_character, " comprises ", var_wiPercent_integer, " of the text."))

# setup a frequency table
obj_frequencies_table <- table(var_cleanedWordList_vector)

# sort the frequency table
obj_sortedFrequencies_table <- sort(obj_frequencies_table , decreasing=T)

# convert to relative frequencies
obj_sortedFrequenciesRel_table <- 100*(obj_sortedFrequencies_table/sum(obj_sortedFrequencies_table))

# run the command below in your console to find the 
# relative frequency of any word
# obj_sortedFrequenciesRel_table["the"]

print(obj_sortedFrequenciesRel_table[1:50])

# plot the word frequencies
var_numToPlot_integer <- 10
plot(obj_sortedFrequenciesRel_table[1:10], main="Relative Word Frequency", type="b", xlab="Top Ten Words", ylab="Percentage of Full Text", xaxt ="n") 
axis(1, 1:var_numToPlot_integer, labels=names(obj_sortedFrequenciesRel_table [1:var_numToPlot_integer]))

# save the results to a csv file
write.csv(obj_sortedFrequencies_table, file = var_outputFile_character)



