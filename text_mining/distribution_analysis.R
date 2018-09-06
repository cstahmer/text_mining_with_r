# Script: distribution_analysis.R
# 
# A script written and distributed as a teaching
# aid for demonstrating how to perform distribution
# analysis in R.  
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
#
# This work is licensed under a Creative Commons 
# Attribution-ShareAlike 4.0 International License.
#
# see http://creativecommons.org/licenses/by-sa/4.0/

library(NLP)
library(tm)

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

# identify the word of interest
var_wordOfInterest = "whale"

###################################
#        Operational Code         #
###################################

# load the file to be analized into a character vector.
# The resulting vector will have as many elements as
# lines in the file with the contents of each line
# contained in a character vector.
var_textLines <- readLines(var_textFile, warn=FALSE)

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


# dispersion plot is calculated on a measure of time
# where each word in the sequence of words in the novel
# represents a beat of the metronome of novelistic time

# generate sequence of beats as list of numbered beats
# this will serve as the x axis (time axis) of our
# dispersion plot
var_spaceTime_vector <- seq(1:length(var_wordList_cleaned))

# calculate the location of every instance of the word
# of interest defined above in the word vector
var_wordLocations <- which(var_wordList_cleaned == var_wordOfInterest)

# y axis must now be constructed as scale of TRUE/FALSE
# values for the presence of the wordofinterest. Create a 
# y-axis vector that is the same length as the x-axis
# vector and fill it with NAs
var_yAxis_vector <- rep(NA,length(var_spaceTime_vector))

# now reset the NAs in the y-axis to 1 (TRUE) every place 
# where the wordofinterest appears
var_yAxis_vector[var_wordLocations] <- 1

# now generate the plot
plot_title <- paste("Dispersion Plot of token '", var_wordOfInterest, "'", sep="")
plot(var_yAxis_vector, main=plot_title,
     xlab="Novel Time - Words Location in Text", ylab=var_wordOfInterest, type="h", ylim=c(0,1), yaxt='n')


