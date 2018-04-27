# Script: sentiment_analysis.R
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

library(sentimentr)

###################################
#         configuration           #
###################################

# identify the text file to analyze
var_textFile = "/Users/cstahmer/workspaces/rstudio_workspace/text_mining_with_r/data/plainText/emerson.txt"

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

# parse by sentence to get individual sentence scores
var_sentences <- get_sentences(var_textBlob)

# calculate sentiment table
obj_sentiment_table <- sentiment(var_sentences)

# plot the sentiment as we move through the text
plot(obj_sentiment_table)

###################################################
# calculate grouped sentiment scores for          #
# speakers from the 2012 Presidential debates.    #
# Note that the presidential_debates_2012         #
# dataset is included as part of the sentimentr   #
# package, so we don't have to load it from disk. #
###################################################

# take a lookt at the structure of presidential_debates_2012
str(presidential_debates_2012)

# run a nexted set of commands that tell the system
# use presidential_debates_2012 as its dataset:
#      with(presidential_debates_2012)
# get sentiment group by person->time->sentence
var_sentiment_table <- with(
  presidential_debates_2012, 
  sentiment_by(
    get_sentences(dialogue), 
    list(person, time)
  )
)

# plot the sentence sentiments
plot(var_sentiment_table)
