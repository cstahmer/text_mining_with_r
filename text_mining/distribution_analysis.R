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

###################################
#         configuration           #
###################################

# set working directory
setwd("~/Documents/rstudio_workspace/digitalmethods/text_mining/")

# identify the text file to analyze
var_textFile_character = "data/plainText/melville.txt"

# define a word of interest that you want to plot
var_wordofinterest_character = "whale"

###################################
#        Operational Code         #
###################################

# load the text
var_textLines_vector <- scan(var_textFile_character, what="character", sep="\n")

# find the location in the vector where the first chapter heading is
var_startElement_integer <- which(var_textLines_vector == "CHAPTER 1. Loomings.")

# find the locatin in the vector where the last word of the text is
var_endElement_integer <- which(var_textLines_vector == "orphan.")

# subset the lines (vector elements) between the start element and 
# the end element into new vector
var_subsettedTextLines_vector <-  var_textLines_vector[var_startElement_integer:var_endElement_integer]

# collapse the vector elements in var_subsettedTextLines_vector
# into a single text blog.  Put spaces between the elements
# when they are collapsed.
var_textBlob_character <- paste(var_subsettedTextLines_vector, collapse=" ")

# convert to lower case
var_lowerCaseBlob_vector <- tolower(var_textBlob_character)

# make a list of words 
var_textWords_list <- strsplit(var_lowerCaseBlob_vector, "\\W")

# convert list to vector
var_textWords_vector <- unlist(var_textWords_list)

# get the elements that are not blank
var_nonBlankElements_vector  <-  which(var_textWords_vector!="")

# re-define the word vector using only 
# those elements that aren't blank
var_textWords_vector <-  var_textWords_vector[var_nonBlankElements_vector]

# dispersion plot is calculated on a measure of time
# where each word in the sequence of words in the novel
# represents a beat of the metronome of novelistic time

# generate sequence of beats as list of numbered beats
# this will serve as the x axis (time axis) of our
# dispersion plot
var_spaceTime_vector <- seq(1:length(var_textWords_vector))

# calculate the location of every instance of the word
# of interest defined above in the word vector
var_wordLocations_vector <- which(var_textWords_vector == var_wordofinterest_character)

# y axis must now be constructed as scale of TRUE/FALSE
# values for the presence of the wordofinterest. R has 
# a special variable valuable of NA (not available) which 
# indicates that a condition is not met in a particular
# element of an index or matrix.  Here we create an 
# y-axis vector that is the same length as the x-axis
# vector and fill it with NAs
var_yAxis_vector <- rep(NA,length(var_spaceTime_vector))

# now reset the NAs in the y-axis to 1 (TRUE) every place 
# where the wordofinterest appears
var_yAxis_vector[var_wordLocations_vector] <- 1

# now generate the plot
plot(var_yAxis_vector, main="Dispersion Plot of `whale' in Moby Dick",
     xlab="Novel Time - Words Location in the Novel", ylab="whale", type="h", ylim=c(0,1), yaxt='n')











