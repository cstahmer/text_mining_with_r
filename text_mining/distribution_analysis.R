# Script: distribution_analysis.R
# 
# A script written and distributed as a teaching
# aid for demonstrating how to perform corpus
# stemming in R.  The script reads a single file
# chunks and annotates sentences and words.
#
# Copyright Carl G Stahmer
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
input.file = "data/plainText/melville.txt"

# define a word of interest that you want to plot
wordofinterest.str = "whale"

###################################
#        Operational Code         #
###################################

# load the text
text.v <- scan(input.file, what="character", sep="\n")

# find the location in the vector where the first chapter heading is
start.e <- which(text.v == "CHAPTER 1. Loomings.")

# find the locatin in the vector where the last word of the text is
end.e <- which(text.v == "orphan.")

# load the lines between the start element and the end element
novel.lines.v <-  text.v[start.e:end.e]

# collapse the vector slices in lines by putting a space in between
novel.v <- paste(novel.lines.v, collapse=" ")

# convert to lower case
novel.lower.v <- tolower(novel.v)

# make a list of words (recursive - lists can contain lists)
moby.words.l <- strsplit(novel.lower.v, "\\W")

# convert list to vector (atomic - you can't have a vector of vectors)
moby.word.v <- unlist(moby.words.l)

# get the elements that are not blank
not.blanks.v  <-  which(moby.word.v!="")

# re-define the word vector using only 
# those elements that aren't blank
moby.word.v <-  moby.word.v[not.blanks.v]

# dispersion plot is calculated on a measure of time
# where each word in the sequence of words in the novel
# represents a beat of the metronome of novelistic time

# generate sequence of beats as list of numbered beats
# this will serve as the x axis (time axis) of our
# dispersion plot
n.time.v <- seq(1:length(moby.word.v))

# calculate the location of every instance of the word
# of interest defined above in the word vector
wordofinterest.v <- which(moby.word.v == wordofinterest.str)

# y axis must now be constructed as scale of TRUE/FALSE
# values for the presence of the wordofinterest. R has 
# a special variable valuable of NA (not available) which 
# indicates that a condition is not met in a particular
# element of an index or matrix.  Here we create an 
# y-axis vector that is the same length as the x-axis
# vector and fill it with NAs
w.count.v <- rep(NA,length(n.time.v))

# now reset the NAs in the y-axis to 1 (TRUE) every place 
# where the wordofinterest appears
w.count.v[wordofinterest.v] <- 1

# now generate the plot
plot(w.count.v, main="Dispersion Plot of `whale' in Moby Dick",
     xlab="Novel Time - Words Location in the Novel", ylab="whale", type="h", ylim=c(0,1), yaxt='n')











