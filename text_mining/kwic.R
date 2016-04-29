# Script: kwic.R
# 
# A script written and distributed as a teaching
# aid for demonstrating how to perform corpus
# stemming in R.  The script loads files from 
# a directory into a corpus and performs keyword
# in context analysis across the corpus based
# on a configured word of interest.
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

# define the input directory for the texts to
# be analyzed
input.dir <- "data/plainText"

# define a word of interest that you want to plot
wordofinterest.str = "dog"

# define the range before and after that you
# want to examine
range.int = 3

###################################
#      function declarations      #
###################################

# A callable function that writes out the contents
# of a vector in human readable form.
show.vector <- function(file.name.v) {
  for(i in 1:length(file.name.v)) { 
      cat(i, file.name.v[i], "\n", sep=" ")
  } 
}

###################################
#        Operational Code         #
###################################

# get a list of all files in the working directory
# that have the appropriate file suffix
files.v <- dir(input.dir, "\\.txt$")

# send our files vector to our show.vector
# function to print to display
show.vector(files.v)

# setup an empty list container to hold the word vectors
# for each text in the working diretory
my.corpus.l <- list()

# now loop throug all files listed in the files vector
# and open and process them

for(i in 1:length(files.v)){
  # read the file in
  text.v <- scan(paste(input.dir, files.v[i], sep="/"), what="character", sep="\n") 
  text.v <- paste(text.v, collapse=" ")
  
  #lowercase and split on non-word characters
  text.lower.v <- tolower(text.v)
  text.words.v <- strsplit(text.lower.v, "\\W")
  text.words.v <- unlist(text.words.v)
  
  #remove the blanks
  text.words.v <- text.words.v[which(text.words.v!="")]
  
  #use the index id from the files.v vector as the "name" in the list 
  my.corpus.l[[files.v[i]]] <- text.words.v
}

# show the first 25 words of text # 1 (Austen)
my.corpus.l[[1]][1:100]

# show the first 25 words of text # 2 (Melvyl)
my.corpus.l[[2]][1:100]

# now loop through each row of the list (each text)
# and calculate the words that surround each instance
# of the word of interest
for(i in 1:length(my.corpus.l)){
  
  # now get the positions of the word of interest
  # in the working text
  positions.v <- which(my.corpus.l[[i]][]==wordofinterest.str)
  
  # now process each position where the
  # word of interest is found, gentering
  # vector of KWiC
  for(ii in 1:length(positions.v)){
    
    # setup the start position for the sub vector
    startpos.int = positions.v[ii] - range.int
    
    # adjust start position if range extends beyond
    # first word of the document
    while (startpos.int < 1) {
      startpos.int = startpos.int + 1
    }
    
    # create the sub vector of word in context
    # for this occurrence of the word of interest
    kwic.v <- my.corpus.l[[i]][(startpos.int):(positions.v[ii] + range.int)]
    
    # colapse the vector into a csv string
    kwic.str <- paste(kwic.v, collapse=", ")
    
    # print the KWiC csv string
    print(kwic.str)
  }
   
}








