# Script: lemmatizer.R
# 
# A script written and distributed as a teaching
# aid for demonstrating how to perform corpus
# stemming in R.  The script demonstrates the use
# of the Morphadorner lemmatizing service to 
# create a lemmatized version of a single text.
#
# Copyright Carl G Stahmer
#
# This work is licensed under a Creative Commons 
# Attribution-ShareAlike 4.0 International License.
#
# see http://creativecommons.org/licenses/by-sa/4.0/

# install.packages("httr")
# install.packages("XML")

# For alternate language openNLPModels see http://datacube.wu.ac.at/src/contrib/

library(httr)
library(XML)

###################################
#         configuration           #
###################################

# set working directory
setwd("~/Documents/rstudio_workspace/digitalmethods/text_mining")

# set the file path
filePath.str = "data/plainText/emerson.txt"

###################################
#      function declarations      #
###################################

lemmatize <- function(wordlist) {
  get.lemma <- function(word, url) {
    response <- GET(url,query=list(spelling=word,standardize="",
                                   wordClass="",wordClass2="",
                                   corpusConfig="ncf",    # Nineteenth Century Fiction
                                   media="xml"))
    content <- content(response,type="text")
    xml     <- xmlInternalTreeParse(content)
    return(xmlValue(xml["//lemma"][[1]]))    
  }
  url <- "http://devadorner.northwestern.edu/maserver/lemmatizer"
  return(sapply(wordlist,get.lemma,url=url))
}

###################################
#        Operational Code         #
###################################

# Do a test run
words <- c("is","am","was","are")
lemmatize(words)

# Now do a real run

# load the file to be analized into a character vector
# the resulting vector will have as many elements as
# lines in the file with the contents of each line
# contained in a character vector.
text.vector <- readLines(filePath.str)

# convert to lower case
text.vector <- tolower(text.vector)

# collapse the vector of lines into a single character 
# vector
text.string <- paste(text.vector, collapse = " ")

# split each line on spaces
text.v <- strsplit(text.string, "\\W")

# unlist to a word vector
text.v <- unlist(text.v)

# do the lemmatization
lemmatized.v <- lemmatize(text.v)

lemmatized.v
