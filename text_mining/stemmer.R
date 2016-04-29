# Script: stemmer.R
# 
# A script written and distributed as a teaching
# aid for demonstrating how to perform corpus
# stemming in R.  The script reads a single file
# and creates a stemmed version of the input text.
#
# Copyright Carl G Stahmer
#
# This work is licensed under a Creative Commons 
# Attribution-ShareAlike 4.0 International License.
#
# see http://creativecommons.org/licenses/by-sa/4.0/


# install.packages("tm")

library(tm)

###################################
#         configuration           #
###################################

# set working directory
setwd("~/Documents/rstudio_workspace/digitalmethods/text_mining")

# set the file path
filePath.str = "data/cleanText/melville.txt"

###################################
#        Operational Code         #
###################################

# load the file to be analized into a character vector
# the resulting vector will have as many elements as
# lines in the file with the contents of each line
# contained in a character vector.
text.vector <- readLines(filePath.str)

# collapse the vector of lines into a single character 
# vector
text.vector <- paste(text.vector, collapse = " ")

# explicitly convert the text.string character to a 
# string class.  Necessary because the NLP is written
# in java.
text.string <- as.String(text.vector)

# create a tm corpus
text.corpus <- VCorpus(VectorSource(c(text.string)))
text.tdm <- TermDocumentMatrix(text.corpus,
                               control = list(removePunctuation = TRUE,
                                              stopwords = TRUE,
                                              stripWhitespace = TRUE,
                                              stemming = FALSE))

stemmed.tdm <- TermDocumentMatrix(text.corpus,
                               control = list(removePunctuation = TRUE,
                                              stopwords = TRUE,
                                              stripWhitespace = TRUE,
                                              stemming = TRUE))

# Get the dimensions of the non-stemmed tdm
dim(text.tdm)


# Get the dimensions of the stemmed tdm
dim(stemmed.tdm)

# View a subsection of the stemmed tdm
inspect(stemmed.tdm[500:550, 1])

