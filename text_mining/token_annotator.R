# Script: token_annotator.R
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


# install.packages("rJava")
# install.packages("NLP")
# install.packages("openNLP")
# install.packages("RWeka")
# install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")

# For alternate language openNLPModels see http://datacube.wu.ac.at/src/contrib/

library(rJava)
library(NLP)
library(openNLP)
library(RWeka)

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

# create sentence and word annotators to parse the document. 
# Annotator will return a map of where sentences and words 
# start and end in the document.You have to annotate senteces
# first in order to annotate words.
sent_ann <- Maxent_Sent_Token_Annotator()
word_ann <- Maxent_Word_Token_Annotator()

# testing just running a word annotator without a sentence one
text.annotator <- annotate(text.string, list(sent_ann, word_ann))

# create an annotated doc.  This is a version of the document that is
# represented as a structured hierarchy of sentences and words
text.doc <- AnnotatedPlainTextDocument(text.string, text.annotator)

# show the fist sentences in the document
# sents(text.doc) %>% head(2)

# show the first 10 words in the document
# words(text.doc) %>% head(10)
