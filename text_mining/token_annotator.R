# Script: token_annotator.R
# 
# A script written and distributed as a teaching
# aid for demonstrating how to tokenize a
# text in R.  The script reads a single file
# chunks and annotates sentences and words.
#
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
var_filePath_character = "data/plainText/emerson.txt"

###################################
#        Operational Code         #
###################################

# load the file to be analized into a character vector
# the resulting vector will have as many elements as
# lines in the file with the contents of each line
# contained in its own element.
var_textLines_vector <- readLines(var_filePath_character)

# collapse the vector of lines into a single character 
# vector
var_textBlob_vector <- paste(var_textLines_vector, collapse = " ")

# explicitly convert the var_textBlob_vector to a 
# string class.  Necessary because the NLP is written
# in java.
var_textBlob_string <- as.String(var_textBlob_vector)

# create sentence and word annotators to parse the document. 
# Annotator will return a map of where sentences and words 
# start and end in the document.You have to annotate senteces
# first in order to annotate words.
obj_sentence_annotator <- Maxent_Sent_Token_Annotator()
obj_word_annotator <- Maxent_Word_Token_Annotator()

# testing just running a word annotator without a sentence one
var_annotationModel_matrix <- annotate(var_textBlob_string, list(obj_sentence_annotator, obj_word_annotator))

# create an annotated doc.  This is a version of the document that is
# represented as a structured hierarchy of sentences and words
obj_annotatedText_doc <- AnnotatedPlainTextDocument(var_textBlob_string, var_annotationModel_matrix)

# get a list of sentences in the text
var_sentences_list <- sents(obj_annotatedText_doc)

# get a list of words in the text
var_words_list <- words(obj_annotatedText_doc)
