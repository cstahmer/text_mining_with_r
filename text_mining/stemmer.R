# Script: stemmer.R
# 
# A script written and distributed as a teaching
# aid for demonstrating how to perform corpus
# stemming in R.  The script reads a single file
# and creates a stemmed version of the input text.
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

# install.packages("tm")

library(tm)

###################################
#         configuration           #
###################################

# set working directory
setwd("~/Documents/rstudio_workspace/digitalmethods/text_mining")

# set the file path
var_filePath_character = "data/cleanText/melville.txt"

###################################
#        Operational Code         #
###################################

# load the file to be analized into a character vector
# the resulting vector will have as many elements as
# lines in the file with the contents of each line
# contained in a character vector.
var_textLines_vector <- readLines(var_filePath_character)

# collapse the vector of lines into a single 
# character blob
var_textBlob_vector <- paste(var_textLines_vector, collapse = " ")

# explicitly convert the text.string character to a 
# string class.  Necessary because the NLP is written
# in java.
var_textBlob_string <- as.String(var_textBlob_vector)

# create a tm corpus
var_text_corpus <- VCorpus(VectorSource(c(var_textBlob_string)))
obj_originalText_tdm <- TermDocumentMatrix(var_text_corpus,
                               control = list(removePunctuation = TRUE,
                                              stopwords = TRUE,
                                              stripWhitespace = TRUE,
                                              stemming = FALSE))

obj_stemmed_tdm <- TermDocumentMatrix(var_text_corpus,
                               control = list(removePunctuation = TRUE,
                                              stopwords = TRUE,
                                              stripWhitespace = TRUE,
                                              stemming = TRUE))

# Get the dimensions of the non-stemmed tdm
dim(obj_originalText_tdm)


# Get the dimensions of the stemmed tdm
dim(obj_stemmed_tdm)

# View a subsection of the stemmed tdm
inspect(obj_stemmed_tdm[500:550, 1])

