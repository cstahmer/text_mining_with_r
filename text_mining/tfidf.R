# Script: tfidf.R
# 
# A script written and distributed as a teaching
# aid for demonstrating how to perform Term 
# Frequency - Inverse Document Frequency analysis
# across a corpus.  The script loads files from
# a designated directory into a corpus and then
# creates a normalized TF-IDF matrix.
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
setwd("~/Documents/rstudio_workspace/digitalmethods/text_mining/")

# define the input directory for the texts to
# be analyzed
var_inputDir_character <- "data/plosOneSubset"

# file path to write output csv files
var_output_write_path <- "/Users/cstahmer/Desktop/tfidf_plosone"

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

# create a tm corpus
obj_termMatrix_corpus <- VCorpus(DirSource(directory = var_inputDir_character), readerControl = list(language = "en"))

# create the tdm from the corpus
obj_stemmed_tdm <- TermDocumentMatrix(obj_termMatrix_corpus,
                                  control = list(removePunctuation = TRUE,
                                                 stopwords = TRUE,
                                                 stripWhitespace = TRUE,
                                                 stemming = TRUE))
# create tf-idf matrix
obj_tfidfMatrix_tdm <- weightTfIdf(obj_stemmed_tdm, normalize = TRUE)

# print the stats of the tfdif matrix
obj_tfidfMatrix_tdm

# get the number of rows in the tfidf
var_numRows_int <- nrow(obj_tfidfMatrix_tdm)

# get the number of columns in the tfidf
var_numcols_int <- ncol(obj_tfidfMatrix_tdm)

# loop throught the matrix by column
for (var_document_integer in 1:var_numcols_int) {
  
  # Convert the TDM to a normal matrix for sorting
  var_singleDoc_matrix <- as.matrix(obj_tfidfMatrix_tdm[, var_document_integer])
  
  # Remove the non-word items from the list
  var_numericLabels_list = grepl(".*[0-9]+.*", rownames(var_singleDoc_matrix)) 
  var_singleDocNonNumeric_matrix <- as.matrix(var_singleDoc_matrix[ !var_numericLabels_list, ]) 
  
  # Sort the results Matrix
  var_sorted_matrix <- var_singleDocNonNumeric_matrix[sort.list(var_singleDocNonNumeric_matrix[,1]), decreasing=TRUE]
  
  # Write CSV in R
  # write.csv(var_sorted_matrix, file = "/Users/cstahmer/SpiderOak Hive/writing/close_reading_ballads/tfidf2.csv")
  var_write_path <- paste(var_output_write_path, "tfidf.", colnames(var_singleDoc_matrix)[1], ".csv", sep = "")
  write.csv(var_sorted_matrix, file = var_write_path)
}