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
# (2) In order to facilitate distinguishing between
# variables, functions, and objects all objects in 
# the code begin with the "obj_" prefix.
#
# (3) Locally defined functions begin with the 
# function_ prefix
#
# Copyright Carl G. Stahmer - 2018
# Director of Data and Digital Scholarship - UC Davis Library
# Associate Director for Humanities - UC Data Science Initiative
# Associate Director - English Broadside Ballad Archive
#
# This work is licensed under a Creative Commons 
# Attribution-ShareAlike 4.0 International License.
#
# see http://creativecommons.org/licenses/by-sa/4.0/

library(tm)

###################################
#         configuration           #
###################################

# define the input directory for the texts to
# be analyzed
var_inputDir <- "/Users/cstahmer/workspaces/rstudio_workspace/text_mining_with_r/data/plosOneSubset"

# file path to write output csv files
var_output_write_path <- "/Users/cstahmer/Desktop/textmining_workshop/tfidf_plosone/"

###################################
#        Operational Code         #
###################################

# create a tm corpus
obj_termMatrix_corpus <- VCorpus(DirSource(directory = var_inputDir), readerControl = list(language = "en"))

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

# look at the results for a single item
obj_single_doc_matrix <- as.matrix(obj_tfidfMatrix_tdm[, 1])

# sort the results
obj_single_doc_matrix_sorted <- obj_single_doc_matrix[sort.list(-obj_single_doc_matrix[,1]), decreasing=TRUE]

# print top ten results
obj_single_doc_matrix_sorted[1:10]

# loop throught the matrix by column
for (var_document_integer in 1:var_numcols_int) {
  
  # Convert the TDM to a normal matrix for sorting
  var_singleDoc_matrix <- as.matrix(obj_tfidfMatrix_tdm[, var_document_integer])
  
  # Remove the non-word items from the list
  var_numericLabels_list = grepl(".*[0-9]+.*", rownames(var_singleDoc_matrix)) 
  var_singleDocNonNumeric_matrix <- as.matrix(var_singleDoc_matrix[ !var_numericLabels_list, ]) 
  
  # Sort the results Matrix
  var_sorted_matrix <- var_singleDocNonNumeric_matrix[sort.list(-var_singleDocNonNumeric_matrix[,1]), decreasing=TRUE]
  
  # Write CSV in R
  var_write_path <- paste(var_output_write_path, "tfidf.", colnames(var_singleDoc_matrix)[1], ".csv", sep = "")
  write.csv(var_sorted_matrix, file = var_write_path)
}