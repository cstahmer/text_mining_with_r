# Script: entity_annotator.R
# 
# A script written and distributed as a teaching
# aid for demonstrating how to perform entity
# extraction in R.  The script reads a single file
# and extracts various entities (such as name, place,
# dates, etc.) from the text based on configuration.
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
# install.packages("stats")
# install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")

# For alternate language openNLPModels see http://datacube.wu.ac.at/src/contrib/

library(rJava)
library(NLP)
library(openNLP)
library(RWeka)
library(stats)

###################################
#         configuration           #
###################################

# set working directory
setwd("~/Documents/rstudio_workspace/digitalmethods/text_mining/data/plainText")

# set the entity type you want to extract.  Legal
# values are: 
#     date
#     location
#     money
#     organization
#     percentage
#     person
#     misc [proper nouns deemed not to fit anther category]
var_entityType_string = "person"

# set the file path
var_filePath_string = "melville.txt"

###################################
#      function declarations      #
###################################

# A callable function that writes out the contents
# of a vector in human readable form.
function_show_vector <- function(var_file_name_vec) {
  for(i in 1:length(var_file_name_vec)) { 
    cat(i, var_file_name_vec[i], "\n", sep=" ")
  } 
}

# A callable function that extracts entities 
# of an identified kind from an 
# AnnotatedPlainTextDocument
function_extractEntities <- function(obj_doc, var_kind_character) {
  var_content_string <- obj_doc$content
  obj_annotations <- annotations(obj_doc)[[1]]
  if(hasArg(var_kind_character)) {
    var_kindFeatures_list <- sapply(obj_annotations$features, `[[`, "kind")
    var_content_string[obj_annotations[var_kindFeatures_list == var_kind_character]]
  } else {
    var_content_string[obj_annotations[obj_annotations$type == "entity"]]
  }
}

# A callable function that checks to see
# if an annotated document has any matching
# entities
function_checkForEntities <- function(obj_doc, var_kind_character) {
  obj_annotations <- annotations(obj_doc)[[1]]
  var_return_boolean <- TRUE
  if(hasArg(var_kind_character)) {
    var_kindFeatures_list <- sapply(obj_annotations$features, `[[`, "kind")
    var_matchCount_list <- obj_annotations[var_kindFeatures_list == var_kind_character]
    var_matchCountSize_int <- length(var_matchCount_list)
    if (var_matchCountSize_int < 1) {
      var_return_boolean <- FALSE
    }
  } else {
    var_return_boolean <- FALSE
  }
  return(var_return_boolean)
}

###################################
#        Operational Code         #
###################################

# load the file to be analized into a character vector
# the resulting vector will have as many elements as
# lines in the file with the contents of each line
# contained in a character vector.
var_readLinesRaw_vector <- readLines(var_filePath_string)

# collapse the vector of lines into a single character 
# vector
var_textBlob_character <- paste(var_readLinesRaw_vector, collapse = " ")

# explicitly convert var_textBlog_character to a Java
# string class.  Necessary because the NLP is written
# in java.
var_textBlob_string <- as.String(var_textBlob_character)

# create the annotators.
obj_sentence_annotator <- Maxent_Sent_Token_Annotator()
obj_word_annotator <- Maxent_Word_Token_Annotator()
obj_entity_annotator <- Maxent_Entity_Annotator(kind = var_entityType_string)

# assemble the list of annotators into a processing
# pipeline that will be used to configure the annotator
var_pipeline_list <- list(obj_sentence_annotator, obj_word_annotator, obj_entity_annotator)

# create the final model
var_annotationModel_matrix <- annotate(var_textBlob_string, var_pipeline_list)

# create an annotated doc.  This is a version of the document that is
# represented as a structured hierarchy of sentences and words
obj_annotatedText_document <- AnnotatedPlainTextDocument(var_textBlob_string, var_annotationModel_matrix)

# check to see if there are any matching entities.
# If so, process. If not, extit.
if (function_checkForEntities(obj_annotatedText_document, var_kind_character = var_entityType_string)) {

  # get all entities of the type we are looking for
  var_foundEntities_vector <- function_extractEntities(obj_annotatedText_document, var_kind_character = var_entityType_string)
  
  # get vector of unique items
  var_uniqueEntities_vector <- unique(var_foundEntities_vector)
  
  # sort the entities vector
  var_sortedEntities_vector <- sort(var_uniqueEntities_vector)
  
  print("Sorted entity list:")
  function_show_vector(var_sortedEntities_vector)
  
  ###################################
  # Code below is for cleaning list #
  ###################################
  
  # review list of returned entities and create list
  # of items entities that should be removed from the list
  var_droplist_vector <- c("With", "Starbuck", "Watts", "Plato")
  
  # now remove items from the droplist from the vector of
  # extracted named entities
  var_cleanedList_vector <- var_sortedEntities_vector[! var_sortedEntities_vector %in% var_droplist_vector ]
  
  print("Filtered entity list:")
  function_show_vector(var_cleanedList_vector)
  
} else {
  print("No entities found")
}

