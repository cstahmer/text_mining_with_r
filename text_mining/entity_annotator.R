# Script: entity_annotator.R
# 
# A script written and distributed as a teaching
# aid for demonstrating how to perform entity
# extraction in R.  The script reads a single file
# and extracts various entities (such as name, place,
# dates, etc.) from the text based on configuration.
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
setwd("~/Documents/rstudio_workspace/digitalmethods/text_mining")

# set the entity type you want to extract.  Legal
# values are: 
#     date
#     location
#     money
#     organization
#     percentage
#     person
#     misc [proper nouns deemed not to fit anther category]
type.string = "person"

# set the file path
filePath.str = "data/cleanText/melville_truncated.txt"

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

# A callable function that extracts entities 
# of an identified kind from an 
# AnnotatedPlainTextDocument
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

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

# create the annotators.
sent_ann <- Maxent_Sent_Token_Annotator()
word_ann <- Maxent_Word_Token_Annotator()
my.ann <- Maxent_Entity_Annotator(kind = type.string)

# assemble the list of annotators to
# send to the annotate function
pipeline.list <- list(sent_ann, word_ann, my.ann)

# create the final annotator model
text.annotator <- annotate(text.string, pipeline.list)

# create an annotated doc.  This is a version of the document that is
# represented as a structured hierarchy of sentences and words
text.doc <- AnnotatedPlainTextDocument(text.string, text.annotator)

# get all entities of the type we are looking for
foundEntities.v <- entities(text.doc, kind = type.string)

# get vector of unique items
uniqueEntities.v <- unique(foundEntities.v)

# sort the entities vector
sortedEntities.v <- sort(uniqueEntities.v)

###################################
# Code below is for cleaning list #
###################################

# review list of returned entities and create list
# of items entities that should be removed from the list
droplist.v <- c("William", "Young", "Zoroaster")

# now remove items from the droplist from the vector of
# extracted named entities
finalNameList.v <- sortedEntities.v[! sortedEntities.v %in% droplist.v ]
