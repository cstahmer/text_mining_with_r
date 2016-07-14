# Script: pos_tagger.R
# 
# A script written and distributed as a teaching
# aid for demonstrating how to perform corpus
# stemming and Part of Speach (POS) annotation in R.
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

# POS Type KEY:
#   CC = Coordinating conjunction
#   CD = Cardinal number
#   DT = Determiner
#   EX = Existential there
#   FW = Foreign word
#   IN = Preposition or subordinating conjunction
#   JJ = Adjective
#   JJR = Adjective, comparative
#   JJS = Adjective, superlative
#   LS = List item marker
#   MD = Modal
#   NN = Noun, singular or mass
#   NNS = Noun, plural
#   NNP = Proper noun, singular
#   NNPS = Proper noun, plural
#   PDT = Predeterminer
#   POS = Possessive ending
#   PRP = Personal pronoun
#   PRP$ = Possessive pronoun
#   RB = Adverb
#   RBR = Adverb, comparative
#   RBS = Adverb, superlative
#   RP = Particle
#   SYM = Symbol
#   TO = to
#   UH = Interjection
#   VB = Verb, base form
#   VBD = Verb, past tense
#   VBG = Verb, gerund or present participle
#   VBN = Verb, past participle
#   VBP = Verb, non¬3rd person singular present
#   VBZ = Verb, 3rd person singular present
#   WDT = Wh¬determiner
#   WP = Wh¬pronoun
#   WP$ = Possessive wh¬pronoun
#   WRB = Wh¬adverb

# install.packages("rJava")
# install.packages("NLP")
# install.packages("openNLP")
# install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
# For alternate language openNLPModels see http://datacube.wu.ac.at/src/contrib/

library(rJava)
library(NLP)
library(openNLP)
library(openNLPmodels.en)

###################################
#         configuration           #
###################################

# set working directory
setwd("~/Documents/rstudio_workspace/digitalmethods/text_mining")

# set the file path
#var_filePath_character = "data/cleanText/melville_truncated.txt"
var_filePath_character = "data/plainText/emerson.txt"

# calculate identification probability
var_showProbability_boolean <- TRUE

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

###################################
#        Operational Code         #
###################################

# load the file to be analized into a character vector.
# The resulting vector will have as many elements as
# lines in the file with the contents of each line
# contained in a character vector.
var_textLines_vector <- readLines(var_filePath_character)

# collapse the vector of lines into a single element
var_textBlog_character <- paste(var_textLines_vector, collapse = " ")

# explicitly convert the var_textBlog_character character 
# to a String class.  Necessary because the NLP is written
# in java.
var_text_string <- as.String(var_textBlog_character)

# create the annotators.
obj_sentence_annotator <- Maxent_Sent_Token_Annotator()
obj_word_annotator <- Maxent_Word_Token_Annotator()
obj_pos_annotator <- Maxent_POS_Tag_Annotator(language = "en", probs = var_showProbability_boolean, model = NULL)
                    # language = language
                    # probs = indicates whether the computed annotations should provide the token
                    #         probabilities obtained from the Maxent model as their ‘POS_prob’ feature
                    # model = a character string giving the path to the Maxent model file to be 
                    #         used, or NULL indicating to use a default model file for the given 
                    #         language 

# create the initial sentence/word annotation model matrix
var_base_annotation_model_matrix <- annotate(var_text_string, list(obj_sentence_annotator, obj_word_annotator))

# create a POS Tag Annotator
var_pos_annotation_model_matrix <- annotate(var_text_string, obj_pos_annotator, var_base_annotation_model_matrix)

print(var_pos_annotation_model_matrix)


# create a model of the text as an ordered collection
# parts of speeach.
var_textRawPOSModel_matrix <- subset(var_pos_annotation_model_matrix, type == "word")
var_cleanedTextPOSModel_matrix <- sapply(var_textRawPOSModel_matrix$features, `[[`, "POS")

print(var_cleanedTextPOSModel_matrix)

# create a POS count table
var_PODCount_table <- table(var_cleanedTextPOSModel_matrix)
print(var_PODCount_table)

# extract token/POS pairs
var_wordPOSpairs_vector <- sprintf("%s/%s", var_text_string[var_textRawPOSModel_matrix], var_cleanedTextPOSModel_matrix)

function_show_vector(var_wordPOSpairs_vector)

