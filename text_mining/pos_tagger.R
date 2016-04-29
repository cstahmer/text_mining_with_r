# Script: pos_tagger.R
# 
# A script written and distributed as a teaching
# aid for demonstrating how to perform corpus
# stemming in R.  The script reads a single file
# and performs part of speech annotation.
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
# install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")

# For alternate language openNLPModels see http://datacube.wu.ac.at/src/contrib/

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
filePath.str = "data/cleanText/melville_truncated.txt"

# calculate identification probability
showProbability.bln <- TRUE

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

# create the sentence and word annotators.
sent_ann <- Maxent_Sent_Token_Annotator()
word_ann <- Maxent_Word_Token_Annotator()
pos_ann <- Maxent_POS_Tag_Annotator(language = "en", probs = showProbability.bln, model = NULL)
                    # language = language
                    # probs = indicates whether the computed annotations should provide the token
                    #         probabilities obtained from the Maxent model as their ‘POS_prob’ feature
                    # model = a character string giving the path to the Maxent model file to be 
                    #         used, or NULL indicating to use a default model file for the given 
                    #         language 

# create the initial annotator model
#### a2 <- annotate(s, list(sent_ann, word_ann))
text.annotator <- annotate(text.string, list(sent_ann, word_ann))

# create a POS Tag Annotator
pos.annotator <- annotate(text.string, pos_ann, text.annotator)

show.vector(pos.annotator)


# Create POS distribution vector.  This creates a version
# of the text where all words are replaced by their POS
tagDistribution.v <- subset(pos.annotator, type == "word")
tags.v <- sapply(tagDistribution.v$features, `[[`, "POS")

show.vector(tags.v)

# Create a POS count table
counts.t <- table(tags.v)
counts.t



# extract token/POS pairs
#sprintf("%s/%s", text.string[tagDistribution.v], tags.v)
pairs.v <- sprintf("%s/%s", text.string[tagDistribution.v], tags.v)

show.vector(pairs.v)

