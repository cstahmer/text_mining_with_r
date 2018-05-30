# Script: ngrams.R
# A script written and distributed as a teaching
# aid for demonstrating how to perform ngram
# extraction and visualization.  
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
# Associate Director for Humanities - UC Davis Data Science Initiative
# Associate Director - English Broadside Ballad Archive
#
# This work is licensed under a Creative Commons 
# Attribution-ShareAlike 4.0 International License.
#
# see http://creativecommons.org/licenses/by-sa/4.0/
#install.packages("tm")0

library(NLP)
library(tm)
library(hunspell)
library(ngram)
library(dplyr)
library(ggraph)
library(igraph)


###################################
#         configuration           #
###################################

# identify the text file to analyze
#var_textFile = "/Users/cstahmer/workspaces/rstudio_workspace/text_mining_with_r/data/cleanText/melville.txt"
var_textFile = "/Users/cstahmer/workspaces/rstudio_workspace/text_mining_with_r/data/plosONE/0000095.txt"

# identify any words you want removed from
# the corpus prior to analysis
var_word_blacklist = c("orphan", 
                       "children",
                       "search",
                       "missing")

# define n for the n-gram
var_n = 2

# define how many ngrams to output
var_plot_ngram_n = 20

# identify a file location to save ngrams
var_outputFile <-"/Users/cstahmer/Desktop/textmining_workshop/ngrams.csv"

###################################
#        Operational Code         #
###################################

# load the file to be analized into a character vector.
# The resulting vector will have as many elements as
# lines in the file with the contents of each line
# contained in a character vector.
var_textLines <- readLines(var_textFile)

# collapse the vector of lines into a single element
var_textBlob <- paste(var_textLines, collapse = " ")

# change to lowercase
var_textBlob_lowercase <- tolower(var_textBlob)

# remove stopwords
var_textBlob_unStopped <- removeWords(var_textBlob_lowercase, stopwords('english'))

# use the below line to remove any other tokens based on blacklist
var_textBlob_blacklisted <- removeWords(var_textBlob_unStopped, var_word_blacklist)

# remove punctuation
var_textBlob_noPunctuation <- removePunctuation(var_textBlob_blacklisted)

# remove numeric elements
var_textBlob_nonNumeric <- gsub("\\d+", "", var_textBlob_noPunctuation)

# collapse multiple spaces
var_textBlob_collapseWhitespace <- gsub("\\s+", " ", var_textBlob_nonNumeric)

# trim leading and trailing spaces
var_textBlob_trimmed <- trimws(var_textBlob_collapseWhitespace)

# convert blob to list of words
var_wordlist = unlist(strsplit(var_textBlob_trimmed, split = ' '))

# get a list of all non-word tokens in the blob.  This can take a long
# time, so the code below includes two lines of code.  To run in production
# uncomment the line that uses the hunspell library.  To run during testing
# and learning comment out the hunspell line and uncomment the line that
# assigns a random list of non-words to the var_nonWord vector.
#var_nonWords <- hunspell(var_textBlob_trimmed, dict = dictionary("en_US"))
var_nonWords <- c("circle", "like", "another", "ixion")

# get a list of all elements in the list of words that
# aren't a real word
var_nonwordList <- match(var_wordlist, var_nonWords)

# subset the original list of words to include only those
# words that don't match the non-word list
var_wordList_cleaned <- var_wordlist[is.na(var_nonwordList)]

# collapse the vector back into a text blob
var_texBlob_cleaned <- paste(var_wordList_cleaned, collapse=" ")

########################################################
# At this point you have a nicely cleaned text blob    # 
# is all lower case, has stop words, stand-alone       #
# digits, a user-defined list of other words,          #
# and any non-word (as compared to dictionary) tokens, #
# and any extra spaces removed.                        #
########################################################

# calculate the n-grams
obj_ngram <- ngram (var_texBlob_cleaned , n = var_n)

# print truncated results to s creen
print(obj_ngram, output = "truncated")

# get a phrase table
obj_phrasetable <- get.phrasetable(obj_ngram)

# make a dataframe representation of the ngrams

# first setup an empty df
obj_directed_ngram_table <- data.frame()

# now loop through the obj_phrasetable and extract,
# split, and arrange the data as needed, then append it
# to the next row of the df.  For instructional purposes
# this is set to process only 20 ngrams.  To do the entire
# ngram list, set loot  1:nrow(obj_phrasetable)
for (i in 1:var_plot_ngram_n){
  # extract the gram list into a string
  var_gram_item <- obj_phrasetable[i,1]
  print(var_gram_item)
  # trim leading and trailing spaces
  var_gram_item <- trimws(var_gram_item)
  # now split into a vector of items
  var_gramslist = unlist(strsplit(var_gram_item, split = ' '))
  # make sure that we actually got a correct sized ngram.
  # if not, don't process
  if (length(var_gramslist == var_n)) {
    # extract the frequency value
    var_freq <- obj_phrasetable[i,2]
    # combine the gramlist and frequency value into a single vector
    var_gramslist <- c(var_gramslist, var_freq)
    print(var_gramslist)
    # bind the new vector to the end of the brigram dataframe
    obj_directed_ngram_table <- rbind(obj_directed_ngram_table, as.data.frame(t(var_gramslist)))
  }
}

# now assign some column names to the ngram table
var_df_colnames <- paste0("Word", 1:var_n)
var_df_colnames <- c(var_df_colnames, "Frequency")
colnames(obj_directed_ngram_table) <- var_df_colnames

# make into graph object
obj_ngrams_graph = obj_directed_ngram_table %>% graph_from_data_frame()

# plot the graph
ggraph(obj_ngrams_graph,
       layout = 'fr') +
  geom_edge_link(aes(edge_alpha = var_plot_ngram_n),
                 show.legend = FALSE,
                 arrow = grid::arrow(type = "open",
                                     length = unit(.10, "inches")),
                 end_cap = circle(.07, "inches")) +
  geom_node_point(color = "lightblue",
                  size = 3) +
  geom_node_text(aes(label = name),
                 repel = TRUE) +
  theme_void()


# save the bigram table
write.csv(obj_directed_ngram_table, file = var_outputFile)