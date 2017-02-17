# Script: kwic.R
# 
# A script written and distributed as a teaching
# aid for demonstrating how to perform corpus
# stemming and analysis in R.  The script loads files from 
# a designated directory into a working corpus and performs 
# keyword in context (KWIC) analysis across the corpus based
# on a configured word of interest.
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

library(qdap)

###################################
#         configuration           #
###################################

# Set working directory
setwd("~/Documents/rstudio_workspace/digitalmethods/text_mining/")

# Define the input directory for the texts to be analyzed.
var_inputDir_character <- "/Users/cstahmer/SpiderOak Hive/writing/close_reading_ballads/runscript"

# Define a word of interest that you want to plot.
var_wordofinterest_str = "wife"

# Define the range before and after that you want to examine.
var_range_int = 3

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

# get a list of all files in the working directory
# that have the appropriate file suffix.
var_files_vector <- dir(var_inputDir_character, "\\.txt$")

# send our files vector to our function_show_vector
# function to print to display.
function_show_vector(var_files_vector)

# setup an empty list container to hold the word vectors
# for each text in the working diretory.
var_corpus_list <- list()

# loop through all files listed in the files vector
# and open and process them.

for(var_iteration_int in 1:length(var_files_vector)){
  
  # define the file path to the file to read by concatenating 
  # (joining) the var_inputDir_character to the filename contained in 
  # the vector element being processed (element[var_iteration_int])
  # and putting a forward shlash between them so that the result
  # is a correct file path.
  var_filepath_str <- paste(var_inputDir_character, var_files_vector[var_iteration_int], sep="/")
  
  
  # read the file into a character vector using the scan funtion, spliting 
  # the input on newlines ("\n" is unix shorthand for newline) so that each
  # element in the vector is a single line of text.
  var_text_lines_vec <- scan(var_filepath_str, what="character", sep="\n") 
  
  # create a new vector that holds the entire text as
  # as a blog (one long running chunk of text).  This is 
  # accomplished by joining (using collapse) the vector of 
  # lines (var_text_raw_vec), putting a space between each
  # each element/line.  The result is a charactre vector
  # containing one element containing the complete text as a
  # blob.
  var_text_vec <- paste(var_text_lines_vec, collapse=" ")
  
  # NOTE:  You might be asking why, in the above, we first
  # read the text into a vector of lines and then collapsed the
  # lines into a blob.  Why not just read the text in as a
  # blob?  The answer has to do with tht scan() function.
  # We used scan() to read the text into a vector, spliting
  # on the newline character ("\n").  If we read the text in as a
  # blob, all those newline characters would still be there
  # and would mess up further processing.  One way or another
  # the newlines have to be removed, and the above process
  # accomplishes this.
  
  # convert it to lowercase so that
  # later comparison doesnt treat "Cat" and "cat" as different.
  var_text_lower_vec <- tolower(var_text_vec)

  
  # split the blob character vector on character that is not
  # a word.  "\W" is unix/regex shorthand for anytying not a
  # non blank space character.  Because we want R to read the "\"
  # as character itself and not as an escape character, we need to 
  # escape it, so we write "\W" as "\\W".  Note that the output
  # of strsplit() is a list and not a vector. A list can contain
  # elements of different types ([1]=charachter, [2]=integer, etc.)
  # whereas a vector is atomic and each element must be of the same
  # base data type.
  var_text_words_list <- strsplit(var_text_lower_vec, "\\W")
  
  # we need to get our data back into atomic (vector) form to do
  # subsequent operations, so here we'll move our list data
  # back into vector space.
  var_text_words_vec <- unlist(var_text_words_list)
  
  # make sure that none of our vector elements are blank
  # by subsetting the vector for anything that is not a
  # blank space.  Remember that "!=" means "not equal to."
  # Also note that here we are re-assigning the vector to 
  # itself rather than creating a whole new vector.
  var_text_words_vec <- var_text_words_vec[which(var_text_words_vec!="")]
  
  # define stop words for removal as desired
  var_droplist_vector <- c("and", "then", "the")
  
  # remove items in the droplist from the vector of
  # extracted named entities
  var_text_words_vec <- var_text_words_vec[! var_text_words_vec %in% var_droplist_vector ]
  
  
  # add our text data to our corpus list [var_corpus_list]
  # using the name of the file as the column name for the text.
  var_corpus_list[[var_files_vector[var_iteration_int]]] <- var_text_words_vec
}

# show the first 25 words of text # 1.  Change the first
# index [1] to [x] to see the words from the "x"th text
# loaded.
var_corpus_list[[1]][1:100]

# loop through each column of the list (each text)
# and calculate the words that surround each instance
# of the word of interest.
for(var_iteration_int in 1:length(var_corpus_list)){
  
  # get the positions of the word of interest
  # in the working text.
  var_positions_vec <- which(var_corpus_list[[var_iteration_int]][]==var_wordofinterest_str)
  
  # check to see if the token was found at all
  # by seing if our positions vector has any
  # elements
  if (length(var_positions_vec) > 0) {
  
    # process each position where the
    # word of interest is found, gentering
    # vector of KWiC.
    for(var_sub_iteration_int in 1:length(var_positions_vec)){
      
      # setup the start position for the sub vector.
      var_startpos_int = var_positions_vec[var_sub_iteration_int] - var_range_int
      
      # adjust start position if range extends beyond
      # first word of the document.
      while (var_startpos_int < 1) {
        var_startpos_int = var_startpos_int + 1
      }
      
      # setup the end position for the sub vector.
      var_endpos_int = (var_startpos_int + (var_range_int*2) +1)
      
      # make sure that the end position isn't past
      # the last word
      while (var_endpos_int < length(var_positions_vec)) {
        var_endpos_int = var_endpos_int - 1
      }    
      
      # create the sub vector of word in context
      # for this occurrence of the word of interest.
      var_kwic_vec <- var_corpus_list[[var_iteration_int]][var_startpos_int:var_endpos_int]
      
      # colapse the vector into a csv string.
      var_kwic_str <- paste(var_kwic_vec, collapse=", ")
      
      # print the KWiC csv string.
      print(var_kwic_str)
    }
  
  }
  
}

