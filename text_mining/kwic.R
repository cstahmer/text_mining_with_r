# Script: kwic.R
# 
# A script written and distributed as a teaching
# aid for demonstrating how to perform corpus
# stemming in R.  The script loads files from 
# a directory into a corpus and performs keyword
# in context analysis across the corpus based
# on a configured word of interest.
#
# Copyright Carl G Stahmer
#
# This work is licensed under a Creative Commons 
# Attribution-ShareAlike 4.0 International License.
#
# see http://creativecommons.org/licenses/by-sa/4.0/


###################################
#         configuration           #
###################################

# Set working directory
setwd("~/Documents/rstudio_workspace/digitalmethods/text_mining/")

# Define the input directory for the texts to be analyzed.
var_input_dir <- "data/plainText"

# Define a word of interest that you want to plot.
var_wordofinterest_str = "dog"

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

# Get a list of all files in the working directory
# that have the appropriate file suffix.
var_files_vector <- dir(var_input_dir, "\\.txt$")

# Send our files vector to our function_show_vector
# function to print to display.
function_show_vector(var_files_vector)

# Setup an empty list container to hold the word vectors
# for each text in the working diretory.
var_corpus_list <- list()

# Now loop through all files listed in the files vector
# and open and process them.

for(var_iteration_int in 1:length(var_files_vector)){
  
  # Define the file path to the file to read by concatenating 
  # (joining) the var_input_dir to the filename contained in 
  # the vector element being processed (element[var_iteration_int])
  # and putting a forward shlash between them so that the result
  # is a correct file path.
  var_filepath_str <- paste(var_input_dir, var_files_vector[var_iteration_int], sep="/")
  
  # Read the file into a character vector using the scan funtion, spliting 
  # the input on newlines ("\n" is unix shorthand for newline) so that each
  # element in the vector is a single line of text.
  var_text_lines_vec <- scan(var_filepath_str, what="character", sep="\n") 
  
  # n=Now create a new vector that holds the entire text as
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
  
  # Now that we have our blob, convert it to lowercase so that
  # later comparison doesnt treat "Cat" and "cat" as different.
  var_text_lower_vec <- tolower(var_text_vec)
  
  # Now split the blob character vector on character that is not
  # a word.  "\W" is unix/regex shorthand for anytying not a
  # non blank space character.  Because we want R to read the "\"
  # as character itself and not as an escape character, we need to 
  # escape it, so we write "\W" as "\\W".  Note that the output
  # of strsplit() is a list and not a vector. A list can contain
  # elements of different types ([1]=charachter, [2]=integer, etc.)
  # whereas a vector is atomic and each element must be of the same
  # base data type.
  var_text_words_list <- strsplit(var_text_lower_vec, "\\W")
  
  # We need to get our data back into atomic (vector) form to do
  # subsequent operations, so here we'll move our list data
  # back into vector space.
  var_text_words_vec <- unlist(var_text_words_list)
  
  # Make sure that none of our vector elements are blank
  # by subsetting the vector for anything that is not a
  # blank space.  Remember that "!=" means "not equal to."
  # Also note that here we are re-assigning the vector to 
  # itself rather than creating a whole new vector.
  var_text_words_vec <- var_text_words_vec[which(var_text_words_vec!="")]
  
  # Now add our text data to our corpus list [var_corpus_list]
  # using the name of the file as the column name for the text.
  var_corpus_list[[var_files_vector[var_iteration_int]]] <- var_text_words_vec
}

# Show the first 25 words of text # 1.  Change the first
# index [1] to [x] to see the words from the "x"th text
# loaded.
var_corpus_list[[1]][1:100]

# Now loop through each column of the list (each text)
# and calculate the words that surround each instance
# of the word of interest.
for(var_iteration_int in 1:length(var_corpus_list)){
  
  # Now get the positions of the word of interest
  # in the working text.
  var_positions_vec <- which(var_corpus_list[[var_iteration_int]][]==var_wordofinterest_str)
  
  # Now process each position where the
  # word of interest is found, gentering
  # vector of KWiC.
  for(var_sub_iteration_int in 1:length(var_positions_vec)){
    
    # Setup the start position for the sub vector.
    var_startpos_int = var_positions_vec[var_sub_iteration_int] - var_range_int
    
    # Adjust start position if range extends beyond
    # first word of the document.
    while (var_startpos_int < 1) {
      var_startpos_int = var_startpos_int + 1
    }
    
    # Setup the end position for the sub vector.
    var_endpos_int = (var_startpos_int + (var_range_int*2) +1)
    
    # Make sure that the end position isn't past
    # the last word
    while (var_endpos_int < length(var_positions_vec)) {
      var_endpos_int = var_endpos_int - 1
    }    
    
    # Create the sub vector of word in context
    # for this occurrence of the word of interest.
    var_kwic_vec <- var_corpus_list[[var_iteration_int]][var_startpos_int:var_endpos_int]
    
    # Colapse the vector into a csv string.
    var_kwic_str <- paste(var_kwic_vec, collapse=", ")
    
    # Print the KWiC csv string.
    print(var_kwic_str)
  }
  
}

