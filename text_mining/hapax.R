# Script: hapax.R
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
# See http://creativecommons.org/licenses/by-sa/4.0/


###################################
#         configuration           #
###################################

# Set working directory
setwd("~/Documents/rstudio_workspace/digitalmethods/text_mining/")

# Define the input directory for the texts to be analyzed.
var_input_dir <- "data/plainText"

###################################
#      function declarations      #
###################################

# A callable function that writes out the contents
# of a vector in human readable form.
function_show_vector <- function(var_vec_to_show_vector) {
  for(i in 1:length(var_vec_to_show_vector)) { 
      cat(i, var_vec_to_show_vector[i], "\n", sep=" ")
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

# Setup an empty list container to hold the raw
# frequency counts for each text
var_text_raw_freqs_list <- list()

# Setup an empty list container to hold the relative
# frequency counts for each text
var_text_freqs_relative_list <- list()

# Loop through all files listed in the files vector
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
  
  # Create a new vector that holds the entire text as
  # as a blob (one long running chunk of text).  This is 
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
  
  # Split the blob character vector on character that is not
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
  
  # Add our text data to our corpus list [var_corpus_list]
  # using the name of the file as the column name for the text.
  var_corpus_list[[var_files_vector[var_iteration_int]]] <- var_text_words_vec
  
  # Create a word frequency table for each text
  var_text_freqs_table <- table(var_text_words_vec)
  
  # Put raw frequency counts into a list
  var_text_raw_freqs_list[[var_files_vector[var_iteration_int]]] <- var_text_freqs_table
  
  # Calculate a relative frequency value for each item
  var_text_freqs_rel_table <- 100*(var_text_freqs_table/sum(var_text_freqs_table))
  
  # Convert the table to a list
  var_text_freqs_relative_list[[var_files_vector[var_iteration_int]]] <- var_text_freqs_rel_table

}

# Show the first x words of text # 1.  Change the first
# index [1] to [x] to see the words from the "x"th text
# loaded.
var_corpus_list[[1]][1:3]
var_text_raw_freqs_list[[1]][1:3]
var_text_freqs_relative_list[[1]][1:3]

# For each item in our freqency count list
# return the sum of the values that are equal
# to 1 to a vector.  This sum represents a 
# count of the number of words in the text
# that appear only once
var_text_hapax_vector <- sapply(var_text_raw_freqs_list, function(x) sum(x == 1))

# Print out the non-relative hapax scores
var_text_hapax_vector

# Calculate word totals for each text so
# that we can calculate relative hpax scors
var_text_lengths_matrix <- do.call(rbind, lapply(var_text_raw_freqs_list,sum))

# Calculate relative hapax scores
var_relative_hapax_matrix <- var_text_hapax_vector / var_text_lengths_matrix

# Print out relative hapax scores
var_relative_hapax_matrix

# Plot hapax richness
barplot(var_relative_hapax_matrix, beside=T,col="grey", names.arg = row.names(var_relative_hapax_matrix))


