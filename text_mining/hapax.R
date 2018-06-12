# Script: hapax.R
# 
# A script written and distributed as a teaching
# aid for demonstrating how to perform corpus
# stemming in R.  The script loads files from 
# a directory into a corpus and performs analysis
# of the linguistic complexity of the works.
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

###################################
#         configuration           #
###################################

# Define the input directory for the texts to be analyzed.
var_inputDir_string <- "/Users/cstahmer/workspaces/rstudio_workspace/text_mining_with_r/data/plainText"

###################################
#      function declarations      #
###################################

# A callable function that writes out the contents
# of a vector in human readable form.
function_showVector <- function(var_vec_to_show_vector) {
  for(i in 1:length(var_vec_to_show_vector)) { 
      cat(i, var_vec_to_show_vector[i], "\n", sep=" ")
  } 
}

###################################
#        Operational Code         #
###################################

# Get a list of all files in the working directory
# that have the appropriate file suffix.
var_files_vector <- dir(var_inputDir_string, "\\.txt$")

# Send our files vector to our function_showVector
# function to print to display.
function_showVector(var_files_vector)

# Setup an empty list container to hold the word vectors
# for each text in the working diretory.
var_corpus_list <- list()

# Setup an empty list container to hold the raw
# frequency counts for each text
var_textRawFreqs_list <- list()

# Setup an empty list container to hold the relative
# frequency counts for each text
var_relativeTextFrequencies_list <- list()

# Loop through all files listed in the files vector
# and open and process them.
for(var_iteration_int in 1:length(var_files_vector)){
  
  # Define the file path to the file to read by concatenating 
  # (joining) the var_inputDir_string to the filename contained in 
  # the vector element being processed (element[var_iteration_int])
  # and putting a forward shlash between them so that the result
  # is a correct file path.
  var_filepath_string <- paste(var_inputDir_string, var_files_vector[var_iteration_int], sep="/")
  
  # Read the file into a character vector using the scan funtion, spliting 
  # the input on newlines ("\n" is unix shorthand for newline) so that each
  # element in the vector is a single line of text.
  var_textLines_vector <- scan(var_filepath_string, what="character", sep="\n") 
  
  # Create a new vector that holds the entire text as
  # as a blob (one long running chunk of text).  This is 
  # accomplished by joining (using collapse) the vector of 
  # lines (var_text_raw_vec), putting a space between each
  # each element/line.  The result is a charactre vector
  # containing one element containing the complete text as a
  # blob.
  var_text_vector <- paste(var_textLines_vector, collapse=" ")
  
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
  var_textLowerCase_vec <- tolower(var_text_vector)
  
  # Split the blob character vector on character that is not
  # a word.  "\W" is unix/regex shorthand for anytying not a
  # non blank space character.  Because we want R to read the "\"
  # as character itself and not as an escape character, we need to 
  # escape it, so we write "\W" as "\\W".  Note that the output
  # of strsplit() is a list and not a vector. A list can contain
  # elements of different types ([1]=charachter, [2]=integer, etc.)
  # whereas a vector is atomic and each element must be of the same
  # base data type.
  var_textWords_list <- strsplit(var_textLowerCase_vec, "\\W")
  
  # We need to get our data back into atomic (vector) form to do
  # subsequent operations, so here we'll move our list data
  # back into vector space.
  var_textWords_vector <- unlist(var_textWords_list)
  
  # Make sure that none of our vector elements are blank
  # by subsetting the vector for anything that is not a
  # blank space.  Remember that "!=" means "not equal to."
  # Also note that here we are re-assigning the vector to 
  # itself rather than creating a whole new vector.
  var_textWords_vector <- var_textWords_vector[which(var_textWords_vector!="")]
  
  # Add our text data to our corpus list [var_corpus_list]
  # using the name of the file as the column name for the text.
  var_corpus_list[[var_files_vector[var_iteration_int]]] <- var_textWords_vector
  
  # Create a word frequency table for each text
  var_textFrequencies_table <- table(var_textWords_vector)
  
  # Put raw frequency counts into a list
  var_textRawFreqs_list[[var_files_vector[var_iteration_int]]] <- var_textFrequencies_table
  
  # Calculate a relative frequency value for each item
  var_relativeTextFrequencies_table <- 100*(var_textFrequencies_table/sum(var_textFrequencies_table))
  
  # Convert the table to a list
  var_relativeTextFrequencies_list[[var_files_vector[var_iteration_int]]] <- var_relativeTextFrequencies_table

}

# Show the first x words of text # 1.  Change the first
# index [1] to [x] to see the words from the "x"th text
# loaded.
var_corpus_list[[1]][1:3]
var_textRawFreqs_list[[1]][1:3]
var_relativeTextFrequencies_list[[1]][1:3]

# For each item in our freqency count list
# return the sum of the values that are equal
# to 1 to a vector.  This sum represents a 
# count of the number of words in the text
# that appear only once
var_textHapax_vector <- sapply(var_textRawFreqs_list, function(x) sum(x == 1))

# Print out the non-relative hapax scores
var_textHapax_vector

# Calculate word totals for each text so
# that we can calculate relative hpax scors
var_textLengths_matrix <- do.call(rbind, lapply(var_textRawFreqs_list,sum))

# Calculate relative hapax scores
var_relativeHapax_matrix <- var_textHapax_vector / var_textLengths_matrix

# Print out relative hapax scores
var_relativeHapax_matrix

# Plot hapax richness
barplot(var_relativeHapax_matrix, beside=T,col="grey", names.arg = row.names(var_relativeHapax_matrix))


