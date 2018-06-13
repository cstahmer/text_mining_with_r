# Script: kwic.R
# 
# A script written and distributed as a teaching
# aid for demonstrating how to perform xpath
# extraction from XML.
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
#
# This work is licensed under a Creative Commons 
# Attribution-ShareAlike 4.0 International License.
#
# see http://creativecommons.org/licenses/by-sa/4.0/

# install.packages("rJava")

library(qdap)
require(XML)

###################################
#         configuration           #
###################################

# Set working directory
setwd("~/Documents/rstudio_workspace/digitalmethods/text_mining/")

# Define the input directory for the texts to be analyzed.
var_inputDir_character <- "/Users/cstahmer/workspaces/rstudio_workspace/text_mining_with_r/data/MEIexamples"

# Define a word of interest that you want to plot.
var_node_str = ""

# Define filename filter
var_filename_filter = ".mei"

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
var_files_vector <- dir(var_inputDir_character, var_filename_filter)
print(length(var_files_vector));




# loop through all files listed in the files vector
# and open and process them.
for(var_iteration_int in 1:length(var_files_vector)){
  #var_filepath_str <- paste(var_inputDir_character, var_files_vector[1], sep="/")
  
  # define the file path to the file to read by concatenating 
  # (joining) the var_inputDir_character to the filename contained in 
  # the vector element being processed (element[var_iteration_int])
  # and putting a forward shlash between them so that the result
  # is a correct file path.
  var_filepath_str <- paste(var_inputDir_character, var_files_vector[var_iteration_int], sep="/")
  print(var_filepath_str)
  
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
  
  # load the XML file
  doc <- xmlParse(var_text_vec)
  root <- xmlRoot(doc)
  listOfMeasureNodes = getNodeSet(doc, "//x:measure/staff", namespaces = "x")
  print(length(listOfMeasureNodes))
}
