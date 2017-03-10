# Script: gutenberg_clean.R
# 
# A script written and distributed as a teaching
# aid for demonstrating how to perform corpus
# stemming in R.  The script constructs a corpus
# of texts from a files in a directory and creates
# a configurable topic model.
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

# Set directory where files live
input_directory_var <- "data/plainText"

# Set the output directory
output_directory_var <- "/Users/cstahmer/Desktop/cleantext"


###################################
#        Operational Code         #
###################################

#load the files from the path into a vector
files.v <- dir(path=input_directory_var, pattern="*")

# loop through all of the documents in the input
# directory and load them into a documents dataframe
for(i in 1:length(files.v)) {
  
  #construct the file path
  filePath <- paste(input_directory_var, "/", files.v[i], sep="")
  
  #load the file in a character vector
  text.v <- scan(filePath, what="character", sep="\n")
  
  # convert to UTF-8 encoding
  text.v <- iconv(text.v,"WINDOWS-1252","UTF-8")
  
  # convert everything to lower case
  text.lower.v <- tolower(text.v)
  
  # calculate locations of metadata start and end
  start.v <- which(regexpr(".*start of this.*", text.lower.v) > 0)
  end.v <-  which(regexpr(".*end of.*project gutenberg.*", text.lower.v) > 0)
  
  # extract main content if metadata found
  if(length(start.v) > 0) {
    content.lines.v <- text.v[(start.v[1]+2):(end.v[1]-2)]
  } else {
    content.lines.v <- text.v
  }

  # construct output file path
  outFilePath <- paste(output_directory_var, "/", files.v[i], sep="")

  # write the new file
  write(content.lines.v, outFilePath, sep = "/n")
  
  # print message
  print(paste("Saved File", outFilePath, sep=""))
}

# say goodbye 
print("Goodbye!")


