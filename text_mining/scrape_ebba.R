# Script: distribution_analysis.R
# 
# A script written and distributed as a teaching
# aid that takes an EBBA search string and 
# performs the following functions:
#
#   1.  Loads search return pages from the English Braodside Ballad Archive
#   2.  Extrats the ID's of search return page using XPath
#   3.  For each ID, loads the TEI text of the ballad 
#   4.  Extracts the <body> from the TEI text
#   5.  Removes all tags from the body
#   6.  Saves the resulting text to a file.
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

###################################
#         configuration           #
###################################

# set working directory
setwd("~/Documents/rstudio_workspace/digitalmethods/text_mining/") 

# define an output path for the Text files 
var_textOutputFilePath_character = "/Users/cstahmer/ballad_text2/ballads_text/"

# define the URL for the search
var_searchURL_character <- "http://ebba.english.ucsb.edu/search_combined/?ss=king&p="

# define the start of the page range for the search
var_searchStartRange_int <- 1

# define an end to the page range for the search
var_searchEndRange_int <- 1


###################################
#        Operational Code         #
###################################

# Initialize process
print("Process Started")

# Define a range of search return pages to get
for (var_page_int in var_searchStartRange_int:var_searchEndRange_int) {
  
  # define the URL for the search
  var_iterationSearchUrl_character <- paste(var_searchURL_character, var_page_int, sep="")
  
  # go out over the web and get the contents of the URL
  var_searchRet_doc <- htmlParse(var_iterationSearchUrl_character)
  
  # use XPath to extract all the ebba-ids that appear in 
  # <span class="ebba-id">xxxxxxxxxxx</span> and load 
  # them into a list
  var_resultList_list <- lapply(var_searchRet_doc['//span[@class="ebba-id"]'],xmlValue)
  
  # loop through each extracted id in the list
  for(var_balladId_character in var_resultList_list) {
    
    # remove all non-numeric characters from the 
    # id string so that you are left with 
    # the id number only
    var_balladIdClean_character <- gsub("\\W+", "", var_balladId_character, TRUE)
  
    # print a processing message
    print(paste("Processing ballad: ", var_balladIdClean_character, sep=""))
    
    # construct the URL to get the actual ballad TEI
    var_textURL_character <- paste("http://ebba.english.ucsb.edu/ballad/", var_balladIdClean_character, "/ebba-xml-", var_balladIdClean_character, sep="")
    
    # go get the TEI from the internet load into an XML doc
    obj_tei_doc <- htmlParse(var_textURL_character)
    
    # use XPath to extract the title
    var_balladTitle_character <- lapply(obj_tei_doc['//title[@type="main"]'],xmlValue)
    print(paste("     ", var_balladTitle_character))
    
    # use XPath to extract the <div type="ballad"></div>
    obj_balladTeiText_list <- lapply(obj_tei_doc['//div[@type="ballad"]'],xmlValue)

    # remove all tei tags from the text
    var_balladPlainText_character <- gsub("<.*?>", " ", obj_balladTeiText_list)
    
    # convert multiple spaces to single spaces
    var_balladPlainTextTrimmed_character <- gsub("\\s+", " ", var_balladPlainText_character, TRUE)

    # define a file path where to save the plain text
    var_writeFilePath_character <- paste(var_textOutputFilePath_character, var_balladIdClean_character, ".txt", sep="")

    # open a file writing connection
    fileConn<-file(var_writeFilePath_character)
    
    # write the file
    writeLines(var_balladPlainTextTrimmed_character, con = fileConn, sep = " ", useBytes = FALSE)
    
    # close the file writing connection
    close(fileConn)
    
    # do next ballad
  }
  
  # do next search return page
}
print("Process Finished")
print("Good Bye :)")