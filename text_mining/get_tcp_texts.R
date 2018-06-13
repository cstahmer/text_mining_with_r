# Script: get_tcp_texts.R
# 
# A script written and distributed as a teaching
# aid that takes a csv list of TCP ID's,
# retrieves each file from the TCP Git, and then
# saves local TEI and Text versions as well as
# a blob of the entire corpus.  
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

# install.packages("RCurl")
# install.packages("XML")
# install.packages("tm")

library(RCurl)
library(XML)
library(tm)
  
###################################
#         configuration           #
###################################
  
# configure the file that contains the TCP IDs 
var_tcpIDFile_character <- "/Users/cstahmer/workspaces/rstudio_workspace/text_mining_with_r/data/ecco-tcp-ids.csv"

# configure the file path to look for and/or write 
# the cacert.pm file for getting files from github
var_cacert_character <- "/Users/cstahmer/workspaces/rstudio_workspace/text_mining_with_r/data/certs/cacert.pem"

# maximum number to get (if set to zero then get all)
var_numTexts_integer <- 25

# output diretory for TEI files
var_teiOutputDir_character <- "/Users/cstahmer/Desktop/temp/textmining/tcp_tei/"

# output directory for plain text
var_textOutputDir_character <- "/Users/cstahmer/Desktop/temp/textmining/tcp_text/"

# output diretory for corpus blob
var_corpusOutputDir_character <- "/Users/cstahmer/Desktop/temp/textmining/tcp_blob/"

###################################
#      function declarations      #
###################################

# Determine how to grab html for a single input element
function_evaluate_input <- function(input) {    
  
  s1 <- "https://raw.githubusercontent.com/textcreationpartnership/"
  s2 <- "/master/"
  s3 <- ".xml"
  
  strUrl <- paste(s1, input, s2, input, s3, sep = "")
  
  # downolad SSL certificate in case of https problem
  if(!file.exists(var_cacert_character)) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile=var_cacert_character)
  return(getURL(strUrl, followlocation = TRUE, cainfo = var_cacert_character))
  
}

# convert HTML to plain text
function_HtmlToText <- function(html) {
  #doc <- htmlParse(html, asText = TRUE)
  #text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
  text <- gsub("[\r\n\t]", " ", html)
  text <- gsub("<teiHeader>.*?</teiHeader>", '', text)
  text <- gsub("<g ref=\"char:EOLhyphen\"/>", "", text)
  text <- gsub("", "", text)
  text <- gsub("<.*?>", "", text)
  text <- gsub("ſ", "s", text)
  text <- gsub("\\s+", " ", text)
  text <- gsub("&amp", "&", text)
  text <- gsub("&lt", "<", text)
  text <- gsub("&gt", "<", text)
  
  #do tm transformations
  docs <- c(text)
  docs <- VCorpus(VectorSource(docs))
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, stripWhitespace)
  
  #reload text object
  text <-  as.character(docs[[1]])
  
  return(text)
}

# format text vector into one character string
collapse_text <- function(txt) {
  return(paste(txt, collapse = " "))
}

###################################
#        Operational Code         #
###################################

# read in the csv file 
var_tcpids_data.frame <- read.csv(file=var_tcpIDFile_character,head=TRUE,sep=",")

# extract jus the IDs into a list
var_tcpids_data.frame <- var_tcpids_data.frame[2]

# convert the list to a vector
var_tcpids_vector <- as.vector(t(var_tcpids_data.frame))

# cap at maximum number of texts if indicated  
if (var_numTexts_integer > 0) {
    length(var_tcpids_vector) <- var_numTexts_integer
}


# create an empty text blob variable that 
# will hold a munged version of the entire corpus
var_textBlob_character = ""

# loop through each ID in our list and process
for (var_id_character in var_tcpids_vector) {
   
  # write a message to the console letting everyone
  # know that things are happening
  var_message_character <- paste("Scraping ", var_id_character)
  print(var_message_character)
  
   
  # get the content 
  obj_fileLines_list <- lapply(var_id_character, function_evaluate_input)
  
  # convert the list of lines to a vector
  var_fileLines_vector <- unlist(obj_fileLines_list)
  
  # set the TEI output file path
  var_outPath_character <- paste(var_teiOutputDir_character, var_id_character, ".tei", sep="")
  
  # write the TEI file
  fileConn<-file(var_outPath_character)
  writeLines(c(var_fileLines_vector), fileConn)
  close(fileConn)
  
  # convert TEI to plain text / .txt (dumb conversion)
  obj_plainFileLines_list <- lapply(obj_fileLines_list, function_HtmlToText)
  
  # convert the list of lines to a vector
  var_plainFileLines_vector <- unlist(obj_plainFileLines_list)
  
  # set the .txt output path
  var_plainOutPath_character <- paste(var_textOutputDir_character, var_id_character, ".txt", sep="")
  
  # write the text file
  fileConn<-file(var_plainOutPath_character)
  writeLines(c(var_plainFileLines_vector), fileConn)
  close(fileConn)  
  
  var_thisTextBlob_character <- c(var_plainFileLines_vector, sep=" ")
  
  var_textBlob_character <- paste(var_textBlob_character, var_thisTextBlob_character, sep = " ")
}
  
# set the blob output file path
var_blobOutPath_character <- paste(var_corpusOutputDir_character, "corpus_blob.txt", sep="")

# write the text file
fileConn<-file(var_blobOutPath_character)
writeLines(c(var_textBlob_character), fileConn)
close(fileConn)  
