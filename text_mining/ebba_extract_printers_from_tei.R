

# load libraries
library("XML")

# read .csv file (ballad_xml table export as .csv)
var_ballads_from_sql <- read.csv("~/Downloads/ballad_xml.csv")

#make an empty vector to hold the publisher data
var_publishers <- character()


# loop through each TEI object, extract publisher,
# and add it to the list if found,
for(obj_this_ballad in 1:nrow(var_ballads_from_sql)) {
    var_pubvalue <- NULL
    print(paste(obj_this_ballad, ": Processing Ballad ", var_ballads_from_sql[obj_this_ballad,1], sep=""))
    try(var_single_xml_doc <- xmlTreeParse(var_ballads_from_sql[obj_this_ballad,2], asText = TRUE))
    try(var_tei_list <- xmlToList(var_single_xml_doc, addAttributes = FALSE))
    try(var_pubvalue <- var_tei_list$teiHeader$fileDesc$sourceDesc[8][[1]]$publisher.orig)
    if (!is.null(var_pubvalue)) {
      var_publishers <- c(var_publishers, var_pubvalue)
      print(var_pubvalue)
    }
    
    
    #print(paste(obj_this_ballad, ": Skipping Ballad", var_ballads_from_sql[obj_this_ballad,1], sep=""))
}

# loop through all the found printers
for (thisimprint in 1:length(var_publishers)) {
 # print(var_publishers[thisimprint])
  
  # setup a vector to hold the imprints
  individual_printers <- character()
  
  # see if the imprint indicates multiple printers
  var_pos = grep(',|and|&', var_publishers[thisimprint])
  
  # if there are multiple printers then process
  if (length(var_pos) > 0) {
    
    # process commas
    var_comma_elements <- trimws(unlist(strsplit(var_publishers[thisimprint], ",")))
    
    # process 'and'
    var_and_elements_final <- character()
    for (thiprinterslice in 1:length(var_comma_elements)) {
      var_and_elements <- trimws(unlist(strsplit(var_comma_elements[thiprinterslice], "and")))
      var_and_elements_final <- c(var_and_elements_final, var_and_elements)
    }
    
    # process '&'
    var_amp_elements <- character()
    for (thisslice in 1:length(var_and_elements_final)) {
      var_amp_temp_elements <- trimws(unlist(strsplit(var_and_elements_final[thisslice], "&")))
      var_amp_elements <- c(var_amp_elements, var_amp_temp_elements)
    } 
    
    print(var_publishers[thisimprint])
    print(var_amp_elements)
  }
}



