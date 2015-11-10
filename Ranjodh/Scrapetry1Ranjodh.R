# This Script performs the following functions:
#   1.  Loads search return pages from the English Braodside Ballad Archive
#   2.  Extrats the ID's of search return page
#   3.  For each ID, loads the TEI text of the ballad 
#   4.  Extracts the <body> from the TEI text
#   5.  Removes all tags from the body
#   6.  Saves the resulting text to a file.

# Initialize process
print("Process Started")

# Define a range of 
# search return pages 
# to get
for (page in 1:10) {
  
  # construct a search url based on the iteration
  url <- paste("http://ebba.english.ucsb.edu/search_combined/?ss=king&p=", page, sep="")
  
  # go out over the web and get the contents of the URL
  doc <- htmlParse(url)
  
  # use XPath to extract all the ebba-ids that appear in 
  # <span class="ebba-id">xxxxxxxxxxx</span> and load 
  # them into a list
  result <- lapply(doc['//span[@class="ebba-id"]'],xmlValue)
  
  # loop through each extracted id in the list
  for(ballad_id in result) {
    
    # print the extracted id
    print(paste("Pre-Processed ID: ", ballad_id, sep=""))
    
    # remove all non-numeric characters from the 
    # id string so that you are left with 
    # the id number only
    ballad_id <- gsub("\\W+", "", ballad_id, TRUE)
    
    # print the Post-Processed id
    print(paste("Post-Processed ID: ", ballad_id, sep=""))
    
    # construct the URL to get the actual ballad TEI
    textURL <- paste("http://ebba.english.ucsb.edu/ballad/", ballad_id, "/ebba-xml-", ballad_id, sep="")
    
    # go get the TEI from the internet
    # load into an XML doc
    tei_doc <- htmlParse(textURL)
    
    # use XPath to extract the title
    ballad_title <- lapply(tei_doc['//title[@type="main"]'],xmlValue)
    
    # use XPath to extract the <div type="ballad"></div>
    ballad_text_tei <- lapply(tei_doc['//div[@type="ballad"]'],xmlValue)
    
    # remove all tei tags from the text
    ballad_text_plain <- gsub("<.*?>", " ", ballad_text_tei)
    
    # convert multiple spaces to single spaces
    ballad_text_plain <- gsub("\\s+", " ", ballad_text_plain, TRUE)
    
    # define a file path where to save the text
    file_path <- paste("/ballad_text/", ballad_id, ".txt", sep="")
    
    # open a file writing 
    # connection
    fileConn<-file(file_path)
    
    # write the file
    writeLines(ballad_text_plain, con = fileConn, sep = " ", useBytes = FALSE)
    
    # close the file 
    # writing connection
    close(fileConn)
    
    # do next ballad
  }
  
  # do next search return page
}
print("Process Finished")
print("Good Bye :)")