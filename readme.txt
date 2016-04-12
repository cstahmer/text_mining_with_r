This repository contains a collection R scripts 
that demonstrate basic text mining functions.
The scrips were developed by Carl G Stahmer,
Director of Digital Scholarship at UC Davis
as a resource for students who wish to develop 
text mining skills. (http://www.carlstahmer.com) 

All code is made available under a
CC BY 3.0 US license.  
See https://creativecommons.org/licenses/by/3.0/us/

Files include:

scrape_ebba.R
	A file that conducts a search of the English Broadside Ballad Archive,
	scrapes the TEI for each item returned, and then saves the body of 
	the text as plain-text.  It demonstrates several R features, including
	getting xml data from the web, using XPath to extract particular
	elements from the returned content, cleaning returned content through
	gsub, and writing of files to the filesystem.  This script relies on
	the XML package developed by Duncan Temple Lang.  Before you can run
	it you need to install.packages("XML")
	

