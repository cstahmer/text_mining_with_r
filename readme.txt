This repository contains R scripts for use in 
Carl Stahmer's Digital Methods graduate Proseminar
at UC Davis.  For course syllabus and information
see http://www.carlstahmer.com/digital-methods/

Files include:

scrape_ebba.R
	A file for conducting a search of the English Broadside Ballad Archive,
	scraping the TEI for each item returned, and then saving the body of 
	the text as plain-text.  It demonstrates several R features, including
	getting xml data from the web, using XPath to extract particular
	elements from the returned content, cleaning returned content through
	gsub, and writing of files to the filesystem.
	

