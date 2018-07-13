# Script: scrape_newsbank_with_rvest.R
# 
# A script written and distributed as a teaching
# aid for demonstrating how to scrape a websit
# using rvest sessions that allow us to maintain
# login sessions and pass cookies, etc.
#
# The easiest way to use this method is to duplicate
# all of the session and cookie information from
# a browser session.  Instructions for how to
# do this in Chrome.  Other browsers will be similar.
#
# Step 1: Visit the website you want to scrape.
#
# Setp 2: Login/Authenticate
# 
# Step 3: Browse/Search/Navigate the site until
# you get to a page that is representative of
# a page that you would scrape.
#
# Step 4: Click on the menu icon (three vertica dots
# or the classic 'hamburger' of stacked lines, 
# depending on your version of Chrome) in the upper
# right corner over the brwoser window.
#
# Step 5: Choose the 'Settings' menu option.
#
# Step 6: Expand the 'Advanced' link at bottom of view.
#
# Step 7: Expand the "Content Settings" option in the
# "Privacy and Security" area of the view.
#
# Step 8: Select "Cookies".
#
# Step 9: Select "See all cookies and data".
#
# Step 10: Copy every cookie name/value pair into the 
# request object in the code below on line 93.
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
# (2) In order to facilitate distinguishing between
# variables, functions, and objects all objects in 
# the code begin with the "obj_" prefix.
#
# (3) Locally defined functions begin with the 
# function_ prefix
#
# Copyright Carl G. Stahmer - 2018
# Director of Data and Digital Scholarship - UC Davis Library
# Associate Director for Humanities - Data Science Initiative
# Associate Director - English Broadside Ballad Archive
#
# This work is licensed under a Creative Commons 
# Attribution-ShareAlike 4.0 International License.
#
# see http://creativecommons.org/licenses/by-sa/4.0/
#
# for information on the rvest package see
# https://cran.r-project.org/web/packages/rvest/rvest.pdf

# Install packages as needed
install.packages("selectr")
install.packages("rvest")
install.packages("httr")
install.packages("XML")

# Load required libraries
library(rvest)
library(httr)
library(XML)

# Specifying the url for the site to be scraped
var_url <- 'http://infoweb.newsbank.com/resources/search/nb?p=AWNB&b=results&action=search&t=country%3AUSA%21USA&fld0=alltext&val0=dissent&bln1=AND&fld1=alltext&val1=protest&sort=YMD_date%3AD'

# Before we can make our reqeust, we need to mimic a session

# Make request as a session after setting cookie vriables
obj_session <- html_session(var_url, set_cookies(
  'SSESSf5a897cf3b8f2872d0e6fe6c381635b0'='czCsfe5j0j0VuoqYFVkQnJBrxQ5te_rrHNRdkcXFdA4'
  , '_ga'='GA1.3.488575821.1531484241'
  , '_gid'='GA1.3.771802559.1531484241'
  , '_ga'='GA1.3.1451833754.1524086183'
  , 'has_js'='1'))

# See the results
str(obj_session)

# See some content
obj_body <- html_nodes(obj_session, "body")
obj_divs <- html_nodes(obj_session, "div")

# Extract the nodeset to a text blob for kicks
obj_html_text <- html_text(obj_body)

