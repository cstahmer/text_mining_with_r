# Script: scrape_twitter_json_oath.R
# 
# A script written and distributed as a teaching
# aid for demonstrating how to scrape Twitter
# using jsonlite and httr oath authentication
#
# To use the Twitter API you must go to
# https://dev.twitter.com/apps and create 
# a Twitter App.  Use the following values
# for the form you are required to fill:
#
# Name:  <anything that makes sense to you>
# 
# Description:  <a useful description>
#
# Website:  <you can put any website domain here>
#
# Callback URLs: 	http://127.0.0.1:1410
#
# Note that form the form values listed above
# you should replace anything inside < > brackets
# with something that you makes sense to you as
# directed.  DO NOT INCLUDE THE < > brackets in 
# your entry.
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

# Install packages as needed
# install.packages("jsonlite")
# install.packages("httr")

# Attache libraries
library(jsonlite)
library(httr)

# Copy the athentication key from your Twitter App.  To create or
# manage an app, go to https://dev.twitter.com/apps.  See header
# comments above for more detail.
var_consumer_key = "xUW4s7yEW2GFEX8j406c69rkD";
var_consumer_secret = "57yG3pJmw7zULrAmOBUzpXZLxDhBJJFEkIpGmxCKIAJ4GQNneU";

#Use basic auth to generate a session and credentials
obj_secret <- jsonlite::base64_enc(paste(var_consumer_key, var_consumer_secret, sep = ":"))
obj_req <- httr::POST("https://api.twitter.com/oauth2/token",
                  httr::add_headers(
                    "Authorization" = paste("Basic", gsub("\n", "", obj_secret)),
                    "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"
                  ),
                  body = "grant_type=client_credentials"
);

# Extract the credentials
httr::stop_for_status(obj_req, "authenticate with twitter")
obj_token <- paste("Bearer", httr::content(obj_req)$access_token)

# Define the API query
var_url <- "https://api.twitter.com/1.1/statuses/user_timeline.json?count=10&screen_name=Rbloggers"

# Execute the API query
obj_req <- httr::GET(var_url, httr::add_headers(Authorization = obj_token))

# Extract the JSON content from the request return
obj_json <- httr::content(obj_req, as = "text")

# Convert from JSON object to data.frame for easier processing
var_tweets <- fromJSON(obj_json)

# look at some text
var_tweets$text