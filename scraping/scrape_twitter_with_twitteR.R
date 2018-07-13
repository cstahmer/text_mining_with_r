# Script: scrape_twitter_with_twitteR.R
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

# Install the library and dependencies if needed
# install.packages("bit")
# install.packages("bit64")
# install.packages("rjson")
# install.packages("DBI")
# install.packages("twitteR")

# tell R you want to use the library
library(twitteR)


# change the next four lines based on your own consumer_key, consume_secret, access_token, and access_secret. 
var_consumer_key <- "xUW4s7yEW2GFEX8j406c69rkD"
var_consumer_secret <- "57yG3pJmw7zULrAmOBUzpXZLxDhBJJFEkIpGmxCKIAJ4GQNneU"
var_access_token <- "336585907-oW0CJNfjWeRqfKYCioFeN1qXhqIEKkO9TWR8uVlg"
var_access_secret <- "4g47YIvWXuHPYxnBRo87bCqHrIzc9VSsXd6YJiJCWFC2G"

# setup the twitter api object
setup_twitter_oauth(var_consumer_key, var_consumer_secret, var_access_token, var_access_secret)

# make a query.  See documentation here: https://cran.r-project.org/web/packages/twitteR/twitteR.pdf
obj_tw = twitteR::searchTwitter('#realDonaldTrump + #HillaryClinton', n = 15, since = '2018-7-01', retryOnRateLimit = 1e3)

# convert to dataframe
var_tweets_df = twitteR::twListToDF(obj_tw)

# show the strcture of the API return
str(var_tweets_df)

# show one of the text elements
var_tweets_df$text[13]

# get all of the text elements into a vector
var_text <- var_tweets_df$text

# subset a dataframe of texts and number of retweets
var_text_and_retweets <- var_tweets_df[,c('text','retweetCount')]

# get a particular user's timeline
obj_user_timeline <- userTimeline('flotus', n=15, maxID=NULL, sinceID=NULL, includeRts=FALSE, excludeReplies=FALSE)

# convert to dataframe
var_user_timeline <- twitteR::twListToDF(obj_user_timeline)

# get a user object
obj_user <- getUser('flotus')

# lookup info from the user object
obj_user_info <- lookupUsers('flotus')

# convert to dataframe
var_user_info <- twitteR::twListToDF(obj_user_info)

# get friends
obj_friends <- friendships(screen_names = c("flotus"))
