# install the library if need be
install.packages("twitteR")

# tell R you want to use the library
library(twitteR)


# change the next four lines based on your own consumer_key, consume_secret, access_token, and access_secret. 
consumer_key <- "xUW4s7yEW2GFEX8j406c69rkD"
consumer_secret <- "57yG3pJmw7zULrAmOBUzpXZLxDhBJJFEkIpGmxCKIAJ4GQNneU"
access_token <- "336585907-oW0CJNfjWeRqfKYCioFeN1qXhqIEKkO9TWR8uVlg"
access_secret <- "4g47YIvWXuHPYxnBRo87bCqHrIzc9VSsXd6YJiJCWFC2G"

# setup the twitter api object
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# make a query.  See documentation here: https://cran.r-project.org/web/packages/twitteR/twitteR.pdf
#tw = twitteR::searchTwitter('#realDonaldTrump + #HillaryClinton', n = 1e4, since = '2016-11-08', retryOnRateLimit = 1e3)
tw = twitteR::searchTwitter('#realDonaldTrump + #HillaryClinton', n = 15, since = '2018-7-01', retryOnRateLimit = 1e3)

# convert to dataframe
d = twitteR::twListToDF(tw)

# show the strcture of the API return
str(d)

# show one of the ext elements
d$text[13]

# get all of the text elements into a vector
text <- d$text

# subset of dataframe of texts and number of retweets
text_and_retweets <- d[,c('text','retweetCount')]

# get a particular user's timeline
user_timeline <- userTimeline('flotus', n=15, maxID=NULL, sinceID=NULL, includeRts=FALSE, excludeReplies=FALSE)

# convert to dataframe
ut <- twitteR::twListToDF(user_timeline)

# get a user object
user_object <- getUser('flotus')

# lookup info from the user object
user_info <- lookupUsers('flotus')

# convert to dataframe
ui <- twitteR::twListToDF(user_info)

# get friends
friends <- friendships(screen_names = c("flotus"))
