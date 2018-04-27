# Script: topic_model.R
# 
# A script written and distributed as a teaching
# aid for demonstrating how to perform corpus
# stemming in R.  The script constructs a corpus
# of texts from a files in a directory and creates
# a configurable topic model.
#
# Copyright Carl G Stahmer
#
# This work is licensed under a Creative Commons 
# Attribution-ShareAlike 4.0 International License.
#
# see http://creativecommons.org/licenses/by-sa/4.0/

# Decoding Mallet Training Output: 
#
# "INFO: <50> LL/token: -8.92223"
#
# <x> = the training iteration
# LL/token =  Log-Liklihood of the topic inference 
#             divided by the total number of topic.
#             The lareger the number, the more accurate
#             the inference is estimated to be.  Note
#             that this is often a negative number like
#             -8.92223, in which case -8.8756 > -8.9223
#         
# "37	0.11628	brain regions activation frontal ms addiction reward"
# 
# 37 = the topic number
# 0.11628 = the distribution probability for the topic
# "brain regions activation..." = the top tokens in the topic

install.packages("mallet")
install.packages("wordcloud")

library(mallet)
library(wordcloud)

###################################
#         configuration           #
###################################

# Set working directory
setwd("/Users/cstahmer/workspaces/rstudio_workspace/text_mining_with_r/")

# Set directory where files live
input_directory_var <- "data/cleanText/"

# Set the Dirichlet alpha parameter
# The lower the value (can be negative)
# the more words in distribution that will
# have probabilities approximating 0. It
# primarily affects the distribution of words
# across the topic (which has a subsequent
# effect on topics across the corpus).
alpha_param_var <- 40

# Hyperperameter that determines the strongly
# the alpha parameter affects the overal model.
# Each time you run an optimization, the topic
# probability distribution departs farther from
# the initial homogeneous distribution.  It
# primarily affects the distribution of topis
# across the corpus (which has a subsequent
# effect on words in each topic). If your goal 
# is to identify small numbers of texts about
# specific themes in a large collection, then a 
# lot of opimization may be good. However, if 
# your goal is to identify topics typical of 
# certain authors, periods, genres or some other 
# reasonably large subset of your collection, 
# then optimize a bit less.
hyperparameter_var <- 80

# Set the number of training iterations.  Trade-off
# between speed and quality. 500 is suggested baseline
# for up to 10,000 articles.
tranining_iterations_var <- 100

# Set number of topics.
number_of_topics_var <- 43

###################################
#        Operational Code         #
###################################

#load the files from the path into a vector
files.v <- dir(path=input_directory_var, pattern="*")

# set up a documents data frame
documents <- data.frame(x = character(length(files.v)), y = character(length(files.v)), stringsAsFactors = FALSE)

# loop through all of the documents in the input
# directory and load them into a documents dataframe
for(i in 1:length(files.v)) {
  
  #construct the file path
  filePath <- paste(input_directory_var, "/", files.v[i], sep="")
  
  #load the file in a character vector
  text.v <- scan(filePath, what="character", sep="\n")
  
  # convert to UTF-8 encoding
  text.v <- iconv(text.v,"WINDOWS-1252","UTF-8")
  
  #convert everything to lower case
  text.lower.v <- tolower(text.v)
  
  #convert to a single string
  text.s <- paste(text.lower.v, collapse = ' ')
  
  #load into the data frame
  documents$x[i] <- files.v[i]
  documents$y[i] <- text.s
  
}

#now instanciate the mallet object
mallet.instances <- mallet.import(documents$x, documents$y,
                                  "text_mining/stoplist.csv",
                                  FALSE,
                                  token.regexp="[\\p{L}']+")

#now setup a trainer
topic.model <- MalletLDA(num.topics = number_of_topics_var)

#now load the docs
topic.model$loadDocuments(mallet.instances)

#get entire vocab if you want it
vocabulary <- topic.model$getVocabulary()

#get word frequency info
word.freqs <- mallet.word.freqs(topic.model)

#tweak number of burn-in iterations and
#interations between optimizations
topic.model$setAlphaOptimization(alpha_param_var, hyperparameter_var)

#set number of training iterations.
#in theory, the higher the better
topic.model$train(tranining_iterations_var)

#########################################
# Get a dataframe that shows weighted   #
# probabilities of all words per topic. #
#########################################

# Get a matrix of words as columns and
# topics as rows
topic.words.m <- mallet.topic.words(topic.model, smoothed=TRUE,normalized=TRUE)

# Get the actual words themselves
vocabulary <- topic.model$getVocabulary()

# Replace the column labels with the actual 
# word to which each column applies
colnames(topic.words.m) <- vocabulary

# Convert the matrix to a dataframe
topic.words.df <- as.data.frame(topic.words.m)

#########################################
# Get a dataframe that shows weighted   #
# probabilities of all topics per text. #
#########################################

#calculate the probability that a topic appears in a each text
doc.topics.m <- mallet.doc.topics(topic.model, smoothed=TRUE,normalized=TRUE)

# get the filenames into a vector
file.ids.v <- documents[,1] 

# get the topics into a data frame
doc.topics.df <- as.data.frame(doc.topics.m)

# set the rownames of the topics dataframe to
# to the filenames
row.names(doc.topics.df) <- file.ids.v


################################################
# The code below this comment block is for     #
# demonstration purposes.  It lets you         #
# randomly select a topic in the model         #
# matrix and then creates a weighted           # 
# wordcloud of the top 100 words in that       # 
# topic.  IRL, this could be made more useful  #
# if you picked a topic with purpose.  For     #
# example, you might find a topic that had     #
# a high probability for two or three          # 
# identified words                             #
################################################

#prepare  matrix for visualization of top topic words
topic.top.words <- mallet.top.words(topic.model, topic.words.m[8,], 50)

#now draw a wordcloud of the top word topics
wordcloud(topic.top.words$words, topic.top.words$weights, c(4,.8), rot.per=0, random.order=F)



