# load needed library
library(tm)

# set working directory
setwd("~/Documents/rstudio_workspace/digitalmethods/text_mining")

# Assign a file path
filePath <- "data/plainText/melville.txt"

# load the file
text.v <- scan(filePath, what="character", sep="\n")
# text.v <- scan("http://www.gutenberg.org/cache/epub/2701/pg2701.txt", what="character", sep="\n")

# convert everything to lower case
text.lower.v <- tolower(text.v)

# remove stopwords
text.lower.v <- removeWords(text.lower.v, stopwords("english"))

# tokenize to words in a list
text.words.l <- strsplit(text.lower.v, "\\W")

# convert my list to a vector
text.word.v <- unlist(text.words.l)

# return vector of slices in list that aren't blank
noblanks.v <- which(text.word.v!="")

# rewrite text vector with only non-blank slices
text.word.v <- text.word.v[noblanks.v]

# get the length of the words vector
# tells us how many words we have
totalwords.i <- length(text.word.v)

# tells us how many words we have
print(totalwords.i)

# get the number of times a specific word appears in the text
wordcount.single.i <- length(text.word.v[which(text.word.v=="must")])

# tell us how many times our word of interest appears
print(wordcount.single.i)

# calculate the percentage frequency of tested word
word.percent.occurence.i <- wordcount.single.i / totalwords.i

# tell us the percentage occurrence
print(word.percent.occurence.i)

# get a word frequency table for all words
text.freqs.t <- table(text.word.v)

# sort the frequency table
sorted.text.freqs.t <- sort(text.freqs.t , decreasing=TRUE)

# output the first 500 words
print(sorted.text.freqs.t[1:500])

# plot the word frequencies
plot(sorted.text.freqs.t[1:500])

# save the results to a csv file
write.csv(sorted.text.freqs.t, file = "/Users/cstahmer/textmining/word_freq_500.csv")

# mark the end of execution
print("Bye Bye!")

