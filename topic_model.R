#install.packages("mallet")

print("Hello")

#set directory where files live
inputDirPath <- "/Users/cstahmer/ballad_text_full/Dir4"

#print the directory just to be safe
print(inputDirPath)

#load the files from the path into a vector
files.v <- dir(path=inputDirPath, pattern=".*txt")

# set up a documents data frame
#documents <- data.frame(x = character(length(files.v)), y = character(length(files.v)), stringsAsFactors = FALSE)
documents <- data.frame(x = character(50), y = character(50), stringsAsFactors = FALSE)

#for(i in 1:length(files.v)) {
for(i in 1:50) {
  print(files.v[i])
  filePath <- paste(inputDirPath, "/", files.v[i], sep="")
  print(filePath)
  
  #load the file
  text.v <- scan(filePath, what="character", sep="\n")
  
  text.v <- iconv(text.v,"WINDOWS-1252","UTF-8")
  
  #convert everything to lower case
  text.lower.v <- tolower(text.v)
  
  #convert to a single string
  text.s <- paste(text.lower.v, collapse = ' ')
  
  documents$x[i] <- files.v[i]
  documents$y[i] <- text.s
  
}

#now instanciate the mallet object
mallet.instances <- mallet.import(documents$x, documents$y,
                                  "/Users/cstahmer/ballad_text_full/Dir4/stoplist.csv",
                                  FALSE,
                                  token.regexp="[\\p{L}']+")

#now setup a trainer
topic.model <- MalletLDA(num.topics=43)

#now load the docs
topic.model$loadDocuments(mallet.instances)

#get entire vocab if you want it
vocabulary <- topic.model$getVocabulary()

#get word frequency info
word.freqs <- mallet.word.freqs(topic.model)

#tweak number of burn-in iterations and
#interations between optimizations
topic.model$setAlphaOptimization(40, 80)

#set number of training iterations.
#in theory, the higher the better
topic.model$train(400)

#see the topic word clusters
topic.words.m <- mallet.topic.words(topic.model, smoothed=TRUE,normalized=TRUE)
vocabulary <- topic.model$getVocabulary() 
colnames(topic.words.m) <- vocabulary
topic.words.m[1:3, 1:3]

#now find rows that have the highest concentration of user identified key words
keywords <- c("king", "maiden")
topic.words.m[, keywords]
imp.row <- which(rowSums(topic.words.m[, keywords]) == max(rowSums(topic.words.m[, keywords])))

#let mallet tell you the most frequently used key terms in topics
mallet.top.words(topic.model, topic.words.m[imp.row,], 10)

#prepare  matrix for visualization of top topic words
topic.top.words <- mallet.top.words(topic.model, topic.words.m[imp.row,], 100)

#now draw a wordcloud of the top word topics
wordcloud(topic.top.words$words, topic.top.words$weights, c(4,.8), rot.per=0, random.order=F)

#calculate the probability that a topic appears in a each text
doc.topics.m <- mallet.doc.topics(topic.model, smoothed=T,normalized=T)


print("Bye Bye!")




