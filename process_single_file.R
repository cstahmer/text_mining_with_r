#Assign a file path
filePath <- "/Users/cstahmer/Desktop/Not_I.txt"

#load the file
text.v <- scan(filePath, what="character", sep="\n")

#convert everything to lower case
text.lower.v <-tolower(text.v)

#tokenize to words in a list
text.words.l <- strsplit(text.lower.v, "\\W")

#convert my list to a vector
text.word.v <- unlist(text.words.l)

#return vector of slices in list that aren't blank
noblanks.v <- which(text.word.v!="")

#rewrite text vector with only non-blank slices
text.word.v <- text.word.v[noblanks.v]

totalwords.i <- length(text.word.v)

#get the number of times a specific word appears in the text
wordcount.single.i <- length(text.word.v[which(text.word.v=="the")])

#calculate the percentage frequency of tested word
word.percent.occurence.i <- wordcount.single.i / totalwords.i

#get a word frequency table
text.freqs.t <- table(text.word.v)

#sort the frequency table
sorted.text.freqs.t <- sort(text.freqs.t , decreasing=TRUE)


totalwords.i
#wordcount.single
#word.percent.occurence.i

sorted.text.freqs.t[1:500]

plot(sorted.text.freqs.t[1:500])

write.table(sorted.text.freqs.t, file = "/Users/cstahmer/Desktop/not_i_top_500.csv", sep = ",", col.names = NA,
            qmethod = "double")

print("Bye Bye!")

