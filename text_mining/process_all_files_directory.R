thiswd <- "/Users/cstahmer/ballad_text/"
setwd(thiswd)
filename <- system("ls",intern=TRUE)

text.f.v <- c("")

for(i in 1:length(filename)){
  file_path <- paste(thiswd, filename[i], "", sep="")
  text.v <- scan(file_path, what="character", sep="\n")
  text.f.v <- c(text.f.v, text.v)
}

print(text.f.v)