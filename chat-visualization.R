require("ggplot2")
require("plyr")

wordCount <- function(x) {
  sapply(strsplit(x, " "), length)
}

df = read.csv(file.choose(), header = FALSE)
colnames(df) <- c("date", "name", "msg")

df$date <- strptime(df$date, "%m/%d/%y, %I:%M:%S %p")

str(df)
occ <- count(df, "name")
occ <- occ[order(-occ$freq),]
pNames <- occ$name[1:2]
totalMsgsByName <- occ$freq[1:2]

msgL <- data.frame(names=df$name, chr=apply(df, 2, wordCount)[,3])
msgLP1Mean <- mean(unlist(subset(msgL, grepl(pNames[1], names), select=chr)))
msgLP2Mean <- mean(unlist(subset(msgL, grepl(pNames[2], names), select=chr)))
cat(sprintf("%s wrote %d messages in total with a mean of %f words per message\n", pNames[1], totalMsgsByName[1], msgLP1Mean))
cat(sprintf("%s wrote %d messages in total with a mean of %f words per message\n", pNames[2], totalMsgsByName[2], msgLP2Mean))
