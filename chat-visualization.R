require("ggplot2")
require("plyr")

#Custom function for splitting a string and finding the number of words
wordCount <- function(x) {
  sapply(strsplit(x, " "), length)
}

#Read all the data from a csv into a data frame
df = read.csv(file.choose(), header = FALSE)

#Label the columns in the data frame
colnames(df) <- c("date", "name", "msg")

#Convert the time column from string to 24-h date format
df$date <- strptime(df$date, "%m/%d/%y, %I:%M:%S %p")

#Find the total number of messages each user has sent
occ <- count(df, "name")
occ <- occ[order(-occ$freq),]
pNames <- occ$name[1:2]
totalMsgsByName <- occ$freq[1:2]

#Get all the messages of each person
dfP1 <- subset(df, grepl(pNames[1], name))
dfP2 <- subset(df, grepl(pNames[2], name))

#Count the number of photos each person sent
photosSentByP1 <- count(subset(dfP1, grepl("image omitted", msg)), "msg")[1, 2]
photosSentByP2 <- count(subset(dfP2, grepl("image omitted", msg)), "msg")[1, 2]
#Count the number of videos each person sent
videosSentByP1 <- count(subset(dfP1, grepl("video omitted", msg)), "msg")[1, 2]
videosSentByP2 <- count(subset(dfP2, grepl("video omitted", msg)), "msg")[1, 2]

#Create a new data frame to find the length of each message
#Then we find the mean of words sent in each message by each user
msgL <- data.frame(names=df$name, chr=apply(df, 2, wordCount)[,3])
msgLP1Mean <- mean(unlist(subset(msgL, grepl(pNames[1], names), select=chr)))
msgLP2Mean <- mean(unlist(subset(msgL, grepl(pNames[2], names), select=chr)))


cat(sprintf("%s wrote %d messages in total with a mean of %f words per message\n", pNames[1], totalMsgsByName[1], msgLP1Mean))
cat(sprintf("%s wrote %d messages in total with a mean of %f words per message\n", pNames[2], totalMsgsByName[2], msgLP2Mean))

cat(sprintf("%s sent %d photos and %d videos\n", pNames[1], photosSentByP1, videosSentByP1))
cat(sprintf("%s sent %d photos and %d videos\n", pNames[2], photosSentByP2, videosSentByP2))
