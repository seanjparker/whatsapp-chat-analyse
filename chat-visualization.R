library("ggplot2")
library("plyr")
library("tm")
library("scales")
library("xtable")
library("fmsb")
library("reshape2")

options(xtable.floating = FALSE)
options(xtable.timestamp = "")

setwd("/Users/seanparker/git/whatsapp-chat-analyse")
#Custom function for splitting a string and finding the number of words
wordCount <- function(x) {
  sapply(strsplit(x, "[[:space:]]"), length)
}

#Custom function to export to a tex file
export_latex <- function(x, filename) {
  print(xtable(x, type = "latex"), file = filename)
}

g_interval <- function(n, min_v, max_v) {
  as.integer((max_v - min_v) / n)
}

#Read all the data from a csv into a data frame
df = read.csv(file.choose(), header = FALSE)

#Label the columns in the data frame
colnames(df) <- c("date", "name", "msg")

#Convert the time column from string to 24-h date format
df$date <- strptime(df$date, "%m/%d/%y, %I:%M:%S %p")

#Find the total number of messages each user has sent
occ <- count(df, "name")
occ = occ[order(-occ$freq),]
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
#Count the number of audios each person sent
audiosSentByP1 <- count(subset(dfP1, grepl("audio omitted", msg)), "msg")[1, 2]
audiosSentByP2 <- count(subset(dfP2, grepl("audio omitted", msg)), "msg")[1, 2]

#Create a new data frame to find the length of each message
#Then we find the mean of words sent in each message by each user
msgL <- data.frame(names=df$name, chr=apply(df, 2, wordCount)[,3])
msgLP1Mean <- mean(unlist(subset(msgL, grepl(pNames[1], names), select=chr)))
msgLP2Mean <- mean(unlist(subset(msgL, grepl(pNames[2], names), select=chr)))

#Get the most commonly used words from all the messages for each person
stopWords <- c("image", "omitted", "video", "audio", stopwords("en"))

words1 <- tolower(unlist(strsplit(gsub("\\.", "", dfP1[,3]), " ")))
corpus <- words1[!(words1 %in% stopWords)]
mostUsedWordsP1 <- sort(table(corpus), decreasing=T)[1:10]
mostUsedWordsP1 <- as.data.frame(mostUsedWordsP1)
mostUsedWordsP1$freq <- percent(mostUsedWordsP1[,2] / totalMsgsByName[1])

words2 <- tolower(unlist(strsplit(gsub("\\.", "", dfP2[,3]), " ")))
corpus <- words2[!(words2 %in% stopWords)]
mostUsedWordsP2 <- sort(table(corpus), decreasing=T)[1:10]
mostUsedWordsP2 <- as.data.frame(mostUsedWordsP2)
mostUsedWordsP2$freq <- percent(mostUsedWordsP2[,2] / totalMsgsByName[2])

#Once we have converted the table to a data frame, label the columns
#and export the table as latex -- saving to a file which we can use later
colnames(mostUsedWordsP1) <- c("Word", "Number", "Frequency")
export_latex(mostUsedWordsP1[, c("Word", "Frequency")], file="tex_data/mostUsedWords1.tex")
colnames(mostUsedWordsP2) <- c("Word", "Number", "Frequency")
export_latex(mostUsedWordsP2[, c("Word", "Frequency")], file="tex_data/mostUsedWords2.tex")

#Construct the data frame for the spider chart of msgs per weekday
msgPerWeekDay <- as.data.frame(table(weekdays(dfP1$date)))
colnames(msgPerWeekDay) <- c("Day", "P1Msg")
msgPerWeekDay$P2Msg <- as.data.frame(table(weekdays(dfP2$date)))[[2]]

#Then format the data frame by getting the transpose, we also prevent conversions to strings
d <- msgPerWeekDay$Day
msgPerWeekDay <- as.data.frame(t(msgPerWeekDay[,-1]))
colnames(msgPerWeekDay) <- d

#Reorder the columns to our desired format (Sun, Mon, Tue, Wed, Thurs, Fri, Sat)
colnames(msgPerWeekDay) <- c("Friday", "Monday", "Saturday", "Sunday", "Thursday", "Tuesday", "Wednesday")
msgPerWeekDay <- msgPerWeekDay[, c(4,2,6,7,5,1,3)]

#Find the min and max values, create new rows of min and max
msgPerWeekDayMin <-  as.integer(min(msgPerWeekDay) * 0.8)
msgPerWeekDayMax <- max(msgPerWeekDay)
interval <- g_interval(4, msgPerWeekDayMin, msgPerWeekDayMax)

#Add new rows for the max and min values
msgPerWeekDay=rbind(rep(msgPerWeekDayMax, 7), rep(msgPerWeekDayMin, 7), msgPerWeekDay)

#Plot a spider chart for messages per weekday
colors_border <- c(rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9))
colors_in <- c(rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4))

#Open a new pdf file for the plot
pdf("tex_data/msgPerWeekPlot.pdf")
#Plot the graph
radarchart(msgPerWeekDay, axistype=1,
  #custom polygon
  pcol=colors_border, pfcol=colors_in, plwd=4, plty=1,

  #custom grid
  cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(msgPerWeekDayMin,msgPerWeekDayMax,interval), cglwd=0.8,

  #custom labels
  vlcex=0.8
)
legend(x=0.9, y=0.5, legend = pNames, bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
#Close the pdf file
dev.off()

#Next, we plot the messages on a continuous bar-time chart
#Combine the two people into a data frame, we find the number of messages sent each day
msgsByDay <- as.data.frame(table(cut(dfP1$date, "1 day")))
msgsByDay$Freq2 <- as.data.frame(table(cut(dfP2$date, "1 day")))[[2]]
msgsByDay$Var1 <- strptime(msgsByDay$Var1, "%Y-%m-%d")
colnames(msgsByDay) <- c("date", "msgs1", "msgs2")

#For ggplot to work, we need time in POSIXct format
msgsByDay$date <- as.POSIXct(msgsByDay$date, tz="Europe/London")

#Finally, we need to convert our table to long format rather than wide
msgsByDay.f <- melt(msgsByDay, id.vars="date")

#Plot the graph
g <- ggplot(data = msgsByDay.f, aes(x = date, y = value, fill = variable)) +
  geom_bar(stat="identity") +
    xlab("Date") +
      ylab("Number of messages") +
  scale_fill_manual(values = c("darkslategray3", "maroon3"), labels = pNames, name = "") +
      scale_x_datetime(labels=date_format("%b %y"), breaks=date_breaks("1 month"))

#Save the graph to a pdf file
ggsave(filename="tex_data/timelinePlot.pdf", plot=g)

sink("tex_data/chatvisualise.tex")

cat("\\documentclass{article}
\\usepackage{coloremoji}
\\usepackage{multicol}
\\usepackage{xcolor}
\\usepackage{graphicx}
\\usepackage[margin=0.5in]{geometry}

\\begin{document}

\\pagecolor{white}
\\definecolor{lb}{RGB}{35, 59, 78}

\\pagenumbering{gobble}
  \\centering
  {\\Huge \\color{lb}\\textbf{Results}}
  \\begin{multicols}{2}
    {\\Large \\textbf{", as.character(pNames[1]), "}}\\\\
    \\begin{center}
    Messages sent: ", totalMsgsByName[1], "\\\\
    \\bigskip
    Pictures sent: ", photosSentByP1, "\\\\
    \\bigskip
    Videos sent:   ", videosSentByP1, "\\\\
    \\bigskip
    Audios sent:   ", audiosSentByP1, "\\\\
    \\bigskip
    Most used words:

    \\input{mostUsedWords1}
    \\end{center}

    \\columnbreak

    {\\Large \\textbf{", as.character(pNames[2]), "}}\\\\
    \\begin{center}
    Messages sent: ", totalMsgsByName[2], "\\\\
    \\bigskip
    Pictures sent: ", photosSentByP2, "\\\\
    \\bigskip
    Videos sent:   ", videosSentByP2, "\\\\
    \\bigskip
    Audios sent:   ", audiosSentByP2, "\\\\
    \\bigskip
    Most used words:

    \\input{mostUsedWords2}
    \\end{center}
  \\end{multicols}
  \\begin{center}
  {\\huge \\color{lb}\\textbf{Messages per Weekday}}

  \\includegraphics[scale=0.8]{msgPerWeekPlot.pdf}

  {\\huge \\color{lb}\\textbf{Chronological Graph}}

  \\includegraphics[]{timelinePlot.pdf}

  \\bigskip
  {\\large \\color{lb}\\textbf{Extra Information}}

  ", as.character(pNames[1]) ," wrote with a mean average of ", msgLP1Mean, " words per message!\\\\
  
  ", as.character(pNames[2]) ," wrote with a mean average of ", msgLP2Mean, " words per message!
  \\end{center}

\\end{document}")
sink()
#cat(sprintf("%s wrote %d messages in total with a mean of %f words per message\n", pNames[1], totalMsgsByName[1], msgLP1Mean))
#cat(sprintf("%s wrote %d messages in total with a mean of %f words per message\n", pNames[2], totalMsgsByName[2], msgLP2Mean))

#cat(sprintf("%s sent %d photos and %d videos\n", pNames[1], photosSentByP1, videosSentByP1))
#cat(sprintf("%s sent %d photos and %d videos\n", pNames[2], photosSentByP2, videosSentByP2))
