import re
import csv
from time import strptime
from collections import defaultdict

def parseLineByLine(lines):
    formatted = []
    names = defaultdict(int)
    for i in lines:
        #Get the time from the current line
        timePos = re.search("\[(\d|\d\d)/(\d|\d\d)/(\d|\d\d),\s(\d|\d\d):(\d|\d\d):(\d|\d\d)\s(AM|PM)\]", i)
        if (i and timePos != None):
            newTime = i[timePos.start() + 1 : timePos.end() - 1]
            #Create a new pattern to find the name
            pattern = re.compile("[^\:]*")
            namePos = pattern.search(i, timePos.end() + 1)
            newName = i[namePos.start() : namePos.end()]
            names[newName] += 1
            #Get the message from the remaining string
            newMsg = i[namePos.end() + 2:]
            formatted.append((newTime, newName, newMsg))
    return formatted, names

def filterToTwo(names):
    remain = names
    if (len(names) > 2):
        min_val = min(names.values())
        remain = names.keys() - (k for k, v in names.items() if v == min_val)
    return remain

lines = (line.rstrip('\n') for line in open("_chat 2.txt"))
newLines, names = parseLineByLine(lines)
names = filterToTwo(names)

with open('formatted_chat1.csv', "w") as chat_file:
    chat_writer = csv.writer(chat_file, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
    for l in newLines:
        if (l[1] in names):
            chat_writer.writerow([l[0], l[1], l[2]])
