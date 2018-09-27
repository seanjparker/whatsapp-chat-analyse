# WhatsApp Chat Analysis


WhatsApp chat analysis program written in R and Python

The chat is processed in two stages:

### Stage 1
Clean the chat to be processed in R

The UK version of WhatsApp has the following format:

`[M/D/Y, H:M:S AM/PM] name: message`

 Sometimes however, WhatsApp adds a spacing Unicode characters between the text `name: ` and `message` which makes parsing more difficult

### Stage 2
Next, I used R to perform text mining and data analysis in order to find data such as:
* Total number of messages sent
* Number of photos, videos, etc.
* Top ten most used words / emojis
* Data to plot a radar chart of messages sent based on weekday
* Stacked bar chart for total messages sent per day
* Average number of words per message

Below is an example of the output once the LaTeX is compiled

![Example compiled Latex from program output](https://seanjparker.me/images/repos/gh_main_wcv.jpg)
