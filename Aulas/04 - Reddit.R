getwd()
reddit <- read.csv('reddit.csv')

table(reddit$employment.status)
summary(reddit)

str(reddit)
# Checar level de uma determinada variável
levels(reddit$age.range)

library(ggplot2)
qplot(data = reddit, x = age.range)
qplot(data = reddit, x = income.range)
