getwd()
setwd('/home/jaapis/Documents/GitHub/RStudio/EDA_Course_Materials/EDA_Course_Materials/lesson3')

library(ggplot2)
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')

# Gráfico de gênero por idade com todas as idades
ggplot(aes(x = gender, y = age),
       data = subset(pf, !is.na(gender))) + geom_boxplot() + 
  stat_summary(fun.y = mean, geom = 'point', shape = 4)

ggplot(aes(x = age, y = friend_count),
       data = subset(pf, !is.na(gender))) + 
  geom_line(aes(color = gender), stat = 'summary', fun.y = median)

library(dplyr)

# Cria novo dataset com filtragem de gênero e com agrupamento por idade
pf.fc_by_age_gender <- pf %>%
  filter(!is.na(gender)) %>%
  group_by(age, gender) %>%
  summarise(mean_friend_count = mean(friend_count),
            median_friend_count = median(friend_count),
            n=n()) %>%
  ungroup %>%
  arrange(age)

head(pf.fc_by_age_gender)

# Gráfico de idade por mediana de amigos separada por gênero
ggplot(aes( x = age, y = median_friend_count),
       data = pf.fc_by_age_gender) + 
  geom_line(aes(color = gender))

# Instala pacote para tidyr dos dados
install.packages("tidyr")
library(tidyr)

spread(subset(pf.fc_by_age_gender, select = c('gender', 'age', 'median_friend_count')), gender, median_friend_count)

pf.fc_by_age_gender.wide <- subset(pf.fc_by_age_gender[c('age', 'gender', 'median_friend_count')],
                              !is.na(gender)) %>%
  spread(gender, median_friend_count) %>%
  mutate(ratio=male/female)

head(pf.fc_by_age_gender.wide)

ggplot(aes(x = age, y = female/male),
       data = pf.fc_by_age_gender.wide) + 
  geom_line() +
  geom_hline(yintercept = 1, alpha = 0.3, linetype = 2)

pf$year_joined = floor(2014 - pf$tenure/365)
summary(pf$year_joined)
table(pf$year_joined)

pf$year_joined.bucket <- cut(pf$year_joined, c(2004, 2009, 2011, 2012, 2014))

table(pf$year_joined.bucket, useNA = 'ifany')

ggplot(aes(x = age, y = friend_count),
           data = subset(pf, !is.na(year_joined.bucket))) + 
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = median)


ggplot(aes(x = age, y = friend_count),
       data = subset(pf, !is.na(year_joined.bucket))) + 
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = mean) + 
  geom_line(stat = 'summary', fun.y = mean, linetype = 2)

with(subset(pf, tenure >=1), summary(friend_count/tenure))

ggplot(aes(x = tenure, y = friendships_initiated/ tenure),
       data = subset(pf, tenure >= 1)) + 
  geom_line(aes(color = year_joined.bucket))

ggplot(aes(x = 7 * round(tenure / 7), y = friendships_initiated/ tenure),
       data = subset(pf, tenure >= 1)) + 
  geom_line(aes(color = year_joined.bucket))


ggplot(aes(x = tenure, y = friendships_initiated/ tenure),
       data = subset(pf, tenure >= 1)) + 
  geom_smooth(aes(color = year_joined.bucket))

install.packages('GGally')
library(GGally)
theme_set(theme_minimal(20))

set.seed(1836)
pf_subset <- pf[, c(2:15)]
names(pf_subset)
ggpairs(pf_subset[sample.int(nrow(pf_subset), 1000),])







