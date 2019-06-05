getwd()
setwd("/home/jaapis/Documents/RStudio/EDA_Course_Materials/EDA_Course_Materials/lesson3/")

list.files()
pf <- read.csv("pseudo_facebook.tsv", sep = '\t')
names(pf)

install.packages('ggplot2')
library(ggplot2)

names(pf)
qplot(x = dob_day, data = pf) + 
  scale_x_continuous(breaks=1:31) + 
  facet_wrap(~dob_month, ncol = 3)

qplot(x = friend_count, data = pf, xlim = c(0,1000))

qplot(x = friend_count, data = pf) +
  scale_x_continuous(limits = c(0,1000))

# Gráfico de Contagem de amigos
qplot(x = friend_count, data = pf, binwidth = 25) + 
  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000,50))

# Gráfico de contagem de amigos por gênero
qplot(x = friend_count, data = pf, binwidth = 25) + 
  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000,50)) +
  facet_wrap(~gender)

# Gráfico de contagem de amigos por gênero, excluindo NA
qplot(x = friend_count, data = na.omit(pf), binwidth = 10) + 
  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000,50)) +
  facet_wrap(~gender)

# Estatísticas por Gênero
table(pf$gender)
by(pf$friend_count, pf$gender, summary)

# Tenure (mandato)
qplot(x = tenure, data = pf, binwidth = 30,
      color = I('black'), fill = I('#099DD9'))

# Resposta Tenure
qplot(x = tenure/365, data = pf, binwidth = 0.5,
      color = I('black'), fill = I('#099DD9')) +
  scale_x_continuous(breaks = seq(1,7,1), limits = c(0,7))

# Rotulando plots
qplot(x = tenure/365, data = pf,
      xlab = 'Number of years using Facebook',
      ylab = 'Number of users in sample',
      color = I('black'), fill = I('#F79420')) +
  scale_x_continuous(breaks = seq(1, 7,1), lim = c(0,7))

# Usando idades
qplot(x = age, data = pf)

qplot(x = age, data = pf,  binwidth = 1,
      fill = I('#099DD9')) +
  scale_x_continuous(breaks = seq(0, 113, 5))

# Transformando dados
qplot(x = friend_count, data = pf)

summary(pf$friend_count)
summary(log10(pf$friend_count + 1))
summary(sqrt(pf$friend_count))

# Transformando dados com gridExtra
install.packages('gridExtra')
library(gridExtra)

p1 <- qplot(x = friend_count, data = pf)
p2 <- qplot(x = log10(friend_count + 1), data = pf)
p3 <- qplot(x = sqrt(friend_count), data = pf)

grid.arrange(p1, p2, p3, ncol = 1)

# Transformando dados modo alternativo
p1 <- ggplot(aes(x = friend_count), data = pf) + geom_histogram()
p2 <- p1 + scale_x_log10()
p3 <- p1 + scale_x_sqrt()

grid.arrange(p1, p2, p3, ncol = 1)

# Adicionando camada de escala
logScale <- qplot(x = log10(friend_count), data = pf)

countScale <- ggplot(aes(x = friend_count), data = pf) +
  geom_histogram() + 
  scale_x_log10()

grid.arrange(logScale, countScale, ncol = 2)

qplot(x = friend_count, data = pf) +
  scale_x_log10()

# Polígonos de frequencia
qplot(x = friend_count, data = subset(pf, !is.na(gender)),
      binwidth = 10) +
  scale_x_continuous(lim = c(0, 1000), breaks = seq(0, 1000, 50)) +
  facet_wrap(~gender)

qplot(x = friend_count, data = subset(pf, !is.na(gender)),
      binwidth = 10, geom = 'freqpoly', color = gender) +
  scale_x_continuous(lim = c(0, 1000), breaks = seq(0, 1000,50))

# Polígonos de frequencia ajustado
qplot(x = friend_count, data = subset(pf, !is.na(gender)),
      binwidth = 10) +
  scale_x_continuous(lim = c(0, 1000), breaks = seq(0, 1000, 50)) +
  facet_wrap(~gender)

qplot(x = friend_count, y = ..count../sum(..count..),
      data = subset(pf, !is.na(gender)),
      xlab = 'Friend Count',
      ylab = 'Proportion of Users with that friend count',
      binwidth = 10, geom = 'freqpoly', color = gender) +
  scale_x_continuous(lim = c(0, 1000), breaks = seq(0, 1000,50))

# Solução Frequencia de Polígonos // likes na web
qplot(x = www_likes, data = subset(pf, !is.na(gender)),
      geom = 'freqpoly', color = gender) + 
  scale_x_continuous()+
  scale_x_log10()

# Solução Likes na Web
by(pf$www_likes,pf$gender, sum)

# Histogramas
qplot(x = friend_count, data = subset(pf, !is.na(gender)),
      binwidth = 25) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) +
  facet_wrap(~gender)

# Box Plots
qplot(x = gender, y = friend_count,
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot')

# Solução Box Plot
qplot(x = gender, y = friend_count,
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot', ylim = c(0,1000))


qplot(x = gender, y = friend_count, 
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot') + 
  scale_y_continuous((limits = c(0, 1000)))
qplot(x = gender, y = friend_count, 
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0, 1000))

# Gráficos de caixa, quartis e amizades
qplot(x = gender, y = friend_count, 
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot')+
  coord_cartesian(ylim = c(0, 250))

by(pf$friend_count, pf$gender, summary)

# Resposta: Gráficos de caixa, quartis e amizades
qplot(x = gender,  y = friendships_initiated,
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0, 150))

by(pf$friendships_initiated, pf$gender, summary)

# Sendo lógico
summary(pf$mobile_likes)
summary(pf$mobile_likes > 0)

mobile_check_in <- NA
pf$mobile_check_in <- ifelse(pf$mobile_likes > 0, 1, 0)
pf$mobile_check_in <- factor(pf$mobile_check_in)
summary(pf$mobile_check_in)

# Resposta: Sendo lógico
summary(pf$mobile_check_in)
sum(pf$mobile_check_in == 1)/length(pf$mobile_check_in)










