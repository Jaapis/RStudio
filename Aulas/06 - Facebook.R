getwd()
setwd('/home/jaapis/Documents/RStudio/EDA_Course_Materials/EDA_Course_Materials/lesson3')

library(ggplot2)
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')

# Gráficos de dispersão
qplot(x = age, y = friend_count, data = pf)
qplot(age, friend_count, data = pf)

# Sintaxe ggplot
ggplot(aes(x = age, y = friend_count), data = pf) + geom_point() + 
  xlim(13, 90)

summary(pf$age)

# Overplotting
ggplot(aes(x = age, y = friend_count), data = pf) + 
  geom_jitter(alpha = 0.05) + 
  xlim(13, 90)

# Coord_trans()
ggplot(aes(x = age, y = friend_count), data = pf) + 
  geom_point(alpha = 0.05, position = position_jitter(h = 0)) + 
  xlim(13, 90)

# Alpha e jitter solução
ggplot(aes(x = age, y = friend_count), data = pf) + 
  geom_jitter(alpha = 0.05) + 
  xlim(13,90) +
  scale_y_sqrt()

# dplyr
install.packages('dplyr')
library(dplyr)

# Meios condicionais
age_groups <- group_by(pf, age)
pf.fc_by_age <- summarise(age_groups, 
          friend_count_mean = mean(friend_count),
          friend_count_median = median(friend_count),
          n = n())
pf.fc_by_age <- arrange(pf.fc_by_age)

head(pf.fc_by_age)

# Meios Condicionais Código Alternativo
pf.dx_by_age <- pf %>%
  group_by(age) %>%
  summarise(friend_count_mean = mean(friend_count),
            friend_count_median = median(friend_count),
            n = n())%>%
  arrange(age)

head(pf.dx_by_age, 20)  

# Meios condicionais gráfico friend_count x age
ggplot(aes(x = age, y = friend_count_mean), data = pf.fc_by_age) + geom_line() + 
  xlim(13, 90)

# Sobrepondo resumos com dados brutos
ggplot(aes(x = age, y = friend_count), data = pf) +
  coord_cartesian(xlim = c(13, 90), ylim = c(0,1200)) +
  geom_point(alpha = 0.05, 
             position = position_jitter(h = 0),
             color = 'orange') + 
  geom_line(stat = 'summary', fun.y = mean) +
  geom_line(stat = 'summary', 
            fun.y = quantile,
            fun.args = list(probs = 0.1),
            linetype = 2, 
            color = 'blue') + 
  geom_line(stat = 'summary', 
            fun.y = quantile,
            fun.args = list(probs = 0.5),
            color = 'purple') + 
  geom_line(stat = 'summary', 
            fun.y = quantile,
            fun.args = list(probs = 0.9),
            linetype = 2, 
            color = 'red')  

# Correlação
?cor.test
cor.test(pf$age, pf$friend_count)

# Forma alternaticva correlação
with(pf, cor.test(pf$age, pf$friend_count, method = 'pearson'))

# Correlação nos subconjuntos
with(subset(pf, age <= 70), cor.test(pf$age, pf$friend_count))

# Métodos de correlação
with(subset(pf, age <= 70), cor.test(pf$age, pf$friend_count, method = 'spearman'))

# Gráficos de dispersão
ggplot(aes(x = www_likes_received, y = likes_received), data = pf) + 
  geom_point(color = 'orange') +  
  xlim(0, quantile(pf$www_likes_received, 0.95)) + 
  ylim(0, quantile(pf$likes_received, 0.95)) +
  geom_smooth(method= 'lm', color = 'red')

with(subset(pf), cor.test(pf$www_likes_received, pf$likes_received))

# Instala alr3
# Rodar sudo apt-get install r-cran-car

install.packages("alr3", repos="http://R-Forge.R-project.org")
library(alr3)
data(Mitchell)

?Mitchell

head(Mitchell)

ggplot(aes(x = (Month%%12), y = Temp), data = Mitchell) + 
  geom_point(color = 'orange') +
  scale_x_discrete(breaks = seq(0, 203, 12))

cor.test(Mitchell$Month, Mitchell$Temp)

# Entendendo o ruído
ggplot(aes(x = age, y = friend_count_mean),
       data = pf.fc_by_age) +
  geom_line()

head(pf.fc_by_age, 10)

pf.fc_by_age[17:19, ]

pf$age_with_months <- pf$age + (12 - pf$dob_month)/12

# Média de idade com meses
pf.fc_by_age_months <- pf %>%
  group_by(age_with_months) %>%
  summarise(friend_count_mean = mean(friend_count),
            friend_count_median = median(friend_count),
            n = n()) %>%
  arrange(age_with_months)

head(pf.fc_by_age_months)

# Solução alternativa
age_with_months_groups <- group_by(pf, age_with_months)
pf.fc_by_age_months2 <- summarise(age_with_months_groups,
                                  friend_count_mean = mean(friend_count),
                                  friend_count_median = median(friend_count),
                                  n = n())
pf.fc_by_age_months2 <- arrange(pf.fc_by_age_months2, age_with_months)

head(pf.fc_by_age_months2)

# Ruído em média condicional
p1 <- ggplot(aes(x = age, y = friend_count_mean),
             data = subset(pf.fc_by_age, age <71)) +
  geom_line() +
  geom_smooth()

p2 <- ggplot(aes(x = age_with_months, y = friend_count_mean), 
          data = subset(pf.fc_by_age_months, age_with_months < 71)) + 
  geom_line() + 
  geom_smooth()

p3 <- ggplot(aes(x = round(age/5)*5, y = friend_count), 
             data = subset(pf, age < 71)) +
  geom_line(stat = 'summary', fun.y = mean)

library(gridExtra)
grid.arrange(p2, p1, p2, ncol = 1)



