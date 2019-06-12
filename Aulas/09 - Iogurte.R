setwd('/home/jaapis/Documents/GitHub/RStudio/EDA_Course_Materials/EDA_Course_Materials/lesson5')

yo <- read.csv('yogurt.csv')
str(yo)

# Converte id de int para factor
yo$id <- factor(yo$id)
str(yo)

# Histograma
qplot(data = yo, x = price, fill = I('#F79420'))

# Número de Compras
summary(yo)
length(unique(yo$price))
table(yo$price)

# Assimila todas as compras em uma nova variável
str(yo)
yo$all.purchases <- yo$strawberry + yo$blueberry + yo$pina.colada + yo$plain + yo$mixed.berry

str(yo)
qplot(x = all.purchases, data = yo, bindwidth = 1, fill = I('#099FF9'))

# Preço por tempo (Scatter plot)
ggplot(aes(x = time, y = price), data = yo) +
         geom_jitter(alpha = 0.25, shape = 21, fill = I('#F79420'))

# Amostras aleatórias das Famílias
set.seed(4230)
sample.ids <- sample(levels(yo$id), 16)

ggplot(aes(x = time, y = price), 
       data = subset(yo, id %in% sample.ids)) +
  facet_wrap( ~ id) +
  geom_line() +
  geom_point(aes(size = all.purchases), pch = 1)


