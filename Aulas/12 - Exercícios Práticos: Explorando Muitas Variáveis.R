library(ggplot2)
data("diamonds")

##### 1 - Histogramas de Preço com Facetas e Cor
# Create a histogram of diamond prices.
# Facet the histogram by diamond color
# and use cut to color the histogram bars.

ggplot(aes(x = log10(price), fill = cut), data = diamonds) +
  labs(x = 'Price', y = 'Frequency',
       title = 'Preço dos Diamantes por Cor e Corte') + 
  geom_histogram(binwidth = .05) +
  facet_wrap( ~color)

##### 2 - Preço vs. Tabela Colorida por Corte
# Create a scatterplot of diamond price vs.
# table and color the points by the cut of
# the diamond.

ggplot(aes(x =table, y = price), data = diamonds) +
  labs(x = 'Table',
       y = 'Price',
       title = 'Preço')+
  geom_point(aes(color = cut)) +
  scale_color_brewer(type = 'qual')

##### 3 - Tabela típica de Valor
# What is the typical table range for the majority of diamonds of ideal cut?
# R: Entre 53 e 57
# What is the typical table range for the majority of diamnods of premium cut?
# R: Entre 58 e 62

##### 4 - Preço vs. Volume e Claridade do Diamante
# Create a scatterplot of diamond price vs.
# volume (x * y * z) and color the points by
# the clarity of diamonds. Use scale on the y-axis
# to take the log10 of price. You should also
# omit the top 1% of diamond volumes from the plot.

diamonds$volume <- diamonds$x * diamonds$y * diamonds$z

ggplot(aes(x = volume, y = price),
       data = subset(diamonds, volume <= quantile(volume, 0.99) & volume > 0)) +
  labs(x = "Volume", y = "Price",
       title = "Price vs. Volume e Clareza do Diamante") +
  geom_point(aes(color = clarity)) +
  coord_trans(y = 'log10') +
  scale_color_brewer(type = 'div')

##### 5 - Proporção de Amizades Iniciadas
# Your task is to create a new variable called 'prop_initiated'
# in the Pseudo-Facebook data set. The variable should contain
# the proportion of friendships that the user initiated.

pf <- read.csv("pseudo_facebook.tsv", sep = '\t')
pf$prop_initiated <- pf$friendships_initiated / pf$friend_count

###### 6 - prop_initiated vs. tenure
# Create a line graph of the median proportion of
# friendships initiated ('prop_initiated') vs.
# tenure and color the line segment by
# year_joined.bucket.

pf$year_joined <- floor(2014 -(pf$tenure/365))
pf$year_joined.bucket <- cut(pf$year_joined, breaks = c(2004, 2009, 2011, 2012, 2014))

ggplot(aes(x = tenure, y = prop_initiated),
       data = pf) +
  labs(x = 'Tenure',
       y = 'Proporção da Mediana das Iniciações das Amizades',
       title = 'Proporção Iniciado x Tenure') + 
  geom_line(aes(color = year_joined.bucket), stat = 'summary',
             fun.y = median) +
  scale_color_brewer('Year Joined', palette = 'Spectral')

##### 7 - prop_initiated vs. tenure Suave
# Smooth the last plot you created of
# of prop_initiated vs tenure colored by
# year_joined.bucket. You can bin together ranges
# of tenure or add a smoother to the plot.

ggplot(aes(x = tenure, y = prop_initiated),
       data = pf) + 
  labs(x = 'Tenure',
       y = 'Proporção Mediana de Amizades Iniciadas',
       title = 'Proporção Iniciada vs. Tenure') + 
  geom_line(aes(color = year_joined.bucket), stat = 'summary',
            fun.y = median) +
  geom_smooth() + 
  scale_colour_brewer('Year Joined', palette = 'Pastel1')

##### 8 
# R: People who joined after 2012

##### 9 
# R: 0.6653892
# R: Maior quantidade de pessoas totais na rede social e, por serem novos, terem mais opções de amizades a iniciar.

##### 10 - Preço/Quilate Armazenado, Facetado e Colorido
# Create a scatter plot of the price/carat ratio
# of diamonds. The variable x should be
# assigned to cut. The points should be colored
# by diamond color, and the plot should be
# faceted by clarity.

ggplot(aes(x = cut, 
           y = price/carat),
           data = diamonds) +
  labs(x = 'Cut',
       y = 'Proporção Price/Carat',
       title = 'Price/Carat Binned, Faceted, Colored')+
  geom_point(aes(color = color), position = 'jitter',
              alpha = 0.2) +
  scale_color_brewer(type = 'div') +
  facet_wrap( ~ clarity, ncol = 2)
  
