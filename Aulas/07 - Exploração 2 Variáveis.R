library(ggplot2)
data("diamonds")

# 1

summary(diamonds)

ggplot(aes(x = price, y = x), data = diamonds) + geom_point() 

# 3
cor.test(diamonds$price, diamonds$x)
cor.test(diamonds$price, diamonds$y)
cor.test(diamonds$price, diamonds$z)

# 4

qplot(aes(x = price, y = depth), data = diamonds) + 
  geom_point(alpha = 0.01) 

# 7
cor.test(diamonds$depth, diamonds$price)

# 8
summary(diamonds)

library(dplyr)

ggplot(aes(x = price, y = carat), data = diamonds) + 
  geom_point(alpha = 0.01) +
  geom_smooth(method= 'lm', color = 'red')

# 9
volume <- NA
diamonds$volume <- diamonds$x*diamonds$y*diamonds$z

ggplot(aes(x = price, y = volume), data = diamonds) + 
  geom_point(alpha = 0.05) +
  geom_smooth(method= 'lm', color = 'red')

# 11

with(subset(diamonds, volume = 0, volume >= 800), cor.test(diamonds$price, diamonds$volume))

summary(diamonds)

# 13
library(dplyr)

diamondsByClarity <- group_by(diamonds)
pf.fc_by_age_months <- pf %>%
  group_by(age_with_months) %>%
  summarise(friend_count_mean = mean(friend_count),
            friend_count_median = median(friend_count),
            n = n()) %>%
  arrange(age_with_months)

head(pf.fc_by_age_months)















