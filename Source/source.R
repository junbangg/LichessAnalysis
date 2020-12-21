library(dplyr)
library(tidyverse) 
library(gridExtra)
library(ggpubr)
library(broom)
library(AICcmodavg)
require(ggplot2)
require(stringr)
require(dplyr)
#read data
data <- read.csv("../Data/games.csv")
head(data, 5)
dim(data)

options(repr.plot.width = 20, repr.plot.height = 10)
#Top 10 
data %>% 
  mutate(firstMove = str_sub(moves, 1, 2)) %>% 
  group_by(firstMove) %>% 
  count(sort = TRUE) %>% 
  head(10) %>% 
  ggplot(aes(reorder(firstMove, n), n, fill = n, label = n))+
  geom_col(show.legend = FALSE)+
  scale_fill_gradient(low = "green", high = "darkgreen")+
  labs(x = "Notation", y = "Frequency", title = "Top 10 First Moves")+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.text = element_text(size = 15),axis.title = element_text(size = 15))

#a <- nrow(filter(data, winner == "white"))
#Top openings
data  %>% 
  count(opening_name, sort = TRUE) %>% 
  head(10) %>% 
  ggplot(aes(reorder(opening_name,n), n, fill = n))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  labs(y = "Frequency", x = "Opening", title = "Top 10 Openings")+
  scale_fill_gradient(low = "blue", high = "darkblue")+
  theme(axis.text = element_text(size = 15),axis.title = element_text(size = 15))
options(repr.plot.width = 20, repr.plot.height = 10)

#white
data %>% 
  group_by(opening_name) %>% 
  summarise(wins = sum(winner == "white")) %>%
  arrange(desc(wins)) %>%
  head(20) %>%
  ggplot(aes(reorder(opening_name, wins), wins, fill = wins))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  labs(y = "Frequency", x = "Opening", title = "Best Openings for White")+
  scale_fill_gradient(low = "red", high = "darkred")+
  theme(axis.text = element_text(size = 15),axis.title = element_text(size = 15))

#black
data %>% 
  group_by(opening_name) %>% 
  summarise(wins = sum(winner == "black")) %>%
  arrange(desc(wins)) %>%
  head(20) %>%
  ggplot(aes(reorder(opening_name, wins), wins, fill = wins))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  labs(y = "Frequency", x = "Opening", title = "Best Openings for White")+
  scale_fill_gradient(low = "gray", high = "darkgray")+
  theme(axis.text = element_text(size = 15),axis.title = element_text(size = 15))

#
rating_white <- data %>% filter(winner == "white") %>% select(winner, white_rating, black_rating)
rating_black <- data %>% filter(winner == "black") %>% select(winner, white_rating, black_rating)

rating_white_wins <- rating_white[sample(nrow(rating_white),5000),]
rating_black_wins <- rating_white[sample(nrow(rating_white),5000),]
#shapiro
shapiro.test(rating_white_wins$white_rating)
shapiro.test(rating_white_wins$black_rating) 
shapiro.test(rating_black_wins$white_rating)
shapiro.test(rating_black_wins$black_rating) 
#pearson
cor.test(rating_white_wins$white_rating, rating_white_wins$black_rating, method = "pearson")
cor.test(rating_black_wins$white_rating, rating_black_wins$black_rating, method = "pearson")
#kendall
cor.test(rating_white_wins$white_rating, rating_white_wins$black_rating, method = "kendall")
cor.test(rating_black_wins$white_rating, rating_black_wins$black_rating, method = "kendall")
#spearman
hypo1 <- cor.test(rating_white_wins$white_rating, rating_white_wins$black_rating, method = "spearman")
hypo2 <- cor.test(rating_black_wins$white_rating, rating_black_wins$black_rating, method = "spearman")
hypo1
hypo2

options(repr.plot.width = 15, repr.plot.height = 8)

ggplot(rating_white_wins, aes(white_rating, black_rating, label = 'W', color = winner))+
  geom_text()+
  theme_minimal()+
  theme(legend.position = "bottom")+
  geom_jitter(alpha = 0.3, size = 2.5, width = 0.3, height = 0.3)+
  geom_smooth(method = "lm", color = "red", lty = 2, se = FALSE, size = 0.6)+
  scale_color_manual(values = c("darkblue","steelblue"))+
  labs(title = paste("Spearman Correlation Coefficient:", round(hypo1$estimate, digits = 2)))

ggplot(rating_black_wins, aes(white_rating, black_rating, label = 'B', color = winner))+
  geom_text()+
  theme_minimal()+
  theme(legend.position = "bottom")+
  geom_jitter(alpha = 0.3, size = 2.5, width = 0.3, height = 0.3)+
  geom_smooth(method = "lm", color = "blue", lty = 2, se = FALSE, size = 0.6)+
  scale_color_manual(values = c("darkred","steelblue"))+
  labs(title = paste("Spearman Correlation Coefficient:", round(hypo2$estimate, digits = 2)))

#Anova
opening_white <- data %>%
  mutate(firstMove = str_sub(moves, 1, 2)) %>%
  filter(winner == "white") %>%
  select(winner, white_rating, black_rating, opening_name, firstMove) %>%

opening_black <- data %>%
  mutate(firstMove = str_sub(moves, 1, 2)) %>%
  filter(winner == "black") %>% 
  select(winner, white_rating, black_rating, opening_name, firstMove)

when_white_wins <- aov(black_rating ~ white_rating, data = opening_white)
summary(when_white_wins)

when_black_wins <- aov(white_rating ~ black_rating, data = opening_white)
summary(when_black_wins)
