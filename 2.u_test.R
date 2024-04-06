# Setup
library(readr)
library(ggplot2)
library(tidyr)

data <- read_csv("./assets/finalized_questionaire.csv")

## remove blank cells
data <- na.omit(data)

# Run the u-test
test_solo_mob_score_1 <- wilcox.test(data$solo_score_1, data$mob_score_1, paired = FALSE)
test_solo_mob_score_2 <- wilcox.test(data$solo_score_2, data$mob_score_2, paired = FALSE)
test_solo_mob_understanding <- wilcox.test(data$solo_understanding, data$mob_understanding, paired = FALSE)
test_solo_mob_fun <- wilcox.test(data$solo_fun, data$mob_fun, paired = FALSE)
test_solo_mob_motivation <- wilcox.test(data$solo_motivation, data$mob_motivation, paired = FALSE)

# Result

## Show the sample size
nrow(data) 

## Show the result of questionaires
test_solo_mob_understanding
mean(data$mob_understanding)
mean(data$solo_understanding)

test_solo_mob_fun
mean(data$mob_fun)
mean(data$solo_fun)

test_solo_mob_motivation
mean(data$mob_motivation)
mean(data$solo_motivation)

## Show the result of scores
test_solo_mob_score_1
mean(data$mob_score_1)
mean(data$solo_score_1)

test_solo_mob_score_2
mean(data$mob_score_2)
mean(data$solo_score_2)


## Make the data for boxplot
long_data_fun <- data %>%
  gather(key = "category", value = "score", c(solo_fun, mob_fun))
long_data_motivation <- data %>%
  gather(key = "category", value = "score", c(solo_motivation, mob_motivation))

## Output the boxplot
ggplot(long_data_fun, aes(x = category, y = score, fill = category)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Fun of Mob v.s. Solo", x = "Category", y = "Score")

ggplot(long_data_motivation, aes(x = category, y = score, fill = category)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Motivation of Mob v.s. Solo", x = "Category", y = "Score")