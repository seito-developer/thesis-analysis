#----Start Analyze------------
library(readxl)
dataset <- read_excel("./assets/questionaire.xlsx")
head(dataset)

ROWS = 28

new_dataset <- data.frame(
  id = c(1:28),
  mob_understanding = c(1:ROWS),
  mob_fun = c(1:ROWS),
  solo_fun = c(1:ROWS),
  mob_motivation = c(1:ROWS),
  solo_motivation = c(1:ROWS)
)

# リッカート尺度に基づいて日本語の文字列を数値に変換する関数
convert_scale <- function(value) {
  if (value == "とてもそう思う") {
    return(5)
  } else if (value == "ややそう思う") {
    return(4)
  } else if (value == "どちらともいえない") {
    return(3)
  } else if (value == "あまりそう思わない") {
    return(2)
  } else if (value == "全くそう思わない") {
    return(1)
  } else {
    return(NA)  # 該当しない場合はNAを返す
  }
}

# 指定された列（E, G, I, K, M, O）の値を変換
new_dataset$mob_understanding <- sapply(dataset$`モブプログラミングを通して、プログラミングの理解が深まったと思いますか？`, convert_scale)
new_dataset$solo_understanding <- sapply(dataset$`ソロプログラミングを通して、プログラミングの理解が深まったと思いますか？`, convert_scale)
new_dataset$mob_fun <- sapply(dataset$`モブプログラミングを通して、プログラミングを楽しいと感じましたか？`, convert_scale)
new_dataset$solo_fun <- sapply(dataset$`ソロプログラミングを通して、プログラミングを楽しいと感じましたか？`, convert_scale)
new_dataset$mob_motivation <- sapply(dataset$`モブプログラミングを通して、プログラミング学習を続けるモチベーションが高まったと思いますか？`, convert_scale)
new_dataset$solo_motivation <- sapply(dataset$`ソロプログラミングを通して、プログラミング学習を続けるモチベーションが高まったと思いますか？`, convert_scale)

# 正規化チェック
shapiro.test(new_dataset$mob_understanding)
shapiro.test(new_dataset$solo_understanding)
shapiro.test(new_dataset$mob_fun)
shapiro.test(new_dataset$solo_fun)
shapiro.test(new_dataset$mob_motivation)
shapiro.test(new_dataset$solo_motivation)

# u-test: understanding
wilcox.test(new_dataset$mob_understanding, new_dataset$solo_understanding, exact = FALSE)
wilcox.test(new_dataset$mob_fun, new_dataset$solo_fun, exact = FALSE)
wilcox.test(new_dataset$mob_motivation, new_dataset$solo_motivation, exact = FALSE)

# avg
mean(new_dataset$mob_understanding)
mean(new_dataset$solo_understanding)
mean(new_dataset$mob_fun)
mean(new_dataset$solo_fun)
mean(new_dataset$mob_fun)
mean(new_dataset$mob_motivation)
mean(new_dataset$solo_motivation)
#----End Analyze------------


#----Start visualiztion------------


# ggplot2 パッケージを読み込む
library(ggplot2)

# ヒストグラム
hist(new_dataset$mob_understanding, main="Histogram of mob_understanding", xlab="mob_understanding")

# QQプロット
qqnorm(new_dataset$mob_understanding)
qqline(new_dataset$mob_understanding)

library(tidyr)


long_data_understanding <- pivot_longer(new_dataset, cols = c(mob_understanding, solo_understanding), 
                                        names_to = "Type", values_to = "Value")
ggplot(long_data_understanding, aes(x = Type, y = Value)) + 
  geom_boxplot() +
  labs(title = "Box plot of mob_understanding vs solo_understanding",
       x = "Type",
       y = "Value")

long_data_fun <- pivot_longer(new_dataset, cols = c(mob_fun, solo_fun), 
                                        names_to = "Type", values_to = "Value")
ggplot(long_data_fun, aes(x = Type, y = Value)) + 
  geom_boxplot() +
  labs(title = "Box plot of mob_fun vs solo_fun",
       x = "Type",
       y = "Value")

long_data_motivation <- pivot_longer(new_dataset, cols = c(mob_motivation, solo_motivation), 
                                        names_to = "Type", values_to = "Value")
ggplot(long_data_motivation, aes(x = Type, y = Value)) + 
  geom_boxplot() +
  labs(title = "Box plot of mob_motivation vs solo_motivation",
       x = "Type",
       y = "Value")

#----End visualiztion------------