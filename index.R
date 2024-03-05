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

new_dataset

