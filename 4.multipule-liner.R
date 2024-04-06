# R言語のコード
library(readr)

# データセットを読み込む
data <- read_csv("./assets/finalized_questionaire.csv")

data <- na.omit(data)

# basic_scoreを説明変数として、mob_understanding, mob_fun, mob_motivationを目的変数とする単回帰分析
reg_study_mob_understanding <- glm(mob_understanding ~ study_experience + age, data = data)
reg_study_mob_fun <- glm(mob_fun ~ study_experience + age, data = data)
reg_study_mob_motivation <- glm(mob_motivation ~ study_experience + age, data = data)

summary(reg_study_mob_understanding)
summary(reg_study_mob_fun)
summary(reg_study_mob_motivation)
