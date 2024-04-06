# R言語のコード
library(readr)

# データセットを読み込む
data <- read_csv("./assets/finalized_questionaire.csv")

data <- na.omit(data)

# basic_scoreを説明変数として、mob_understanding, mob_fun, mob_motivationを目的変数とする単回帰分析
reg_basic_mob_understanding <- lm(mob_understanding ~ basic_score, data = data)
reg_basic_mob_fun <- lm(mob_fun ~ basic_score, data = data)
reg_basic_mob_motivation <- lm(mob_motivation ~ basic_score, data = data)

# study_experienceを説明変数として、mob_understanding, mob_fun, mob_motivationを目的変数とする単回帰分析
reg_study_mob_understanding <- lm(mob_understanding ~ study_experience, data = data)
reg_study_mob_fun <- lm(mob_fun ~ study_experience, data = data)
reg_study_mob_motivation <- lm(mob_motivation ~ study_experience, data = data)

# 結果の要約を表示
summary(reg_basic_mob_understanding)
summary(reg_basic_mob_fun)
summary(reg_basic_mob_motivation)

summary(reg_study_mob_understanding)
summary(reg_study_mob_fun)
summary(reg_study_mob_motivation)
