# データセットを読み込む
data <- read_csv("./assets/finalized_questionaire.csv")

data <- na.omit(data)

# is_mentar列を因子型に変換し、Yes=1, No=0 とする
data$is_mentar <- as.factor(data$is_mentar)

# mob_understandingを説明変数としてロジスティック回帰分析
logit_model_understanding <- glm(mob_understanding ~ is_mentar, data = data, family = binomial)

# mob_funを説明変数としてロジスティック回帰分析
logit_model_fun <- glm(mob_fun ~ is_mentar, data = data, family = binomial)

# mob_motivationを説明変数としてロジスティック回帰分析
logit_model_motivation <- glm(mob_motivation ~ is_mentar, data = data, family = binomial)

# 結果の要約を表示
summary(logit_model_understanding)
summary(logit_model_fun)
summary(logit_model_motivation)