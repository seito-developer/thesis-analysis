# R言語のコード
library(readr)

# データセットを読み込む
data <- read_csv("./assets/finalized_questionaire.csv")

data <- na.omit(data)

## study_experience
shapiro.test(x=data$study_experience)
hist(data$study_experience, main="Study experience",  xlab="Period")

## basic_score
shapiro.test(x=data$basic_score)
hist(data$basic_score, main="Day 1 test score(Basic Score)",  xlab="Score")

## Understanding, Fun, Motivation (Solo, Mob)
shapiro.test(x=data$solo_understanding)
hist(data$solo_understanding, main="Understanding (Solo)",  xlab="Score")
shapiro.test(x=data$solo_fun)
hist(data$solo_fun, main="Fun (Solo)",  xlab="Score")
shapiro.test(x=data$solo_motivation)
hist(data$solo_motivation, main="Motivation (Solo)",  xlab="Score")

shapiro.test(x=data$mob_understanding)
hist(data$mob_understanding, main="Understanding (Mob)",  xlab="Score")
shapiro.test(x=data$mob_fun)
hist(data$mob_fun, main="Fun (Mob)",  xlab="Score")
shapiro.test(x=data$mob_motivation)
hist(data$mob_motivation, main="Motivation (Mob)",  xlab="Score")

## Test Score (Solo, Mob)
solo_score <- data$solo_score_1 + data$solo_score_2
shapiro.test(x=solo_score)
hist(solo_score, main="Test Score (Solo)",  xlab="Score")

mob_score <- data$mob_score_1 + data$mob_score_2
shapiro.test(x=mob_score)
hist(mob_score, main="Mob test score",  xlab="Score")

hist(mob_score, col=rgb(1, 0, 0, 0.5), main="Test score (Solor v.s. Mob)", xlab="Score", xlim=range(c(solo_score, mob_score)), ylim=c(0, max(hist(solo_score, plot=FALSE)$counts, hist(mob_score, plot=FALSE)$counts)))
hist(solo_score, col=rgb(0, 0, 1, 0.5), add=TRUE)
legend("topright", legend=c("Mob Score", "Solo Score"), fill=c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)))

mean(solo_score)
mean(mob_score)

min(solo_score)
min(mob_score)

max(solo_score)
max(mob_score)

### t-test
t.test(solo_score, mob_score, paired = TRUE, conf.level = 0.95)

### bootstrap
bootstrap_min <- function(data, n_bootstrap) {
  bootstrap_mins <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    sample_data <- sample(data, replace = TRUE)
    bootstrap_mins[i] <- min(sample_data)
  }
  return(bootstrap_mins)
}

# ブートストラップサンプリング
set.seed(123)  # 結果の再現性を保証
n_bootstrap <- 10000
solo_min <- bootstrap_min(solo_score, n_bootstrap)
mob_min <- bootstrap_min(mob_score, n_bootstrap)

# 最小値の差の分布
min_diffs <- mob_min - solo_min

# 差の95%信頼区間
quantile(min_diffs, c(0.025, 0.975))

# p値の計算
mean(min_diffs >= 0)  

