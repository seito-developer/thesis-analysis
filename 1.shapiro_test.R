library(readr)

# Load dataset
data <- read_csv("./assets/finalized_questionaire.csv")

data <- na.omit(data)

## study_experience
shapiro.test(x=data$study_experience)
hist(data$study_experience, main="Study experience",  xlab="Period")

## basic_score
shapiro.test(x=data$basic_score)
hist(data$basic_score, main="Day 1 test score(Basic Score)",  xlab="Score")

## Understanding, Fun, Motivation (Solo, Mob)
max_y <- 30
shapiro.test(x=data$solo_understanding)
hist(data$solo_understanding, main="Understanding (Solo)",  xlab="Score", ylim=c(0, max_y))
abline(h = seq(0, max_y, by = 5), col = "gray", lty = "dotted")

shapiro.test(x=data$solo_fun)
hist(data$solo_fun, main="Fun (Solo)",  xlab="Score", ylim=c(0, max_y))
abline(h = seq(0, max_y, by = 5), col = "gray", lty = "dotted")

shapiro.test(x=data$solo_motivation)
hist(data$solo_motivation, main="Motivation (Solo)",  xlab="Score", ylim=c(0, max_y))
abline(h = seq(0, max_y, by = 5), col = "gray", lty = "dotted")

shapiro.test(x=data$mob_understanding)
hist(data$mob_understanding, main="Understanding (Mob)",  xlab="Score", ylim=c(0, max_y))
abline(h = seq(0, max_y, by = 5), col = "gray", lty = "dotted")

shapiro.test(x=data$mob_fun)
hist(data$mob_fun, main="Fun (Mob)",  xlab="Score", ylim=c(0, max_y))
abline(h = seq(0, max_y, by = 5), col = "gray", lty = "dotted")

shapiro.test(x=data$mob_motivation)
hist(data$mob_motivation, main="Motivation (Mob)",  xlab="Score", ylim=c(0, max_y))
abline(h = seq(0, max_y, by = 5), col = "gray", lty = "dotted")

## Draw histgram
max_y <- 14
max_x <- 16
solo_score <- data$solo_score_1 + data$solo_score_2
mob_score <- data$mob_score_1 + data$mob_score_2
hist(solo_score, main="Test Score (Solo)",  xlab="Score", xlim=c(0, max_x), ylim=c(0, max_y))
hist(mob_score, main="Mob test score",  xlab="Score", xlim=c(0, max_x), ylim=c(0, max_y))
hist(mob_score, col=rgb(1, 0, 0, 0.5), 
     main="Test score (Solor v.s. Mob)", xlab="Score", 
     xlim=c(0, max_x), 
     ylim=c(0, max_y))
hist(solo_score, col=rgb(0, 0, 1, 0.5), add=TRUE)
legend("topright", legend=c("Mob Score", "Solo Score"), fill=c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)))

## t-test

### Validate
shapiro.test(solo_score) #0.08228
shapiro.test(mob_score) #0.07027

### Analyze by mean
t.test(mob_score, solo_score, paired=TRUE) #p-val: 0.0927

### Analyze by under 25%
q1_mob <- quantile(mob_score, 0.25)
q1_solo <- quantile(solo_score, 0.25)
lower_mob_scores <- mob_score[mob_score <= q1_mob]
lower_solo_scores <- solo_score[solo_score <= q1_solo]

#### align lower data length
min_length <- min(length(lower_mob_scores), length(lower_solo_scores))
set.seed(123)  # 結果の再現性のためにシードを設定
lower_mob_scores_sample <- sample(lower_mob_scores, min_length)
lower_solo_scores_sample <- sample(lower_solo_scores, min_length)

t.test(lower_mob_scores_sample, lower_solo_scores_sample, paired=TRUE) 
#t = 6.1865, df = 13, p-value = 3.289e-05

### F-test
solo_score <- data$solo_score_1 + data$solo_score_2
mob_score <- data$mob_score_1 + data$mob_score_2
var.test(solo_score, mob_score)
