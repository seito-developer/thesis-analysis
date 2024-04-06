# データセットを読み込む
data <- read_csv("./assets/finalized_questionaire.csv")
data <- na.omit(data)

# is_mentar
data$is_mentar <- as.factor(data$is_mentar)

## mob_understanding
mob_understanding_mentar <- data[data$is_mentar == "Yes", "mob_understanding"][[1]]

mob_understanding_mentar <- ifelse(mob_understanding_mentar <= 2, "low", ifelse(mob_understanding_mentar == 3, "middle", "high"))
mob_understanding_non_mentar <- data[data$is_mentar == "No", "mob_understanding"][[1]]
mob_understanding_non_mentar <- ifelse(mob_understanding_non_mentar <= 2, "low", ifelse(mob_understanding_non_mentar == 3, "middle", "high"))
mob_understanding_mentar <- c(low = sum(mob_understanding_mentar == "low"), 
                              middle = sum(mob_understanding_mentar == "middle"), 
                              high = sum(mob_understanding_mentar == "high"))
mob_understanding_non_mentar <- c(low = sum(mob_understanding_non_mentar == "low"), 
                                  middle = sum(mob_understanding_non_mentar == "middle"), 
                                  high = sum(mob_understanding_non_mentar == "high"))
mob_understanding_matrix <- rbind(mentor = mob_understanding_mentar, non_mentor = mob_understanding_non_mentar)

## mob_fun
mob_fun_mentar <- data[data$is_mentar == "Yes", "mob_fun"][[1]]
mob_fun_mentar <- ifelse(mob_fun_mentar <= 2, "low", ifelse(mob_fun_mentar == 3, "middle", "high"))
mob_fun_non_mentar <- data[data$is_mentar == "No", "mob_fun"][[1]]
mob_fun_non_mentar <- ifelse(mob_fun_non_mentar <= 2, "low", ifelse(mob_fun_non_mentar == 3, "middle", "high"))
mob_fun_mentar <- c(low = sum(mob_fun_mentar == "low"), 
                    middle = sum(mob_fun_mentar == "middle"), 
                    high = sum(mob_fun_mentar == "high"))
mob_fun_non_mentar <- c(low = sum(mob_fun_non_mentar == "low"), 
                        middle = sum(mob_fun_non_mentar == "middle"), 
                        high = sum(mob_fun_non_mentar == "high"))
mob_fun_matrix <- rbind(mentor = mob_fun_mentar, non_mentor = mob_fun_non_mentar)

## mob_motivation
mob_motivation_mentar <- data[data$is_mentar == "Yes", "mob_motivation"][[1]]
mob_motivation_mentar <- ifelse(mob_motivation_mentar <= 2, "low", ifelse(mob_motivation_mentar == 3, "middle", "high"))
mob_motivation_non_mentar <- data[data$is_mentar == "No", "mob_motivation"][[1]]
mob_motivation_non_mentar <- ifelse(mob_motivation_non_mentar <= 2, "low", ifelse(mob_motivation_non_mentar == 3, "middle", "high"))
mob_motivation_mentar <- c(low = sum(mob_motivation_mentar == "low"), 
                           middle = sum(mob_motivation_mentar == "middle"), 
                           high = sum(mob_motivation_mentar == "high"))
mob_motivation_non_mentar <- c(low = sum(mob_motivation_non_mentar == "low"), 
                               middle = sum(mob_motivation_non_mentar == "middle"), 
                               high = sum(mob_motivation_non_mentar == "high"))
mob_motivation_matrix <- rbind(mentor = mob_motivation_mentar, non_mentor = mob_motivation_non_mentar)

## Run fisher
fisher.test(mob_understanding_matrix)
fisher.test(mob_fun_matrix)
fisher.test(mob_motivation_matrix)