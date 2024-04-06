# データセットを読み込む
data <- read_csv("./assets/finalized_questionaire.csv")
data <- na.omit(data)

# study_experience
data$study_experience <- ifelse(data$study_experience <= 3, "low", "high")

## mob_understanding
mob_understanding_higher <- data[data$study_experience == "high", "mob_understanding"][[1]]
mob_understanding_lower <- data[data$study_experience == "low", "mob_understanding"][[1]]
mob_understanding_higher <- ifelse(mob_understanding_higher <= 2, "low", ifelse(mob_understanding_higher == 3, "middle", "high"))
mob_understanding_lower <- ifelse(mob_understanding_lower <= 2, "low", ifelse(mob_understanding_lower == 3, "middle", "high"))

mob_understanding_higher<- c(low = sum(mob_understanding_higher == "low"), 
                             middle = sum(mob_understanding_higher == "middle"), 
                             high = sum(mob_understanding_higher == "high"))
mob_understanding_lower <- c(low = sum(mob_understanding_lower == "low"), 
                             middle = sum(mob_understanding_lower == "middle"), 
                             high = sum(mob_understanding_lower == "high"))
mob_understanding_matrix <- rbind(
  highger = mob_understanding_higher, lower = mob_understanding_lower)

## mob_fun
mob_fun_higher <- data[data$study_experience == "high", "mob_fun"][[1]]
mob_fun_lower <- data[data$study_experience == "low", "mob_fun"][[1]]
mob_fun_higher <- ifelse(mob_fun_higher <= 2, "low", ifelse(mob_fun_higher == 3, "middle", "high"))
mob_fun_lower <- ifelse(mob_fun_lower <= 2, "low", ifelse(mob_fun_lower == 3, "middle", "high"))

mob_fun_higher<- c(low = sum(mob_fun_higher == "low"), 
                   middle = sum(mob_fun_higher == "middle"), 
                   high = sum(mob_fun_higher == "high"))
mob_fun_lower <- c(low = sum(mob_fun_lower == "low"), 
                   middle = sum(mob_fun_lower == "middle"), 
                   high = sum(mob_fun_lower == "high"))
mob_fun_matrix <- rbind(
  highger = mob_fun_higher, lower = mob_fun_lower)

## mob_motivation
mob_motivation_higher <- data[data$study_experience == "high", "mob_motivation"][[1]]
mob_motivation_lower <- data[data$study_experience == "low", "mob_motivation"][[1]]
mob_motivation_higher <- ifelse(mob_motivation_higher <= 2, "low", ifelse(mob_motivation_higher == 3, "middle", "high"))
mob_motivation_lower <- ifelse(mob_motivation_lower <= 2, "low", ifelse(mob_motivation_lower == 3, "middle", "high"))

mob_motivation_higher<- c(low = sum(mob_motivation_higher == "low"), 
                          middle = sum(mob_motivation_higher == "middle"), 
                          high = sum(mob_motivation_higher == "high"))
mob_motivation_lower <- c(low = sum(mob_motivation_lower == "low"), 
                          middle = sum(mob_motivation_lower == "middle"), 
                          high = sum(mob_motivation_lower == "high"))
mob_motivation_matrix <- rbind(
  highger = mob_motivation_higher, lower = mob_motivation_lower)

## Run fisher
fisher.test(mob_understanding_matrix)
fisher.test(mob_fun_matrix)
fisher.test(mob_motivation_matrix)

