# データセットを読み込む
data <- read_csv("./assets/finalized_questionaire_with_genger.csv")
data <- na.omit(data)

## mob_understanding
mob_understanding_m <- data[data$genger == "m", "mob_understanding"][[1]]
mob_understanding_f <- data[data$genger == "f", "mob_understanding"][[1]]
mob_understanding_m <- ifelse(mob_understanding_m <= 2, "low", ifelse(mob_understanding_m == 3, "middle", "high"))
mob_understanding_f <- ifelse(mob_understanding_f <= 2, "low", ifelse(mob_understanding_f == 3, "middle", "high"))

mob_understanding_m<- c(low = sum(mob_understanding_m == "low"), 
                             middle = sum(mob_understanding_m == "middle"), 
                             high = sum(mob_understanding_m == "high"))
mob_understanding_f <- c(low = sum(mob_understanding_f == "low"), 
                             middle = sum(mob_understanding_f == "middle"), 
                             high = sum(mob_understanding_f == "high"))
mob_understanding_matrix <- rbind(
  m = mob_understanding_m, f = mob_understanding_f)

## mob_fun
mob_fun_m <- data[data$genger == "m", "mob_fun"][[1]]
mob_fun_f <- data[data$genger == "f", "mob_fun"][[1]]
mob_fun_m <- ifelse(mob_fun_m <= 2, "low", ifelse(mob_fun_m == 3, "middle", "high"))
mob_fun_f <- ifelse(mob_fun_f <= 2, "low", ifelse(mob_fun_f == 3, "middle", "high"))

mob_fun_m<- c(low = sum(mob_fun_m == "low"), 
              middle = sum(mob_fun_m == "middle"), 
              high = sum(mob_fun_m == "high"))
mob_fun_f <- c(low = sum(mob_fun_f == "low"), 
               middle = sum(mob_fun_f == "middle"), 
               high = sum(mob_fun_f == "high"))
mob_fun_matrix <- rbind(
  m = mob_fun_m, f = mob_fun_f)

## mob_motivation
mob_motivation_m <- data[data$genger == "m", "mob_motivation"][[1]]
mob_motivation_f <- data[data$genger == "f", "mob_motivation"][[1]]
mob_motivation_m <- ifelse(mob_motivation_m <= 2, "low", ifelse(mob_motivation_m == 3, "middle", "high"))
mob_motivation_f <- ifelse(mob_motivation_f <= 2, "low", ifelse(mob_motivation_f == 3, "middle", "high"))

mob_motivation_m<- c(low = sum(mob_motivation_m == "low"), 
                     middle = sum(mob_motivation_m == "middle"), 
                     high = sum(mob_motivation_m == "high"))
mob_motivation_f <- c(low = sum(mob_motivation_f == "low"), 
                      middle = sum(mob_motivation_f == "middle"), 
                      high = sum(mob_motivation_f == "high"))
mob_motivation_matrix <- rbind(
  m = mob_motivation_m, f = mob_motivation_f)

## Run fisher
fisher.test(mob_understanding_matrix)
fisher.test(mob_fun_matrix)
fisher.test(mob_motivation_matrix)

mob_understanding_matrix
mob_fun_matrix
mob_motivation_matrix
