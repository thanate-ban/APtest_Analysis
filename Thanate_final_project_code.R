install.packages("readxl")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")

library(readxl)
library(rpart)
library(rpart.plot)
library(gmodels)
library(caret)
library(dplyr)

aptest <- read_excel('C:/Users/TopKunG/Desktop/CSC7015 Intro to Data Science/Project/Dataset/DataSet_National_Summary_13.xls', sheet = "All")

#Data preparation
#Delete unnecessary rows and columns
aptest_clear <- aptest[, -c(1:2, 4, 39, 40)]
aptest_clear <- aptest_clear[-c(1:5, 76:80),]
aptest_clear <- aptest_clear[-c(6:7, 13:14, 20:21, 27:28, 34:35, 41:42, 48:49, 55:56, 62:63, 69:70),]


#Name columns
column_names <- c("AP_result", "Art_history", "Biology", "Calculus_AB", "Calculus_BC", "Chemistry", "Chinese", "Computer_science", 
                  "Economic_Macro", "Economic_Micro", "Englissh_lang", "English_lit", "Environmental_science", "European_history", "French_lang", 
                  "German_lang", "Government_comp", "Government_US", "Human_geography", "Italian_lang", "Japanese_lang", "Latin", "Music_theory", 
                  "Physic_B", "Physic_C_ele", "Physic_C_mech", "Psychology", "Spanish_lang", "Spanish_lit", "Statistics", "Studio_art_2D", 
                  "Studio_art_3D", "Studio_art_drawing", "US_history", "World_history")
names(aptest_clear) <- column_names

#Replace missing values Like N/A]
aptest_clear[is.na(aptest_clear)] <- 0
aptest_clear[aptest_clear == "*"] <- 0

#Change column class to numeric
aptest_clear[, column_names] <- lapply(aptest_clear[, column_names], as.numeric)

#focus on Math and Sci exams
math_sci_exams <- c("AP_result", "Biology", "Calculus_AB", "Calculus_BC", "Chemistry", 
                    "Environmental_science", "Physic_B", "Physic_C_ele", "Physic_C_mech",
                    "Statistics")

#create the dataset of Math and Sci
ap_math_sci <- aptest_clear[, math_sci_exams]

#separate to 2 group by ap score
ap_math_sci$AP_result[ap_math_sci$AP_result > 2] <- "pass"
ap_math_sci$AP_result[ap_math_sci$AP_result < 3] <- "fail"

#Doing percentage the dataset
#percentage the numeric variables each ethnic group
percen1 <- ap_math_sci[1:5,]
percen1 <- mutate(percen1, Biology_pct = Biology / sum(Biology),
                  Calculus_AB_pct = Calculus_AB / sum(Calculus_AB),
                  Calculus_BC_pct = Calculus_BC / sum(Calculus_BC),
                  Chemistry_pct = Chemistry / sum(Chemistry),
                  Environmental_science_pct = Environmental_science / sum(Environmental_science),
                  Physic_B_pct = Physic_B / sum(Physic_B),
                  Physic_C_ele_pct = Physic_C_ele / sum(Physic_C_ele),
                  Physic_C_mech_pct = Physic_C_mech / sum(Physic_C_mech),
                  Statistics_pct = Statistics / sum(Statistics))
percen1 <- percen1[, -c(2:10)]

percen2 <- ap_math_sci[6:10,]
percen2 <- mutate(percen2, Biology_pct = Biology / sum(Biology),
                  Calculus_AB_pct = Calculus_AB / sum(Calculus_AB),
                  Calculus_BC_pct = Calculus_BC / sum(Calculus_BC),
                  Chemistry_pct = Chemistry / sum(Chemistry),
                  Environmental_science_pct = Environmental_science / sum(Environmental_science),
                  Physic_B_pct = Physic_B / sum(Physic_B),
                  Physic_C_ele_pct = Physic_C_ele / sum(Physic_C_ele),
                  Physic_C_mech_pct = Physic_C_mech / sum(Physic_C_mech),
                  Statistics_pct = Statistics / sum(Statistics))
percen2 <- percen2[, -c(2:10)]

percen3 <- ap_math_sci[11:15,]
percen3 <- mutate(percen3, Biology_pct = Biology / sum(Biology),
                  Calculus_AB_pct = Calculus_AB / sum(Calculus_AB),
                  Calculus_BC_pct = Calculus_BC / sum(Calculus_BC),
                  Chemistry_pct = Chemistry / sum(Chemistry),
                  Environmental_science_pct = Environmental_science / sum(Environmental_science),
                  Physic_B_pct = Physic_B / sum(Physic_B),
                  Physic_C_ele_pct = Physic_C_ele / sum(Physic_C_ele),
                  Physic_C_mech_pct = Physic_C_mech / sum(Physic_C_mech),
                  Statistics_pct = Statistics / sum(Statistics))
percen3 <- percen3[, -c(2:10)]

percen4 <- ap_math_sci[16:20,]
percen4 <- mutate(percen4, Biology_pct = Biology / sum(Biology),
                  Calculus_AB_pct = Calculus_AB / sum(Calculus_AB),
                  Calculus_BC_pct = Calculus_BC / sum(Calculus_BC),
                  Chemistry_pct = Chemistry / sum(Chemistry),
                  Environmental_science_pct = Environmental_science / sum(Environmental_science),
                  Physic_B_pct = Physic_B / sum(Physic_B),
                  Physic_C_ele_pct = Physic_C_ele / sum(Physic_C_ele),
                  Physic_C_mech_pct = Physic_C_mech / sum(Physic_C_mech),
                  Statistics_pct = Statistics / sum(Statistics))
percen4 <- percen4[, -c(2:10)]

percen5 <- ap_math_sci[21:25,]
percen5 <- mutate(percen5, Biology_pct = Biology / sum(Biology),
                  Calculus_AB_pct = Calculus_AB / sum(Calculus_AB),
                  Calculus_BC_pct = Calculus_BC / sum(Calculus_BC),
                  Chemistry_pct = Chemistry / sum(Chemistry),
                  Environmental_science_pct = Environmental_science / sum(Environmental_science),
                  Physic_B_pct = Physic_B / sum(Physic_B),
                  Physic_C_ele_pct = Physic_C_ele / sum(Physic_C_ele),
                  Physic_C_mech_pct = Physic_C_mech / sum(Physic_C_mech),
                  Statistics_pct = Statistics / sum(Statistics))
percen5 <- percen5[, -c(2:10)]

percen6 <- ap_math_sci[26:30,]
percen6 <- mutate(percen6, Biology_pct = Biology / sum(Biology),
                  Calculus_AB_pct = Calculus_AB / sum(Calculus_AB),
                  Calculus_BC_pct = Calculus_BC / sum(Calculus_BC),
                  Chemistry_pct = Chemistry / sum(Chemistry),
                  Environmental_science_pct = Environmental_science / sum(Environmental_science),
                  Physic_B_pct = Physic_B / sum(Physic_B),
                  Physic_C_ele_pct = Physic_C_ele / sum(Physic_C_ele),
                  Physic_C_mech_pct = Physic_C_mech / sum(Physic_C_mech),
                  Statistics_pct = Statistics / sum(Statistics))
percen6 <- percen6[, -c(2:10)]

percen7 <- ap_math_sci[31:35,]
percen7 <- mutate(percen7, Biology_pct = Biology / sum(Biology),
                  Calculus_AB_pct = Calculus_AB / sum(Calculus_AB),
                  Calculus_BC_pct = Calculus_BC / sum(Calculus_BC),
                  Chemistry_pct = Chemistry / sum(Chemistry),
                  Environmental_science_pct = Environmental_science / sum(Environmental_science),
                  Physic_B_pct = Physic_B / sum(Physic_B),
                  Physic_C_ele_pct = Physic_C_ele / sum(Physic_C_ele),
                  Physic_C_mech_pct = Physic_C_mech / sum(Physic_C_mech),
                  Statistics_pct = Statistics / sum(Statistics))
percen7 <- percen7[, -c(2:10)]

percen8 <- ap_math_sci[36:40,]
percen8 <- mutate(percen8, Biology_pct = Biology / sum(Biology),
                  Calculus_AB_pct = Calculus_AB / sum(Calculus_AB),
                  Calculus_BC_pct = Calculus_BC / sum(Calculus_BC),
                  Chemistry_pct = Chemistry / sum(Chemistry),
                  Environmental_science_pct = Environmental_science / sum(Environmental_science),
                  Physic_B_pct = Physic_B / sum(Physic_B),
                  Physic_C_ele_pct = Physic_C_ele / sum(Physic_C_ele),
                  Physic_C_mech_pct = Physic_C_mech / sum(Physic_C_mech),
                  Statistics_pct = Statistics / sum(Statistics))
percen8 <- percen8[, -c(2:10)]

percen9 <- ap_math_sci[41:45,]
percen9 <- mutate(percen9, Biology_pct = Biology / sum(Biology),
                  Calculus_AB_pct = Calculus_AB / sum(Calculus_AB),
                  Calculus_BC_pct = Calculus_BC / sum(Calculus_BC),
                  Chemistry_pct = Chemistry / sum(Chemistry),
                  Environmental_science_pct = Environmental_science / sum(Environmental_science),
                  Physic_B_pct = Physic_B / sum(Physic_B),
                  Physic_C_ele_pct = Physic_C_ele / sum(Physic_C_ele),
                  Physic_C_mech_pct = Physic_C_mech / sum(Physic_C_mech),
                  Statistics_pct = Statistics / sum(Statistics))
percen9 <- percen9[, -c(2:10)]

percen10 <- ap_math_sci[46:50,]
percen10 <- mutate(percen10, Biology_pct = Biology / sum(Biology),
                  Calculus_AB_pct = Calculus_AB / sum(Calculus_AB),
                  Calculus_BC_pct = Calculus_BC / sum(Calculus_BC),
                  Chemistry_pct = Chemistry / sum(Chemistry),
                  Environmental_science_pct = Environmental_science / sum(Environmental_science),
                  Physic_B_pct = Physic_B / sum(Physic_B),
                  Physic_C_ele_pct = Physic_C_ele / sum(Physic_C_ele),
                  Physic_C_mech_pct = Physic_C_mech / sum(Physic_C_mech),
                  Statistics_pct = Statistics / sum(Statistics))
percen10 <- percen10[, -c(2:10)]


#Round 1 (train: 1-9, Test: 10)
#Create the training and test data
train_pct1 <- rbind(percen1, percen2, percen3, percen4, percen5, percen6, percen7, percen8, percen9)
test_pct1 <- percen10

#Crate labels to check later
train_pct1_labels <- train_pct1$AP_result
test_pct1_labels <- test_pct1$AP_result

#create model for each dataset
pct1_model <- rpart(AP_result~., method = "class", data = train_pct1)
rpart.plot(pct1_model, extra = 4)
pct1_pred <- predict(pct1_model, test_pct1, type = "class")
CrossTable(pct1_pred, test_pct1_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('Predicted', 'Actual'))

#Round 2 (train: 1-8, 10, Test: 9)
#Create the training and test data
train_pct2 <- rbind(percen1, percen2, percen3, percen4, percen5, percen6, percen7, percen8, percen10)
test_pct2 <- percen9

#Crate labels to check later
train_pct2_labels <- train_pct2$AP_result
test_pct2_labels <- test_pct2$AP_result

#create model for each dataset
pct2_model <- rpart(AP_result~., method = "class", data = train_pct2)
rpart.plot(pct2_model, extra = 4)
pct2_pred <- predict(pct2_model, test_pct2, type = "class")
CrossTable(pct2_pred, test_pct2_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('Predicted', 'Actual'))

#Round 3 (train: 1-7, 9-10, Test: 8)
#Create the training and test data
train_pct3 <- rbind(percen1, percen2, percen3, percen4, percen5, percen6, percen7, percen9, percen10)
test_pct3 <- percen8

#Crate labels to check later
train_pct3_labels <- train_pct3$AP_result
test_pct3_labels <- test_pct3$AP_result

#create model for each dataset
pct3_model <- rpart(AP_result~., method = "class", data = train_pct3)
rpart.plot(pct3_model, extra = 4)
pct3_pred <- predict(pct3_model, test_pct3, type = "class")
CrossTable(pct3_pred, test_pct3_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('Predicted', 'Actual'))


#Round 4 (train: 1-6, 8-10, Test: 7)
#Create the training and test data
train_pct4 <- rbind(percen1, percen2, percen3, percen4, percen5, percen6, percen8, percen9, percen10)
test_pct4 <- percen7

#Crate labels to check later
train_pct4_labels <- train_pct4$AP_result
test_pct4_labels <- test_pct4$AP_result

#create model for each dataset
pct4_model <- rpart(AP_result~., method = "class", data = train_pct4)
rpart.plot(pct4_model, extra = 4)
pct4_pred <- predict(pct4_model, test_pct4, type = "class")
CrossTable(pct4_pred, test_pct4_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('Predicted', 'Actual'))

#Round 5 (train: 1-5, 7-10, Test: 6)
#Create the training and test data
train_pct5 <- rbind(percen1, percen2, percen3, percen4, percen5, percen7, percen8, percen9, percen10)
test_pct5 <- percen6

#Crate labels to check later
train_pct5_labels <- train_pct5$AP_result
test_pct5_labels <- test_pct5$AP_result

#create model for each dataset
pct5_model <- rpart(AP_result~., method = "class", data = train_pct5)
rpart.plot(pct5_model, extra = 4)
pct5_pred <- predict(pct5_model, test_pct5, type = "class")
CrossTable(pct5_pred, test_pct5_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('Predicted', 'Actual'))

#Round 6 (train: 1-4, 6-10, Test: 5)
#Create the training and test data
train_pct6 <- rbind(percen1, percen2, percen3, percen4, percen6, percen7, percen8, percen9, percen10)
test_pct6 <- percen5

#Crate labels to check later
train_pct6_labels <- train_pct6$AP_result
test_pct6_labels <- test_pct6$AP_result

#create model for dataset
pct6_model <- rpart(AP_result~., method = "class", data = train_pct6)
rpart.plot(pct6_model, extra = 4)
pct6_pred <- predict(pct6_model, test_pct6, type = "class")
CrossTable(pct6_pred, test_pct6_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('Predicted', 'Actual'))

#Round 7 (train: 1-3, 5-10, Test: 4)
#Create the training and test data
train_pct7 <- rbind(percen1, percen2, percen3, percen5, percen6, percen7, percen8, percen9, percen10)
test_pct7 <- percen4

#Crate labels to check later
train_pct7_labels <- train_pct7$AP_result
test_pct7_labels <- test_pct7$AP_result

#create model for each dataset
pct7_model <- rpart(AP_result~., method = "class", data = train_pct7)
rpart.plot(pct7_model, extra = 4)
pct7_pred <- predict(pct7_model, test_pct7, type = "class")
CrossTable(pct7_pred, test_pct7_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('Predicted', 'Actual'))

#Round 8 (train: 1-2, 4-10, Test: 3)
#Create the training and test data
train_pct8 <- rbind(percen1, percen2, percen4, percen5, percen6, percen7, percen8, percen9, percen10)
test_pct8 <- percen3

#Crate labels to check later
train_pct8_labels <- train_pct8$AP_result
test_pct8_labels <- test_pct8$AP_result

#create model for each dataset
pct8_model <- rpart(AP_result~., method = "class", data = train_pct8)
rpart.plot(pct8_model, extra = 4)
pct8_pred <- predict(pct8_model, test_pct8, type = "class")
CrossTable(pct8_pred, test_pct8_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('Predicted', 'Actual'))

#Round 9 (train: 1, 3-10, Test: 2)
#Create the training and test data
train_pct9 <- rbind(percen1, percen3, percen4, percen5, percen6, percen7, percen8, percen9, percen10)
test_pct9 <- percen2

#Crate labels to check later
train_pct9_labels <- train_pct9$AP_result
test_pct9_labels <- test_pct9$AP_result

#create model for val 9
pct9_model <- rpart(AP_result~., method = "class", data = train_pct9)
rpart.plot(pct9_model, extra = 4)
pct9_pred <- predict(pct9_model, test_pct9, type = "class")
CrossTable(pct9_pred, test_pct9_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('Predicted', 'Actual'))

#Round 10 (train: 2-10, Test: 1)
#Create the training and test data
train_pct10 <- rbind(percen2, percen3, percen4, percen5, percen6, percen7, percen8, percen9, percen10)
test_pct10 <- percen1

#Crate labels to check later
train_pct10_labels <- train_pct10$AP_result
test_pct10_labels <- test_pct10$AP_result

#create model for each dataset
pct10_model <- rpart(AP_result~., method = "class", data = train_pct10)
rpart.plot(pct10_model, extra = 4)
pct10_pred <- predict(pct10_model, test_pct10, type = "class")
CrossTable(pct10_pred, test_pct10_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('Predicted', 'Actual'))

