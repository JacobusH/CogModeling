library(tidyr)
library(MASS)
library(mclust)
library(gtools)

Task1 <- function() {
  fine_sampling <<- read.table("E:\\Homework\\Cognitive Modeling\\Lab3\\fine_sampling.data", header = FALSE)
  colnames(fine_sampling) <<- c("filename"
                                , "duration"
                                , "f0.steady"
                                , "f1.steady"
                                , "f2.steady"
                                , "f3.steady"
                                , "f1.10"
                                , "f2.10"
                                , "f3.10"
                                , "f1.20"
                                , "f2.20"
                                , "f3.20"
                                , "f1.30"
                                , "f2.30"
                                , "f3.30"
                                , "f1.40"
                                , "f2.40"
                                , "f3.40"
                                , "f1.50"
                                , "f2.50"
                                , "f3.50"
                                , "f1.60"
                                , "f2.60"
                                , "f3.60"
                                , "f1.70"
                                , "f2.70"
                                , "f3.70"
                                , "f1.80"
                                , "f2.80"
                                , "f3.80") 
  
  fine_sampling <<- extract(data = fine_sampling, "filename", c("gender", "ID", "vowel"), "(.)(..)(..)")
}

Task1.plot <- function() {
  plot(fine_sampling$f2.steady, fine_sampling$f1.steady, xlim=rev(range(700:3500)),
       ylim=rev(range(300:1300)), col="1", xlab="F2", ylab="F1", pch=1,
       main="The vowel space", cex.lab=1.3, cex.axis=1.3, cex.sub=1.3)
}

Task2 <- function() {
  point_vowels <<- fine_sampling[with(fine_sampling, vowel == "iy" | vowel == "ah" | vowel == "uw"), ]
  
  plot(point_vowels$f2.steady, point_vowels$f1.steady, xlim=rev(range(700:3500)),
       ylim=rev(range(300:1300)), col="1", xlab="F2", ylab="F1", pch=1,
       main="The vowel space", cex.lab=1.3, cex.axis=1.3, cex.sub=1.3)
}

Task3 <- function() {
  # iy == 1 | ah == 2 | uw == 3
  
  mean_iy_f1 <- mean(point_vowels[point_vowels$vowel == "iy", ]$f1.steady)
  mean_iy_f2 <- mean(point_vowels[point_vowels$vowel == "iy", ]$f2.steady)
  mean_ah_f1 <- mean(point_vowels[point_vowels$vowel == "ah", ]$f1.steady)
  mean_ah_f2 <- mean(point_vowels[point_vowels$vowel == "ah", ]$f2.steady)
  mean_uw_f1 <- mean(point_vowels[point_vowels$vowel == "uw", ]$f1.steady)
  mean_uw_f2 <- mean(point_vowels[point_vowels$vowel == "uw", ]$f2.steady)
  
  mat_iy <- cbind(point_vowels[point_vowels$vowel == "iy", ]$f1.steady
                  , point_vowels[point_vowels$vowel == "iy", ]$f2.steady)
  mat_ah <- cbind(point_vowels[point_vowels$vowel == "ah", ]$f1.steady
                  , point_vowels[point_vowels$vowel == "ah", ]$f2.steady)
  mat_uw <- cbind(point_vowels[point_vowels$vowel == "uw", ]$f1.steady
                  , point_vowels[point_vowels$vowel == "uw", ]$f2.steady)
  
  cov_iy_f1f2 <- cov(mat_iy)
  cov_ah_f1f2 <- cov(mat_ah)
  cov_uw_f1f2 <- cov(mat_uw)
  
  # now fake the training data
  mydata_iy_train <- mvrnorm(n = 2000, mu = c(mean_iy_f1, mean_iy_f2), Sigma = cov_iy_f1f2)
  mydata_ah_train <- mvrnorm(n = 2000, mu = c(mean_ah_f1, mean_ah_f2), Sigma = cov_ah_f1f2)
  mydata_uw_train <- mvrnorm(n = 2000, mu = c(mean_uw_f1, mean_uw_f2), Sigma = cov_uw_f1f2)
  
  # now fake the test data
  test_iy <- mvrnorm(n = 2000, mu = c(mean_iy_f1, mean_iy_f2), Sigma = cov_iy_f1f2)
  test_ah <- mvrnorm(n = 2000, mu = c(mean_ah_f1, mean_ah_f2), Sigma = cov_ah_f1f2)
  test_uw <- mvrnorm(n = 2000, mu = c(mean_uw_f1, mean_uw_f2), Sigma = cov_uw_f1f2)
  
  # add the label column to test data
  test_iy <- cbind(test_iy, rep(1, 2000))
  test_ah <- cbind(test_ah, rep(2, 2000))
  test_uw <- cbind(test_uw, rep(3, 2000))
  
  # now combine all test and train data into their own matrices
  train_data <- rbind(mydata_iy_train, mydata_ah_train, mydata_uw_train)
  test_data <- rbind(test_iy, test_ah, test_uw)
  
  # now randomize the data
  train_data <<- train_data[sample(nrow(train_data)),] 
  test_data <<- test_data[sample(nrow(test_data)),] 
  
  # now make some column names
  colnames(train_data) <<- c("f1.steady", "f2.steady")
  colnames(test_data) <<- c("f1.steady", "f2.steady", "vowel")
}

Task3.plot <- function() {
  plot(train_data[, 2], train_data[, 1], xlim=rev(range(700:3500)),
       ylim=rev(range(300:1300)), col="1", xlab="F2", ylab="F1", pch=1,
       main="The simulated vowel space", cex.lab=1.3, cex.axis=1.3, cex.sub=1.3)
}

Task4.test <- function() {
  training_model <- Mclust(train_data, modelName = "VVV", G = 3, prior = priorControl())
  
  predictions.prob <- estep("VVV", data=test_data[,1:2], parameters=training_model$parameters)
  
  tmp_pred_1 <- predictions.prob$z[,c(1,3,2)]
  tmp_pred_2 <- predictions.prob$z[,c(1,2,3)]
  tmp_pred_3 <- predictions.prob$z[,c(2,1,3)]
  tmp_pred_4 <- predictions.prob$z[,c(2,3,1)]
  tmp_pred_5 <- predictions.prob$z[,c(3,1,2)]
  tmp_pred_6 <- predictions.prob$z[,c(3,2,1)]
  
  pr_1 <- apply(tmp_pred_1, 1, function(x) which(x == max(x))) # orig
  pr_2 <- apply(tmp_pred_2, 1, function(x) which(x == max(x))) # orig
  pr_3 <- apply(tmp_pred_3, 1, function(x) which(x == max(x))) # orig
  pr_4 <- apply(tmp_pred_4, 1, function(x) which(x == max(x))) # orig
  pr_5 <- apply(tmp_pred_5, 1, function(x) which(x == max(x))) # orig
  pr_6 <- apply(tmp_pred_6, 1, function(x) which(x == max(x))) # orig
  
  # Attach predictions to test set each time
  test_data_1 = data.frame(cbind(test_data, pr_1))
  colnames(test_data_1) = c("f1.steady", "f2.steady", "TrueCat", "Pred")
  test_data_2 = data.frame(cbind(test_data, pr_2))
  colnames(test_data_2) = c("f1.steady", "f2.steady", "TrueCat", "Pred")
  test_data_3 = data.frame(cbind(test_data, pr_3))
  colnames(test_data_3) = c("f1.steady", "f2.steady", "TrueCat", "Pred")
  test_data_4 = data.frame(cbind(test_data, pr_4))
  colnames(test_data_4) = c("f1.steady", "f2.steady", "TrueCat", "Pred")
  test_data_5 = data.frame(cbind(test_data, pr_5))
  colnames(test_data_5) = c("f1.steady", "f2.steady", "TrueCat", "Pred")
  test_data_6 = data.frame(cbind(test_data, pr_6))
  colnames(test_data_6) = c("f1.steady", "f2.steady", "TrueCat", "Pred")
  
  # Calculate proportion of correct responses
  accuracy_1 = length(test_data_1$TrueCat[test_data_1$TrueCat == test_data_1$Pred]) /
    length(test_data_1$TrueCat)
  accuracy_2 = length(test_data_2$TrueCat[test_data_2$TrueCat == test_data_2$Pred]) /
    length(test_data_2$TrueCat)
  accuracy_3 = length(test_data_3$TrueCat[test_data_3$TrueCat == test_data_3$Pred]) /
    length(test_data_3$TrueCat)
  accuracy_4 = length(test_data_4$TrueCat[test_data_4$TrueCat == test_data_4$Pred]) /
    length(test_data_4$TrueCat)
  accuracy_5 = length(test_data_5$TrueCat[test_data_5$TrueCat == test_data_5$Pred]) /
    length(test_data_5$TrueCat)
  accuracy_6 = length(test_data_6$TrueCat[test_data_6$TrueCat == test_data_6$Pred]) /
    length(test_data_6$TrueCat)
  
  acc_vec <- c(accuracy_1, accuracy_2, accuracy_3, accuracy_4, accuracy_5, accuracy_6)
  
  acc_max <- max(acc_vec)
  
  # predictions.disc <- apply(predictions.prob$z, 1, function(x) which(x == max(x))) # orig
  
  # Attach predictions to test set
  # test_data = data.frame(cbind(test_data, predictions.disc))
  # colnames(test_data) = c("f1.steady", "f2.steady", "TrueCat", "Pred")
  
  # Calculate proportion of correct responses
  accuracy = length(test_data$TrueCat[test_data$TrueCat == test_data$Pred]) /
    length(test_data$TrueCat)
  
  # test every category
  
  
  print(accuracy)
}

Task4 <- function() {
  training_model <- Mclust(train_data, modelName = "VVV", G = 3, prior = priorControl())
  predictions.prob <- estep("VVV", data=test_data[,1:2], parameters=training_model$parameters)
  
  cat_ords <- permutations(n = 3, r= 3, v = 1:3)
  
  final_label_ord <- c()
  final_pred_labels <- c()
  cur_max <- -1000000000
  for(i in 1:length(cat_ords[,1])) {
    cur_ord <- cat_ords[i, ]
    
    cur_pred <- predictions.prob$z[,cur_ord]
    cur_pred_vec <- apply(cur_pred, 1, function(x) which(x == max(x)))
    
    # compare accuracies
    true_labels <- test_data[,3]
    pred_labels <- cur_pred_vec
    tmp_acc = length(true_labels[true_labels == pred_labels]) /
      length(true_labels)
    
    if(tmp_acc > cur_max) {
      cur_max <- tmp_acc
      final_label_ord <- cur_ord
      final_pred_labels <- cur_pred_vec
    }
    
  }
  
  Task4_data <<- cbind(test_data, final_pred_labels)
  
  tmp <- 'tmp'
  
}

### NOTE: change this from question 4
Task5 <- function() {
  training_model <- Mclust(train_data, modelName = "VVV", G = 3, prior = priorControl())
  predictions.prob <- estep("VVV", data=test_data[,1:2], parameters=training_model$parameters)
  
  cat_ords <- permutations(n = 3, r= 3, v = 1:3)
  
  final_label_ord <- c()
  final_pred_labels <- c()
  cur_max <- -1000000000
  for(i in 1:length(cat_ords[,1])) {
    cur_ord <- cat_ords[i, ]
    
    cur_pred <- predictions.prob$z[,cur_ord]
    cur_pred_vec <- apply(cur_pred, 1, function(x) which(x == max(x)))
    
    # compare accuracies
    true_labels <- test_data[,3]
    pred_labels <- cur_pred_vec
    tmp_acc = length(true_labels[true_labels == pred_labels]) /
      length(true_labels)
    
    if(tmp_acc > cur_max) {
      cur_max <- tmp_acc
      final_label_ord <- cur_ord
      final_pred_labels <- cur_pred_vec
    }
    
  }
  
  Task4_data <<- cbind(test_data, final_pred_labels)
  
  tmp <- 'tmp'
  
}

### NOTE: Same as 5, use iy, ih, eh | make eh half as often as others
Task6 <- function() {
  # iy == 1 | ih == 2 | eh == 3
  point_vowels_6 <<- fine_sampling[with(fine_sampling, vowel == "iy" | vowel == "ih" | vowel == "eh"), ]
  
  mean_iy_f1 <- mean(point_vowels_6[point_vowels_6$vowel == "iy", ]$f1.steady)
  mean_iy_f2 <- mean(point_vowels_6[point_vowels_6$vowel == "iy", ]$f2.steady)
  mean_ih_f1 <- mean(point_vowels_6[point_vowels_6$vowel == "ih", ]$f1.steady)
  mean_ih_f2 <- mean(point_vowels_6[point_vowels_6$vowel == "ih", ]$f2.steady)
  mean_eh_f1 <- mean(point_vowels_6[point_vowels_6$vowel == "eh", ]$f1.steady)
  mean_eh_f2 <- mean(point_vowels_6[point_vowels_6$vowel == "eh", ]$f2.steady)
  
  mat_iy <- cbind(point_vowels_6[point_vowels_6$vowel == "iy", ]$f1.steady
                  , point_vowels_6[point_vowels_6$vowel == "iy", ]$f2.steady)
  mat_ih <- cbind(point_vowels_6[point_vowels_6$vowel == "ih", ]$f1.steady
                  , point_vowels_6[point_vowels_6$vowel == "ih", ]$f2.steady)
  mat_eh <- cbind(point_vowels_6[point_vowels_6$vowel == "eh", ]$f1.steady
                  , point_vowels_6[point_vowels_6$vowel == "eh", ]$f2.steady)
  
  cov_iy_f1f2 <- cov(mat_iy)
  cov_ih_f1f2 <- cov(mat_ih)
  cov_eh_f1f2 <- cov(mat_eh)
  
  # now fake the training data
  mydata_iy_train <- mvrnorm(n = 2000, mu = c(mean_iy_f1, mean_iy_f2), Sigma = cov_iy_f1f2)
  mydata_ih_train <- mvrnorm(n = 2000, mu = c(mean_ih_f1, mean_ih_f2), Sigma = cov_ih_f1f2)
  mydata_eh_train <- mvrnorm(n = 2000, mu = c(mean_eh_f1, mean_eh_f2), Sigma = cov_eh_f1f2)
  
  # now fake the test data
  test_iy <- mvrnorm(n = 2000, mu = c(mean_iy_f1, mean_iy_f2), Sigma = cov_iy_f1f2)
  test_ih <- mvrnorm(n = 2000, mu = c(mean_ih_f1, mean_ih_f2), Sigma = cov_ih_f1f2)
  test_eh <- mvrnorm(n = 1000, mu = c(mean_eh_f1, mean_eh_f2), Sigma = cov_eh_f1f2)
  
  # add the label column to test data
  test_iy <- cbind(test_iy, rep(1, 2000))
  test_ih <- cbind(test_ih, rep(2, 2000))
  test_eh <- cbind(test_eh, rep(3, 1000)) # make this half as much as the others
  
  # now combine all test and train data into their own matrices
  train_data <- rbind(mydata_iy_train, mydata_ih_train, mydata_eh_train)
  test_data <- rbind(test_iy, test_ih, test_eh)
  
  # now randomize the data
  train_data_6 <<- train_data[sample(nrow(train_data)),] 
  test_data_6 <<- test_data[sample(nrow(test_data)),] 
  
  # now make some column names
  colnames(train_data) <<- c("f1.steady", "f2.steady")
  colnames(test_data) <<- c("f1.steady", "f2.steady", "vowel")
  
  ###
  ### Now we do the model training
  ###
  
  training_model <- Mclust(train_data_6, modelName = "VVV", G = 3, prior = priorControl())
  predictions.prob <- estep("VVV", data=test_data_6[,1:2], parameters=training_model$parameters)
  
  cat_ords <- permutations(n = 3, r= 3, v = 1:3)
  
  final_label_ord <- c()
  final_pred_labels <- c()
  cur_max <- -1000000000
  for(i in 1:length(cat_ords[,1])) {
    cur_ord <- cat_ords[i, ]
    
    cur_pred <- predictions.prob$z[,cur_ord]
    cur_pred_vec <- apply(cur_pred, 1, function(x) which(x == max(x)))
    
    # compare accuracies
    true_labels <- test_data_6[,3]
    pred_labels <- cur_pred_vec
    tmp_acc = length(true_labels[true_labels == pred_labels]) /
      length(true_labels)
    
    if(tmp_acc > cur_max) {
      cur_max <- tmp_acc
      final_label_ord <- cur_ord
      final_pred_labels <- cur_pred_vec
    }
    
  }
  
  Task6_data <<- cbind(test_data_6, final_pred_labels)
  
  print(paste("Task6 Accuracy: ", cur_max))
}

Task7.1 <- function() {
  # iy == 1 | ih == 2 | eh == 3
  point_vowels_7 <<- fine_sampling[with(fine_sampling, vowel == "iy" | vowel == "ih" | vowel == "eh"), ]
  
  mean_iy_f1 <- mean(point_vowels_7[point_vowels_7$vowel == "iy", ]$f1.steady)
  mean_iy_f2 <- mean(point_vowels_7[point_vowels_7$vowel == "iy", ]$f2.steady)
  mean_iy_f3 <- mean(point_vowels_7[point_vowels_7$vowel == "iy", ]$f3.steady)
  
  mean_ih_f1 <- mean(point_vowels_7[point_vowels_7$vowel == "ih", ]$f1.steady)
  mean_ih_f2 <- mean(point_vowels_7[point_vowels_7$vowel == "ih", ]$f2.steady)
  mean_ih_f3 <- mean(point_vowels_7[point_vowels_7$vowel == "ih", ]$f3.steady)
  
  mean_eh_f1 <- mean(point_vowels_7[point_vowels_7$vowel == "eh", ]$f1.steady)
  mean_eh_f2 <- mean(point_vowels_7[point_vowels_7$vowel == "eh", ]$f2.steady)
  mean_eh_f3 <- mean(point_vowels_7[point_vowels_7$vowel == "eh", ]$f3.steady)
  
  mat_iy <- cbind(point_vowels_7[point_vowels_7$vowel == "iy", ]$f1.steady
                  , point_vowels_7[point_vowels_7$vowel == "iy", ]$f2.steady
                  , point_vowels_7[point_vowels_7$vowel == "iy", ]$f3.steady)
  mat_ih <- cbind(point_vowels_7[point_vowels_7$vowel == "ih", ]$f1.steady
                  , point_vowels_7[point_vowels_7$vowel == "ih", ]$f2.steady
                  , point_vowels_7[point_vowels_7$vowel == "ih", ]$f3.steady)
  mat_eh <- cbind(point_vowels_7[point_vowels_7$vowel == "eh", ]$f1.steady
                  , point_vowels_7[point_vowels_7$vowel == "eh", ]$f2.steady
                  , point_vowels_7[point_vowels_7$vowel == "eh", ]$f3.steady)
  
  cov_iy_f1f2 <- cov(mat_iy)
  cov_ih_f1f2 <- cov(mat_ih)
  cov_eh_f1f2 <- cov(mat_eh)
  
  # now fake the training data
  mydata_iy_train <- mvrnorm(n = 2000, mu = c(mean_iy_f1, mean_iy_f2, mean_iy_f3), Sigma = cov_iy_f1f2)
  mydata_ih_train <- mvrnorm(n = 2000, mu = c(mean_ih_f1, mean_ih_f2, mean_ih_f3), Sigma = cov_ih_f1f2)
  mydata_eh_train <- mvrnorm(n = 2000, mu = c(mean_eh_f1, mean_eh_f2, mean_eh_f3), Sigma = cov_eh_f1f2)
  
  # now fake the test data
  test_iy <- mvrnorm(n = 2000, mu = c(mean_iy_f1, mean_iy_f2, mean_iy_f3), Sigma = cov_iy_f1f2)
  test_ih <- mvrnorm(n = 2000, mu = c(mean_ih_f1, mean_ih_f2, mean_ih_f3), Sigma = cov_ih_f1f2)
  test_eh <- mvrnorm(n = 2000, mu = c(mean_eh_f1, mean_eh_f2, mean_eh_f3), Sigma = cov_eh_f1f2)
  
  # add the label column to test data
  test_iy <- cbind(test_iy, rep(1, 2000))
  test_ih <- cbind(test_ih, rep(2, 2000))
  test_eh <- cbind(test_eh, rep(3, 2000)) # make this half as much as the others
  
  # now combine all test and train data into their own matrices
  train_data <- rbind(mydata_iy_train, mydata_ih_train, mydata_eh_train)
  test_data <- rbind(test_iy, test_ih, test_eh)
  
  # now randomize the data
  train_data_7 <<- train_data[sample(nrow(train_data)),] 
  test_data_7 <<- test_data[sample(nrow(test_data)),] 
  
  # now make some column names
  colnames(train_data_7) <<- c("f1.steady", "f2.steady", "f3.steady")
  colnames(test_data_7) <<- c("f1.steady", "f2.steady", "f3.steady", "vowel")
  
  ###
  ### Now we do the model training
  ###
  
  training_model <- Mclust(train_data_7, modelName = "VVV", G = 3, prior = priorControl())
  predictions.prob <- estep("VVV", data=test_data_7[,1:3], parameters=training_model$parameters)
  
  cat_ords <- permutations(n = 3, r= 3, v = 1:3)
  
  final_label_ord <- c()
  final_pred_labels <- c()
  cur_max <- -1000000000
  for(i in 1:length(cat_ords[,1])) {
    cur_ord <- cat_ords[i, ]
    
    cur_pred <- predictions.prob$z[,cur_ord]
    cur_pred_vec <- apply(cur_pred, 1, function(x) which(x == max(x)))
    
    # compare accuracies
    true_labels <- test_data_7[,4]
    pred_labels <- cur_pred_vec
    tmp_acc = length(true_labels[true_labels == pred_labels]) /
      length(true_labels)
    
    if(tmp_acc > cur_max) {
      cur_max <- tmp_acc
      final_label_ord <- cur_ord
      final_pred_labels <- cur_pred_vec
    }
    
  }
  
  Task7_data <<- cbind(test_data_7, final_pred_labels)
  
  print(paste("Task7 Accuracy: ", cur_max))
}

### Use duration
Task7.2 <- function() {
  # iy == 1 | ih == 2 | eh == 3
  point_vowels_72 <<- fine_sampling[with(fine_sampling, vowel == "iy" | vowel == "ih" | vowel == "eh"), ]
  
  mean_iy_f1 <- mean(point_vowels_72[point_vowels_72$vowel == "iy", ]$f1.steady)
  mean_iy_f2 <- mean(point_vowels_72[point_vowels_72$vowel == "iy", ]$f2.steady)
  mean_iy_f3 <- mean(point_vowels_72[point_vowels_72$vowel == "iy", ]$duration)
  
  mean_ih_f1 <- mean(point_vowels_72[point_vowels_72$vowel == "ih", ]$f1.steady)
  mean_ih_f2 <- mean(point_vowels_72[point_vowels_72$vowel == "ih", ]$f2.steady)
  mean_ih_f3 <- mean(point_vowels_72[point_vowels_72$vowel == "ih", ]$duration)
  
  mean_eh_f1 <- mean(point_vowels_72[point_vowels_72$vowel == "eh", ]$f1.steady)
  mean_eh_f2 <- mean(point_vowels_72[point_vowels_72$vowel == "eh", ]$f2.steady)
  mean_eh_f3 <- mean(point_vowels_72[point_vowels_72$vowel == "eh", ]$duration)
  
  mat_iy <- cbind(point_vowels_72[point_vowels_72$vowel == "iy", ]$f1.steady
                  , point_vowels_72[point_vowels_72$vowel == "iy", ]$f2.steady
                  , point_vowels_72[point_vowels_72$vowel == "iy", ]$duration)
  mat_ih <- cbind(point_vowels_72[point_vowels_72$vowel == "ih", ]$f1.steady
                  , point_vowels_72[point_vowels_72$vowel == "ih", ]$f2.steady
                  , point_vowels_72[point_vowels_72$vowel == "ih", ]$duration)
  mat_eh <- cbind(point_vowels_72[point_vowels_72$vowel == "eh", ]$f1.steady
                  , point_vowels_72[point_vowels_72$vowel == "eh", ]$f2.steady
                  , point_vowels_72[point_vowels_72$vowel == "eh", ]$duration)
  
  cov_iy_f1f2 <- cov(mat_iy)
  cov_ih_f1f2 <- cov(mat_ih)
  cov_eh_f1f2 <- cov(mat_eh)
  
  # now fake the training data
  mydata_iy_train <- mvrnorm(n = 2000, mu = c(mean_iy_f1, mean_iy_f2, mean_iy_f3), Sigma = cov_iy_f1f2)
  mydata_ih_train <- mvrnorm(n = 2000, mu = c(mean_ih_f1, mean_ih_f2, mean_ih_f3), Sigma = cov_ih_f1f2)
  mydata_eh_train <- mvrnorm(n = 2000, mu = c(mean_eh_f1, mean_eh_f2, mean_eh_f3), Sigma = cov_eh_f1f2)
  
  # now fake the test data
  test_iy <- mvrnorm(n = 2000, mu = c(mean_iy_f1, mean_iy_f2, mean_iy_f3), Sigma = cov_iy_f1f2)
  test_ih <- mvrnorm(n = 2000, mu = c(mean_ih_f1, mean_ih_f2, mean_ih_f3), Sigma = cov_ih_f1f2)
  test_eh <- mvrnorm(n = 2000, mu = c(mean_eh_f1, mean_eh_f2, mean_eh_f3), Sigma = cov_eh_f1f2)
  
  # add the label column to test data
  test_iy <- cbind(test_iy, rep(1, 2000))
  test_ih <- cbind(test_ih, rep(2, 2000))
  test_eh <- cbind(test_eh, rep(3, 2000)) 
  
  # now combine all test and train data into their own matrices
  train_data <- rbind(mydata_iy_train, mydata_ih_train, mydata_eh_train)
  test_data <- rbind(test_iy, test_ih, test_eh)
  
  # now randomize the data
  train_data_72 <<- train_data[sample(nrow(train_data)),] 
  test_data_72 <<- test_data[sample(nrow(test_data)),] 
  
  # now make some column names
  colnames(train_data_72) <<- c("f1.steady", "f2.steady", "duration")
  colnames(test_data_72) <<- c("f1.steady", "f2.steady", "duration", "vowel")
  
  ###
  ### Now we do the model training
  ###
  
  training_model <- Mclust(train_data_72, modelName = "VVV", G = 3, prior = priorControl())
  predictions.prob <- estep("VVV", data=test_data_72[,1:3], parameters=training_model$parameters)
  
  cat_ords <- permutations(n = 3, r= 3, v = 1:3)
  
  final_label_ord <- c()
  final_pred_labels <- c()
  cur_max <- -1000000000
  for(i in 1:length(cat_ords[,1])) {
    cur_ord <- cat_ords[i, ]
    
    cur_pred <- predictions.prob$z[,cur_ord]
    cur_pred_vec <- apply(cur_pred, 1, function(x) which(x == max(x)))
    
    # compare accuracies
    true_labels <- test_data_72[,4]
    pred_labels <- cur_pred_vec
    tmp_acc = length(true_labels[true_labels == pred_labels]) /
      length(true_labels)
    
    if(tmp_acc > cur_max) {
      cur_max <- tmp_acc
      final_label_ord <- cur_ord
      final_pred_labels <- cur_pred_vec
    }
    
  }
  
  Task72_data <<- cbind(test_data_72, final_pred_labels)
  
  print(paste("Task7.2 Accuracy: ", cur_max))
}



