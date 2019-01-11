library("tidyr")
library("MASS")

task1 <- function() {
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
}






