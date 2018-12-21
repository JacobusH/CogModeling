library(raster)  

load.data <- function() {
  # loads necessary data into global variables
  table_neuralResponses <<- read.table("E:\\Homework\\Cognitive Modeling\\Lab2\\RSA lab assignment\\NeuralResponses")
  table_categoryVecs <<- read.table("E:\\Homework\\Cognitive Modeling\\Lab2\\RSA lab assignment\\CategoryVectors")
  table_categoryLabels <<- read.table("E:\\Homework\\Cognitive Modeling\\Lab2\\RSA lab assignment\\CategoryLabels")
}


addNoise <- function() {
  # To simulate data for each subject, add normally-distributed noise to the starting data
  # (using rnorm), with a mean of zero and a standard deviation of one. Repeat this 12
  # times to simulate 12 'subjects'. Keep the original data and the data from each of the 12 'subjects'.
  
  list_subjects <<- list()
  for(x in 1:12) {
    subject_data <- as.data.frame(lapply(table_neuralResponses, function(x) { x +  rnorm(n = 1, mean = 0, sd = 1) }))
    list_subjects[[x]] <<- subject_data
  }
}


question1 <- function() {
  # ask about what "mean response" means....I have no idea
}

question2 <- function() {
  # add original table at end of list for comparisons
  list_subjects[[13]] <- table_neuralResponses
  
  RDM_Comparisons_t <<- list()
  for(i in 1:13) {
    t_sub <- t(list_subjects[[i]])
    RDM_Comparisons_t[[i]] <<- 1 - cor(t_sub, use = "everything", method = "pearson") 
  }
  
  for(x in length(RDM_Comparisons)) {
    plot(raster(RDM_Comparisons_t[[x]]))
    # image(RDM_Comparisons[[1]])
  }
}

question2.plot <- function(idx, asImage = FALSE) {
  if(!asImage) {
    plot(raster(RDM_Comparisons_t[[idx]]))
  }
  else {
    image(RDM_Comparisons[[idx]])
  }
}

question3 <- function() {
  table_avgSubject <<- data.frame(matrix(NA, nrow = 92, ncol = 100))
  for(row in 1:92) {
    for(col in 1:100) {
      cur_vec <- c()
      for(cur_tableIdx in 1:12) {
        cur_vec <- c(cur_vec, list_subjects[[cur_tableIdx]][row, col])
      }
      cur_cellMean <- mean(cur_vec)
      table_avgSubject[row, col] <<- cur_cellMean 
    }
  }
}

question4 <- function() {
  # use col 1 of category_vectors | animate objects
  # compare each pair of values | 1 for same, 0 for different
  # use two-sided unpaired t-test to compare dissimilarity of pairs w/ different animacy
  # use original data for this question, comapre every row

  # make vector of 0's and 1's for animacy
  tableMap_animacy <<- data.frame(matrix(NA, nrow = 92, ncol = 92))
  tableMap_animacyDiff <<- data.frame(matrix(NA, nrow = 92, ncol = 92))
  for(i in 1:length(table_categoryVecs[, 1])) {
    for(j in 1:length(table_categoryVecs[, 1])) {
      if(table_categoryVecs[i, 1] == table_categoryVecs[j, 1]) { # col 1 is animacy
        tableMap_animacy[i, j] <<- TRUE
        tableMap_animacyDiff[i, j] <<- FALSE
      }
      else {
        tableMap_animacy[i, j] <<- FALSE
        tableMap_animacyDiff[i, j] <<- TRUE
      }
    }
  }
  
  # apply the mask to make two tables of same-animacy vs diff-animacy on the RDM of orig data
  anim_same <- RDM_Comparisons_t[[13]]
  anim_same[] <- as.matrix(RDM_Comparisons_t[[13]])[as.logical(NA^!tableMap_animacy)]
  
  anim_diff <- RDM_Comparisons_t[[13]]
  anim_diff[] <- as.matrix(RDM_Comparisons_t[[13]])[as.logical(NA^!tableMap_animacyDiff)]
  
  # cut the tables in half along the diagonal, so we don't double compare in the t-test
  for(row in 1:92) {
    for(col in row:92){
      if(row != col) {
        anim_same[row, col] <- NA
        anim_diff[row, col] <- NA
      }
    }
  }
  
  # now perform the t-test
  t.test(anim_same, anim_diff)
}

question5 <- function() {
  # compare t and p values for indiv subj against avg sub from q3
}

question6 <- function() {
  # make vector of 0's and 1's for faceness
  tableMap_faceness <<- data.frame(matrix(NA, nrow = 92, ncol = 92))
  for(i in 1:length(table_categoryVecs[, 1])) {
    for(j in 1:length(table_categoryVecs[, 1])) {
      if(table_categoryVecs[i, 6] == table_categoryVecs[j, 6]) { # col 6 is face
        tableMap_faceness[i, j] <<- TRUE
      }
      else {
        tableMap_faceness[i, j] <<- FALSE
      }
    }
  }
  
  # test <- table_neuralResponses[with(tableMap_animacy, tableMap_animacy == 1), ]
  # table_anim <- 
  #   for(i in 1:92) {
  #     for(j in 1:92) {
  #       if(tableMap_faceness[i, j] == 1) { # same animacy
  #         
  #       }
  #     }
  #   } 
}

question8 <- function() {
  # make vector of 0's and 1's for humanness
  tableMap_humanness <<- data.frame(matrix(NA, nrow = 92, ncol = 92))
  for(i in 1:length(table_categoryVecs[, 1])) {
    for(j in 1:length(table_categoryVecs[, 1])) {
      if(table_categoryVecs[i, 3] == table_categoryVecs[j, 3]) { # col 3 is human
        tableMap_humanness[i, j] <<- TRUE
      }
      else {
        tableMap_humanness[i, j] <<- FALSE
      }
    }
  }
}

