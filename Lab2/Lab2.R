library(raster)  
library(heplots)

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
  table_avgSubject <<- data.frame(matrix(NA, nrow = 92, ncol = 92))
  for(row in 1:92) {
    for(col in 1:92) {
      cur_vec <- c()
      for(cur_tableIdx in 1:12) {
        cur_vec <- c(cur_vec, RDM_Comparisons_t[[cur_tableIdx]][row, col])
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
  # use original data for this question, compare every row

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
      anim_same[row, col] <- NA
      anim_diff[row, col] <- NA
    }
  }
  
  # now perform the t-test
  print("T-test for same vs dif animacy")
  print(t.test(anim_same, anim_diff))
  
  # set up for q9
  q9_animacy <<- anim_same
}

question5 <- function() {
  # compare t and p values for indiv subj against avg sub from q3
  
  # apply the mask to make two tables of same-animacy vs diff-animacy on the RDM of a subject
  anim_same_subj <- RDM_Comparisons_t[[2]]
  anim_same_subj[] <- as.matrix(RDM_Comparisons_t[[2]])[as.logical(NA^!tableMap_animacy)]
  anim_diff_subj <- RDM_Comparisons_t[[2]]
  anim_diff_subj[] <- as.matrix(RDM_Comparisons_t[[2]])[as.logical(NA^!tableMap_animacyDiff)]
  
  # apply the mask to make two tables of same-animacy vs diff-animacy on the RDM of avg subject
  anim_same_avgSubj <- table_avgSubject
  anim_same_avgSubj[] <- as.matrix(table_avgSubject)[as.logical(NA^!tableMap_animacy)]
  anim_diff_avgSubj <- table_avgSubject
  anim_diff_avgSubj[] <- as.matrix(table_avgSubject)[as.logical(NA^!tableMap_animacyDiff)]
  
  # cut the tables in half along the diagonal, so we don't double compare in the t-test
  for(row in 1:92) {
    for(col in row:92){
      anim_same_subj[row, col] <- NA
      anim_diff_subj[row, col] <- NA
      
      anim_same_avgSubj[row, col] <- NA
      anim_diff_avgSubj[row, col] <- NA
    }
  }
  
  # now perform the t-test
  normal_subj <- t.test(anim_same_subj, anim_diff_subj)
  avg_subj <- t.test(anim_same_avgSubj, anim_diff_avgSubj)
  
  tmp <- 'tmp'
  
}

question6 <- function() {
  # make vector of 0's and 1's for faceness
  face_vector <- table_categoryVecs[,6]
  same_faceness <- c()
  diff_faceness <- c()
  
  for(i in 1:91){
    for(r in (i + 1):92){
      if(face_vector[i] == face_vector[r]){
        same_faceness <- c(same_faceness, RDM_Comparisons_t[[13]][i,r])
      } else if(face_vector[i] != face_vector[r]){
        diff_faceness <- c(diff_faceness, RDM_Comparisons_t[[13]][i,r])
      }
    }
  }
  
  #t-test between same faceness and different faceness
  print("T-test for same vs dif faceness")
  print(t.test(face_same, face_diff))
  
  
}

question7 <- function(){
  animacy_vector <<- table_categoryVecs[,1]
  face_same_wo_inanimate <- face_same
  face_diff_wo_inanimate <- face_diff
  
  #removing values, where either of the objects is inanimate
  for(i in 1:92){
    for(r in 1:92){
      if(animacy_vector[i] == 0 || animacy_vector[r] == 0){
        face_same_wo_inanimate[i][r] <- NA
        face_diff_wo_inanimate[i][r] <- NA
      }
    }
  }
  
  #t-test between same faceness between different faceness by comparing only with animate objects
  print("T-test for same vs dif faceness")
  print(t.test(face_same_wo_inanimate, face_diff_wo_inanimate))
  
  # setup for q9
  q9_faceness <<- face_same_wo_inanimate
  
}

question8 <- function() {
  # TODO: need to combine with animacy. this is animacy + humaness, not just humanness
  # make vector of 0's and 1's for humanness
  # keep only animate-animate, get rid of animate-in and in-in
  animacy_vector <<- table_categoryVecs[,1]
  humaness_vector <<- table_categoryVecs[,3]
  vec_same <- c()
  vec_diff <- c()
  
  for(i in 1:92) {
    for(j in (i + 1):92) {
      if(animacy_vector[i] == 1 && animacy_vector[j] == 1
         && humaness_vector[i] == humaness_vector[j] ) { # if both are animate and have same humaness
            vec_same <- c(vec_same, RDM_Comparisons_t[[13]][i, j])
      } 
      else if(animacy_vector[i] == 1 && animacy_vector[j] == 1
             && !(humaness_vector[i] == humaness_vector[j]) ) {# both animate and diff humaness
        vec_diff <- c(vec_diff, RDM_Comparisons_t[[13]][i, j])
      }
    }
  }
  
  #t-test between same faceness between different faceness by comparing only with animate objects
  print("T-test for same vs dif animacy+humaness")
  print(t.test(vec_same, vec_diff))
}

question9 <- function() {
  # from the faceness, make faceness + both-animate
  tmp <- q9_faceness
  for(i in 1:92) {
    for(j in 1:92) {
      # set anything not animate-animate to NA
      if(table_categoryVecs[i, 1] != 1 && table_categoryVecs[j, 1] != 1) { 
        tmp[i, j] <- NA
      }
    }
  }
  
  
  # use 3 groups, original, faceness, animacy
  vec_orig <- as.vector(RDM_Comparisons_t[[13]])
  
  vec_animacy <- as.vector(q9_animacy)
  vec_animacy[is.na(vec_animacy)] <- 0

  vec_facenessAnimacy <- as.vector(tmp)
  vec_facenessAnimacy[is.na(vec_facenessAnimacy)] <- 0
  
  res <- cbind(vec_orig, vec_animacy, vec_facenessAnimacy)
  colnames(res) <- c("orig", "animacy", "facenessAnimacy")
  
  ovv <<- aov(orig ~ animacy + facenessAnimacy, data = as.data.frame(res))
  print(summary(ovv))
}

question10 <- function() {
  print(etasq(ovv))
}

