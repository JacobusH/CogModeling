library("combinat")
library("ggplot2")

loadData <- function() {
  data <- read.csv("E:/Homework/Cognitive Modeling/Lab1/keyPressDataWithLaneDeviation.csv", header = TRUE
           , sep = ",", quote = "\"", dec = ".")

  table_drift <- read.csv("E:/Homework/Cognitive Modeling/Lab1/tableOfDriftValuesCalibration.csv", header = TRUE
                   , sep = ",", quote = "\"", dec = ".")
}

#########
## QUESTION 1
#########
question1.stats <- function(vec, asSec = FALSE) {
  trial_sd <- sd(vec) # get sd of time in those rows
  trial_mean <- mean(vec) # get mean
  trial_stderr <- trial_sd/sqrt(length(vec)) # get std err
  
  if(asSec) {
    trial_sd <- trial_sd / 1000
    trial_mean <- trial_mean / 1000
    trial_stderr <- trial_stderr / 1000
  }
  
  
  return(c(trial_sd, trial_mean, trial_stderr))
}

question1 <- function() {
  question1.a()
  question1.b()
  question1.c()
}

question1.a <- function() {
  # Dual Steer Focus
  steer <- data[with(data, partOfExperiment == "dualSteerFocus" & typingErrorMadeOnTrial == 0 & phoneNrLengthAfterKeyPress == 11), ]
  # steer <- steer[with(steer, phoneNrLengthAfterKeyPress == 11), ]
  steer_pp_mean <- aggregate(steer$timeRelativeToTrialStart, list(steer$pp), mean)

  # Dual Dial Focus
  dial <- data[with(data, partOfExperiment == "dualDialFocus" & typingErrorMadeOnTrial == 0), ]
  dial <- dial[with(dial, phoneNrLengthAfterKeyPress == 11), ]
  dial_pp_mean <- aggregate(dial$timeRelativeToTrialStart, list(dial$pp), mean)

  steered <- question1.stats(steer_pp_mean$x, TRUE)
  dialed <- question1.stats(dial_pp_mean$x, TRUE)
  
  res <- matrix(c(steered, dialed), byrow = 1, nrow = 2)
  rownames(res) <- c("Steering Focus", "Driving Focus")
  colnames(res) <- c("M (s)", "SD (s)", "SE (s)")
  
  csteer <<- steer
  cdial <<- dial
  
  print(res)
}

question1.b <- function() {
  # lane deviation
  devi_steer <- data[with(data, partOfExperiment == "dualSteerFocus" & typingErrorMadeOnTrial == 0), ]
  devi_steer_pp_mean <- aggregate(abs(devi_steer$lanePosition), list(devi_steer$pp), mean)
  devi_steer_pp_mad <- mad(devi_steer_pp_mean$x)

  devi_dial <- data[with(data, partOfExperiment == "dualDialFocus" & typingErrorMadeOnTrial == 0), ]
  devi_dial_pp_mean <- aggregate(abs(devi_dial$lanePosition), list(devi_dial$pp), mean)
  devi_dial_pp_mad <- mad(devi_dial_pp_mean$x)
  
  steered <- question1.stats(devi_steer_pp_mean$x, FALSE)
  dialed <- question1.stats(devi_dial_pp_mean$x, FALSE)
  
  res <- matrix(c(steered, dialed), byrow = 1, nrow = 2)
  rownames(res) <- c("Steering Focus", "Driving Focus")
  colnames(res) <- c("M (m)", "SD (m)", "SE (m)")
  print(res)
}

question1.c <- function() {
  # lane deviation
  devi_steer <- data[with(data, partOfExperiment == "dualSteerFocus" & typingErrorMadeOnTrial == 0), ]
  devi_steer_pp_mean <- aggregate(abs(devi_steer$lanePosition)
                            , list(participant = devi_steer$pp, keypress = devi_steer$phoneNrLengthAfterKeyPress)
                            , mean)
  devi_steer_keypress_mean <- aggregate(devi_steer_pp_mean$x
                                        , list(keypress = devi_steer_pp_mean$keypress)
                                        , mean)
  
  devi_dial <- data[with(data, partOfExperiment == "dualDialFocus" & typingErrorMadeOnTrial == 0), ]
  devi_dial_pp_mean <- aggregate(abs(devi_dial$lanePosition)
                                  , list(participant = devi_dial$pp, keypress = devi_dial$phoneNrLengthAfterKeyPress)
                                  , mean)
  devi_dial_keypress_mean <- aggregate(devi_dial_pp_mean$x
                                        , list(keypress = devi_dial_pp_mean$keypress)
                                        , mean)
  
  # combine into one frame
  all_results <- data.frame(devi_dial_keypress_mean$keypress, devi_dial_keypress_mean$x
                            , devi_steer_keypress_mean$keypress, devi_steer_keypress_mean$x)
  
  # ggplotting
  gplot <- ggplot(all_results)
  gplot <- gplot + geom_point(aes(x = all_results$devi_dial_keypress_mean.keypress
                                  , y = all_results$devi_dial_keypress_mean.x
                                  , colour = "Dialing Focus"), shape = 15)
  gplot <- gplot + geom_point(aes(x = all_results$devi_steer_keypress_mean.keypress
                                  , y = all_results$devi_steer_keypress_mean.x
                                  , colour = "Steering Focus"), shape = 15)
  
  # error bars
  steer_err <- 0.055
  dial_err <- 0.065
  # steer_err <- 0.38
  # dial_err <- 0.26
  gplot <- gplot + geom_errorbar(mapping=aes(x=all_results$devi_steer_keypress_mean.keypress
                                             , ymin=all_results$devi_steer_keypress_mean.x - (steer_err)
                                             , ymax=all_results$devi_steer_keypress_mean.x + (steer_err))
                                 , col = "blue"
                                 , width=.2)
  gplot <- gplot + geom_errorbar(mapping=aes(x=all_results$devi_dial_keypress_mean.keypress
                                             , ymin=all_results$devi_dial_keypress_mean.x - (dial_err)
                                             , ymax=all_results$devi_dial_keypress_mean.x + (dial_err))
                                 , col = "red"
                                 , width=.2)
  
  
  
  gplot <- gplot + scale_x_continuous(breaks = seq(0, 12, by = 2))
  gplot <- gplot + scale_y_continuous(breaks = seq(0, 1, by = 0.2))
  
  gplot <- gplot + scale_colour_manual(
    limits=c("Dialing Focus", "Steering Focus"),
    values=c("red", "blue")
  )
  
  gplot <- gplot + labs(title="Changes in Lane Deviation | Steering Focus vs Dialing Focus",
                        x ="Key Presses", y = "Lane Deviation")
  
  # legend title
  gplot$labels$colour <- "Experimental Conditions"
  
  plot(gplot)
  
    
  #plotting
  # dial_err <- 0.26
  # plot(devi_dial_keypress_mean$keypress, devi_dial_keypress_mean$x, col = "red", ylim = c(-.3,1.3), pch = 2,
  #      main = "Changes of lane deviations with each key press for two experimental conditions"
  #      , ylab = "Lane Deviation"
  #      , xlab = "Key Presses")
  # arrows(devi_dial_keypress_mean$keypress, devi_dial_keypress_mean$x - dial_err
  #        , devi_dial_keypress_mean$keypress, devi_dial_keypress_mean$x + dial_err
  #        , length=0.05, angle=90, code=3, col = "red") # error bars
  # 
  # steer_err <- 0.38
  # points(devi_steer_keypress_mean$keypress, devi_steer_keypress_mean$x, col = "blue", ylim = c(0,1))
  # arrows(devi_steer_keypress_mean$keypress, devi_steer_keypress_mean$x - steer_err
  #        , devi_steer_keypress_mean$keypress, devi_steer_keypress_mean$x + steer_err
  #        , length=0.05, angle=90, code=3, col = "blue")
  # 
  # legend(0, 1, legend=c("Dual Dial", "Dual Steer"),
  #        col=c("red", "blue"), lty=1:2, cex=0.8)
  
  
  
  ## TODO: put error bars
}

#########
## QUESTION 2
#########
question2 <- function() {
  trial_vals <- question2.a()
  simul_vals <- question2.b()
  question2.c(trial_vals, simul_vals)
}

question2.a <- function() {
  drift_per_driver <- table_drift[with(table_drift, trialTime >= 15000 & trialTime <= 18000), ]
  colors <- sample(colors(), 20)
  plot(NULL, xlim = c(15000, 18000), ylim = c(-1, 1.5) )
  
  retq2a <<- c()
  i <- 1;
  while(i <= 20) { # 20 trials
    with(drift_per_driver[drift_per_driver$trial == i,]
         , lines(trialTime, posX, col = colors[i]))
    
    posXs <- drift_per_driver[with(drift_per_driver, trial == i), ]$posX
    
    tmp <- drift_per_driver[drift_per_driver$trial == i, ]
    retq2a <<- c(retq2a, posXs) # collect posx for 2c
    
    i <- i + 1
  }
  
}

question2.b <- function() {
  # plot a simulated drift example (no real data)
  num_measure <- 3000 / 50
  plot(NULL, xlim = c(0, 3000), ylim = c(-2, 2) )
  colors <- sample(colors(), 20)
  x_points <- seq(50, 3000, 50)
  
  retq2b <<- c()
  i <- 1
  while(i <= 20) {
    y_points <- cumsum(rnorm(n = num_measure, mean = 0, sd = .13))
    lines(x_points, y_points, col = colors[i])
    
    retq2b <<- c(retq2b, y_points) # collect posx for 2c
    i <- i + 1
  }
  
}

question2.c <- function(trial_vals, simul_vals) {
  # hist of human trials
  hist(retq2a, col = c("green", "pink"))
  
  # hist of simulated trials
  hist(retq2b)
}

question2.d <- function() {
  sd(retq2a)
  sd(retq2b)
}

question2.e <- function() {
  # plot a simulated drift example (no real data)
  num_measure <- 3000 / 50
  plot(NULL, xlim = c(0, 3000), ylim = c(-2, 2) )
  colors <- sample(colors(), 50)
  x_points <- seq(50, 3000, 50)
  
  retval <- c()
  i <- 1
  while(i <= 50) {
    y_points <- cumsum(rnorm(n = num_measure, mean = 0, sd = .055))
    lines(x_points, y_points, col = sample(colors(), 1))
    
    retval <- c(retval, y_points) # collect posx for 2c
    i <- i + 1
  }
  
  sd(retval)
}

#########
## QUESTION 3
#########

question3.a <- function() {
  # key_press <- data[with(data, typingErrorMadeOnTrial == 0 & partOfExperiment == "singleDialing2"), ]
  # key_press_pp_mean <- aggregate(key_press$timeRelativeToTrialStart, list(key_press$pp), diff)
  # 
  key_press_pp_mean <- with(data[data$typingErrorMadeOnTrial == 0 & data$partOfExperiment == "singleDialing2",]
                            , aggregate(timeRelativeToTrialStart,list(participant=pp),diff))

  len <- length(key_press_pp_mean[ ,2])
  i <- 1
  all_means <- c()
  while(i <= len) {
    curvec <- key_press_pp_mean[i, 2]
    curvec <- as.numeric(unlist(curvec))
    curvec[curvec < 0] <- NA # no negatives
    
    curvec_mean <- mean(curvec, na.rm = TRUE)
    all_means <- c(all_means, curvec_mean)
    
    i <- i + 1
  }
  
  avg <- mean(all_means)
  
  frame
  
  tmp <- 'tmp'
}

#########
## QUESTION 5
#########

question5.a <- function() {
  phnr <- c(0,7,8,5,4,3,2,5,6,9,8)
  breakpoints <- length(phnr) - 1 # don't run last one
  
  for(i in 11) {
    print('hi')
    
    
    # breakpoints <- combn(length(phnr), i)
    # 
    # for(myvar in 1:length(breakpoints)) {
    #   cur_breakpoints <- breakpoints[ , col]
    #   # runexpr(cursplitpoints)
    # }
  }
  
}

question5.c <- function() {
  # Look
  # which	model	strategies	result	in	performance	that	falls	inside	square	area	
  # defined	by	the	error	bars	of	the	human	data.	Specifically,	you	can	use	the	data	to	
  # check	how	frequently	these	model	strategies	interleave,	and	at	which	position	
  # they	do	the	interleaving	for	the	very	first	time	(i.e.,	before	or	after	the	natural	breakpoint?).
  
  ourModel_50sim <- read.table("E:\\Homework\\Cognitive Modeling\\Lab1\\simulation_r3\\50_simul_ourDrift_ourIKI")
  
  # get the human ERROROROROROS
  csteer <<- data[with(data, partOfExperiment == "dualSteerFocus" & typingErrorMadeOnTrial == 0 & Event1 == "Correct"), ]
  steer_ending_times_mean <- mean(csteer$timeRelativeToTrialStart)
  steer_ending_dev_mean <- mean(abs(csteer$lanePosition))
  
  cdial <<- data[with(data, partOfExperiment == "dualDialFocus" & typingErrorMadeOnTrial == 0 & Event1 == "Correct"), ]
  dial_ending_times_mean <- mean(cdial$timeRelativeToTrialStart)
  dial_ending_dev_mean <- mean(abs(cdial$lanePosition))
  
  # err bars
  steer_err_dialtimes <- sd(csteer$timeRelativeToTrialStart)/sqrt(length(csteer$timeRelativeToTrialStart)) # get std err
  dial_err_dialtimes <- sd(cdial$timeRelativeToTrialStart)/sqrt(length(cdial$timeRelativeToTrialStart)) # get std err
  
  steer_err_dev_time <- 0.38
  dial_err_dev_time <- 0.26
  
  steer_err_dev_meter <- 0.055
  dial_err_dev_meter <- 0.065
  
  # dial err bars vertical
  steer_ending_times_mean <- steer_ending_times_mean / 1000
  dial_ending_times_mean <- dial_ending_times_mean / 1000

  # steering human error square area
  steer_ymax <- steer_ending_dev_mean + (steer_err_dev_meter)
  steer_ymin <- steer_ending_dev_mean - (steer_err_dev_meter)
  
  steer_xmax <- steer_ending_times_mean + (dial_err_dev_time)
  steer_xmin <- steer_ending_times_mean - (dial_err_dev_time)
  
  # inside_sqErr <- ourModel_50sim[ourModel_50sim$dev >= steer_ymin && ourModel_50sim$dev <= steer_ymax
  #                                && ourModel_50sim$TrailTime / 1000 >= steer_xmin 
  #                                && ourModel_50sim$TrailTime / 1000 <= steer_xmax]
  
  
  inside_sqErr <- ourModel_50sim[with(ourModel_50sim
                                        , dev >= steer_ymin 
                                          & dev <= steer_ymax 
                                          & (TrialTime / 1000) >= steer_xmin
                                          & (TrialTime / 1000) <= steer_xmax), ]
  
  tmp <- 'tmp'
  
}

#########
## BONUSES
#########

bonus1 <- function() {
  # find average key press speed per participant
  # key_press_pp_mean <- with(data[data$typingErrorMadeOnTrial == 0 & data$partOfExperiment == "singleDialing2",]
  #                           , aggregate(timeRelativeToTrialStart,list(participant=pp),diff))
  # 
  # len <- length(key_press_pp_mean[ ,2])
  # i <- 1
  # all_means <- c()
  # while(i <= len) {
  #   curvec <- key_press_pp_mean[i, 2]
  #   curvec <- as.numeric(unlist(curvec))
  #   curvec[curvec < 0] <- NA # no negatives
  #   
  #   curvec_mean <- mean(curvec, na.rm = TRUE)
  #   all_means <- c(all_means, curvec_mean)
  #   
  #   i <- i + 1
  # }
  
  # load the 3 model results
  bonus_186 <- read.table("E:\\Homework\\Cognitive Modeling\\Lab1\\bonus1\\bonus3_186_final");
  bonus_278 <- read.table("E:\\Homework\\Cognitive Modeling\\Lab1\\bonus1\\bonus3_278_final");
  bonus_321 <- read.table("E:\\Homework\\Cognitive Modeling\\Lab1\\bonus1\\bonus3_321_final");
  
  # combine into one frame 
  all_results <- data.frame(bonus_186$TrialTime, bonus_186$dev
               , bonus_278$TrialTime, bonus_278$dev
               , bonus_321$TrialTime, bonus_321$dev)
  
  # get the human ERROROROROROS
  csteer <<- data[with(data, partOfExperiment == "dualSteerFocus" & typingErrorMadeOnTrial == 0 & Event1 == "Correct"), ]
  steer_ending_times_mean <- mean(csteer$timeRelativeToTrialStart)
  steer_ending_dev_mean <- mean(abs(csteer$lanePosition))
  
  cdial <<- data[with(data, partOfExperiment == "dualDialFocus" & typingErrorMadeOnTrial == 0 & Event1 == "Correct"), ]
  dial_ending_times_mean <- mean(cdial$timeRelativeToTrialStart)
  dial_ending_dev_mean <- mean(abs(cdial$lanePosition))
  
  # err bars
  steer_err_dialtimes <- sd(csteer$timeRelativeToTrialStart)/sqrt(length(csteer$timeRelativeToTrialStart)) # get std err
  dial_err_dialtimes <- sd(cdial$timeRelativeToTrialStart)/sqrt(length(cdial$timeRelativeToTrialStart)) # get std err
  
  steer_err_dev_time <- 0.38
  dial_err_dev_time <- 0.26
  
  steer_err_dev_meter <- 0.055
  dial_err_dev_meter <- 0.065
  
  # dial err bars vertical
  steer_ending_times_mean <- steer_ending_times_mean / 1000
  dial_ending_times_mean <- dial_ending_times_mean / 1000
  
  ### PLOT
  # Plot 3 models with different pp's IKI on top of each other
  # gplot <- ggplot(all_results, aes(x=TrialTime/1000, y=abs(dev), color = "Model_186")) + geom_point() 

  
  gplot <- ggplot(all_results)
  gplot <- gplot + geom_point(aes(x = all_results$bonus_321.TrialTime / 1000, y = all_results$bonus_321.dev, colour = "Model_321"), shape =1)
  gplot <- gplot + geom_point(aes(x = all_results$bonus_278.TrialTime  / 1000, y = all_results$bonus_278.dev, colour = "Model_278"), shape = 0)
  gplot <- gplot + geom_point(aes(x = all_results$bonus_186.TrialTime  / 1000, y = all_results$bonus_186.dev, colour = "Model_186"), shape = 8)
  
  
  # human err steer
  gplot <- gplot + geom_point(aes(x=steer_ending_times_mean, y=steer_ending_dev_mean
                                  , colour="Steering-Focus"), shape = 15)
  # human err dial
  gplot <- gplot + geom_point(aes(x=dial_ending_times_mean, y=dial_ending_dev_mean
                                  , colour="Dialing-Focus"), shape = 17)
  
  # err bars
  #vert 
  #time
  gplot <- gplot + geom_errorbar(mapping=aes(x=dial_ending_times_mean
                                             , ymin=dial_ending_dev_mean - (dial_err_dev_meter)
                                             , ymax=dial_ending_dev_mean + (dial_err_dev_meter))
                                 , col = "black"
                                 , width=.2)
  #steer
  gplot <- gplot + geom_errorbar(mapping=aes(x=steer_ending_times_mean
                                             , ymin=steer_ending_dev_mean - (steer_err_dev_meter)
                                             , ymax=steer_ending_dev_mean + (steer_err_dev_meter))
                                 , col = "black"
                                 , width=.2)
  # horiz
  # time
  gplot <- gplot + geom_errorbarh(mapping=aes(y=dial_ending_dev_mean
                                              , xmin=dial_ending_times_mean - (dial_err_dev_time)
                                              , xmax=dial_ending_times_mean + (dial_err_dev_time))
                                  , col = "black"
                                  , height=.05)
  # steer
  gplot <- gplot + geom_errorbarh(mapping=aes(y=steer_ending_dev_mean
                                              , xmin=steer_ending_times_mean - (dial_err_dev_time)
                                              , xmax=steer_ending_times_mean + (dial_err_dev_time))
                                  , col = "black"
                                  , height=.05)
  
  
  # manual scale colors
  # gplot <- gplot + scale_colour_manual(
  #   limits=c("Model_186", "Model_278", "Model_321", "Steering-Focus", "Dialing-Focus"),
  #   values=c("black", "grey", "lightgrey", "blue", "orange")
  # )
  gplot <- gplot + scale_colour_manual(
    limits=c("Model_321", "Model_278", "Model_186", "Steering-Focus", "Dialing-Focus"),
    values=c("green", "red", "blue", "black", "black")
  )
  #legend shapes
  gplot <- gplot + guides(colour = guide_legend(override.aes = list(shape = c(1, 0, 8, 15, 17))))
  # legend title
  gplot$labels$colour <- "Categories"
  # main plot labels
  gplot <- gplot + labs(title="Predicted Dial Time & Lateral Deviation",
                        x ="Dial Time (s)", y = "Average Lateral Deviation (m)")
  
  plot(gplot)
}

bonus3 <- function() {
  
}

bonus4 <- function() {
  results <- read.table("E:\\Homework\\Cognitive Modeling\\Lab1\\simulation_r3\\50_simul_ourDrift_ourIKI")
  
  break_3_5_7_9 <- results[with(results, strat == "3,5,7,9"), ]
  break_4_7 <- results[with(results, strat == "4,7"), ]
  break_6 <- results[with(results, strat == "6"), ]
  break_12345678910 <- results[with(results, strat == "1,2,3,4,5,6,7,8,9,10"), ]
  break_1 <- results[with(results, strat == "1"), ]
  break_9 <- results[with(results, strat == "9"), ]
  break_2_8 <- results[with(results, strat == "2,8"), ]
  
  # combine into one frame 
  # all_results <- data.frame(break_3_5_7_9$TrialTime, break_3_5_7_9$dev
  #                           , break_4_7$TrialTime, break_4_7$dev
  #                           , break_6$TrialTime, break_6$dev
  #                           , break_12345678910$TrialTime, break_12345678910$dev)
  all_results <- data.frame(break_3_5_7_9$TrialTime, break_3_5_7_9$dev
                            , break_4_7$TrialTime, break_4_7$dev
                            , break_6$TrialTime, break_6$dev
                            , break_1$TrialTime, break_1$dev
                            , break_9$TrialTime, break_9$dev
                            , break_2_8$TrialTime, break_2_8$dev
                            , break_12345678910$TrialTime, break_12345678910$dev)
  
  gplot <- ggplot(all_results, aes(x=(all_results$break_3_5_7_9.TrialTime / 1000)
                                   , y = all_results$break_3_5_7_9.dev, color = "3, 5, 7, 9")) + geom_point()
  
  
  # gplot <- ggplot(all_results)
  # gplot <- gplot + geom_point(aes(x = all_results$break_3_5_7_9.TrialTime / 1000
  #                                 , y = all_results$break_3_5_7_9))
  gplot <- gplot + geom_point(aes(x = (all_results$break_4_7.TrialTime  / 1000)
                                  , y = all_results$break_4_7.dev, color = "4, 7"), shape = 0)
  gplot <- gplot + geom_point(aes(x = (all_results$break_6.TrialTime  / 1000)
                                  , y = all_results$break_6.dev, color = "6"), shape = 8)
  
  gplot <- gplot + geom_point(aes(x = (all_results$break_1.TrialTime  / 1000)
                                  , y = all_results$break_1.dev, color = "1"), shape = 9)
  gplot <- gplot + geom_point(aes(x = (all_results$break_9.TrialTime  / 1000)
                                  , y = all_results$break_9.dev, color = "9"), shape = 10)
  
  gplot <- gplot + geom_point(aes(x = (all_results$break_2_8.TrialTime  / 1000)
                                  , y = all_results$break_2_8.dev, color = "2, 8"), shape = 11)
  
  gplot <- gplot + geom_point(aes(x = (all_results$break_12345678910.TrialTime  / 1000)
                                  , y = all_results$break_12345678910.dev, color = "1, 2, 3, 4, 5, 6, 7, 8, 9, 10"), shape = 9)

  gplot <- gplot + scale_colour_manual(
    limits=c("3, 5, 7, 9", "4, 7", "6", "1", "9", "2, 8", "1, 2, 3, 4, 5, 6, 7, 8, 9, 10"),
    values=c("green", "red", "blue", "orange", "grey", "purple", "black")
  )
  # #legend shapes
  # gplot <- gplot + guides(colour = guide_legend(override.aes = list(shape = c(1))))
  # 
  # # legend title
  gplot$labels$colour <- "Model Breakpoints"
  # # main plot labels
  gplot <- gplot + labs(title="Comparison of breakpoints on Model",
                        x ="Dial Time (s)", y = "Average Lateral Deviation (m)")
  # 
  plot(gplot)
  
}