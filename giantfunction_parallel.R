library(weights)
library(anesrake)
library(dplyr)
library(tidyr)
library(ggplot2)

#Parallel packages
library(doParallel)
library(foreach)

expit <- function(x){
  out <- exp(x) / (1+exp(x))
  return(out)
}


nsim <- 1000 # number of simulations

### input population df, partial raking version (numeric)
# version3: 1 = 3 categorical variables with 1 PS (overlap)
# 2 = 3 cat variables without overlap

### returns list with means df (unadjusted, PS, Raking, Partial Raking), CI df df, 
#  


### Separate V1 into its own function
### 


f_three <- function(pop, version, samp_method){
  
  start <- Sys.time()
  results<-list()
  samplist <- list()
  CIlist <- list()
  #names(CI_df) <- ()
  
  N <- 1000000
  
  ## stratified sampling
  # pop <- pop %>% group_by(I_age_old, I_sex_F, I_race_B)
  # samp <- sample_n(pop, 400/8, replace = FALSE)
  # pop <- ungroup(pop)
  
  ## stratified sampling with 2 variables
  
  if (samp_method == "strat2"){
    pop <- pop %>% group_by(I_age_old, I_sex_F)
    for (i in 1:nsim){
      samplist[[i]] <- sample_n(pop, 800/4, replace = FALSE)
    }
    pop <- ungroup(pop)
  }
  
  ## SRS sampling
  if (samp_method == "SRS"){
    pop <- ungroup(pop)
    for (i in 1:nsim){
      samplist[[i]] <- sample_n(pop, 800, replace=FALSE)
    }
  }
  
  # 
  # ## unequal probability sampling 1
  if (samp_method == "prob1"){
    pop <- ungroup(pop)
    for (i in 1:nsim){
      samplist[[i]] <- sample_n(pop, 800, replace = FALSE, weight = 1 -0.2*pop$I_age_old - 0.4*pop$I_race_B)
      
    }
  }
  
  # ## unequal probability sampling 2
  if (samp_method == "prob2"){
    pop <- ungroup(pop)
    for (i in 1:nsim){
      samplist[[i]] <- sample_n(pop, 800, replace = FALSE, weight = 1 -0.2*pop$I_age_old - 0.4*pop$I_race_B + 
                                  2*pop$I_age_old*pop$I_sex_F*pop$I_race_B + 0.4*pop$I_sex_F)
      
    }
  }
  
  # ## unequal probability sampling 3
  # pop <- ungroup(pop)
  if (samp_method == "prob3"){
    pop <- ungroup(pop)
    for (i in 1:nsim){
      samplist[[i]] <- sample_n(pop, 800, replace = FALSE, weight = 1 + 0.2*pop$I_age_old - 0.4*pop$I_race_B +
                                  pop$I_age_old*pop$I_sex_F*pop$I_race_B + 0.4*pop$I_sex_F)
      
    }
  }
  
  all_results <- foreach(i = 1:nsim, #) %dopar% {
                         #.export = c("start", "results", "samplist", "CI_df"),
                         .packages = c("weights", "anesrake", "dplyr", "tidyr"),
                         #.combine = list) %dopar% {
                         .combine='comb',
                         .multicombine = TRUE,
                         .init=list(list, list(), list())) %dopar% {
                           
                           samp <- samplist[[i]]
                           
                           ###### check sample makeup, order by groups
                           samp <- samp %>% group_by(I_age_old, I_sex_F, I_race_B)
                           sampFreq <- samp %>% summarise(count = n() / nrow(samp))
                           #sampFreq
                           
                           pop <- pop %>% group_by(I_age_old, I_sex_F, I_race_B)
                           popFreq <- pop %>% summarise(count = n()/N)
                           #popFreq
                           
                           ###############################
                           #post stratification
                           ###############################
                           
                           sampFreq$psweight <- popFreq$count / sampFreq$count
                           samp <- merge(samp, sampFreq, 
                                         by.x = c("I_age_old","I_sex_F","I_race_B"), 
                                         by.y = c("I_age_old","I_sex_F","I_race_B"), 
                                         all.x=TRUE)
                           
                           ###############################
                           # raking
                           ###############################
                           trueage <- wpct(pop$I_age_old)
                           truesex <- wpct(pop$I_sex_F)
                           truerace <- wpct(pop$I_race_B)
                           targets <- list(trueage, truesex, truerace)
                           names(targets) <- c("I_age_old", "I_sex_F", "I_race_B")
                           
                           #anesrakefinder(targets, samp, choosemethod = "total")
                           # Raking starts here.  
                           samp$caseid <- 1:nrow(samp)
                           
                           samp$I_age_old <- as.factor(samp$I_age_old)
                           samp$I_sex_F <- as.factor(samp$I_sex_F)
                           samp$I_race_B <- as.factor(samp$I_race_B)
                           
                           anes <- anesrake(targets, samp, caseid = samp$caseid, cap = 200, 
                                            choosemethod = "total", 
                                            center.baseweights = FALSE,
                                            type = "nolim")
                           samp$rakeweight <- anes$weightvec 
                           
                           if (version == 1){
                             ### Not Partial Raking, just Raking with Age and Sex combined into 1 variable
                             samp$I_age_sex <- 0
                             samp <- samp %>%
                               mutate(I_age_sex = ifelse(I_age_old == 0 & I_sex_F == 0, 1,
                                                         ifelse(I_age_old == 0 & I_sex_F == 1, 2,
                                                                ifelse(I_age_old == 1 & I_sex_F == 0, 3, 4))))
                             levels(samp$I_age_sex)
                             
                             popcopy <- pop %>%
                               mutate(I_age_sex = ifelse(I_age_old == 0 & I_sex_F == 0, 1,
                                                         ifelse(I_age_old == 0 & I_sex_F == 1, 2,
                                                                ifelse(I_age_old == 1 & I_sex_F == 0, 3, 4))))
                             
                             
                             trueagesex <- wpct(popcopy$I_age_sex)
                             truerace <- wpct(popcopy$I_race_B)
                             targets <- list(trueagesex, truerace)
                             names(targets) <- c("I_age_sex", "I_race_B")
                             
                             samp$caseid <- 1:nrow(samp)
                             
                             samp$I_age_sex <- as.factor(samp$I_age_sex)
                             samp$I_race_B <- as.factor(samp$I_race_B)
                             
                             anes <- anesrake(targets, samp, caseid = samp$caseid, cap = 200,
                                              choosemethod = "total",
                                              center.baseweights = FALSE,
                                              type = "nolim")
                             samp$prweight <- anes$weightvec
                             
                           }
                           
                           if (version == 2){
                             ### age & sex, age & race (partial raking)
                             
                             trueage <- wpct(pop$I_age_old)
                             targets <- list(trueage)
                             names(targets) <- c("I_age_old")
                             
                             #anesrakefinder(targets, samp, choosemethod = "total")
                             # Raking starts here.
                             samp$caseid <- 1:nrow(samp)
                             
                             samp$I_age_old <- as.factor(samp$I_age_old)
                             
                             # anes <- anesrake(targets, samp, caseid = samp$caseid, cap = 200,
                             #                  choosemethod = "total",
                             #                  center.baseweights = FALSE,
                             #                  type = "nolim")
                             # samp$prweight <- anes$weightvec
                             samp$prweight <- samp$rakeweight
                             
                             check_weights <- list()
                             check_weights2 <- list()
                             
                             for (k in 1:10) {
                               
                               
                               anes <- anesrake(targets, samp, caseid = samp$caseid, cap = 200,
                                                choosemethod = "total",
                                                center.baseweights = FALSE,
                                                weightvec = samp$prweight,
                                                type = "nolim")
                               samp$prweight <- anes$weightvec
                               
                               for (j in 0:1) {
                                 subpop <- subset(pop, I_age_old == j)
                                 truesexsub <- wpct(subpop$I_sex_F)
                                 trueracesub <- wpct(subpop$I_race_B)
                                 subtargets <- list(truesexsub, trueracesub)
                                 names(subtargets) <- c("I_sex_F", "I_race_B")
                                 
                                 subsamp <- subset(samp, I_age_old == j)
                                 
                                 
                                 subsamp$caseid <-1:nrow(subsamp)
                                 subsamp$I_sex_F <- as.factor(subsamp$I_sex_F)
                                 subsamp$I_race_B <- as.factor(subsamp$I_race_B)
                                 
                                 
                                 anes <- anesrake(subtargets, subsamp, caseid = subsamp$caseid, cap = 200,
                                                  weightvec = subsamp$prweight,
                                                  center.baseweights = FALSE,
                                                  choosemethod = "total",
                                                  type = "nolim")
                                 
                                 samp$prweight[samp$I_age_old == j] <- anes$weightvec
                                 
                                 check_weights[[j + 1]] <- samp$prweight
                               }
                               # print(unique(samp$prweight))
                               #print(sum(abs(check_weights[[1]] - check_weights[[2]])))
                               check_weights2[[k]] <- samp$prweight
                             }
                             
                             # samp <- samp %>%
                             #   mutate(I_age_sex = ifelse(I_age_old == 0 & I_sex_F == 0, 1,
                             #                             ifelse(I_age_old == 0 & I_sex_F == 1, 2,
                             #                                    ifelse(I_age_old == 1 & I_sex_F == 0, 3, 4))),
                             #          I_age_race = ifelse(I_age_old == 0 & I_race_B == 0, 1,
                             #                             ifelse(I_age_old == 0 & I_race_B == 1, 2,
                             #                                    ifelse(I_age_old == 1 & I_race_B == 0, 3, 4))))
                             # 
                             # popcopy <- pop %>%
                             #   mutate(I_age_sex = ifelse(I_age_old == 0 & I_sex_F == 0, 1,
                             #                             ifelse(I_age_old == 0 & I_sex_F == 1, 2,
                             #                                    ifelse(I_age_old == 1 & I_sex_F == 0, 3, 4))),
                             #          I_age_race = ifelse(I_age_old == 0 & I_race_B == 0, 1,
                             #                              ifelse(I_age_old == 0 & I_race_B == 1, 2,
                             #                                     ifelse(I_age_old == 1 & I_race_B == 0, 3, 4))))
                             # 
                             # 
                             # trueagesex <- wpct(popcopy$I_age_sex)
                             # trueagerace <- wpct(popcopy$I_age_race)
                             # targets <- list(trueagesex, trueagerace)
                             # names(targets) <- c("I_age_sex", "I_age_race")
                             # 
                             # #trueage <- wpct(popcopy$I_age_old)
                             # #targets <- list(trueagesex, trueage)
                             # 
                             # samp <- as.data.frame(samp)
                             # 
                             # samp$caseid <- 1:nrow(samp)
                             # #samp$I_age_old <- as.factor(samp$I_age_old)
                             # 
                             # samp$I_age_sex <- as.factor(samp$I_age_sex)
                             # samp$I_age_race <- as.factor(samp$I_age_race)
                             # 
                             # names(targets) <- c("I_age_sex", "I_age_race")
                             # 
                             # anes <- anesrake(targets, samp, caseid = samp$caseid, cap = 200,
                             #                  #weightvec = subsamp$prweight,
                             #                  center.baseweights = FALSE,
                             #                  choosemethod = "total",
                             #                  type = "nolim")
                             # samp$prweight <- anes$weightvec
                           }
                           
                           
                           if (version == 3) {
                             ### Age & Sex, Sex & Race, Race & Age
                             
                             trueage <- wpct(pop$I_age_old)
                             targets <- list(trueage)
                             names(targets) <- c("I_age_old")
                             
                             #anesrakefinder(targets, samp, choosemethod = "total")
                             # Raking starts here.
                             samp$caseid <- 1:nrow(samp)
                             
                             samp$I_age_old <- as.factor(samp$I_age_old)
                             
                             # anes <- anesrake(targets, samp, caseid = samp$caseid, cap = 200,
                             #                  choosemethod = "total",
                             #                  center.baseweights = FALSE,
                             #                  type = "nolim")
                             # samp$prweight <- anes$weightvec
                             samp$rakeweight 
                             
                             check_weights <- list()
                             for (k in 1:10){
                               # Condition on age, Rake on sex and race
                               for (j in 0:1) {
                                 subpop <- subset(pop, I_age_old == j)
                                 truesexsub <- wpct(subpop$I_sex_F)
                                 trueracesub <- wpct(subpop$I_race_B)
                                 subtargets <- list(truesexsub, trueracesub)
                                 names(subtargets) <- c("I_sex_F", "I_race_B")
                                 
                                 subsamp <- subset(samp, I_age_old == j)
                                 
                                 
                                 subsamp$caseid <-1:nrow(subsamp)
                                 subsamp$I_sex_F <- as.factor(subsamp$I_sex_F)
                                 subsamp$I_race_B <- as.factor(subsamp$I_race_B)
                                 
                                 
                                 anes <- anesrake(subtargets, subsamp, caseid = subsamp$caseid, cap = 200, 
                                                  weightvec = subsamp$prweight,
                                                  center.baseweights = FALSE,
                                                  choosemethod = "total", 
                                                  type = "nolim")
                                 
                                 samp$prweight[samp$I_age_old == j] <- anes$weightvec
                                 
                                 check_weights[[j + 1]] <- samp$prweight
                               }
                               #print(sum(abs(check_weights[[1]] - check_weights[[2]])))
                               # Condition on Sex, rake on Age and Race
                               for (j in 0:1) {
                                 subpop <- subset(pop, I_sex_F == j)
                                 trueagesub <- wpct(subpop$I_age_old)
                                 trueracesub <- wpct(subpop$I_race_B)
                                 subtargets <- list(trueagesub, trueracesub)
                                 names(subtargets) <- c("I_age_old", "I_race_B")
                                 
                                 subsamp <- subset(samp, I_sex_F == j)
                                 
                                 
                                 subsamp$caseid <-1:nrow(subsamp)
                                 subsamp$I_age_old <- as.factor(subsamp$I_age_old)
                                 subsamp$I_race_B <- as.factor(subsamp$I_race_B)
                                 
                                 
                                 anes <- anesrake(subtargets, subsamp, caseid = subsamp$caseid, cap = 200, 
                                                  weightvec = subsamp$prweight,
                                                  center.baseweights = FALSE,
                                                  choosemethod = "total", 
                                                  type = "nolim")
                                 
                                 samp$prweight[samp$I_sex_F == j] <- anes$weightvec
                                 
                                 check_weights[[j + 2]] <- samp$prweight
                               }
                               #print(sum(abs(check_weights[[1]] - check_weights[[2]])))
                               
                               # Condition on Race, rake on Age and Sex
                               for (j in 0:1) {
                                 subpop <- subset(pop, I_race_B == j)
                                 truesexsub <- wpct(subpop$I_sex_F)
                                 trueagesub <- wpct(subpop$I_age_old)
                                 subtargets <- list(truesexsub, trueagesub)
                                 names(subtargets) <- c("I_sex_F", "I_age_old")
                                 
                                 subsamp <- subset(samp, I_race_B == j)
                                 
                                 
                                 subsamp$caseid <-1:nrow(subsamp)
                                 subsamp$I_sex_F <- as.factor(subsamp$I_sex_F)
                                 subsamp$I_age_old <- as.factor(subsamp$I_age_old)
                                 
                                 
                                 anes <- anesrake(subtargets, subsamp, caseid = subsamp$caseid, cap = 200, 
                                                  weightvec = subsamp$prweight,
                                                  center.baseweights = FALSE,
                                                  choosemethod = "total", 
                                                  type = "nolim")
                                 
                                 samp$prweight[samp$I_race_B == j] <- anes$weightvec
                                 
                                 check_weights[[j + 3]] <- samp$prweight
                               }
                               
                             }
                             
                           } #  end v3
                           
                           
                           # else {
                           #   stop("Need to include version number!")
                           # }
                           ###############################
                           # results
                           ###############################
                           results <- c(popMean = mean(pop$income), 
                                        sampMean = mean(samp$income), 
                                        sampPSmean = weighted.mean(samp$income, samp$psweight), 
                                        samprakemean = weighted.mean(samp$income, samp$rakeweight),
                                        sampPRmean = weighted.mean(samp$income, samp$prweight))
                           
                           
                           ####bootstrapping
                           #samp <- samp %>% group_by(I_age_old, I_sex_F, I_race_B)
                           samp <- ungroup(samp)
                           
                           bsSampMean <- c()
                           bsPSMean <- c()
                           bsRakeMean <- c()
                           bsPRMean <- c()
                           for (b in 1:500){
                             bs_samp <- sample_n(samp, 2000, replace=TRUE)
                             
                             bsSampMean[b] <- mean(bs_samp$income)
                             bsPSMean[b] <- weighted.mean(bs_samp$income, bs_samp$psweight)
                             bsRakeMean[b] <- weighted.mean(bs_samp$income, bs_samp$rakeweight)
                             bsPRMean[b] <- weighted.mean(bs_samp$income, bs_samp$prweight)
                             
                             
                           }
                           
                           
                           ### Bias Correction w/ Bootstrap
                           ##### find bias based off bootstrap 
                           CI_df <- matrix(rep(0, 16), ncol=16)
                           
                           Sampbias <- mean(bsSampMean) - mean(samp$income)
                           CI_df[1,1] <- quantile(bsSampMean + Sampbias, .025)
                           CI_df[1,2] <- quantile(bsSampMean + Sampbias, .975)
                           
                           PSbias <- mean(bsPSMean) - weighted.mean(samp$income,samp$psweight)
                           CI_df[1,3] <- quantile(bsPSMean + PSbias, .025)
                           CI_df[1,4] <- quantile(bsPSMean + PSbias, .975)
                           
                           rakebias <- mean(bsRakeMean) - weighted.mean(samp$income,samp$rakeweight)
                           CI_df[1,5] <- quantile(bsRakeMean + rakebias, .025)
                           CI_df[1,6] <- quantile(bsRakeMean + rakebias, .975)
                           
                           PRbias <- mean(bsPRMean) - weighted.mean(samp$income,samp$prweight)
                           CI_df[1,7] <- quantile(bsPRMean + PRbias, .025)
                           CI_df[1,8] <- quantile(bsPRMean + PRbias, .975)
                           
                           CI_df[1,9] <- quantile(bsSampMean, .025) #Samp
                           CI_df[1,10] <- quantile(bsSampMean, .975)
                           
                           CI_df[1,11] <- quantile(bsPSMean, .025) #PS
                           CI_df[1,12] <- quantile(bsPSMean, .975)
                           
                           CI_df[1,13] <- quantile(bsRakeMean, .025) #Rake
                           CI_df[1,14] <- quantile(bsRakeMean, .975)
                           
                           CI_df[1,15] <- quantile(bsPRMean, .025) #PR
                           CI_df[1,16] <- quantile(bsPRMean, .975)
                           
                           
                           return(list(results, samp, CI_df))
                         } # end nsim loop
  
  
  end <- Sys.time()
  print(end - start)
  return(all_results)
  
  #return(list(results, CI_df, samplist))
  
  
} # end function

set.seed(89)
N <- 1000000
# ###pop3 aka pop2 but with a larger income variability
pop <- data.frame(I_age_old = rbinom(N, 1, 0.6), I_sex_F = 0, I_race_B = 0) #, I_ins_A = 0)
#sex
p <- expit(0 + 0.9 * pop$I_age_old)
pop$I_sex_F <- rbinom(N, 1, prob = p)
#race
p <- expit(1 - 0.8 * pop$I_age_old + pop$I_sex_F)
pop$I_race_B <- rbinom(N, 1, prob = p)
# #Insurance
# p <- expit(0 + 0.6 * pop$I_age_old * pop$I_race_B - pop$I_sex_F)
# pop$I_ins_A <- rbinom(N, 1, prob = p)

pop <- pop[order(pop$I_age_old, pop$I_sex_F, pop$I_race_B),]

pop <- pop %>%
  mutate(I_race_age = ifelse(I_race_B ==0 & I_age_old == 0, 1,
                             ifelse(I_race_B == 0 & I_age_old == 1, 2,
                                    ifelse(I_race_B & I_age_old == 0, 3, 4))))

pop <- pop %>% group_by(I_age_old, I_sex_F, I_race_B) #, I_ins_A)
popFreq <- pop %>% summarise(count = n() / N)
popFreq

pop$income <- 25000 + 
  40000 * pop$I_age_old +
  25000 * pop$I_sex_F * pop$I_race_B + 
  30000 * pop$I_sex_F * pop$I_age_old +  
  30000 * pop$I_age_old * pop$I_race_B + #* pop$I_ins_A + 
  20000 * pop$I_age_old * pop$I_sex_F * pop$I_race_B + #* pop$I_ins_A +
  rnorm(N, 0, 10000)


comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]]) ) )
}


#Parallel
detectCores()

numCores <- detectCores() - 1
print(numCores)
cl <- makeCluster(numCores, type = "FORK")
registerDoParallel(cl)


stopImplicitCluster()







####  strat2 Sampling

set.seed(89)
testone <- f_three(pop, 1, "strat2")
testone[[1]][1] <- NULL

strat2_res1 <- as.data.frame(do.call(rbind, testone[[1]]))  ## why do you need double [] ?

strat2_res1$biasSamp <- strat2_res1$sampMean - strat2_res1$popMean
strat2_res1$biasSampPercent <- 100 * strat2_res1$biasSamp / strat2_res1$popMean
strat2_res1$biasPS <- strat2_res1$sampPSmean - strat2_res1$popMean
strat2_res1$biasPSPercent <- 100 * strat2_res1$biasPS / strat2_res1$popMean
strat2_res1$biasrake <- strat2_res1$samprakemean - strat2_res1$popMean
strat2_res1$biasrakePercent <- 100 * strat2_res1$biasrake / strat2_res1$popMean
strat2_res1$biasPR <- strat2_res1$sampPRmean - strat2_res1$popMean
strat2_res1$biasPRPercent <- 100 * strat2_res1$biasPR / strat2_res1$popMean

strat2_biastbl1 <- apply(strat2_res1, 2, mean)
strat2_biastbl1

strat2_vartbl1 <- apply(strat2_res1[, 2:5], 2, sd)
strat2_vartbl1



set.seed(89)
testtwo <- f_three(pop, 2, "strat2")
testtwo[[1]][1] <- NULL

strat2_res2 <- as.data.frame(do.call(rbind, testtwo[[1]]))  

strat2_res2$biasSamp <- strat2_res2$sampMean-strat2_res2$popMean
strat2_res2$biasSampPercent <- 100 * strat2_res2$biasSamp / strat2_res2$popMean
strat2_res2$biasPS <- strat2_res2$sampPSmean-strat2_res2$popMean
strat2_res2$biasPSPercent <- 100 * strat2_res2$biasPS / strat2_res2$popMean
strat2_res2$biasrake <- strat2_res2$samprakemean-strat2_res2$popMean
strat2_res2$biasrakePercent <- 100 * strat2_res2$biasrake / strat2_res2$popMean
strat2_res2$biasPR <- strat2_res2$sampPRmean-strat2_res2$popMean
strat2_res2$biasPRPercent <- 100 * strat2_res2$biasPR / strat2_res2$popMean

strat2_biastbl2 <- apply(strat2_res2, 2, mean)
strat2_biastbl2

strat2_vartbl2 <- apply(strat2_res2[, 2:5], 2, sd)
strat2_vartbl2

samp2 <- testtwo[[3]]

## -1.7574026 


set.seed(89)
testthree <- f_three(pop, 3, "strat2")
testthree[[1]][1] <- NULL

strat2_res3 <- as.data.frame(do.call(rbind, testthree[[1]]))  ## why do you need double [] ?

strat2_res3$biasSamp <- strat2_res3$sampMean-strat2_res3$popMean
strat2_res3$biasSampPercent <- 100 * strat2_res3$biasSamp / strat2_res3$popMean
strat2_res3$biasPS <- strat2_res3$sampPSmean-strat2_res3$popMean
strat2_res3$biasPSPercent <- 100 * strat2_res3$biasPS / strat2_res3$popMean
strat2_res3$biasrake <- strat2_res3$samprakemean-strat2_res3$popMean
strat2_res3$biasrakePercent <- 100 * strat2_res3$biasrake / strat2_res3$popMean
strat2_res3$biasPR <- strat2_res3$sampPRmean-strat2_res3$popMean
strat2_res3$biasPRPercent <- 100 * strat2_res3$biasPR / strat2_res3$popMean

strat2_biastbl3 <- apply(strat2_res3, 2, mean)
strat2_biastbl3

strat2_vartbl3 <- apply(strat2_res3[, 2:5], 2, sd)
strat2_vartbl3


strat2 <- list(strat2_biastbl1, strat2_biastbl2, strat2_biastbl3, 
               strat2_vartbl1, strat2_vartbl2, strat2_vartbl3, 
              strat2_res1, strat2_res2, strat2_res3,
              testone[[2]]) 
save(strat2, file = "strat2.rda")




####  SRS Sampling

set.seed(89)
testone <- f_three(pop, 1, "SRS")
testone[[1]][1] <- NULL

SRS_res1 <- as.data.frame(do.call(rbind, testone[[1]]))  ## why do you need double [] ?

SRS_res1$biasSamp <- SRS_res1$sampMean - SRS_res1$popMean
SRS_res1$biasSampPercent <- 100 * SRS_res1$biasSamp / SRS_res1$popMean
SRS_res1$biasPS <- SRS_res1$sampPSmean - SRS_res1$popMean
SRS_res1$biasPSPercent <- 100 * SRS_res1$biasPS / SRS_res1$popMean
SRS_res1$biasrake <- SRS_res1$samprakemean - SRS_res1$popMean
SRS_res1$biasrakePercent <- 100 * SRS_res1$biasrake / SRS_res1$popMean
SRS_res1$biasPR <- SRS_res1$sampPRmean - SRS_res1$popMean
SRS_res1$biasPRPercent <- 100 * SRS_res1$biasPR / SRS_res1$popMean

SRS_biastbl1 <- apply(SRS_res1, 2, mean)
SRS_biastbl1

SRS_vartbl1 <- apply(SRS_res1[, 2:5], 2, sd)
SRS_vartbl1



set.seed(89)
testtwo <- f_three(pop, 2, "SRS")
testtwo[[1]][1] <- NULL

SRS_res2 <- as.data.frame(do.call(rbind, testtwo[[1]]))  

SRS_res2$biasSamp <- SRS_res2$sampMean-SRS_res2$popMean
SRS_res2$biasSampPercent <- 100 * SRS_res2$biasSamp / SRS_res2$popMean
SRS_res2$biasPS <- SRS_res2$sampPSmean-SRS_res2$popMean
SRS_res2$biasPSPercent <- 100 * SRS_res2$biasPS / SRS_res2$popMean
SRS_res2$biasrake <- SRS_res2$samprakemean-SRS_res2$popMean
SRS_res2$biasrakePercent <- 100 * SRS_res2$biasrake / SRS_res2$popMean
SRS_res2$biasPR <- SRS_res2$sampPRmean-SRS_res2$popMean
SRS_res2$biasPRPercent <- 100 * SRS_res2$biasPR / SRS_res2$popMean

SRS_biastbl2 <- apply(SRS_res2, 2, mean)
SRS_biastbl2

SRS_vartbl2 <- apply(SRS_res2[, 2:5], 2, sd)
SRS_vartbl2

samp2 <- testtwo[[3]]

## -1.7574026 


set.seed(89)
testthree <- f_three(pop, 3, "SRS")
testthree[[1]][1] <- NULL

SRS_res3 <- as.data.frame(do.call(rbind, testthree[[1]]))  ## why do you need double [] ?

SRS_res3$biasSamp <- SRS_res3$sampMean-SRS_res3$popMean
SRS_res3$biasSampPercent <- 100 * SRS_res3$biasSamp / SRS_res3$popMean
SRS_res3$biasPS <- SRS_res3$sampPSmean-SRS_res3$popMean
SRS_res3$biasPSPercent <- 100 * SRS_res3$biasPS / SRS_res3$popMean
SRS_res3$biasrake <- SRS_res3$samprakemean-SRS_res3$popMean
SRS_res3$biasrakePercent <- 100 * SRS_res3$biasrake / SRS_res3$popMean
SRS_res3$biasPR <- SRS_res3$sampPRmean-SRS_res3$popMean
SRS_res3$biasPRPercent <- 100 * SRS_res3$biasPR / SRS_res3$popMean

SRS_biastbl3 <- apply(SRS_res3, 2, mean)
SRS_biastbl3

SRS_vartbl3 <- apply(SRS_res3[, 2:5], 2, sd)
SRS_vartbl3


SRS <- list(SRS_biastbl1, SRS_biastbl2, SRS_biastbl3, SRS_vartbl1, SRS_vartbl2, SRS_vartbl3, 
            SRS_res1, SRS_res2, SRS_res3,
            testone[[2]]) 
save(SRS, file = "SRS.rda")






####  prob1 Sampling

set.seed(89)
testone <- f_three(pop, 1, "prob1")
testone[[1]][1] <- NULL

prob1_res1 <- as.data.frame(do.call(rbind, testone[[1]]))  ## why do you need double [] ?

prob1_res1$biasSamp <- prob1_res1$sampMean - prob1_res1$popMean
prob1_res1$biasSampPercent <- 100 * prob1_res1$biasSamp / prob1_res1$popMean
prob1_res1$biasPS <- prob1_res1$sampPSmean - prob1_res1$popMean
prob1_res1$biasPSPercent <- 100 * prob1_res1$biasPS / prob1_res1$popMean
prob1_res1$biasrake <- prob1_res1$samprakemean - prob1_res1$popMean
prob1_res1$biasrakePercent <- 100 * prob1_res1$biasrake / prob1_res1$popMean
prob1_res1$biasPR <- prob1_res1$sampPRmean - prob1_res1$popMean
prob1_res1$biasPRPercent <- 100 * prob1_res1$biasPR / prob1_res1$popMean

prob1_biastbl1 <- apply(prob1_res1, 2, mean)
prob1_biastbl1

prob1_vartbl1 <- apply(prob1_res1[, 2:5], 2, sd)
prob1_vartbl1



set.seed(89)
testtwo <- f_three(pop, 2, "prob1")
testtwo[[1]][1] <- NULL

prob1_res2 <- as.data.frame(do.call(rbind, testtwo[[1]]))  

prob1_res2$biasSamp <- prob1_res2$sampMean-prob1_res2$popMean
prob1_res2$biasSampPercent <- 100 * prob1_res2$biasSamp / prob1_res2$popMean
prob1_res2$biasPS <- prob1_res2$sampPSmean-prob1_res2$popMean
prob1_res2$biasPSPercent <- 100 * prob1_res2$biasPS / prob1_res2$popMean
prob1_res2$biasrake <- prob1_res2$samprakemean-prob1_res2$popMean
prob1_res2$biasrakePercent <- 100 * prob1_res2$biasrake / prob1_res2$popMean
prob1_res2$biasPR <- prob1_res2$sampPRmean-prob1_res2$popMean
prob1_res2$biasPRPercent <- 100 * prob1_res2$biasPR / prob1_res2$popMean

prob1_biastbl2 <- apply(prob1_res2, 2, mean)
prob1_biastbl2

prob1_vartbl2 <- apply(prob1_res2[, 2:5], 2, sd)
prob1_vartbl2

samp2 <- testtwo[[3]]

## -1.7574026 


set.seed(89)
testthree <- f_three(pop, 3, "prob1")
testthree[[1]][1] <- NULL

prob1_res3 <- as.data.frame(do.call(rbind, testthree[[1]]))  ## why do you need double [] ?

prob1_res3$biasSamp <- prob1_res3$sampMean-prob1_res3$popMean
prob1_res3$biasSampPercent <- 100 * prob1_res3$biasSamp / prob1_res3$popMean
prob1_res3$biasPS <- prob1_res3$sampPSmean-prob1_res3$popMean
prob1_res3$biasPSPercent <- 100 * prob1_res3$biasPS / prob1_res3$popMean
prob1_res3$biasrake <- prob1_res3$samprakemean-prob1_res3$popMean
prob1_res3$biasrakePercent <- 100 * prob1_res3$biasrake / prob1_res3$popMean
prob1_res3$biasPR <- prob1_res3$sampPRmean-prob1_res3$popMean
prob1_res3$biasPRPercent <- 100 * prob1_res3$biasPR / prob1_res3$popMean

prob1_biastbl3 <- apply(prob1_res3, 2, mean)
prob1_biastbl3

prob1_vartbl3 <- apply(prob1_res3[, 2:5], 2, sd)
prob1_vartbl3


prob1 <- list(prob1_biastbl1, prob1_biastbl2, prob1_biastbl3, 
              prob1_vartbl1, prob1_vartbl2, prob1_vartbl3, 
            prob1_res1, prob1_res2, prob1_res3,
            testone[[2]]) 
save(prob1, file = "prob1.rda")






####  prob2 Sampling

set.seed(89)
testone <- f_three(pop, 1, "prob2")
testone[[1]][1] <- NULL

prob2_res1 <- as.data.frame(do.call(rbind, testone[[1]]))  ## why do you need double [] ?

prob2_res1$biasSamp <- prob2_res1$sampMean - prob2_res1$popMean
prob2_res1$biasSampPercent <- 100 * prob2_res1$biasSamp / prob2_res1$popMean
prob2_res1$biasPS <- prob2_res1$sampPSmean - prob2_res1$popMean
prob2_res1$biasPSPercent <- 100 * prob2_res1$biasPS / prob2_res1$popMean
prob2_res1$biasrake <- prob2_res1$samprakemean - prob2_res1$popMean
prob2_res1$biasrakePercent <- 100 * prob2_res1$biasrake / prob2_res1$popMean
prob2_res1$biasPR <- prob2_res1$sampPRmean - prob2_res1$popMean
prob2_res1$biasPRPercent <- 100 * prob2_res1$biasPR / prob2_res1$popMean

prob2_biastbl1 <- apply(prob2_res1, 2, mean)
prob2_biastbl1

prob2_vartbl1 <- apply(prob2_res1[, 2:5], 2, sd)
prob2_vartbl1



set.seed(89)
testtwo <- f_three(pop, 2, "prob2")
testtwo[[1]][1] <- NULL

prob2_res2 <- as.data.frame(do.call(rbind, testtwo[[1]]))  

prob2_res2$biasSamp <- prob2_res2$sampMean-prob2_res2$popMean
prob2_res2$biasSampPercent <- 100 * prob2_res2$biasSamp / prob2_res2$popMean
prob2_res2$biasPS <- prob2_res2$sampPSmean-prob2_res2$popMean
prob2_res2$biasPSPercent <- 100 * prob2_res2$biasPS / prob2_res2$popMean
prob2_res2$biasrake <- prob2_res2$samprakemean-prob2_res2$popMean
prob2_res2$biasrakePercent <- 100 * prob2_res2$biasrake / prob2_res2$popMean
prob2_res2$biasPR <- prob2_res2$sampPRmean-prob2_res2$popMean
prob2_res2$biasPRPercent <- 100 * prob2_res2$biasPR / prob2_res2$popMean

prob2_biastbl2 <- apply(prob2_res2, 2, mean)
prob2_biastbl2

prob2_vartbl2 <- apply(prob2_res2[, 2:5], 2, sd)
prob2_vartbl2

samp2 <- testtwo[[3]]

## -1.7574026 


set.seed(89)
testthree <- f_three(pop, 3, "prob2")
testthree[[1]][1] <- NULL

prob2_res3 <- as.data.frame(do.call(rbind, testthree[[1]]))  ## why do you need double [] ?

prob2_res3$biasSamp <- prob2_res3$sampMean-prob2_res3$popMean
prob2_res3$biasSampPercent <- 100 * prob2_res3$biasSamp / prob2_res3$popMean
prob2_res3$biasPS <- prob2_res3$sampPSmean-prob2_res3$popMean
prob2_res3$biasPSPercent <- 100 * prob2_res3$biasPS / prob2_res3$popMean
prob2_res3$biasrake <- prob2_res3$samprakemean-prob2_res3$popMean
prob2_res3$biasrakePercent <- 100 * prob2_res3$biasrake / prob2_res3$popMean
prob2_res3$biasPR <- prob2_res3$sampPRmean-prob2_res3$popMean
prob2_res3$biasPRPercent <- 100 * prob2_res3$biasPR / prob2_res3$popMean

prob2_biastbl3 <- apply(prob2_res3, 2, mean)
prob2_biastbl3

prob2_vartbl3 <- apply(prob2_res3[, 2:5], 2, sd)
prob2_vartbl3


prob2 <- list(prob2_biastbl1, prob2_biastbl2, prob2_biastbl3, prob2_vartbl1, prob2_vartbl2, prob2_vartbl3, 
              prob2_res1, prob2_res2, prob2_res3,
              testone[[2]]) 
save(prob2, file = "prob2.rda")









####  prob3 Sampling

set.seed(89)
testone <- f_three(pop, 1, "prob3")
testone[[1]][1] <- NULL

prob3_res1 <- as.data.frame(do.call(rbind, testone[[1]]))  ## why do you need double [] ?

prob3_res1$biasSamp <- prob3_res1$sampMean - prob3_res1$popMean
prob3_res1$biasSampPercent <- 100 * prob3_res1$biasSamp / prob3_res1$popMean
prob3_res1$biasPS <- prob3_res1$sampPSmean - prob3_res1$popMean
prob3_res1$biasPSPercent <- 100 * prob3_res1$biasPS / prob3_res1$popMean
prob3_res1$biasrake <- prob3_res1$samprakemean - prob3_res1$popMean
prob3_res1$biasrakePercent <- 100 * prob3_res1$biasrake / prob3_res1$popMean
prob3_res1$biasPR <- prob3_res1$sampPRmean - prob3_res1$popMean
prob3_res1$biasPRPercent <- 100 * prob3_res1$biasPR / prob3_res1$popMean

prob3_biastbl1 <- apply(prob3_res1, 2, mean)
prob3_biastbl1

prob3_vartbl1 <- apply(prob3_res1[, 2:5], 2, sd)
prob3_vartbl1



set.seed(89)
testtwo <- f_three(pop, 2, "prob3")
testtwo[[1]][1] <- NULL

prob3_res2 <- as.data.frame(do.call(rbind, testtwo[[1]]))  

prob3_res2$biasSamp <- prob3_res2$sampMean-prob3_res2$popMean
prob3_res2$biasSampPercent <- 100 * prob3_res2$biasSamp / prob3_res2$popMean
prob3_res2$biasPS <- prob3_res2$sampPSmean-prob3_res2$popMean
prob3_res2$biasPSPercent <- 100 * prob3_res2$biasPS / prob3_res2$popMean
prob3_res2$biasrake <- prob3_res2$samprakemean-prob3_res2$popMean
prob3_res2$biasrakePercent <- 100 * prob3_res2$biasrake / prob3_res2$popMean
prob3_res2$biasPR <- prob3_res2$sampPRmean-prob3_res2$popMean
prob3_res2$biasPRPercent <- 100 * prob3_res2$biasPR / prob3_res2$popMean

prob3_biastbl2 <- apply(prob3_res2, 2, mean)
prob3_biastbl2

prob3_vartbl2 <- apply(prob3_res2[, 2:5], 2, sd)
prob3_vartbl2

samp2 <- testtwo[[3]]

## -1.7574026 


set.seed(89)
testthree <- f_three(pop, 3, "prob3")
testthree[[1]][1] <- NULL

prob3_res3 <- as.data.frame(do.call(rbind, testthree[[1]]))  ## why do you need double [] ?

prob3_res3$biasSamp <- prob3_res3$sampMean-prob3_res3$popMean
prob3_res3$biasSampPercent <- 100 * prob3_res3$biasSamp / prob3_res3$popMean
prob3_res3$biasPS <- prob3_res3$sampPSmean-prob3_res3$popMean
prob3_res3$biasPSPercent <- 100 * prob3_res3$biasPS / prob3_res3$popMean
prob3_res3$biasrake <- prob3_res3$samprakemean-prob3_res3$popMean
prob3_res3$biasrakePercent <- 100 * prob3_res3$biasrake / prob3_res3$popMean
prob3_res3$biasPR <- prob3_res3$sampPRmean-prob3_res3$popMean
prob3_res3$biasPRPercent <- 100 * prob3_res3$biasPR / prob3_res3$popMean

prob3_biastbl3 <- apply(prob3_res3, 2, mean)
prob3_biastbl3

prob3_vartbl3 <- apply(prob3_res3[, 2:5], 2, sd)
prob3_vartbl3


prob3 <- list(prob3_biastbl1, prob3_biastbl2, prob3_biastbl3, prob3_vartbl1, prob3_vartbl2, prob3_vartbl3, 
              prob3_res1, prob3_res2, prob3_res3,
              testone[[2]]) 
save(prob3, file = "prob3.rda")



#a <- list(biastbl1, biastbl2, biastbl3, vartbl1, vartbl2, vartbl3)
a

# atwo <- list(biastbl1, biastbl2, biastbl3, vartbl1, vartbl2, vartbl3, 
#            res1, res2, res3) # stratified sampling on 2 variables
# atwo

SRS <- list(biastbl1, biastbl2, biastbl3, vartbl1, vartbl2, vartbl3, 
            res1, res2, res3) 
a1

b <- list(biastbl1, biastbl2, biastbl3, vartbl1, vartbl2, vartbl3,
          res1, res2, res3)
b

c <- list(biastbl1, biastbl2, biastbl3, vartbl1, vartbl2, vartbl3,
          res1, res2, res3)
c

#d <- list(biastbl1, biastbl2, biastbl3, vartbl1, vartbl2, vartbl3)
d

asamp <- testone[3]

biastbl1
biastbl2
biastbl3
vartbl1
vartbl2
vartbl3



library(xtable)

SRS_res2 <- SRS[[8]][, 1:5]
SRS_res2 <- SRS_res2[, c(1, 2, 5, 3, 4)]
names(SRS_res2) <- c("Pop Mean", "Unadj", "Rake", "Part Rake", "Post Strat")
hist <- ggplot(gather(SRS_res2)) +
  geom_vline(xintercept = mean(pop$income), color = "#A3A500") +
  geom_histogram(alpha=.75, aes(x = value, fill = key), position = "identity", bins=75) +
  ggtitle("Simple Random Sampling Est. Means") +
  #xlab("Income") +
  # scale_fill_manual(#values=c("#999999", "#E69F00", "#56B4E9"),
  #                   values = c("#C77CFF", "#00BFC4", "#7CAE00", "black", "yellow"),
  #                   name="Method",
  #                   #breaks=c("ctrl", "trt1", "trt2"),
  #                   labels=c("Pop. Mean", "Unadj.", "Rake", "Part. Rake", "Post Strat.")) +
  #xlim(70000, 100000) + 
  ylim(0, 250)
hist


prob2_res2 <- prob2[[8]][, 1:5]
prob2_res2 <- prob2_res2[, c(1, 2, 5, 3, 4)]
names(prob2_res2) <- c("Pop Mean", "Unadj", "Rake", "Part Rake", "Post Strat")

hist <- ggplot(gather(prob2_res2[,1:5])) +
  geom_vline(xintercept = mean(pop$income), color = "#A3A500") +
  geom_histogram(alpha=.75, aes(x = value, fill = key), position = "identity", bins=75) +
  ggtitle("Weighted Sampling Est. Means") +
  #xlab("Income") +
  # scale_fill_manual(#values=c("#999999", "#E69F00", "#56B4E9"),
  #   values = c("#C77CFF", "#00BFC4", "#7CAE00", "black", "yellow"),
  #   name="Method",
  #   #breaks=c("ctrl", "trt1", "trt2"),
  #   labels=c("Pop. Mean", "Unadj.", "Rake", "Part. Rake", "Post Strat.")) +
  #xlim(70000, 100000) + 
  ylim(0, 400)
hist


save(hist, file = "/Users/joylee/Documents/Research Project/ConditionalRaking/hist_SRSv2.rda")
save(res, file = "/Users/joylee/Documents/Research Project/ConditionalRaking/res_pop3unequal2v2.rda")
save(biastbl, file = "/Users/joylee/Documents/Research Project/ConditionalRaking/biastbl_pop3unequal2v2.rda")
save(vartbl, file = "/Users/joylee/Documents/Research Project/ConditionalRaking/vartbl_pop3unequal2v2.rda")

save(Samp, file =" /Users/joylee/Documents/Research Project/ConditionalRaking/samp_pop3unequal2.rda")

histv2.2 <- hist
save(histv2.2, file = "/Users/joylee/Research Project/ConditionalRaking/histv22.rda")
resv2.2 <- res
save(resv2.2, file = "/Users/joylee/Research Project/ConditionalRaking/resv22.rda")
save(biastblv2, file = "/Users/joylee/Research Project/ConditionalRaking/biastblv2.rda")

## res/bias/etc 3
# pop1 w/ sex & race & age, age & ins (partial rake V1)
xtable()



coverage <- function(CI){
  cover <- 0
  
  for (i in 1:nrow(CI)){
    cover <- cover + ifelse(CI[i, 1] < mean(pop$income) & mean(pop$income) < CI[i, 2], 1, 0)
  }
  
  return(cover / nrow(CI))
}
coverage(Samp_CI)


coverage(PS_CI)
#coverage(PS_CIbias)

coverage(PR_CI)
#coverage(PR_CIbias)

#coverage(Rake_CIbias)
coverage(Rake_CI)

ci_tbl <- data.frame(Samp = coverage(Samp_CI),
                     Sampbias = coverage(Samp_CIbias),
                     
                     PS = coverage(PS_CI),
                     PSbias = coverage(PS_CIbias),
                     
                     Rake = coverage(Rake_CIbias),
                     Rakebias = coverage(Rake_CI),
                     
                     PR = coverage(PR_CI),
                     PRbias = coverage(PR_CIbias))
ci_tbl

#ci_tblv1 <- ci_tbl
ci_tblv2 <- ci_tbl
save(ci_tbl, file="/Users/joylee/Documents/Research Project/ConditionalRaking/citbl_pop3unequalv1.rda")


ci_length <- function(CI){
  ci_l <- 0
  
  for (i in 1:length(CI)){
    ci_l <- ci_l + CI[i, 2] - CI[i, 1]
  }
  return(ci_l / length(CI))
}

ci_length(PS_CI)
ci_length(PR_CI)
ci_length(Rake_CI)
ci_length(Samp_CI)











pop1Freq <- pop %>% summarise(realcount = n())
pop1Freq$count <- pop1Freq$realcount/N
chisq.test(pop1Freq$realcount, p = rep(1 / length(pop1Freq$realcount), length(pop1Freq$realcount)))

### Does samp match pop?
chisq3 <- list()
chisq3p <- list()

for(i in 1:length(Samp)){
  samp <- Samp[[i]] %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
  samplecount <- samp %>% summarise(realcount = n())
  chisq3p[[i]] <- chisq.test(samplecount$realcount, p = pop1Freq$count)$p.value
  chisq3[[i]] <- chisq.test(samplecount$realcount, p = pop1Freq$count)$statistic
}

chisq3meanp <- mean(unlist(chisq3p))
chisq3meanp   # 3.047258e-31

chisq3meanx <- mean(unlist(chisq3))
chisq3meanx   # 184.4205

# for pop2
### balanced pop?
pop2Freq <- pop %>% summarise(realcount = n())
pop2Freq$count <- pop2Freq$realcount/N
chisq.test(pop2Freq$realcount, 
           p = rep(1/length(pop2Freq$realcount), 
                   length(pop2Freq$realcount)))











############################################################
############ Giant Function Version ###############
############################################################

function(pop, samp_method, version){
  #### some day in the far, far off future, maaaaybe it'll use a list of PS and raking variables
  # but probably not
  results <- list()
  Samp <- list()
  
  for (i in 1:nsim){
    pop <- ungroup(pop)
    samp <- samp_method(pop)
    
    ###### check sample makeup, order by groups
    samp <- samp %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
    sampFreq <- samp %>% summarise(count = n()/nrow(samp))
    sampFreq
    
    Samp[[i]] <- samp
  }
}



f_four <- function(pop, version) {
  
  results <- list()
  samplist <- list()
  CI_df <- as.data.frame(matrix(rep(16*nsim), ncol=16))
  #names(CI_df) <- ()
  
  for (i in 1:nsim) {
    
    #samp <- samp_method(pop)
    
    ### stratified sampling
    pop <- pop %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
    samp <- sample_n(pop, 400/16, replace = FALSE)
    
    ### SRS sampling    
    # pop <- ungroup(pop)
    # samp <- sample_n(pop, 400, replace = FALSE)
    # 
    ### unequal probability sampling
    # pop <- ungroup(pop)
    # samp <- sample_n(pop, 2000, replace = FALSE, weight = 1 -0.2 * pop$I_age_old - 0.4 * pop$I_race_B)
    
    ###### check sample makeup, order by groups
    samp <- samp %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
    sampFreq <- samp %>% summarise(count = n() / nrow(samp))
    sampFreq
    
    samplist[[i]] <- samp
    
    pop <- pop %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
    popFreq <- pop %>% summarise(count = n()/N)
    
    
    ###############################
    # post strat
    ###############################
    pop <- pop %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
    popFreq <- pop %>% summarise(count = n() / N)
    
    sampFreq$psweight <- popFreq$count / sampFreq$count
    samp <- merge(samp, 
                  sampFreq, 
                  by.x = c("I_age_old", "I_sex_F", "I_race_B", "I_ins_A"),
                  by.y = c("I_age_old", "I_sex_F", "I_race_B", "I_ins_A"), 
                  all.x = TRUE)
    
    
    ###############################
    # raking
    ###############################
    trueage <- wpct(pop$I_age_old)
    truesex <- wpct(pop$I_sex_F)
    truerace <- wpct(pop$I_race_B)
    trueins <- wpct(pop$I_ins_A)
    targets <- list(trueage, truesex, truerace, trueins)
    names(targets) <- c("I_age_old", "I_sex_F", "I_race_B", "I_ins_A")
    
    #anesrakefinder(targets, samp, choosemethod = "total")
    # Raking starts here.  
    samp$caseid <- 1:nrow(samp)
    
    samp$I_age_old <- as.factor(samp$I_age_old)
    samp$I_sex_F <- as.factor(samp$I_sex_F)
    samp$I_race_B <- as.factor(samp$I_race_B)
    samp$I_ins_A <- as.factor(samp$I_ins_A)
    
    
    anes <- anesrake(targets, samp, caseid = samp$caseid, cap = 200, 
                     choosemethod = "total", 
                     type = "nolim")
    samp$rakeweight <- anes$weightvec 
    
    
    if (version == 1) {
      ###############################
      #partial raking   V1
      ###############################
      ##know sex & race & age, age & ins
      trueage <- wpct(pop$I_age_old)
      targets <- list(trueage)
      names(targets) <- c("I_age_old")
      
      #anesrakefinder(targets, samp, choosemethod = "total")
      # Raking starts here.
      samp$caseid <- 1:nrow(samp)
      
      samp$I_age_old <- as.factor(samp$I_age_old)
      
      anes <- anesrake(targets, samp, caseid = samp$caseid, cap = 20, choosemethod = "total")
      samp$prweight <- anes$weightvec
      
      samp$I_sex_race <- 0
      
      # adding combined sex & race variable
      samp <- samp %>%
        mutate(I_sex_race = ifelse(I_sex_F == 0 & I_race_B == 0, 1,
                                   ifelse(I_sex_F == 0 & I_race_B == 1, 2,
                                          ifelse(I_sex_F == 1 & I_race_B == 0, 3, 4))))
      
      # creating a copy of the pop df w/ combined sex & race variable
      popcopy <- pop %>%
        mutate(I_sex_race = ifelse(I_sex_F == 0 & I_race_B == 0, 1,
                                   ifelse(I_sex_F == 0 & I_race_B == 1, 2,
                                          ifelse(I_sex_F == 1 & I_race_B == 0, 3, 4))))
      
      for (k in 1:10) {
        
        for(j in 0:1){
          subpop <- subset(popcopy, I_age_old == j)
          truesexracesub <- wpct(subpop$I_sex_race)
          trueinssub <- wpct(subpop$I_ins_A)
          subtargets <- list(truesexracesub, trueinssub)
          names(subtargets) <- c("I_sex_race", "I_ins_A")
          
          subsamp <- subset(samp, I_age_old==j)
          
          
          subsamp$caseid <- 1:nrow(subsamp)
          subsamp$I_sex_race <- as.factor(subsamp$I_sex_race)
          subsamp$I_ins_A <- as.factor(subsamp$I_ins_A)
          
          
          anes <- anesrake(subtargets, subsamp, caseid = subsamp$caseid, cap = 20, 
                           choosemethod = "total", weightvec = subsamp$prweight, center.baseweights = FALSE)
          
          samp$prweight[samp$I_age_old == j] <- anes$weightvec
          
        }
      }
      
      
    }
    
    
    
    
    
    if (version == 2) {
      # ###############################
      # #partial raking  V2
      # ###############################
      # # know sex & race, race & age, age & ins
      samp$I_race_age <- 0
      samp <- samp %>%
        mutate(I_race_age = ifelse(I_race_B == 0 & I_age_old == 0, 1,
                                   ifelse(I_race_B == 0 & I_age_old == 1, 2,
                                          ifelse(I_race_B == 1 & I_age_old == 0, 3, 4))))
      levels(samp$I_race_age)
      popcopy <- pop %>%
        mutate(I_race_age = ifelse(I_race_B == 0 & I_age_old == 0, 1,
                                   ifelse(I_race_B == 0 & I_age_old == 1, 2,
                                          ifelse(I_race_B == 1 & I_age_old == 0, 3, 4))))
      
      popcopy$I_race_age <- as.factor(popcopy$I_race_age)
      trueraceage <- wpct(popcopy$I_race_age)
      targets <- list(trueraceage)
      names(targets) <- c("I_race_age")
      
      # PS on race & age
      samp$caseid <- 1:nrow(samp)
      samp$I_race_age <- as.factor(samp$I_race_age)
      
      samp <- as.data.frame(samp)
      anes <- anesrake(targets, samp, caseid = samp$caseid, cap = 20, choosemethod = "total",
                       type = "nolim")
      samp$prweight <- anes$weightvec
      
      # Condition on race, PS on sex
      for(j in 0:1){
        subpop <- subset(popcopy, I_race_B == j)
        truesexsub <- wpct(subpop$I_sex_F)
        subtargets <- list(truesexsub)
        names(subtargets) <- c("I_sex_F")
        
        subsamp<- subset(samp, I_race_B == j)
        
        subsamp$caseid <- 1:nrow(subsamp)
        subsamp$I_sex_F <- as.factor(subsamp$I_sex_F)
        
        anes <- anesrake(subtargets, subsamp, caseid = subsamp$caseid, cap = 20, choosemethod = "total", 
                         weightvec = subsamp$prweight, center.baseweights = FALSE,
                         type = "nolim")
        
        samp$prweight[samp$I_race_B == j] <- anes$weightvec
        
      }
      
      # Condition on age, PS on insurance
      for(j in 0:1){
        subpop <- subset(popcopy, I_age_old == j)
        trueinssub <- wpct(subpop$I_ins_A)
        subtargets <- list(trueinssub)
        names(subtargets) <- c("I_ins_A")
        
        subsamp <- subset(samp, I_age_old == j)
        
        
        subsamp$caseid <- 1:nrow(subsamp)
        subsamp$I_ins_A <- as.factor(subsamp$I_ins_A)
        
        anes <- anesrake(subtargets, subsamp, caseid = subsamp$caseid, cap = 200, 
                         choosemethod = "total", 
                         weightvec=subsamp$prweight,center.baseweights = FALSE,
                         type = "nolim")
        
        
        samp$prweight[samp$I_age_old == j] <- anes$weightvec
        
      }
    }
    
    if (version == 3) {
      # ###############################
      # #partial raking  V3
      # ###############################
      # # know sex & race, race & age, age & ins, ins & sex
      trueage <- wpct(pop$I_age_old)
      targets <- list(trueage)
      names(targets) <- c("I_age_old")
      
      #anesrakefinder(targets, samp, choosemethod = "total")
      # Raking starts here.
      samp$caseid <- 1:nrow(samp)
      
      samp$I_age_old <- as.factor(samp$I_age_old)
      
      anes <- anesrake(targets, samp, caseid = samp$caseid, cap = 200,
                       choosemethod = "total",
                       center.baseweights = FALSE,
                       type = "nolim")
      samp$prweight <- anes$weightvec
      
      check_weights <- list()
      for (k in 1:10){
        # Condition on age, Rake on sex and ins
        for (j in 0:1) {
          subpop <- subset(pop, I_age_old == j)
          truesexsub <- wpct(subpop$I_sex_F)
          trueinssub <- wpct(subpop$I_ins_A)
          subtargets <- list(truesexsub, trueinssub)
          names(subtargets) <- c("I_sex_F", "I_ins_A")
          
          subsamp <- subset(samp, I_age_old == j)
          
          
          subsamp$caseid <-1:nrow(subsamp)
          subsamp$I_sex_F <- as.factor(subsamp$I_sex_F)
          subsamp$I_ins_A <- as.factor(subsamp$I_ins_A)
          
          
          anes <- anesrake(subtargets, subsamp, caseid = subsamp$caseid, cap = 200, 
                           weightvec = subsamp$prweight,
                           center.baseweights = FALSE,
                           choosemethod = "total", 
                           type = "nolim")
          
          samp$prweight[samp$I_age_old == j] <- anes$weightvec
          
          check_weights[[j + 1]] <- samp$prweight
        }
        #print(sum(abs(check_weights[[1]] - check_weights[[2]])))
        # Condition on Sex, rake on Age and Race
        for (j in 0:1) {
          subpop <- subset(pop, I_sex_F == j)
          trueagesub <- wpct(subpop$I_age_old)
          trueracesub <- wpct(subpop$I_race_B)
          subtargets <- list(trueagesub, trueracesub)
          names(subtargets) <- c("I_age_old", "I_race_B")
          
          subsamp <- subset(samp, I_sex_F == j)
          
          
          subsamp$caseid <-1:nrow(subsamp)
          subsamp$I_age_old <- as.factor(subsamp$I_age_old)
          subsamp$I_race_B <- as.factor(subsamp$I_race_B)
          
          
          anes <- anesrake(subtargets, subsamp, caseid = subsamp$caseid, cap = 200, 
                           weightvec = subsamp$prweight,
                           center.baseweights = FALSE,
                           choosemethod = "total", 
                           type = "nolim")
          
          samp$prweight[samp$I_sex_F == j] <- anes$weightvec
          
          check_weights[[j + 1]] <- samp$prweight
        }
        #print(sum(abs(check_weights[[1]] - check_weights[[2]])))
        
        # Condition on Race, rake on Age and Sex
        for (j in 0:1) {
          subpop <- subset(pop, I_race_B == j)
          truesexsub <- wpct(subpop$I_sex_F)
          trueagesub <- wpct(subpop$I_age_old)
          subtargets <- list(truesexsub, trueagesub)
          names(subtargets) <- c("I_sex_F", "I_age_old")
          
          subsamp <- subset(samp, I_race_B == j)
          
          
          subsamp$caseid <-1:nrow(subsamp)
          subsamp$I_sex_F <- as.factor(subsamp$I_sex_F)
          subsamp$I_age_old <- as.factor(subsamp$I_age_old)
          
          
          anes <- anesrake(subtargets, subsamp, caseid = subsamp$caseid, cap = 200, 
                           weightvec = subsamp$prweight,
                           center.baseweights = FALSE,
                           choosemethod = "total", 
                           type = "nolim")
          
          samp$prweight[samp$I_race_B == j] <- anes$weightvec
          
          check_weights[[j + 1]] <- samp$prweight
        }
        
      }
    }
    # else {
    #   stop("Need to include version number!")
    # }
    
    ###############################
    # results
    ###############################
    results[[i]] <- c(popMean = mean(pop$income), 
                      sampMean = mean(samp$income), 
                      sampPSmean = weighted.mean(samp$income, samp$psweight), 
                      samprakemean = weighted.mean(samp$income, samp$rakeweight),
                      sampPRmean = weighted.mean(samp$income, samp$prweight))
    
    
    ####bootstrapping
    #samp <- samp %>% group_by(I_age_old, I_sex_F, I_race_B)
    samp <- ungroup(samp)
    
    bsSampMean <- c()
    bsPSMean <- c()
    bsRakeMean <- c()
    bsPRMean <- c()
    for (b in 1:500){
      bs_samp <- sample_n(samp, 2000, replace = TRUE)
      
      bsSampMean[b] <- mean(bs_samp$income)
      bsPSMean[b] <- weighted.mean(bs_samp$income, bs_samp$psweight)
      bsRakeMean[b] <- weighted.mean(bs_samp$income, bs_samp$rakeweight)
      bsPRMean[b] <- weighted.mean(bs_samp$income, bs_samp$prweight)
      
      
    }
    
    
    ### Bias Correction w/ Bootstrap
    ##### find bias based off bootstrap 
    
    Sampbias <- mean(bsSampMean) - mean(samp$income)
    CI_df[i, 1] <- quantile(bsSampMean + Sampbias, .025)
    CI_df[i, 2] <- quantile(bsSampMean + Sampbias, .975)
    
    PSbias <- mean(bsPSMean) - weighted.mean(samp$income,samp$psweight)
    CI_df[i, 3] <- quantile(bsPSMean + PSbias, .025)
    CI_df[i, 4] <- quantile(bsPSMean + PSbias, .975)
    
    rakebias <- mean(bsRakeMean) - weighted.mean(samp$income,samp$rakeweight)
    CI_df[i, 5] <- quantile(bsRakeMean + rakebias, .025)
    CI_df[i, 6] <- quantile(bsRakeMean + rakebias, .975)
    
    PRbias <- mean(bsPRMean) - weighted.mean(samp$income,samp$prweight)
    CI_df[i, 7] <- quantile(bsPRMean + PRbias, .025)
    CI_df[i, 8] <- quantile(bsPRMean + PRbias, .975)
    
    CI_df[i, 9] <- quantile(bsSampMean, .025) #Samp
    CI_df[i, 10] <- quantile(bsSampMean, .975)
    
    CI_df[i, 11] <- quantile(bsPSMean, .025) #PS
    CI_df[i, 12] <- quantile(bsPSMean, .975)
    
    CI_df[i, 13] <- quantile(bsRakeMean, .025) #Rake
    CI_df[i, 14] <- quantile(bsRakeMean, .975)
    
    CI_df[i, 15] <- quantile(bsPRMean, .025) #PR
    CI_df[i, 16] <- quantile(bsPRMean, .975)
    
    
    
  } # end nsim loop
  
  return(list(results, CI_df))
  
  end <- Sys.time()
  end-start
} # end function

