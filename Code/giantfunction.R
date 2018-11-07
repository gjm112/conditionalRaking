library(weights)
library(anesrake)
library(dplyr)
library(tidyr)
library(ggplot2)

expit <- function(x){
  out <- exp(x) / (1+exp(x))
  return(out)
}

nsim <- 100 # number of simulations

### input population df, partial raking version (numeric)
    # version3: 1 = 3 categorical variables with 1 PS (overlap)
      # 2 = 3 cat variables without overlap
     
### returns list with means df (unadjusted, PS, Raking, Partial Raking), CI df df, 
    #  


### Separate V1 into its own function
    ### 


f_three <- function(pop, version){
  start <- Sys.time()
  results<-list()
  samplist <- list()
  CI_df <- as.data.frame(matrix(rep(0, 16 * nsim), ncol=16))
  #names(CI_df) <- ()
  
  for (i in 1:nsim){
    
    #samp <- samp_method(pop)
    
    ## stratified sampling
    # pop <- pop %>% group_by(I_age_old, I_sex_F, I_race_B)
    # samp <- sample_n(pop, 400/8, replace = FALSE)
    # pop <- ungroup(pop)
    
    ## stratified sampling with 2 variables
    # pop <- pop %>% group_by(I_age_old, I_sex_F)
    # samp <- sample_n(pop, 800/4, replace = FALSE)
    # pop <- ungroup(pop)
    
    ## SRS sampling    #WHY DOES THIS NO LONGER WORK???
    # pop <- ungroup(pop)
    # samp <- sample_n(pop, 800, replace=FALSE)
    # 
    # ## unequal probability sampling 1
    # pop <- ungroup(pop)
    # samp <- sample_n(pop, 800, replace = FALSE, weight = 1 -0.2*pop$I_age_old - 0.4*pop$I_race_B)
    
    # ## unequal probability sampling 2
    pop <- ungroup(pop)
    samp <- sample_n(pop, 800, replace = FALSE, weight = 1 -0.2*pop$I_age_old - 0.4*pop$I_race_B +
                       2*pop$I_age_old*pop$I_sex_F*pop$I_race_B + 0.4*pop$I_sex_F)
    
    # ## unequal probability sampling 3
    # pop <- ungroup(pop)
    # samp <- sample_n(pop, 800, replace = FALSE, weight = 1 + 0.2*pop$I_age_old - 0.4*pop$I_race_B + 
    #                    pop$I_age_old*pop$I_sex_F*pop$I_race_B + 0.4*pop$I_sex_F)
    
    ###### check sample makeup, order by groups
    samp <- samp %>% group_by(I_age_old, I_sex_F, I_race_B)
    sampFreq <- samp %>% summarise(count = n() / nrow(samp))
    sampFreq
    
    samplist[[i]] <- samp
    
    pop <- pop %>% group_by(I_age_old, I_sex_F, I_race_B)
    popFreq <- pop %>% summarise(count = n()/N)
    popFreq
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

      check_weights <- c(rep(0, nrow(samp)))
      check_weights2 <- list()
      count <- 1

      while (max(abs(check_weights - samp$prweight) > .01)) {
      #for (k in 1:10){


        # anes <- anesrake(targets, samp, caseid = samp$caseid, cap = 200,
        #                  choosemethod = "total",
        #                  center.baseweights = FALSE,
        #                  weightvec = samp$prweight,
        #                  type = "nolim")
        # samp$prweight <- anes$weightvec
        check_weights <- samp$prweight

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

          #check_weights[[j + 1]] <- samp$prweight
        }
        # print(unique(samp$prweight))
        #print(sum(abs(check_weights[[1]] - check_weights[[2]])))
        #check_weights2[[k]] <- samp$prweight
        
        print(count)
        count <- count + 1
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

      anes <- anesrake(targets, samp, caseid = samp$caseid, cap = 200,
                       choosemethod = "total",
                       center.baseweights = FALSE,
                       type = "nolim")
      samp$prweight <- anes$weightvec

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
      bs_samp <- sample_n(samp, 2000, replace=TRUE)
      
      bsSampMean[b] <- mean(bs_samp$income)
      bsPSMean[b] <- weighted.mean(bs_samp$income, bs_samp$psweight)
      bsRakeMean[b] <- weighted.mean(bs_samp$income, bs_samp$rakeweight)
      bsPRMean[b] <- weighted.mean(bs_samp$income, bs_samp$prweight)
      
      
    }
    
    
    ### Bias Correction w/ Bootstrap
    ##### find bias based off bootstrap 
    
    Sampbias <- mean(bsSampMean) - mean(samp$income)
    CI_df[i,1] <- quantile(bsSampMean + Sampbias, .025)
    CI_df[i,2] <- quantile(bsSampMean + Sampbias, .975)
    
    PSbias <- mean(bsPSMean) - weighted.mean(samp$income,samp$psweight)
    CI_df[i,3] <- quantile(bsPSMean + PSbias, .025)
    CI_df[i,4] <- quantile(bsPSMean + PSbias, .975)
    
    rakebias <- mean(bsRakeMean) - weighted.mean(samp$income,samp$rakeweight)
    CI_df[i,5] <- quantile(bsRakeMean + rakebias, .025)
    CI_df[i,6] <- quantile(bsRakeMean + rakebias, .975)
    
    PRbias <- mean(bsPRMean) - weighted.mean(samp$income,samp$prweight)
    CI_df[i,7] <- quantile(bsPRMean + PRbias, .025)
    CI_df[i,8] <- quantile(bsPRMean + PRbias, .975)
    
    CI_df[i,9] <- quantile(bsSampMean, .025) #Samp
    CI_df[i,10] <- quantile(bsSampMean, .975)
    
    CI_df[i,11] <- quantile(bsPSMean, .025) #PS
    CI_df[i,12] <- quantile(bsPSMean, .975)
    
    CI_df[i,13] <- quantile(bsRakeMean, .025) #Rake
    CI_df[i,14] <- quantile(bsRakeMean, .975)
    
    CI_df[i,15] <- quantile(bsPRMean, .025) #PR
    CI_df[i,16] <- quantile(bsPRMean, .975)
    
    
    
  } # end nsim loop
  
  end <- Sys.time()
  print(end - start)
  return(list(results, CI_df, samplist))
  
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

 


set.seed(89)
testone <- f_three(pop, 1)


res1 <- as.data.frame(do.call(rbind, testone[[1]]))  ## why do you need double [] ?

res1$biasSamp <- res1$sampMean - res1$popMean
res1$biasSampPercent <- 100 * res1$biasSamp / res1$popMean
res1$biasPS <- res1$sampPSmean - res1$popMean
res1$biasPSPercent <- 100 * res1$biasPS / res1$popMean
res1$biasrake <- res1$samprakemean - res1$popMean
res1$biasrakePercent <- 100 * res1$biasrake / res1$popMean
res1$biasPR <- res1$sampPRmean - res1$popMean
res1$biasPRPercent <- 100 * res1$biasPR / res1$popMean

biastbl1 <- apply(res1, 2, mean)
biastbl1

vartbl1 <- apply(res1[, 2:5], 2, sd)
vartbl1

samp1 <- testone[3]

set.seed(89)
testtwo <- f_three(pop, 2)


res2 <- as.data.frame(do.call(rbind, testtwo[[1]]))  

res2$biasSamp <- res2$sampMean-res2$popMean
res2$biasSampPercent <- 100 * res2$biasSamp / res2$popMean
res2$biasPS <- res2$sampPSmean-res2$popMean
res2$biasPSPercent <- 100 * res2$biasPS / res2$popMean
res2$biasrake <- res2$samprakemean-res2$popMean
res2$biasrakePercent <- 100 * res2$biasrake / res2$popMean
res2$biasPR <- res2$sampPRmean-res2$popMean
res2$biasPRPercent <- 100 * res2$biasPR / res2$popMean

biastbl2 <- apply(res2, 2, mean)
biastbl2

vartbl2 <- apply(res2[, 2:5], 2, sd)
vartbl2

samp2 <- testtwo[3]

## -1.7574026 


set.seed(89)
testthree <- f_three(pop, 3)


res3 <- as.data.frame(do.call(rbind, testthree[[1]]))  ## why do you need double [] ?

res3$biasSamp <- res3$sampMean-res3$popMean
res3$biasSampPercent <- 100 * res3$biasSamp / res3$popMean
res3$biasPS <- res3$sampPSmean-res3$popMean
res3$biasPSPercent <- 100 * res3$biasPS / res3$popMean
res3$biasrake <- res3$samprakemean-res3$popMean
res3$biasrakePercent <- 100 * res3$biasrake / res3$popMean
res3$biasPR <- res3$sampPRmean-res3$popMean
res3$biasPRPercent <- 100 * res3$biasPR / res3$popMean

biastbl3 <- apply(res3, 2, mean)
biastbl3

vartbl3 <- apply(res3[, 2:5], 2, sd)
vartbl3

samp3 <- testthree[3]

#a <- list(biastbl1, biastbl2, biastbl3, vartbl1, vartbl2, vartbl3)
a

# atwo <- list(biastbl1, biastbl2, biastbl3, vartbl1, vartbl2, vartbl3, 
#            res1, res2, res3) # stratified sampling on 2 variables
# atwo

a18 <- list(biastbl1, biastbl2, biastbl3, vartbl1, vartbl2, vartbl3, 
           res1, res2, res3) #seed = 18; 1; 
a18

a2019 <- list(biastbl1, biastbl2, biastbl3, vartbl1, vartbl2, vartbl3, 
            res1, res2, res3) #seed = 2019; 
a2019





asamp <- testone[3]

biastbl1
biastbl2
biastbl3
vartbl1
vartbl2
vartbl3



library(xtable)

hist <- ggplot(gather(res1[, 2:5])) +
  geom_vline(xintercept = mean(pop$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  ggtitle("testone") +
  #xlab("Income") +
  xlim(65000, 100000) #
#ylim(0, 75)
hist




save(hist, file = "/Users/joylee/Documents/Research Project/ConditionalRaking/hist_pop3unequal2v2.rda")
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

