library(weights)
library(anesrake)
library(dplyr)
library(tidyr)
library(ggplot2)


N<-100000     # population size
set.seed(89)

expit <- function(x){
  out <- exp(x) / (1+exp(x))
  return(out)
}

<<<<<<< HEAD
#pop: data.frame and contains the true population
#sampmethod: function
=======

# function for sample stats
# inputs   pop = population df, sampmethod = sampling function
# returns  list of res=mean results,  results=raw results, allSamp = dataframe of all the samples

>>>>>>> 82eb7eff85d389ffdffe04c22d728994fa082b11
resfunc <- function(pop, sampmethod){
  set.seed(89)
  
  ############################################
  #Sampling
  ############################################
  nsim<-100
  results<-list()
  allSamp <- list()
  allSampFreq <- list()
  resultsLM <- list()
  resultsLM[["lmRaw"]] <- resultsLM[["lmPS"]] <- resultsLM[["lmRake"]] <- resultsLM[["lmPR"]] <- matrix(NA, ncol=5,nrow=nsim)
  for (i in 1:nsim){
    
    sampList <- list()
    samp <- sampmethod(pop)
    
    ###### check sample makeup, order by groups, add to output list
    samp <- samp %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
    sampFreq <- samp %>% summarise(count = n()/nrow(samp))
    #sampFreq
    
    allSamp[[i]] <- samp
    allSampFreq [[i]] <- sampFreq
    
    
    ###############################
    # post strat
    ###############################
    pop <- pop %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
    popFreq <- pop %>% summarise(count = n()/N)
    
    sampFreq$psweight <- popFreq$count/sampFreq$count
    samp<-merge(samp,sampFreq, by.x = c("I_age_old","I_sex_F","I_race_B", "I_ins_A"), by.y = c("I_age_old","I_sex_F","I_race_B", "I_ins_A"),all.x=TRUE)
    
    
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
    
    anes <- anesrake(targets, samp, caseid= samp$caseid, cap= 20, choosemethod = "total")
    samp$rakeweight <- anes$weightvec 
    
    ###############################
    #partial raking
    ###############################
    trueage <- wpct(pop$I_age_old)
    targets <- list(trueage)
    names(targets) <- c("I_age_old")
    
    #anesrakefinder(targets, samp, choosemethod = "total")
    # Raking starts here.  
    samp$caseid <- 1:nrow(samp)
    
    samp$I_age_old <- as.factor(samp$I_age_old)
    
    anes <- anesrake(targets, samp, caseid= samp$caseid, cap= 20, choosemethod = "total")
    samp$prweight <- anes$weightvec
    
    samp$I_sex_race <- 0
    
    # adding combined sex $ race variable
    samp <- samp %>% 
      mutate(I_sex_race = ifelse(I_sex_F ==0 & I_race_B ==0, 1,
                                 ifelse(I_sex_F ==0 & I_race_B == 1, 2,
                                        ifelse(I_sex_F==1 & I_race_B ==0, 3, 4))))
    
    # creating a copy of the pop df w/ combined sex & race variable
    popcopy <- pop %>% 
      mutate(I_sex_race = ifelse(I_sex_F ==0 & I_race_B ==0, 1,
                                 ifelse(I_sex_F ==0 & I_race_B == 1, 2,
                                        ifelse(I_sex_F==1 & I_race_B ==0, 3, 4))))
    
    
    for(j in 0:1){
      subpop <- subset(popcopy, I_age_old==j)
      truesexracesub <- wpct(subpop$I_sex_race)
      trueinssub <- wpct(subpop$I_ins_A)
      subtargets <- list(truesexracesub, trueinssub)
      names(subtargets) <- c("I_sex_race", "I_ins_A")
      
      subsamp<- subset(samp, I_age_old==j)
      
      
      subsamp$caseid <-1:nrow(subsamp)
      subsamp$I_sex_race <- as.factor(subsamp$I_sex_race)
      subsamp$I_ins_A <- as.factor(subsamp$I_ins_A)
      
      
      anes <- anesrake(subtargets, subsamp, caseid= subsamp$caseid, cap=20, choosemethod = "total", weightvec=subsamp$prweight,center.baseweights = FALSE)
      
      samp$prweight[samp$I_age_old==j] <- anes$weightvec
      
    }
    
    results[[i]]<-c(popMean=mean(pop$income), sampMean=mean(samp$income), sampPSmean=weighted.mean(samp$income,samp$psweight), samprakemean=weighted.mean(samp$income,samp$rakeweight),sampPRmean = weighted.mean(samp$income,samp$prweight))
    
    pop_model <- lm(income~I_age_old+I_sex_F+I_race_B+I_ins_A,data=pop)
    # resultsLM[["lmRaw"]][i,] <- lm(income~I_age_old+I_sex_F+I_race_B,data=samp)$coefficients
    # resultsLM[["lmPS"]][i,] <- lm(income~I_age_old+I_sex_F+I_race_B,data=samp,weight=psweight)$coefficients
    # resultsLM[["lmRake"]][i,] <- lm(income~I_age_old+I_sex_F+I_race_B,data=samp,weight=rakeweight)$coefficients
    # resultsLM[["lmPR"]][i,] <-lm(income~I_age_old+I_sex_F+I_race_B,data=samp,weight=prweight)$coefficients
    
    resultsLM[["lmRaw"]][i,] <- lm(income~I_age_old + I_sex_F + I_race_B + I_ins_A,data=samp)$coefficients
    resultsLM[["lmPS"]][i,] <- lm(income~I_age_old + I_sex_F + I_race_B + I_ins_A,data=samp,weight=psweight)$coefficients
    resultsLM[["lmRake"]][i,] <- lm(income~I_age_old + I_sex_F + I_race_B + I_ins_A,data=samp,weight=rakeweight)$coefficients
    resultsLM[["lmPR"]][i,] <-lm(income~I_age_old + I_sex_F + I_race_B + I_ins_A,data=samp,weight=prweight)$coefficients
    
    
  }
  res<-as.data.frame(do.call(rbind,results))
  
  # res = mean results, results = raw results, allSamp = df of all the samples
  outputList <- list("res" = res, "results" = results, "allSamp" = allSamp)
  return(outputList)
}

   
############  Trials   ###################
####### Pop A
popA <- data.frame(I_age_old = rbinom(N, 1, 0.6), I_sex_F = 0, I_race_B = 0, I_ins_A = 0)
#sex
p <- expit(0 + 0.9 * popA$I_age_old)
popA$I_sex_F <- rbinom(N,1,prob = p)
#race
p <- expit(1 + 0.9 * popA$I_age_old * popA$I_sex_F)
popA$I_race_B <- rbinom(N,1,prob = p)
#Insurance
p <- expit(0 + 1.2 * popA$I_age_old * popA$I_race_B * popA$I_sex_F)
popA$I_ins_A <- rbinom(N,1,prob = p)

popA <- popA %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
popAFreq <- popA %>% summarise(count = n()/N)
popAFreq

###### Pop B
popB <- data.frame(I_age_old = rbinom(N, 1, 0.8), I_sex_F = 0, I_race_B = 0, I_ins_A = 0)
#sex
p <- expit(0 - 0.9 * popB$I_age_old)
popB$I_sex_F <- rbinom(N,1,prob = p)
#race
p <- expit(1 - 0.9 * popB$I_age_old * popB$I_sex_F)
popB$I_race_B <- rbinom(N,1,prob = p)
#Insurance
p <- expit(0 + 1.2 * popB$I_age_old * popB$I_race_B * popB$I_sex_F)
popB$I_ins_A <- rbinom(N,1,prob = p)

popB <- popB %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
popBFreq <- popB %>% summarise(count = n()/N)
popBFreq

###### Pop C
# popC <- data.frame(I_age_old = rbinom(N, 1, 0.4), I_sex_F = 0, I_race_B = 0, I_ins_A = 0)
# sex
# p <- expit(0 + 0.9 * popC$I_age_old)
# popC$I_sex_F <- rbinom(N,1,prob = p)
# #race
# p <- expit(1 + 2 * popC$I_age_old * popC$I_sex_F)
# popC$I_race_B <- rbinom(N,1,prob = p)
popC <- data.frame(I_age_old = rbinom(N, 1, 0.4), I_sex_F = 0, I_race_B = 0)
#sex
p <- expit(0 + 1.2 * popC$I_age_old)
popC$I_sex_F <- rbinom(N,1,prob = p)
#race
p <- expit(1 + 0.6 * popC$I_age_old * popC$I_sex_F)
popC$I_race_B <- rbinom(N,1,prob = p) 
#ins
p <- expit(0 + 1.2 * popC$I_age_old * popC$I_sex_F + 1.8 * popC$I_race_B)
popC$I_ins_A <- rbinom(N,1,prob = p)

popC <- popC %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
popCFreq <- popC %>% summarise(count = n()/N)
popCFreq

###### Pop D
popD <- data.frame(I_age_old = rbinom(N, 1, 0.5), I_sex_F = 0, I_race_B = 0, I_ins_A = 0)
#sex
p <- expit(0 + 0.0 * popD$I_age_old)
popD$I_sex_F <- rbinom(N,1,prob = p)
#race
p <- expit(0 + 0.0 * popD$I_age_old * popD$I_sex_F)
popD$I_race_B <- rbinom(N,1,prob = p)
#Insurance
p <- expit( 0.0 * popD$I_age_old * popD$I_race_B * popD$I_sex_F)
popD$I_ins_A <- rbinom(N,1,prob = p)

popD <- popD %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
popDFreq <- popD %>% summarise(count = n()/N)
popDFreq

# Trial 1: Three Variable Stratified, Pop A-----------------------------------------------------------------

# pop1 <- data.frame(I_age_old = rbinom(N, 1, 0.6), I_sex_F = 0, I_race_B = 0, I_ins_A = 0)
# #sex
# p <- expit(0 + 0.9 * pop1$I_age_old)
# pop1$I_sex_F <- rbinom(N,1,prob = p)
# #race
# p <- expit(1 + 0.9 * pop1$I_age_old * pop1$I_sex_F)
# pop1$I_race_B <- rbinom(N,1,prob = p)
# #Insurance
# p <- expit(0 + 1.2 * pop1$I_age_old * pop1$I_race_B * pop1$I_sex_F)
# pop1$I_ins_A <- rbinom(N,1,prob = p)
# pop1 <- pop1 %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
# pop1Freq <- pop1 %>% summarise(count = n()/N)
# pop1Freq
pop1 <- popA

pop1$income <- 25000 + 
  40000 * pop1$I_age_old +
  25000 * pop1$I_sex_F * pop1$I_race_B + 
  30000 * pop1$I_sex_F * pop1$I_age_old +  
  30000 * pop1$I_age_old * pop1$I_race_B * pop1$I_ins_A + 
  20000 * pop1$I_age_old * pop1$I_sex_F * pop1$I_race_B * pop1$I_ins_A +
  rnorm(N,0,5000)


method1 <- function(pop){
  sampList <- list()
  s_size <- 400/8
  ind <- sample(1:sum(pop$I_age_old==1 & pop$I_sex_F==1 & pop$I_race_B == 1), s_size, replace=FALSE)
  sampList[[1]] <- pop[pop$I_age_old==1 & pop$I_sex_F==1 & pop$I_race_B == 1,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==1 & pop$I_sex_F==1 & pop$I_race_B == 0), s_size, replace=FALSE)
  sampList[[2]] <- pop[pop$I_age_old==1 & pop$I_sex_F==1 & pop$I_race_B == 0,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==1 & pop$I_sex_F==0 & pop$I_race_B == 1), s_size, replace=FALSE)
  sampList[[3]] <- pop[pop$I_age_old==1 & pop$I_sex_F==0 & pop$I_race_B == 1,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==1 & pop$I_sex_F==0 & pop$I_race_B == 0), s_size, replace=FALSE)
  sampList[[4]] <- pop[pop$I_age_old==1 & pop$I_sex_F==0 & pop$I_race_B == 0,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==0 & pop$I_sex_F==1 & pop$I_race_B == 1), s_size, replace=FALSE)
  sampList[[5]] <- pop[pop$I_age_old==0 & pop$I_sex_F==1 & pop$I_race_B == 1,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==0 & pop$I_sex_F==1 & pop$I_race_B == 0), s_size, replace=FALSE)
  sampList[[6]] <- pop[pop$I_age_old==0 & pop$I_sex_F==1 & pop$I_race_B == 0,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==0 & pop$I_sex_F==0 & pop$I_race_B == 1), s_size, replace=FALSE)
  sampList[[7]] <- pop[pop$I_age_old==0 & pop$I_sex_F==0 & pop$I_race_B == 1,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==0 & pop$I_sex_F==0 & pop$I_race_B == 0), s_size, replace=FALSE)
  sampList[[8]] <- pop[pop$I_age_old==0 & pop$I_sex_F==0 & pop$I_race_B == 0,][ind,]
  samp <- do.call(rbind, sampList)
  
  return(samp)
}

restrial1 <- resfunc(pop1, method1)

resbias1 <- restrial1$res
resbias1$biasPS <- resbias1$sampPSmean-resbias1$popMean
resbias1$biasrake <- resbias1$samprakemean-resbias1$popMean
resbias1$biasPR <- resbias1$sampPRmean-resbias1$popMean
apply(resbias1,2,mean)

res1 <- subset(gather(restrial1$res), key != "popMean")

p1 <- ggplot(res1) +
  geom_vline(xintercept = mean(pop1$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), bins = 100) +
  ggtitle("3 var Stratified Samp, Pop A") +
  xlim(50000, 100000) +
  ylim(0, 100)
p1



### balanced pop?
pop1Freq <- pop1 %>% summarise(realcount = n())
pop1Freq$count <- pop1Freq$realcount/N
chisq.test(pop1Freq$realcount, p = rep(1/length(pop1Freq$realcount), length(pop1Freq$realcount)))
# p value 2.2e-16 

### Does samp match pop?
chisq1 <- list()

for(i in 1:length(restrial1$allSamp)){
  restrial1$allSamp[[i]] <- restrial1$allSamp[[i]] %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
  samplecount <- restrial1$allSamp[[i]] %>% summarise(realcount = n())
  chisq1[i] <- chisq.test(samplecount$realcount, p = pop1Freq$count)$p.value
}

#chisq1
chisq1meanp <- mean(unlist(chisq1))
chisq1meanp   # 4.415979e-42




# Trial 2: Four variable Strat, Pop A ---------------------------------------
# 2: uses sample_n

# #age
# pop2 <- data.frame(I_age_old = rbinom(N, 1, 0.6), I_sex_F = 0, I_race_B = 0, I_ins_A = 0)
# #sex
# p <- expit(0 + 0.9 * pop2$I_age_old)
# pop2$I_sex_F <- rbinom(N,1,prob = p)
# #race
# p <- expit(1 + 0.9 * pop2$I_age_old * pop2$I_sex_F)
# pop2$I_race_B <- rbinom(N,1,prob = p)
# #Insurance
# p <- expit(0 + 1.2 * pop2$I_age_old * pop2$I_race_B * pop2$I_sex_F)
# pop2$I_ins_A <- rbinom(N,1,prob = p)

pop2 <- popA
pop2 <- pop2 %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
pop2Freq <- pop2 %>% summarise(count = n()/N)
pop2Freq

pop2$income <- 25000 + 
  40000 * pop2$I_age_old +
  25000 * pop2$I_sex_F * pop2$I_race_B + 
  30000 * pop2$I_sex_F * pop2$I_age_old +  
  30000 * pop2$I_age_old * pop2$I_race_B * pop2$I_ins_A + 
  20000 * pop2$I_age_old * pop2$I_sex_F * pop2$I_race_B * pop2$I_ins_A +
  rnorm(N,0,5000)

method2 <- function(pop){
  samp <- sample_n(pop, 400/16, replace = FALSE)
  return(samp)
}

restrial2 <- resfunc(pop2, method2)

resbias2 <- restrial2$res
resbias2$biasPS <- resbias2$sampPSmean-resbias2$popMean
resbias2$biasrake <- resbias2$samprakemean-resbias2$popMean
resbias2$biasPR <- resbias2$sampPRmean-resbias2$popMean
apply(resbias2,2,mean)

res2 <- subset(gather(restrial2$res), key != "popMean")
p2 <- ggplot(res2) +
  geom_vline(xintercept = mean(pop2$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  ggtitle("4 Var Stratified Samp, Pop A") +
  xlim(50000, 100000) + 
  ylim(0, 100)
p2

### balanced pop?
pop2Freq <- pop2 %>% summarise(realcount = n())
pop2Freq$count <- pop2Freq$realcount/N
pop2Freq

chisq.test(pop2Freq$realcount, p = rep(1/length(pop2Freq$count), length(pop2Freq$count)))
#x-squared = 98446, df = 15,  p-value < 2.2e-16

### Does samp match pop?
chisq2 <- list()

for(i in 1:length(restrial2$allSamp)){
  restrial2$allSamp[[i]] <- restrial2$allSamp[[i]] %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
  samplecount <- restrial2$allSamp[[i]] %>% summarise(realcount = n())
  chisq2[i] <- chisq.test(samplecount$realcount, p = pop2Freq$count)$p.value
}

chisq2
chisq2meanp <- mean(unlist(chisq2))
chisq2meanp   # 2.249692e-42

# Trial 3: Random Sample, Pop A -----------------------------------------------------------------
############### 3: Random sample
# pop3 <- data.frame(I_age_old = rbinom(N, 1, 0.6), I_sex_F = 0, I_race_B = 0, I_ins_A = 0)
# #sex
# p <- expit(0 + 0.9 * pop3$I_age_old)
# pop3$I_sex_F <- rbinom(N,1,prob = p)
# #race
# p <- expit(1 + 0.9 * pop3$I_age_old * pop3$I_sex_F)
# pop3$I_race_B <- rbinom(N,1,prob = p)
# #Insurance
# p <- expit(0 + 1.2 * pop3$I_age_old * pop3$I_race_B * pop3$I_sex_F)
# pop3$I_ins_A <- rbinom(N,1,prob = p)
pop3 <- popA
pop3 <- pop3 %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
pop3Freq <- pop3 %>% summarise(count = n()/N)
pop3Freq

pop3$income <- 25000 + 
  40000 * pop3$I_age_old +
  25000 * pop3$I_sex_F * pop3$I_race_B + 
  30000 * pop3$I_sex_F * pop3$I_age_old +  
  30000 * pop3$I_age_old * pop3$I_race_B * pop3$I_ins_A + 
  20000 * pop3$I_age_old * pop3$I_sex_F * pop3$I_race_B * pop3$I_ins_A +
  rnorm(N,0,5000)

method3 <- function(pop){
  pop <- ungroup(pop)
  samp <- sample_frac(pop, 400/N, replace= FALSE)
  return(samp)
}

restrial3 <- resfunc(pop3, method3)

resbias3 <- restrial3$res
resbias3$biasPS <- resbias3$sampPSmean-resbias3$popMean
resbias3$biasrake <- resbias3$samprakemean-resbias3$popMean
resbias3$biasPR <- resbias3$sampPRmean-resbias3$popMean
apply(resbias3,2,mean)


res3 <- subset(gather(restrial3$res), key != "popMean")
p3 <- ggplot(res3) +  
  geom_vline(xintercept = mean(pop3$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  ggtitle("Random Samp, Pop A") +
  xlim(50000, 100000)
p3

# pop3Freq <- pop3 %>% summarise(realcount = n())
# pop3Freq$count <- pop3Freq$realcount/N
# 
# ### balanced pop?
# chisq.test(pop3Freq$realcount, p = rep(1/length(pop3Freq$realcount), length(pop3Freq$realcount)))
# # p value 2.2e-16 nope!
# 
# ### Does samp match pop?
# chisq3 <- list()
# 
# for(i in 1:length(restrial3$allSamp)){
#   restrial3$allSamp[[i]] <- restrial3$allSamp[[i]] %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
#   samplecount <- restrial3$allSamp[[i]] %>% summarise(realcount = n())
#   chisq3[i] <- chisq.test(samplecount$realcount, p = pop3Freq$count)$p.value
# }
# 
# chisq3
# chisq3meanp <- mean(unlist(chisq3))
# chisq3meanp   # 0.5304324
# 

# Trial 4: Three Var Stratified *** w/ Pop B -----------------------------------------------------------------

#### 3 Var Stratified samp (with at least 1 obs from each group), w/ Population B

# pop4 <- data.frame(I_age_old = rbinom(N, 1, 0.7), I_sex_F = 0, I_race_B = 0, I_ins_A = 0)
# #sex
# p <- expit(1 + 0.9 * pop4$I_age_old)
# pop4$I_sex_F <- rbinom(N,1,prob = p)
# #race
# p <- expit(1 - 0.9 * pop4$I_age_old * pop4$I_sex_F)
# pop4$I_race_B <- rbinom(N,1,prob = p)
# #Insurance
# p <- expit(-1 - 1.2 * pop4$I_sex_F * pop4$I_race_B +
#              1.0 * pop4$I_age_old)
# pop4$I_ins_A <- rbinom(N,1,prob = p)
pop4 <- popB
pop4 <- pop4 %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
pop4Freq <- pop4 %>% summarise(count = n()/N)
pop4Freq

pop4$income <- 25000 + 
  40000 * pop4$I_age_old +
  25000 * pop4$I_sex_F * pop4$I_race_B + 
  30000 * pop4$I_sex_F * pop4$I_age_old +  
  30000 * pop4$I_age_old * pop4$I_race_B * pop4$I_ins_A + 
  20000 * pop4$I_age_old * pop4$I_sex_F * pop4$I_race_B * pop4$I_ins_A +
  rnorm(N,0,5000)
mean(pop4$income)

method4 <- function(pop){
  sampList <- list()
  s_size <- (400-16)/8
  ind <- sample(1:sum(pop$I_age_old==1 & pop$I_sex_F==1 & pop$I_race_B == 1), s_size, replace=FALSE)
  sampList[[1]] <- pop[pop$I_age_old==1 & pop$I_sex_F==1 & pop$I_race_B == 1,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==1 & pop$I_sex_F==1 & pop$I_race_B == 0), s_size, replace=FALSE)
  sampList[[2]] <- pop[pop$I_age_old==1 & pop$I_sex_F==1 & pop$I_race_B == 0,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==1 & pop$I_sex_F==0 & pop$I_race_B == 1), s_size, replace=FALSE)
  sampList[[3]] <- pop[pop$I_age_old==1 & pop$I_sex_F==0 & pop$I_race_B == 1,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==1 & pop$I_sex_F==0 & pop$I_race_B == 0), s_size, replace=FALSE)
  sampList[[4]] <- pop[pop$I_age_old==1 & pop$I_sex_F==0 & pop$I_race_B == 0,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==0 & pop$I_sex_F==1 & pop$I_race_B == 1), s_size, replace=FALSE)
  sampList[[5]] <- pop[pop$I_age_old==0 & pop$I_sex_F==1 & pop$I_race_B == 1,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==0 & pop$I_sex_F==1 & pop$I_race_B == 0), s_size, replace=FALSE)
  sampList[[6]] <- pop[pop$I_age_old==0 & pop$I_sex_F==1 & pop$I_race_B == 0,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==0 & pop$I_sex_F==0 & pop$I_race_B == 1), s_size, replace=FALSE)
  sampList[[7]] <- pop[pop$I_age_old==0 & pop$I_sex_F==0 & pop$I_race_B == 1,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==0 & pop$I_sex_F==0 & pop$I_race_B == 0), s_size, replace=FALSE)
  sampList[[8]] <- pop[pop$I_age_old==0 & pop$I_sex_F==0 & pop$I_race_B == 0,][ind,]
  
  # make sure at least 1 person from each group is sampled
  sampList[[9]] <- sample_n(pop, 1, replace = FALSE)
  samp <- do.call(rbind, sampList)
  
  return(samp)
}
restrial4 <- resfunc(pop4, method4)  

resbias4 <- restrial4$res
resbias4$biasPS <- resbias4$sampPSmean-resbias4$popMean
resbias4$biasrake <- resbias4$samprakemean-resbias4$popMean
resbias4$biasPR <- resbias4$sampPRmean-resbias4$popMean
apply(resbias4,2,mean)

res4 <- subset(gather(restrial4$res), key != "popMean")
p4 <- ggplot(res4) +
  geom_vline(xintercept = mean(pop4$income), color="orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  ggtitle("3 Var Stratified Samp, Pop B") +
  xlim(50000, 100000) +
  ylim(0, 100)
p4


# ### balanced pop?
# pop4Freq <- pop4 %>% summarise(realcount = n())
# pop4Freq$count <- pop4Freq$realcount/N
# #pop4Freq
# 
# chisq.test(pop4Freq$realcount, p = rep(1/length(pop4Freq$count), length(pop4Freq$count)))
# 
# ### Does samp match pop?
# chisq4 <- list()
# 
# for(i in 1:length(restrial4$allSamp)){
#   restrial4$allSamp[[i]] <- restrial4$allSamp[[i]] %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
#   samplecount <- restrial4$allSamp[[i]] %>% summarise(realcount = n())
#   chisq4[i] <- chisq.test(samplecount$realcount, p = pop4Freq$count)$p.value
# }
# 
# chisq4
# chisq4meanp <- mean(unlist(chisq4))
# chisq4meanp   # for t2: 2.249692e-42
# 
# pop4Freq <- pop4 %>% summarise(realcount = n())
# pop4Freq$count <- pop4Freq$realcount / N
# pop4Freq
# chisq.test(pop4Freq$realcount,p = rep(1/length(pop4Freq$count), length(pop4Freq$count)))
# #2.2 e -16



# Trial 5: Four Var Strat, Pop B-----------------------------------------------------------------
################ 5: completely stratified (sample_n) with a different unbalanced pop
# pop5 <- data.frame(I_age_old = rbinom(N, 1, 0.7), I_sex_F = 0, I_race_B = 0, I_ins_A = 0)
# #sex
# p <- expit(1 + 0.9 * pop5$I_age_old)
# pop5$I_sex_F <- rbinom(N,1,prob = p)
# #race
# p <- expit(1 - 0.9 * pop5$I_age_old * pop5$I_sex_F)
# pop5$I_race_B <- rbinom(N,1,prob = p)
# #Insurance
# p <- expit(-1 - 1.2 * pop5$I_sex_F * pop5$I_race_B +
#              1.0 * pop5$I_age_old)
# pop5$I_ins_A <- rbinom(N,1,prob = p)
pop5 <- popB
pop5 <- pop5 %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
pop5Freq <- pop5 %>% summarise(count = n()/N)
pop5Freq

pop5$income <- 25000 + 
  40000 * pop5$I_age_old +
  25000 * pop5$I_sex_F * pop5$I_race_B + 
  30000 * pop5$I_sex_F * pop5$I_age_old +  
  30000 * pop5$I_age_old * pop5$I_race_B * pop5$I_ins_A + 
  20000 * pop5$I_age_old * pop5$I_sex_F * pop5$I_race_B * pop5$I_ins_A +
  rnorm(N,0,5000)
mean(pop5$income)


method5 <- function(population){
  population <- population %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
  samp <- sample_n(population, 400/16, replace = FALSE)
  return(samp)
}
restrial5 <- resfunc(pop5, method5)  

resbias5 <- restrial5$res
resbias5$biasPS <- resbias5$sampPSmean-resbias5$popMean
resbias5$biasrake <- resbias5$samprakemean-resbias5$popMean
resbias5$biasPR <- resbias5$sampPRmean-resbias5$popMean
apply(resbias5,2,mean)

res5 <- subset(gather(restrial5$res), key != "popMean")
p5 <- ggplot(res5) +
  geom_vline(xintercept = mean(pop5$income), color="orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  ggtitle("4 Var Strat Samp, Pop B") +
  xlim(50000, 100000) +
  ylim(0, 100)
p5


# ### balanced pop?
# pop5Freq <- pop5 %>% summarise(realcount = n())
# pop5Freq$count <- pop5Freq$realcount/N
# #pop5Freq
# 
# chisq.test(pop5Freq$realcount, p = rep(1/length(pop5Freq$count), length(pop5Freq$count)))
# 
# ### Does samp match pop?
# chisq5 <- list()
# 
# for(i in 1:length(restrial5$allSamp)){
#   restrial5$allSamp[[i]] <- restrial5$allSamp[[i]] %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
#   samplecount <- restrial5$allSamp[[i]] %>% summarise(realcount = n())
#   chisq5[i] <- chisq.test(samplecount$realcount, p = pop5Freq$count)$p.value
# }
# 
# chisq5
# chisq5meanp <- mean(unlist(chisq5))
# chisq5meanp   # for t2: 2.259692e-52
# 
# pop5Freq <- pop5 %>% summarise(realcount = n())
# pop5Freq$count <- pop5Freq$realcount / N
# pop5Freq
# chisq.test(pop5Freq$realcount,p = rep(1/length(pop5Freq$count), length(pop5Freq$count)))
# #2.2 e -16



# Trial 6: Random Sample ***, Pop B -----------------------------------------------------------------
########## At least 1 obs from each group, the rest chosen via Random Sampling, Population B

pop6 <- popB

pop6$income <- 25000 + 
  40000 * pop6$I_age_old +
  25000 * pop6$I_sex_F * pop6$I_race_B + 
  30000 * pop6$I_sex_F * pop6$I_age_old +  
  30000 * pop6$I_age_old * pop6$I_race_B * pop6$I_ins_A + 
  20000 * pop6$I_age_old * pop6$I_sex_F * pop6$I_race_B * pop6$I_ins_A +
  rnorm(N,0,5000)
mean(pop6$income)

method6 <- function(pop){
  sampList <- list()
  s_size <- 400-16
  ind <- sample(1:N, s_size, replace=FALSE)
  sampList[[1]] <- pop[][ind,]
  sampList[[2]] <- sample_n(pop, 1, replace = FALSE)
  samp <- do.call(rbind, sampList)
  return(samp)
  }

restrial6 <- resfunc(pop6, method6)

resbias6 <- restrial6$res
resbias6$biasPS <- resbias6$sampPSmean-resbias6$popMean
resbias6$biasrake <- resbias6$samprakemean-resbias6$popMean
resbias6$biasPR <- resbias6$sampPRmean-resbias6$popMean
apply(resbias6,2,mean)

res6 <- subset(gather(restrial6$res), key != "popMean")
p6 <- ggplot(res6) +
  geom_vline(xintercept = mean(pop6$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  ggtitle("Random* Samp, Pop B") +
  xlim(50000, 100000)
p6

pop6Freq <- pop6 %>% summarise(realcount = n())
pop6Freq$count <- pop6Freq$realcount/N
pop6Freq



# Trial 7: Three Variable Stratified, Pop C-----------------------------------------------------------------
######### 7: Three Variable Stratified,Pop C
pop7 <- popC

pop7$income <- 25000 + 
  40000 * pop7$I_age_old +
  25000 * pop7$I_sex_F * pop7$I_race_B + 
  30000 * pop7$I_sex_F * pop7$I_age_old +  
  30000 * pop7$I_age_old * pop7$I_race_B * pop7$I_ins_A + 
  20000 * pop7$I_age_old * pop7$I_sex_F * pop7$I_race_B * pop7$I_ins_A +
  rnorm(N,0,5000)
mean(pop7$income)

method7 <- function(pop){
  sampList <- list()
  s_size <- (800-16)/8
  ind <- sample(1:sum(pop$I_age_old==1 & pop$I_sex_F==1 & pop$I_race_B == 1), s_size, replace=FALSE)
  sampList[[1]] <- pop[pop$I_age_old==1 & pop$I_sex_F==1 & pop$I_race_B == 1,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==1 & pop$I_sex_F==1 & pop$I_race_B == 0), s_size, replace=FALSE)
  sampList[[2]] <- pop[pop$I_age_old==1 & pop$I_sex_F==1 & pop$I_race_B == 0,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==1 & pop$I_sex_F==0 & pop$I_race_B == 1), s_size, replace=FALSE)
  sampList[[3]] <- pop[pop$I_age_old==1 & pop$I_sex_F==0 & pop$I_race_B == 1,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==1 & pop$I_sex_F==0 & pop$I_race_B == 0), s_size, replace=FALSE)
  sampList[[4]] <- pop[pop$I_age_old==1 & pop$I_sex_F==0 & pop$I_race_B == 0,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==0 & pop$I_sex_F==1 & pop$I_race_B == 1), s_size, replace=FALSE)
  sampList[[5]] <- pop[pop$I_age_old==0 & pop$I_sex_F==1 & pop$I_race_B == 1,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==0 & pop$I_sex_F==1 & pop$I_race_B == 0), s_size, replace=FALSE)
  sampList[[6]] <- pop[pop$I_age_old==0 & pop$I_sex_F==1 & pop$I_race_B == 0,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==0 & pop$I_sex_F==0 & pop$I_race_B == 1), s_size, replace=FALSE)
  sampList[[7]] <- pop[pop$I_age_old==0 & pop$I_sex_F==0 & pop$I_race_B == 1,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==0 & pop$I_sex_F==0 & pop$I_race_B == 0), s_size, replace=FALSE)
  sampList[[8]] <- pop[pop$I_age_old==0 & pop$I_sex_F==0 & pop$I_race_B == 0,][ind,]
  
  # make sure at least 1 person from each group is sampled
  sampList[[9]] <- sample_n(pop, 1, replace = FALSE)
  samp <- do.call(rbind, sampList)
  
  return(samp)
}
restrial7 <- resfunc(pop7, method7)  

resbias7 <- restrial7$res
resbias7$biasPS <- resbias7$sampPSmean-resbias7$popMean
resbias7$biasrake <- resbias7$samprakemean-resbias7$popMean
resbias7$biasPR <- resbias7$sampPRmean-resbias7$popMean
apply(resbias7,2,mean)

res7 <- subset(gather(restrial7$res), key != "popMean")
p7 <- ggplot(res7) +
  geom_vline(xintercept = mean(pop7$income), color="orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  ggtitle("3 Var Stratified Samp, Pop C") +
  xlim(50000, 100000) +
  ylim(0, 100)
p7


# ### balanced pop?
# pop7Freq <- pop7 %>% summarise(realcount = n())
# pop7Freq$count <- pop7Freq$realcount/N
# #pop7Freq
# 
# chisq.test(pop7Freq$realcount, p = rep(1/length(pop7Freq$count), length(pop7Freq$count)))
# 
# ### Does samp match pop?
# chisq7 <- list()
# 
# for(i in 1:length(restrial7$allSamp)){
#   restrial7$allSamp[[i]] <- restrial7$allSamp[[i]] %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
#   samplecount <- restrial7$allSamp[[i]] %>% summarise(realcount = n())
#   chisq7[i] <- chisq.test(samplecount$realcount, p = pop7Freq$count)$p.value
# }
# 
# chisq7
# chisq7meanp <- mean(unlist(chisq7))
# chisq7meanp  
# 
# pop7Freq <- pop7 %>% summarise(realcount = n())
# pop7Freq$count <- pop7Freq$realcount / N
# pop7Freq
# chisq.test(pop7Freq$realcount,p = rep(1/length(pop7Freq$count), length(pop7Freq$count)))





# Trial 8: Four variable Strat, Pop C-----------------------------------------------------------------

pop8 <- popC
pop8$income <- 25000 + 
  40000 * pop8$I_age_old +
  25000 * pop8$I_sex_F + 
  30000 * pop8$I_sex_F * pop8$I_race_B +  
  30000 * pop8$I_age_old * pop8$I_race_B * pop8$I_ins_A + 
  20000 * pop8$I_age_old * pop8$I_sex_F * pop8$I_race_B * pop8$I_ins_A +
  rnorm(N,0,5000)
mean(pop8$income)


method8 <- function(pop){
  pop <- pop %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
  samp <- sample_n(pop, 400/16, replace = FALSE)
  return(samp)
}

restrial8 <- resfunc(pop8, method8)  

resbias8 <- restrial8$res
resbias8$biasPS <- resbias8$sampPSmean-resbias8$popMean
resbias8$biasrake <- resbias8$samprakemean-resbias8$popMean
resbias8$biasPR <- resbias8$sampPRmean-resbias8$popMean
apply(resbias8,2,mean)

res8 <- subset(gather(restrial8$res), key != "popMean")
p8 <- ggplot(res8) +
  geom_vline(xintercept = mean(pop8$income), color="orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  ggtitle("4 Var Strat Samp, Pop C") +
  xlim(50000, 100000) +
  ylim(0, 100)
p8


# Trial 9: Random***, Pop C -----------------------------------------------------------------

pop9 <- popC

pop9$income <- 25000 + 
  40000 * pop9$I_age_old +
  25000 * pop9$I_sex_F * pop9$I_race_B + 
  30000 * pop9$I_sex_F * pop9$I_age_old +  
  30000 * pop9$I_age_old * pop9$I_race_B * pop9$I_ins_A + 
  20000 * pop9$I_age_old * pop9$I_sex_F * pop9$I_race_B * pop9$I_ins_A +
  rnorm(N,0,5000)
mean(pop9$income)

method9 <- function(pop){
  sampList <- list()
  s_size <- 400-16
  ind <- sample(1:N, s_size, replace=FALSE)
  sampList[[1]] <- pop[][ind,]
  sampList[[2]] <- sample_n(pop, 1, replace = FALSE)
  samp <- do.call(rbind, sampList)
  return(samp)
}

restrial9 <- resfunc(pop9, method9)

resbias9 <- restrial9$res
resbias9$biasPS <- resbias9$sampPSmean-resbias9$popMean
resbias9$biasrake <- resbias9$samprakemean-resbias9$popMean
resbias9$biasPR <- resbias9$sampPRmean-resbias9$popMean
apply(resbias9,2,mean)

res9 <- subset(gather(restrial9$res), key != "popMean")
p9 <- ggplot(res9) +
  geom_vline(xintercept = mean(pop9$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  ggtitle("Random* Samp, Pop C") +
  xlim(50000, 100000)
p9


# Trial 10: Three Var Strat, Pop D ----------------------------------------------------------------
pop10 <- popD

pop10$income <- 25000 + 
  40000 * pop10$I_age_old +
  25000 * pop10$I_sex_F * pop10$I_race_B + 
  30000 * pop10$I_sex_F * pop10$I_age_old +  
  30000 * pop10$I_age_old * pop10$I_race_B * pop10$I_ins_A + 
  20000 * pop10$I_age_old * pop10$I_sex_F * pop10$I_race_B * pop10$I_ins_A +
  rnorm(N,0,5000)
mean(pop10$income)

method10 <- function(pop){
  sampList <- list()
  s_size <- (400-16)/8
  ind <- sample(1:sum(pop$I_age_old==1 & pop$I_sex_F==1 & pop$I_race_B == 1), s_size, replace=FALSE)
  sampList[[1]] <- pop[pop$I_age_old==1 & pop$I_sex_F==1 & pop$I_race_B == 1,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==1 & pop$I_sex_F==1 & pop$I_race_B == 0), s_size, replace=FALSE)
  sampList[[2]] <- pop[pop$I_age_old==1 & pop$I_sex_F==1 & pop$I_race_B == 0,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==1 & pop$I_sex_F==0 & pop$I_race_B == 1), s_size, replace=FALSE)
  sampList[[3]] <- pop[pop$I_age_old==1 & pop$I_sex_F==0 & pop$I_race_B == 1,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==1 & pop$I_sex_F==0 & pop$I_race_B == 0), s_size, replace=FALSE)
  sampList[[4]] <- pop[pop$I_age_old==1 & pop$I_sex_F==0 & pop$I_race_B == 0,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==0 & pop$I_sex_F==1 & pop$I_race_B == 1), s_size, replace=FALSE)
  sampList[[5]] <- pop[pop$I_age_old==0 & pop$I_sex_F==1 & pop$I_race_B == 1,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==0 & pop$I_sex_F==1 & pop$I_race_B == 0), s_size, replace=FALSE)
  sampList[[6]] <- pop[pop$I_age_old==0 & pop$I_sex_F==1 & pop$I_race_B == 0,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==0 & pop$I_sex_F==0 & pop$I_race_B == 1), s_size, replace=FALSE)
  sampList[[7]] <- pop[pop$I_age_old==0 & pop$I_sex_F==0 & pop$I_race_B == 1,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==0 & pop$I_sex_F==0 & pop$I_race_B == 0), s_size, replace=FALSE)
  sampList[[8]] <- pop[pop$I_age_old==0 & pop$I_sex_F==0 & pop$I_race_B == 0,][ind,]
  
  # make sure at least 1 person from each group is sampled
  sampList[[9]] <- sample_n(pop, 1, replace = FALSE)
  samp <- do.call(rbind, sampList)
  
  return(samp)
}
restrial10 <- resfunc(pop10, method10)  

resbias10 <- restrial10$res
resbias10$biasPS <- resbias10$sampPSmean-resbias10$popMean
resbias10$biasrake <- resbias10$samprakemean-resbias10$popMean
resbias10$biasPR <- resbias10$sampPRmean-resbias10$popMean
apply(resbias10,2,mean)

res10 <- subset(gather(restrial10$res), key != "popMean")
p10 <- ggplot(res10) +
  geom_vline(xintercept = mean(pop10$income), color="orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  ggtitle("3 Var Stratified Samp, Pop D") +
  xlim(50000, 100000) +
  ylim(0, 100)
p10

# Trial 11: Pop D ----------------------------------------------------------------
pop11 <- popD

pop11$income <- 25000 + 
  40000 * pop11$I_age_old +
  25000 * pop11$I_sex_F * pop11$I_race_B + 
  30000 * pop11$I_sex_F * pop11$I_age_old +  
  30000 * pop11$I_age_old * pop11$I_race_B * pop11$I_ins_A + 
  20000 * pop11$I_age_old * pop11$I_sex_F * pop11$I_race_B * pop11$I_ins_A +
  rnorm(N,0,5000)
mean(pop11$income)

method11 <- function(pop){
  pop <- pop %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
  samp <- sample_n(pop, 400/16, replace = FALSE)
  return(samp)
}

restrial11 <- resfunc(pop11, method11)  

resbias11 <- restrial11$res
resbias11$biasPS <- resbias11$sampPSmean-resbias11$popMean
resbias11$biasrake <- resbias11$samprakemean-resbias11$popMean
resbias11$biasPR <- resbias11$sampPRmean-resbias11$popMean
apply(resbias11,2,mean)

res11 <- subset(gather(restrial11$res), key != "popMean")
p11 <- ggplot(res11) +
  geom_vline(xintercept = mean(pop11$income), color="orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  ggtitle("4 Var Strat Samp, Pop D") +
  xlim(50000, 100000) +
  ylim(0, 100)
p11



# Trial 12: Random**, Pop D ----------------------------------------------------------------
pop12 <- popD

pop12$income <- 25000 + 
  40000 * pop12$I_age_old +
  25000 * pop12$I_sex_F * pop12$I_race_B + 
  30000 * pop12$I_sex_F * pop12$I_age_old +  
  30000 * pop12$I_age_old * pop12$I_race_B * pop12$I_ins_A + 
  20000 * pop12$I_age_old * pop12$I_sex_F * pop12$I_race_B * pop12$I_ins_A +
  rnorm(N,0,5000)
mean(pop12$income)


method12 <- function(pop){
  sampList <- list()
  s_size <- 400-16
  ind <- sample(1:N, s_size, replace=FALSE)
  sampList[[1]] <- pop[][ind,]
  sampList[[2]] <- sample_n(pop, 1, replace = FALSE)
  samp <- do.call(rbind, sampList)
  return(samp)
}

restrial12 <- resfunc(pop12, method12)

resbias12 <- restrial12$res
resbias12$biasPS <- resbias12$sampPSmean-resbias12$popMean
resbias12$biasrake <- resbias12$samprakemean-resbias12$popMean
resbias12$biasPR <- resbias12$sampPRmean-resbias12$popMean
apply(resbias12,2,mean)

res12 <- subset(gather(restrial12$res), key != "popMean")
p12 <- ggplot(res12) +
  geom_vline(xintercept = mean(pop12$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  ggtitle("Random* Samp, Pop D") +
  xlim(50000, 100000)
p12


# Trial 13: Income Samp (2 lvls), 4 Var Strat, Pop A ----------------------------------------------------------------
pop13 <- popA

pop13$income <- 25000 + 
  40000 * pop13$I_age_old +
  25000 * pop13$I_sex_F * pop13$I_race_B + 
  30000 * pop13$I_sex_F * pop13$I_age_old +  
  30000 * pop13$I_age_old * pop13$I_race_B * pop13$I_ins_A + 
  20000 * pop13$I_age_old * pop13$I_sex_F * pop13$I_race_B * pop13$I_ins_A +
  rnorm(N,0,5000)
mean(pop13$income)


method13 <- function(pop){
  low_income <- subset(pop, income < 70000)
  high_income <- subset(pop, income >= 70000)
  low_samp <- sample_frac(low_income, (300-16)/length(low_income$income), replace = FALSE)
  high_samp <- sample_frac(high_income, 100/length(high_income$income), replace = FALSE)
  sampcheck <- sample_n(pop, 1, replace = FALSE)
  samp <- rbind(low_samp, high_samp, sampcheck)
  #sampFreqtest <- summarise(samp, count=n()/length(samp$income))
  return(samp)
}

restrial13 <- resfunc(pop13, method13)

resbias13 <- restrial13$res
resbias13$biasPS <- resbias13$sampPSmean-resbias13$popMean
resbias13$biasrake <- resbias13$samprakemean-resbias13$popMean
resbias13$biasPR <- resbias13$sampPRmean-resbias13$popMean
apply(resbias13,2,mean)

res13 <- subset(gather(restrial13$res), key != "popMean")
p13 <- ggplot(res13) +
  geom_vline(xintercept = mean(pop13$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  ggtitle("Income (2 lvls) 4 Var Stratified Samp, Pop A") +
  xlim(50000, 100000)
p13


# Trial 14: Income Biased 3 Var Strat, Pop A ----------------------------------------------------------------
## samples more lower incomes
pop14 <- popA

pop14$income <- 25000 + 
  40000 * pop14$I_age_old +
  25000 * pop14$I_sex_F * pop14$I_race_B + 
  30000 * pop14$I_sex_F * pop14$I_age_old +  
  30000 * pop14$I_age_old * pop14$I_race_B * pop14$I_ins_A + 
  20000 * pop14$I_age_old * pop14$I_sex_F * pop14$I_race_B * pop14$I_ins_A +
  rnorm(N,0,5000)
mean(pop14$income)


method14 <- function(pop){
  pop <- pop %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
  sampcheck <- sample_n(pop, 1, replace = FALSE, weight = income)
  pop <- pop %>% group_by(I_age_old, I_sex_F, I_race_B)
  samp <- bind_rows(sampcheck, sample_n(pop, (400-16)/8, replace = FALSE, weight = 1/income))
  samp <- samp %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
  sampFreqtest <- summarise(samp, count=n()/length(samp$income))
  return(samp)
}
method14(pop14)

restrial14 <- resfunc(pop14, method14)

resbias14 <- restrial14$res
resbias14$biasPS <- resbias14$sampPSmean-resbias14$popMean
resbias14$biasrake <- resbias14$samprakemean-resbias14$popMean
resbias14$biasPR <- resbias14$sampPRmean-resbias14$popMean
apply(resbias14,2,mean)

res14 <- subset(gather(restrial14$res), key != "popMean")
p14 <- ggplot(res14) +
  geom_vline(xintercept = mean(pop14$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  ggtitle("Income Biased 3 Var Strat, Pop A") +
  xlim(50000, 100000)
p14


# Trial 15: Income & Insurance Biased Sampling, Pop A ----------------------------------------------------------------
pop15 <- popA

pop15$income <- 25000 + 
  40000 * pop15$I_age_old +
  25000 * pop15$I_sex_F * pop15$I_race_B + 
  30000 * pop15$I_sex_F * pop15$I_age_old +  
  30000 * pop15$I_age_old * pop15$I_race_B * pop15$I_ins_A + 
  20000 * pop15$I_age_old * pop15$I_sex_F * pop15$I_race_B * pop15$I_ins_A +
  rnorm(N,0,5000)
mean(pop15$income)

method15 <- function(pop){
  pop <- pop %>% mutate(i_weight = ifelse(income < 50000, income/max(income) * 10 + 2 * I_ins_A, income/max(income) + 2* I_ins_A))
  pop <- pop %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
  sampcheck <- sample_n(pop, 1, replace = FALSE, weight = i_weight)
  pop <- ungroup(pop)
  samp <- sample_n(pop, (400-16), replace = FALSE, weight = i_weight)
  samp <- bind_rows(sampcheck, samp)
  sampFreqtest <- summarise(samp, count=n()/length(samp$income))
  return(samp)
}

restrial15 <- resfunc(pop15, method15)

resbias15 <- restrial15$res
resbias15$biasPS <- resbias15$sampPSmean-resbias15$popMean
resbias15$biasrake <- resbias15$samprakemean-resbias15$popMean
resbias15$biasPR <- resbias15$sampPRmean-resbias15$popMean
apply(resbias15,2,mean)

res15 <- subset(gather(restrial15$res), key != "popMean")
p15 <- ggplot(res15) +
  geom_vline(xintercept = mean(pop15$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  ggtitle("Income & Ins Samp, Pop A") +
  xlim(50000, 100000)
p15


# Trial 16: 4 Var Strat, Pop A, Ins X ----------------------------------
pop16 <- popA

pop16$income <- 25000 + 
  20000 * pop16$I_age_old +
  25000 * pop16$I_sex_F +
  30000 * pop16$I_sex_F * pop16$I_age_old +  
  30000 * pop16$I_age_old * pop16$I_ins_A + 
  40000 * pop16$I_age_old * pop16$I_sex_F * pop16$I_race_B * pop16$I_ins_A +
  rnorm(N,0,5000)
mean(pop16$income)


method16 <- function(pop){
  pop <- pop %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
  samp <- sample_n(pop, 400/16, replace = FALSE)
  return(samp)
}

restrial16 <- resfunc(pop16, method16)

resbias16 <- restrial16$res
resbias16$biasPS <- resbias16$sampPSmean-resbias16$popMean
resbias16$biasrake <- resbias16$samprakemean-resbias16$popMean
resbias16$biasPR <- resbias16$sampPRmean-resbias16$popMean
apply(resbias16,2,mean)

res16 <- subset(gather(restrial16$res), key != "popMean")
p16 <- ggplot(res16) +
  geom_vline(xintercept = mean(pop16$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  ggtitle("Income X, Pop A") +
  xlim(50000, 110000)
p16


# Trial 17: 4 Var Strat, Pop A, Ins Y ----------------------------------
pop17 <- popA

pop17$income <- 25000 + 
  20000 * pop17$I_age_old +
  25000 * pop17$I_sex_F +
  15000 * pop17$I_ins_A +
  35000 * pop17$I_sex_F * pop17$I_age_old +  
  10000 * pop17$I_age_old * pop17$I_ins_A + 
  10000 * pop17$I_age_old * pop17$I_sex_F * pop17$I_race_B * pop17$I_ins_A +
  rnorm(N,0,5000)
mean(pop17$income)


method17 <- function(pop){
  pop <- pop %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
  samp <- sample_n(pop, 400/16, replace = FALSE)
  return(samp)
}

restrial17 <- resfunc(pop17, method17)

resbias17 <- restrial17$res
resbias17$biasPS <- resbias17$sampPSmean-resbias17$popMean
resbias17$biasrake <- resbias17$samprakemean-resbias17$popMean
resbias17$biasPR <- resbias17$sampPRmean-resbias17$popMean
apply(resbias17,2,mean)

res17 <- subset(gather(restrial17$res), key != "popMean")
p17 <- ggplot(res17) +
  geom_vline(xintercept = mean(pop17$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  ggtitle("Income Y, Pop A") +
  xlim(50000, 110000)
p17


### bias population, stratified AND biased (income?) sampling
### income w/ multiple levels or base sampling liklihood based on income 

allplots <- list(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16)
save(allplots, file="/Users/joylee/Research Project/ConditionalRaking/CRplots.rda")


library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, nrow=4)
grid.arrange(p1, p2, p3, p13, p14, p15, nrow=2)
grid.arrange(p1, p4, p7, p10, nrow=2)
grid.arrange(p2, p5, p8, p11, nrow=2)
grid.arrange(p3, p5, p9, p12, nrow=2)
