######## x1x2 x2x3 x3x4

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


# function for sample stats
# inputs   pop = population df, sampmethod = sampling function
# returns  list of res=mean results,  results=raw results, allSamp = dataframe of all the samples

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
    # know sex & race, race & age, age & ins
    samp$caseid <- 1:nrow(samp)
    samp$I_race_age <- as.factor(samp$I_race_age)
    
    trueraceage <- wpct(pop$I_race_age)
    targets <- list(trueraceage)
    names(targets) <- c("I_race_age")
    
    # PS on race & age
    
    anes <- anesrake(targets, samp, caseid= samp$caseid, cap= 20, choosemethod = "total")
    samp$prweight <- anes$weightvec
    
    # Condition on race, PS on sex
    for(j in 0:1){
      subpop <- subset(pop, I_race_B==j)
      truesexsub <- wpct(subpop$I_sex_F)
      subtargets <- list(truesexsub)
      names(subtargets) <- c("I_sex_F")
      
      subsamp<- subset(samp, I_race_B==j)
      
      subsamp$caseid <-1:nrow(subsamp)
      subsamp$I_sex_F <- as.factor(subsamp$I_sex_F)
      
      anes <- anesrake(subtargets, subsamp, caseid= subsamp$caseid, cap=20, choosemethod = "total", weightvec=subsamp$prweight,center.baseweights = FALSE)
      
      samp$prweight[samp$I_race_B==j] <- anes$weightvec
      
    }
    
    # Condition on age, PS on insurance
    for(j in 0:1){
      subpop <- subset(pop, I_age_old==j)
      trueinssub <- wpct(subpop$I_ins_A)
      subtargets <- list(trueinssub)
      names(subtargets) <- c("I_ins_A")
      
      subsamp<- subset(samp, I_age_old==j)
      
      
      subsamp$caseid <-1:nrow(subsamp)
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
  
  # res = means df, results = means list, allSamp = df of all the samples
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

popA <- popA %>% 
  mutate(I_race_age = ifelse(I_race_B ==0 & I_age_old ==0, 1,
                             ifelse(I_race_B ==0 & I_age_old == 1, 2,
                                    ifelse(I_race_B & I_age_old ==0, 3, 4))))

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
#insurance
p <- expit(0 + 1.2 * popB$I_age_old * popB$I_race_B * popB$I_sex_F)
popB$I_ins_A <- rbinom(N,1,prob = p)
#race_age
popB <- popB %>% 
  mutate(I_race_age = ifelse(I_race_B ==0 & I_age_old ==0, 1,
                             ifelse(I_race_B ==0 & I_age_old == 1, 2,
                                    ifelse(I_race_B & I_age_old ==0, 3, 4))))

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
p <- expit(0 + 1.2 * popC$I_age_old + popC$I_sex_F + 1.8 * popC$I_race_B)
popC$I_ins_A <- rbinom(N,1,prob = p)

popC <- popC %>% mutate(I_race_age = ifelse(I_race_B ==0 & I_age_old ==0, 1,
                                            ifelse(I_race_B ==0 & I_age_old == 1, 2,
                                                   ifelse(I_race_B & I_age_old ==0, 3, 4))))
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
#Make race&age variable
popD <- popB %>% 
  mutate(I_race_age = ifelse(I_race_B ==0 & I_age_old ==0, 1,
                             ifelse(I_race_B ==0 & I_age_old == 1, 2,
                                    ifelse(I_race_B & I_age_old ==0, 3, 4))))

popD <- popD %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
popDFreq <- popD %>% summarise(count = n()/N)
popDFreq


# Trial 1: Three Variable Stratified, Pop A-----------------------------------------------------------------

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
resbias1$biasSamp <- resbias1$sampMean-resbias1$popMean
resbias1$biasSampPercent <- 100 * resbias1$biasSamp / resbias1$popMean
resbias1$biasPS <- resbias1$sampPSmean-resbias1$popMean
resbias1$biasPSPercent <- 100 * resbias1$biasPS / resbias1$popMean
resbias1$biasrake <- resbias1$samprakemean-resbias1$popMean
resbias1$biasrakePercent <- 100 * resbias1$biasrake / resbias1$popMean
resbias1$biasPR <- resbias1$sampPRmean-resbias1$popMean
resbias1$biasPRPercent <- 100 * resbias1$biasPR / resbias1$popMean
bias1<- apply(resbias1,2,mean)

resMSE1 <- resbias1[,c("biasSamp", "biasPS", "biasrake", "biasPR")]
resMSE1$samp <- resMSE1$biasSamp^2
resMSE1$PS <- resMSE1$biasPS^2
resMSE1$rake <- resMSE1$biasrake^2
resMSE1$PR <- resMSE1$biasPR^2
MSE1 <- apply(resMSE1, 2, mean)

resCI1 <- resbias1[,1:5]
resCI1$sampL <- resCI1$sampMean - 1.96 * (sd(pop1$income)/sqrt(400))
resCI1$sampU <- resCI1$sampMean + 1.96 * (sd(pop1$income)/sqrt(400))
resCI1$samplength <- 2*1.96*(sd(pop1$income)/sqrt(400))
resCI1 <- resCI1 %>% mutate(sampcoverage = ifelse(popMean > sampL & popMean < sampU, 1, 0)) 

dat1 <- data.frame(trial=numeric(), method=character(), bias=numeric(), percentBias=numeric(), MSE=numeric(), ci_length=numeric(), ci_coverage=numeric(),
                   stringsAsFactors=FALSE)
dat1[1,] <- list(1, "unadjusted", bias1[6], bias1[7], MSE1["samp"], mean(resCI1$samplength), sum(resCI1$sampcoverage)/length(resCI1$sampcoverage))
dat1[2,] <- list(1, "PS", bias1[8], bias1[9], MSE1["PS"], NA, NA)
dat1[3,] <- list(1, "Rake", bias1[10], bias1[11], MSE1["rake"], NA, NA)
dat1[4,] <- list(1, "PR", bias1[12], bias1[13], MSE1["PR"], NA, NA)
dat1

res1 <- subset(gather(restrial1$res), key != "popMean")

hist1 <- ggplot(res1) +
  geom_vline(xintercept = mean(pop1$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), bins = 100) +
  #ggtitle("Fig. 1: Mean Income for samples stratified on 3 variables") +
  xlim(50000, 100000) +
  ylim(0, 75)
hist1

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
####### 2: uses sample_n

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
resbias2$biasSamp <- resbias2$sampMean-resbias2$popMean
resbias2$biasSampPercent <- 100 * resbias2$biasSamp / resbias2$popMean
resbias2$biasPS <- resbias2$sampPSmean-resbias2$popMean
resbias2$biasPSPercent <- 100 * resbias2$biasPS / resbias2$popMean
resbias2$biasrake <- resbias2$samprakemean-resbias2$popMean
resbias2$biasrakePercent <- 100 * resbias2$biasrake / resbias2$popMean
resbias2$biasPR <- resbias2$sampPRmean-resbias2$popMean
resbias2$biasPRPercent <- 100 * resbias2$biasPR / resbias2$popMean
bias2<- apply(resbias2,2,mean)

resMSE2 <- resbias2[,c("biasSamp", "biasPS", "biasrake", "biasPR")]
resMSE2$samp <- resMSE2$biasSamp^2
resMSE2$PS <- resMSE2$biasPS^2
resMSE2$rake <- resMSE2$biasrake^2
resMSE2$PR <- resMSE2$biasPR^2
MSE2 <- apply(resMSE2, 2, mean)

resCI2 <- resbias2[,1:5]
resCI2$sampL <- resCI2$sampMean - 1.96 * (sd(pop2$income)/sqrt(400))
resCI2$sampU <- resCI2$sampMean + 1.96 * (sd(pop2$income)/sqrt(400))
resCI2$samplength <- 2*1.96*(sd(pop2$income)/sqrt(400))
resCI2 <- resCI2 %>% mutate(sampcoverage = ifelse(popMean > sampL & popMean < sampU, 1, 0)) 

dat2 <- data.frame(trial=numeric(), method=character(), bias=numeric(), percentBias=numeric(), MSE=numeric(), ci_length=numeric(), ci_coverage=numeric(),
                   stringsAsFactors=FALSE)
dat2[1,] <- list(2, "unadjusted", bias2[6], bias2[7], MSE2["samp"], mean(resCI2$samplength), sum(resCI2$sampcoverage)/length(resCI2$sampcoverage))
dat2[2,] <- list(2, "PS", bias2[8], bias2[9], MSE2["PS"], NA, NA)
dat2[3,] <- list(2, "Rake", bias2[10], bias2[11], MSE2["rake"], NA, NA)
dat2[4,] <- list(2, "PR", bias2[12], bias2[13], MSE2["PR"], NA, NA)
dat2


res2 <- subset(gather(restrial2$res), key != "popMean")
hist2 <- ggplot(res2) +
  geom_vline(xintercept = mean(pop2$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  #ggtitle("Fig. 2: Mean income for samples stratified on 4 variables") +
  xlim(50000, 100000) + 
  ylim(0, 75)
hist2

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
resbias3$biasSamp <- resbias3$sampMean-resbias3$popMean
resbias3$biasSampPercent <- 100 * resbias3$biasSamp / resbias3$popMean
resbias3$biasPS <- resbias3$sampPSmean-resbias3$popMean
resbias3$biasPSPercent <- 100 * resbias3$biasPS / resbias3$popMean
resbias3$biasrake <- resbias3$samprakemean-resbias3$popMean
resbias3$biasrakePercent <- 100 * resbias3$biasrake / resbias3$popMean
resbias3$biasPR <- resbias3$sampPRmean-resbias3$popMean
resbias3$biasPRPercent <- 100 * resbias3$biasPR / resbias3$popMean
bias3<- apply(resbias3,2,mean)

resMSE3 <- resbias3[,c("biasSamp", "biasPS", "biasrake", "biasPR")]
resMSE3$samp <- resMSE3$biasSamp^2
resMSE3$PS <- resMSE3$biasPS^2
resMSE3$rake <- resMSE3$biasrake^2
resMSE3$PR <- resMSE3$biasPR^2
MSE3 <- apply(resMSE3, 2, mean)

resCI3 <- resbias3[,1:5]
resCI3$sampL <- resCI3$sampMean - 1.96 * (sd(pop3$income)/sqrt(400))
resCI3$sampU <- resCI3$sampMean + 1.96 * (sd(pop3$income)/sqrt(400))
resCI3$samplength <- 2*1.96*(sd(pop3$income)/sqrt(400))
resCI3 <- resCI3 %>% mutate(sampcoverage = ifelse(popMean > sampL & popMean < sampU, 1, 0)) 

dat3 <- data.frame(trial=numeric(), method=character(), bias=numeric(), percentBias=numeric(), MSE=numeric(), ci_length=numeric(), ci_coverage=numeric(),
                   stringsAsFactors=FALSE)
dat3[1,] <- list(3, "unadjusted", bias3[6], bias3[7], MSE3["samp"], mean(resCI3$samplength), sum(resCI3$sampcoverage)/length(resCI3$sampcoverage))
dat3[2,] <- list(3, "PS", bias3[8], bias3[9], MSE3["PS"], NA, NA)
dat3[3,] <- list(3, "Rake", bias3[10], bias3[11], MSE3["rake"], NA, NA)
dat3[4,] <- list(3, "PR", bias3[12], bias3[13], MSE3["PR"], NA, NA)
dat3


res3 <- subset(gather(restrial3$res), key != "popMean")
hist3 <- ggplot(res3) +  
  geom_vline(xintercept = mean(pop3$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  #ggtitle("Fig. 3: Mean income for samples wiht random sampling") +
  xlim(50000, 100000) +
  ylim(0, 75)
hist3

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
resbias4$biasSamp <- resbias4$sampMean-resbias4$popMean
resbias4$biasSampPercent <- 100 * resbias4$biasSamp / resbias4$popMean
resbias4$biasPS <- resbias4$sampPSmean-resbias4$popMean
resbias4$biasPSPercent <- 100 * resbias4$biasPS / resbias4$popMean
resbias4$biasrake <- resbias4$samprakemean-resbias4$popMean
resbias4$biasrakePercent <- 100 * resbias4$biasrake / resbias4$popMean
resbias4$biasPR <- resbias4$sampPRmean-resbias4$popMean
resbias4$biasPRPercent <- 100 * resbias4$biasPR / resbias4$popMean
bias4<- apply(resbias4,2,mean)

resMSE4 <- resbias4[,c("biasSamp", "biasPS", "biasrake", "biasPR")]
resMSE4$samp <- resMSE4$biasSamp^2
resMSE4$PS <- resMSE4$biasPS^2
resMSE4$rake <- resMSE4$biasrake^2
resMSE4$PR <- resMSE4$biasPR^2
MSE4 <- apply(resMSE4, 2, mean)

resCI4 <- resbias4[,1:5]
resCI4$sampL <- resCI4$sampMean - 1.96 * (sd(pop4$income)/sqrt(400))
resCI4$sampU <- resCI4$sampMean + 1.96 * (sd(pop4$income)/sqrt(400))
resCI4$samplength <- 2*1.96*(sd(pop4$income)/sqrt(400))
resCI4 <- resCI4 %>% mutate(sampcoverage = ifelse(popMean > sampL & popMean < sampU, 1, 0)) 

dat4 <- data.frame(trial=numeric(), method=character(), bias=numeric(), percentBias=numeric(), MSE=numeric(), ci_length=numeric(), ci_coverage=numeric(),
                   stringsAsFactors=FALSE)
dat4[1,] <- list(4, "unadjusted", bias4[6], bias4[7], MSE4["samp"], mean(resCI4$samplength), sum(resCI4$sampcoverage)/length(resCI4$sampcoverage))
dat4[2,] <- list(4, "PS", bias4[8], bias4[9], MSE4["PS"], NA, NA)
dat4[3,] <- list(4, "Rake", bias4[10], bias4[11], MSE4["rake"], NA, NA)
dat4[4,] <- list(4, "PR", bias4[12], bias4[13], MSE4["PR"], NA, NA)
dat4

res4 <- subset(gather(restrial4$res), key != "popMean")
hist4 <- ggplot(res4) +
  geom_vline(xintercept = mean(pop4$income), color="orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  #ggtitle("Fig. 4: Mean income for samples stratified on 3 variables") +
  xlim(50000, 100000) +
  ylim(0, 75)
hist4


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
resbias5$biasSamp <- resbias5$sampMean-resbias5$popMean
resbias5$biasSampPercent <- 100 * resbias5$biasSamp / resbias5$popMean
resbias5$biasPS <- resbias5$sampPSmean-resbias5$popMean
resbias5$biasPSPercent <- 100 * resbias5$biasPS / resbias5$popMean
resbias5$biasrake <- resbias5$samprakemean-resbias5$popMean
resbias5$biasrakePercent <- 100 * resbias5$biasrake / resbias5$popMean
resbias5$biasPR <- resbias5$sampPRmean-resbias5$popMean
resbias5$biasPRPercent <- 100 * resbias5$biasPR / resbias5$popMean
bias5<- apply(resbias5,2,mean)

resMSE5 <- resbias5[,c("biasSamp", "biasPS", "biasrake", "biasPR")]
resMSE5$samp <- resMSE5$biasSamp^2
resMSE5$PS <- resMSE5$biasPS^2
resMSE5$rake <- resMSE5$biasrake^2
resMSE5$PR <- resMSE5$biasPR^2
MSE5 <- apply(resMSE5, 2, mean)

resCI5 <- resbias5[,1:5]
resCI5$sampL <- resCI5$sampMean - 1.96 * (sd(pop5$income)/sqrt(400))
resCI5$sampU <- resCI5$sampMean + 1.96 * (sd(pop5$income)/sqrt(400))
resCI5$samplength <- 2*1.96*(sd(pop5$income)/sqrt(400))
resCI5 <- resCI5 %>% mutate(sampcoverage = ifelse(popMean > sampL & popMean < sampU, 1, 0)) 

dat5 <- data.frame(trial=numeric(), method=character(), bias=numeric(), percentBias=numeric(), MSE=numeric(), ci_length=numeric(), ci_coverage=numeric(),
                   stringsAsFactors=FALSE)
dat5[1,] <- list(5, "unadjusted", bias5[6], bias5[7], MSE5["samp"], mean(resCI5$samplength), sum(resCI5$sampcoverage)/length(resCI5$sampcoverage))
dat5[2,] <- list(5, "PS", bias5[8], bias5[9], MSE5["PS"], NA, NA)
dat5[3,] <- list(5, "Rake", bias5[10], bias5[11], MSE5["rake"], NA, NA)
dat5[4,] <- list(5, "PR", bias5[12], bias5[13], MSE5["PR"], NA, NA)
dat5

res5 <- subset(gather(restrial5$res), key != "popMean")
hist5 <- ggplot(res5) +
  geom_vline(xintercept = mean(pop5$income), color="orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  #ggtitle("Fig. 5: Mean income for samples stratified on 4 variables") +
  xlim(50000, 100000) +
  ylim(0, 75)
hist5


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
resbias6$biasSamp <- resbias6$sampMean-resbias6$popMean
resbias6$biasSampPercent <- 100 * resbias6$biasSamp / resbias6$popMean
resbias6$biasPS <- resbias6$sampPSmean-resbias6$popMean
resbias6$biasPSPercent <- 100 * resbias6$biasPS / resbias6$popMean
resbias6$biasrake <- resbias6$samprakemean-resbias6$popMean
resbias6$biasrakePercent <- 100 * resbias6$biasrake / resbias6$popMean
resbias6$biasPR <- resbias6$sampPRmean-resbias6$popMean
resbias6$biasPRPercent <- 100 * resbias6$biasPR / resbias6$popMean
bias6<- apply(resbias6,2,mean)

resMSE6 <- resbias6[,c("biasSamp", "biasPS", "biasrake", "biasPR")]
resMSE6$samp <- resMSE6$biasSamp^2
resMSE6$PS <- resMSE6$biasPS^2
resMSE6$rake <- resMSE6$biasrake^2
resMSE6$PR <- resMSE6$biasPR^2
MSE6 <- apply(resMSE6, 2, mean)

resCI6 <- resbias6[,1:5]
resCI6$sampL <- resCI6$sampMean - 1.96 * (sd(pop6$income)/sqrt(400))
resCI6$sampU <- resCI6$sampMean + 1.96 * (sd(pop6$income)/sqrt(400))
resCI6$samplength <- 2*1.96*(sd(pop6$income)/sqrt(400))
resCI6 <- resCI6 %>% mutate(sampcoverage = ifelse(popMean > sampL & popMean < sampU, 1, 0)) 

dat6 <- data.frame(trial=numeric(), method=character(), bias=numeric(), percentBias=numeric(), MSE=numeric(), ci_length=numeric(), ci_coverage=numeric(),
                   stringsAsFactors=FALSE)
dat6[1,] <- list(6, "unadjusted", bias6[6], bias6[7], MSE6["samp"], mean(resCI6$samplength), sum(resCI6$sampcoverage)/length(resCI6$sampcoverage))
dat6[2,] <- list(6, "PS", bias6[8], bias6[9], MSE6["PS"], NA, NA)
dat6[3,] <- list(6, "Rake", bias6[10], bias6[11], MSE6["rake"], NA, NA)
dat6[4,] <- list(6, "PR", bias6[12], bias6[13], MSE6["PR"], NA, NA)
dat6

res6 <- subset(gather(restrial6$res), key != "popMean")
hist6 <- ggplot(res6) +
  geom_vline(xintercept = mean(pop6$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  #ggtitle("Fig 6. Random* Sample with Pop B") +
  xlim(50000, 100000) +
  ylim(0, 75)
hist6

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
resbias7$biasSamp <- resbias7$sampMean-resbias7$popMean
resbias7$biasSampPercent <- 100 * resbias7$biasSamp / resbias7$popMean
resbias7$biasPS <- resbias7$sampPSmean-resbias7$popMean
resbias7$biasPSPercent <- 100 * resbias7$biasPS / resbias7$popMean
resbias7$biasrake <- resbias7$samprakemean-resbias7$popMean
resbias7$biasrakePercent <- 100 * resbias7$biasrake / resbias7$popMean
resbias7$biasPR <- resbias7$sampPRmean-resbias7$popMean
resbias7$biasPRPercent <- 100 * resbias7$biasPR / resbias7$popMean
bias7<- apply(resbias7,2,mean)

resMSE7 <- resbias7[,c("biasSamp", "biasPS", "biasrake", "biasPR")]
resMSE7$samp <- resMSE7$biasSamp^2
resMSE7$PS <- resMSE7$biasPS^2
resMSE7$rake <- resMSE7$biasrake^2
resMSE7$PR <- resMSE7$biasPR^2
MSE7 <- apply(resMSE7, 2, mean)

resCI7 <- resbias7[,1:5]
resCI7$sampL <- resCI7$sampMean - 1.96 * (sd(pop7$income)/sqrt(400))
resCI7$sampU <- resCI7$sampMean + 1.96 * (sd(pop7$income)/sqrt(400))
resCI7$samplength <- 2*1.96*(sd(pop7$income)/sqrt(400))
resCI7 <- resCI7 %>% mutate(sampcoverage = ifelse(popMean > sampL & popMean < sampU, 1, 0)) 

dat7 <- data.frame(trial=numeric(), method=character(), bias=numeric(), percentBias=numeric(), MSE=numeric(), ci_length=numeric(), ci_coverage=numeric(),
                   stringsAsFactors=FALSE)
dat7[1,] <- list(7, "unadjusted", bias7[6], bias7[7], MSE7["samp"], mean(resCI7$samplength), sum(resCI7$sampcoverage)/length(resCI7$sampcoverage))
dat7[2,] <- list(7, "PS", bias7[8], bias7[9], MSE7["PS"], NA, NA)
dat7[3,] <- list(7, "Rake", bias7[10], bias7[11], MSE7["rake"], NA, NA)
dat7[4,] <- list(7, "PR", bias7[12], bias7[13], MSE7["PR"], NA, NA)
dat7

res7 <- subset(gather(restrial7$res), key != "popMean")
hist7 <- ggplot(res7) +
  geom_vline(xintercept = mean(pop7$income), color="orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  #ggtitle("Fig 7. 3 Variable Stratified Sample with Pop C") +
  xlim(50000, 100000) +
  ylim(0, 75)
hist7


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
resbias8$biasSamp <- resbias8$sampMean-resbias8$popMean
resbias8$biasSampPercent <- 100 * resbias8$biasSamp / resbias8$popMean
resbias8$biasPS <- resbias8$sampPSmean-resbias8$popMean
resbias8$biasPSPercent <- 100 * resbias8$biasPS / resbias8$popMean
resbias8$biasrake <- resbias8$samprakemean-resbias8$popMean
resbias8$biasrakePercent <- 100 * resbias8$biasrake / resbias8$popMean
resbias8$biasPR <- resbias8$sampPRmean-resbias8$popMean
resbias8$biasPRPercent <- 100 * resbias8$biasPR / resbias8$popMean
bias8<- apply(resbias8,2,mean)

resMSE8 <- resbias8[,c("biasSamp", "biasPS", "biasrake", "biasPR")]
resMSE8$samp <- resMSE8$biasSamp^2
resMSE8$PS <- resMSE8$biasPS^2
resMSE8$rake <- resMSE8$biasrake^2
resMSE8$PR <- resMSE8$biasPR^2
MSE8 <- apply(resMSE8, 2, mean)

resCI8 <- resbias8[,1:5]
resCI8$sampL <- resCI8$sampMean - 1.96 * (sd(pop8$income)/sqrt(400))
resCI8$sampU <- resCI8$sampMean + 1.96 * (sd(pop8$income)/sqrt(400))
resCI8$samplength <- 2*1.96*(sd(pop8$income)/sqrt(400))
resCI8 <- resCI8 %>% mutate(sampcoverage = ifelse(popMean > sampL & popMean < sampU, 1, 0)) 

dat8 <- data.frame(trial=numeric(), method=character(), bias=numeric(), percentBias=numeric(), MSE=numeric(), ci_length=numeric(), ci_coverage=numeric(),
                   stringsAsFactors=FALSE)
dat8[1,] <- list(8, "unadjusted", bias8[6], bias8[7], MSE8["samp"], mean(resCI8$samplength), sum(resCI8$sampcoverage)/length(resCI8$sampcoverage))
dat8[2,] <- list(8, "PS", bias8[8], bias8[9], MSE8["PS"], NA, NA)
dat8[3,] <- list(8, "Rake", bias8[10], bias8[11], MSE8["rake"], NA, NA)
dat8[4,] <- list(8, "PR", bias8[12], bias8[13], MSE8["PR"], NA, NA)
dat8

res8 <- subset(gather(restrial8$res), key != "popMean")
hist8 <- ggplot(res8) +
  geom_vline(xintercept = mean(pop8$income), color="orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  #ggtitle("Fig 8. 4 Var Strat Samp, Pop C") +
  xlim(50000, 100000) +
  ylim(0, 75)
hist8


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
resbias9$biasSamp <- resbias9$sampMean-resbias9$popMean
resbias9$biasSampPercent <- 100 * resbias9$biasSamp / resbias9$popMean
resbias9$biasPS <- resbias9$sampPSmean-resbias9$popMean
resbias9$biasPSPercent <- 100 * resbias9$biasPS / resbias9$popMean
resbias9$biasrake <- resbias9$samprakemean-resbias9$popMean
resbias9$biasrakePercent <- 100 * resbias9$biasrake / resbias9$popMean
resbias9$biasPR <- resbias9$sampPRmean-resbias9$popMean
resbias9$biasPRPercent <- 100 * resbias9$biasPR / resbias9$popMean
bias9<- apply(resbias9,2,mean)

resMSE9 <- resbias9[,c("biasSamp", "biasPS", "biasrake", "biasPR")]
resMSE9$samp <- resMSE9$biasSamp^2
resMSE9$PS <- resMSE9$biasPS^2
resMSE9$rake <- resMSE9$biasrake^2
resMSE9$PR <- resMSE9$biasPR^2
MSE9 <- apply(resMSE9, 2, mean)

resCI9 <- resbias9[,1:5]
resCI9$sampL <- resCI9$sampMean - 1.96 * (sd(pop9$income)/sqrt(400))
resCI9$sampU <- resCI9$sampMean + 1.96 * (sd(pop9$income)/sqrt(400))
resCI9$samplength <- 2*1.96*(sd(pop9$income)/sqrt(400))
resCI9 <- resCI9 %>% mutate(sampcoverage = ifelse(popMean > sampL & popMean < sampU, 1, 0)) 

dat9 <- data.frame(trial=numeric(), method=character(), bias=numeric(), percentBias=numeric(), MSE=numeric(), ci_length=numeric(), ci_coverage=numeric(),
                   stringsAsFactors=FALSE)
dat9[1,] <- list(9, "unadjusted", bias9[6], bias9[7], MSE9["samp"], mean(resCI9$samplength), sum(resCI9$sampcoverage)/length(resCI9$sampcoverage))
dat9[2,] <- list(9, "PS", bias9[8], bias9[9], MSE9["PS"], NA, NA)
dat9[3,] <- list(9, "Rake", bias9[10], bias9[11], MSE9["rake"], NA, NA)
dat9[4,] <- list(9, "PR", bias9[12], bias9[13], MSE9["PR"], NA, NA)
dat9

res9 <- subset(gather(restrial9$res), key != "popMean")
hist9 <- ggplot(res9) +
  geom_vline(xintercept = mean(pop9$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  #ggtitle("Fig 9. Random* Samp, Pop C") +
  xlim(50000, 100000) +
  ylim(0, 75)
hist9


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
resbias10$biasSamp <- resbias10$sampMean-resbias10$popMean
resbias10$biasSampPercent <- 100 * resbias10$biasSamp / resbias10$popMean
resbias10$biasPS <- resbias10$sampPSmean-resbias10$popMean
resbias10$biasPSPercent <- 100 * resbias10$biasPS / resbias10$popMean
resbias10$biasrake <- resbias10$samprakemean-resbias10$popMean
resbias10$biasrakePercent <- 100 * resbias10$biasrake / resbias10$popMean
resbias10$biasPR <- resbias10$sampPRmean-resbias10$popMean
resbias10$biasPRPercent <- 100 * resbias10$biasPR / resbias10$popMean
bias10<- apply(resbias10,2,mean)

resMSE10 <- resbias10[,c("biasSamp", "biasPS", "biasrake", "biasPR")]
resMSE10$samp <- resMSE10$biasSamp^2
resMSE10$PS <- resMSE10$biasPS^2
resMSE10$rake <- resMSE10$biasrake^2
resMSE10$PR <- resMSE10$biasPR^2
MSE10 <- apply(resMSE10, 2, mean)

resCI10 <- resbias10[,1:5]
resCI10$sampL <- resCI10$sampMean - 1.96 * (sd(pop10$income)/sqrt(400))
resCI10$sampU <- resCI10$sampMean + 1.96 * (sd(pop10$income)/sqrt(400))
resCI10$samplength <- 2*1.96*(sd(pop10$income)/sqrt(400))
resCI10 <- resCI10 %>% mutate(sampcoverage = ifelse(popMean > sampL & popMean < sampU, 1, 0)) 

dat10 <- data.frame(trial=numeric(), method=character(), bias=numeric(), percentBias=numeric(), MSE=numeric(), ci_length=numeric(), ci_coverage=numeric(),
                    stringsAsFactors=FALSE)
dat10[1,] <- list(10, "unadjusted", bias10[6], bias10[7], MSE10["samp"], mean(resCI10$samplength), sum(resCI10$sampcoverage)/length(resCI10$sampcoverage))
dat10[2,] <- list(10, "PS", bias10[8], bias10[9], MSE10["PS"], NA, NA)
dat10[3,] <- list(10, "Rake", bias10[10], bias10[11], MSE10["rake"], NA, NA)
dat10[4,] <- list(10, "PR", bias10[12], bias10[13], MSE10["PR"], NA, NA)
dat10

res10 <- subset(gather(restrial10$res), key != "popMean")
hist10 <- ggplot(res10) +
  geom_vline(xintercept = mean(pop10$income), color="orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  #ggtitle("Fig 10. 3 Var Stratified Samp, Pop D") +
  xlim(50000, 100000) +
  ylim(0, 75)
hist10

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
resbias11$biasSamp <- resbias11$sampMean-resbias11$popMean
resbias11$biasSampPercent <- 100 * resbias11$biasSamp / resbias11$popMean
resbias11$biasPS <- resbias11$sampPSmean-resbias11$popMean
resbias11$biasPSPercent <- 100 * resbias11$biasPS / resbias11$popMean
resbias11$biasrake <- resbias11$samprakemean-resbias11$popMean
resbias11$biasrakePercent <- 100 * resbias11$biasrake / resbias11$popMean
resbias11$biasPR <- resbias11$sampPRmean-resbias11$popMean
resbias11$biasPRPercent <- 100 * resbias11$biasPR / resbias11$popMean
bias11<- apply(resbias11,2,mean)

resMSE11 <- resbias11[,c("biasSamp", "biasPS", "biasrake", "biasPR")]
resMSE11$samp <- resMSE11$biasSamp^2
resMSE11$PS <- resMSE11$biasPS^2
resMSE11$rake <- resMSE11$biasrake^2
resMSE11$PR <- resMSE11$biasPR^2
MSE11 <- apply(resMSE11, 2, mean)

resCI11 <- resbias11[,1:5]
resCI11$sampL <- resCI11$sampMean - 1.96 * (sd(pop11$income)/sqrt(400))
resCI11$sampU <- resCI11$sampMean + 1.96 * (sd(pop11$income)/sqrt(400))
resCI11$samplength <- 2*1.96*(sd(pop11$income)/sqrt(400))
resCI11 <- resCI11 %>% mutate(sampcoverage = ifelse(popMean > sampL & popMean < sampU, 1, 0)) 

dat11 <- data.frame(trial=numeric(), method=character(), bias=numeric(), percentBias=numeric(), MSE=numeric(), ci_length=numeric(), ci_coverage=numeric(),
                    stringsAsFactors=FALSE)
dat11[1,] <- list(11, "unadjusted", bias11[6], bias11[7], MSE11["samp"], mean(resCI11$samplength), sum(resCI11$sampcoverage)/length(resCI11$sampcoverage))
dat11[2,] <- list(11, "PS", bias11[8], bias11[9], MSE11["PS"], NA, NA)
dat11[3,] <- list(11, "Rake", bias11[10], bias11[11], MSE11["rake"], NA, NA)
dat11[4,] <- list(11, "PR", bias11[12], bias11[13], MSE11["PR"], NA, NA)
dat11

res11 <- subset(gather(restrial11$res), key != "popMean")
hist11 <- ggplot(res11) +
  geom_vline(xintercept = mean(pop11$income), color="orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  #ggtitle("Fig 11. 4 Var Strat Samp, Pop D") +
  xlim(50000, 100000) +
  ylim(0, 75)
hist11



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
resbias12$biasSamp <- resbias12$sampMean-resbias12$popMean
resbias12$biasSampPercent <- 100 * resbias12$biasSamp / resbias12$popMean
resbias12$biasPS <- resbias12$sampPSmean-resbias12$popMean
resbias12$biasPSPercent <- 100 * resbias12$biasPS / resbias12$popMean
resbias12$biasrake <- resbias12$samprakemean-resbias12$popMean
resbias12$biasrakePercent <- 100 * resbias12$biasrake / resbias12$popMean
resbias12$biasPR <- resbias12$sampPRmean-resbias12$popMean
resbias12$biasPRPercent <- 100 * resbias12$biasPR / resbias12$popMean
bias12<- apply(resbias12,2,mean)

resMSE12 <- resbias12[,c("biasSamp", "biasPS", "biasrake", "biasPR")]
resMSE12$samp <- resMSE12$biasSamp^2
resMSE12$PS <- resMSE12$biasPS^2
resMSE12$rake <- resMSE12$biasrake^2
resMSE12$PR <- resMSE12$biasPR^2
MSE12 <- apply(resMSE12, 2, mean)

resCI12 <- resbias12[,1:5]
resCI12$sampL <- resCI12$sampMean - 1.96 * (sd(pop12$income)/sqrt(400))
resCI12$sampU <- resCI12$sampMean + 1.96 * (sd(pop12$income)/sqrt(400))
resCI12$samplength <- 2*1.96*(sd(pop12$income)/sqrt(400))
resCI12 <- resCI12 %>% mutate(sampcoverage = ifelse(popMean > sampL & popMean < sampU, 1, 0)) 

dat12 <- data.frame(trial=numeric(), method=character(), bias=numeric(), percentBias=numeric(), MSE=numeric(), ci_length=numeric(), ci_coverage=numeric(),
                    stringsAsFactors=FALSE)
dat12[1,] <- list(12, "unadjusted", bias12[6], bias12[7], MSE12["samp"], mean(resCI12$samplength), sum(resCI12$sampcoverage)/length(resCI12$sampcoverage))
dat12[2,] <- list(12, "PS", bias12[8], bias12[9], MSE12["PS"], NA, NA)
dat12[3,] <- list(12, "Rake", bias12[10], bias12[11], MSE12["rake"], NA, NA)
dat12[4,] <- list(12, "PR", bias12[12], bias12[13], MSE12["PR"], NA, NA)
dat12

res12 <- subset(gather(restrial12$res), key != "popMean")
hist12 <- ggplot(res12) +
  geom_vline(xintercept = mean(pop12$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  #ggtitle("Fig 12: Random* Samp, Pop D") +
  xlim(50000, 100000) +
  ylim(0, 75)
hist12


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
resbias13$biasSamp <- resbias13$sampMean-resbias13$popMean
resbias13$biasSampPercent <- 100 * resbias13$biasSamp / resbias13$popMean
resbias13$biasPS <- resbias13$sampPSmean-resbias13$popMean
resbias13$biasPSPercent <- 100 * resbias13$biasPS / resbias13$popMean
resbias13$biasrake <- resbias13$samprakemean-resbias13$popMean
resbias13$biasrakePercent <- 100 * resbias13$biasrake / resbias13$popMean
resbias13$biasPR <- resbias13$sampPRmean-resbias13$popMean
resbias13$biasPRPercent <- 100 * resbias13$biasPR / resbias13$popMean
bias13<- apply(resbias13,2,mean)

resMSE13 <- resbias13[,c("biasSamp", "biasPS", "biasrake", "biasPR")]
resMSE13$samp <- resMSE13$biasSamp^2
resMSE13$PS <- resMSE13$biasPS^2
resMSE13$rake <- resMSE13$biasrake^2
resMSE13$PR <- resMSE13$biasPR^2
MSE13 <- apply(resMSE13, 2, mean)

resCI13 <- resbias13[,1:5]
resCI13$sampL <- resCI13$sampMean - 1.96 * (sd(pop13$income)/sqrt(400))
resCI13$sampU <- resCI13$sampMean + 1.96 * (sd(pop13$income)/sqrt(400))
resCI13$samplength <- 2*1.96*(sd(pop13$income)/sqrt(400))
resCI13 <- resCI13 %>% mutate(sampcoverage = ifelse(popMean > sampL & popMean < sampU, 1, 0)) 

dat13 <- data.frame(trial=numeric(), method=character(), bias=numeric(), percentBias=numeric(), MSE=numeric(), ci_length=numeric(), ci_coverage=numeric(),
                    stringsAsFactors=FALSE)
dat13[1,] <- list(13, "unadjusted", bias13[6], bias13[7], MSE13["samp"], mean(resCI13$samplength), sum(resCI13$sampcoverage)/length(resCI13$sampcoverage))
dat13[2,] <- list(13, "PS", bias13[8], bias13[9], MSE13["PS"], NA, NA)
dat13[3,] <- list(13, "Rake", bias13[10], bias13[11], MSE13["rake"], NA, NA)
dat13[4,] <- list(13, "PR", bias13[12], bias13[13], MSE13["PR"], NA, NA)
dat13

res13 <- subset(gather(restrial13$res), key != "popMean")
hist13 <- ggplot(res13) +
  geom_vline(xintercept = mean(pop13$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  #ggtitle("Fig 13. Income (2 lvls) 4 Var Stratified Samp, Pop A") +
  xlim(50000, 100000) +
  ylim(0, 75)
hist13


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
resbias14$biasSamp <- resbias14$sampMean-resbias14$popMean
resbias14$biasSampPercent <- 100 * resbias14$biasSamp / resbias14$popMean
resbias14$biasPS <- resbias14$sampPSmean-resbias14$popMean
resbias14$biasPSPercent <- 100 * resbias14$biasPS / resbias14$popMean
resbias14$biasrake <- resbias14$samprakemean-resbias14$popMean
resbias14$biasrakePercent <- 100 * resbias14$biasrake / resbias14$popMean
resbias14$biasPR <- resbias14$sampPRmean-resbias14$popMean
resbias14$biasPRPercent <- 100 * resbias14$biasPR / resbias14$popMean
bias14<- apply(resbias14,2,mean)

resMSE14 <- resbias14[,c("biasSamp", "biasPS", "biasrake", "biasPR")]
resMSE14$samp <- resMSE14$biasSamp^2
resMSE14$PS <- resMSE14$biasPS^2
resMSE14$rake <- resMSE14$biasrake^2
resMSE14$PR <- resMSE14$biasPR^2
MSE14 <- apply(resMSE14, 2, mean)

resCI14 <- resbias14[,1:5]
resCI14$sampL <- resCI14$sampMean - 1.96 * (sd(pop14$income)/sqrt(400))
resCI14$sampU <- resCI14$sampMean + 1.96 * (sd(pop14$income)/sqrt(400))
resCI14$samplength <- 2*1.96*(sd(pop14$income)/sqrt(400))
resCI14 <- resCI14 %>% mutate(sampcoverage = ifelse(popMean > sampL & popMean < sampU, 1, 0)) 

dat14 <- data.frame(trial=numeric(), method=character(), bias=numeric(), percentBias=numeric(), MSE=numeric(), ci_length=numeric(), ci_coverage=numeric(),
                    stringsAsFactors=FALSE)
dat14[1,] <- list(14, "unadjusted", bias14[6], bias14[7], MSE14["samp"], mean(resCI14$samplength), sum(resCI14$sampcoverage)/length(resCI14$sampcoverage))
dat14[2,] <- list(14, "PS", bias14[8], bias14[9], MSE14["PS"], NA, NA)
dat14[3,] <- list(14, "Rake", bias14[10], bias14[11], MSE14["rake"], NA, NA)
dat14[4,] <- list(14, "PR", bias14[12], bias14[13], MSE14["PR"], NA, NA)
dat14

res14 <- subset(gather(restrial14$res), key != "popMean")
hist14 <- ggplot(res14) +
  geom_vline(xintercept = mean(pop14$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  #ggtitle("Fig 14. Income Biased 3 Var Strat, Pop A") +
  xlim(50000, 100000) +
  ylim(0, 75)
hist14


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
resbias15$biasSamp <- resbias15$sampMean-resbias15$popMean
resbias15$biasSampPercent <- 100 * resbias15$biasSamp / resbias15$popMean
resbias15$biasPS <- resbias15$sampPSmean-resbias15$popMean
resbias15$biasPSPercent <- 100 * resbias15$biasPS / resbias15$popMean
resbias15$biasrake <- resbias15$samprakemean-resbias15$popMean
resbias15$biasrakePercent <- 100 * resbias15$biasrake / resbias15$popMean
resbias15$biasPR <- resbias15$sampPRmean-resbias15$popMean
resbias15$biasPRPercent <- 100 * resbias15$biasPR / resbias15$popMean
bias15<- apply(resbias15,2,mean)

resMSE15 <- resbias15[,c("biasSamp", "biasPS", "biasrake", "biasPR")]
resMSE15$samp <- resMSE15$biasSamp^2
resMSE15$PS <- resMSE15$biasPS^2
resMSE15$rake <- resMSE15$biasrake^2
resMSE15$PR <- resMSE15$biasPR^2
MSE15 <- apply(resMSE15, 2, mean)

resCI15 <- resbias15[,1:5]
resCI15$sampL <- resCI15$sampMean - 1.96 * (sd(pop15$income)/sqrt(400))
resCI15$sampU <- resCI15$sampMean + 1.96 * (sd(pop15$income)/sqrt(400))
resCI15$samplength <- 2*1.96*(sd(pop15$income)/sqrt(400))
resCI15 <- resCI15 %>% mutate(sampcoverage = ifelse(popMean > sampL & popMean < sampU, 1, 0)) 

dat15 <- data.frame(trial=numeric(), method=character(), bias=numeric(), percentBias=numeric(), MSE=numeric(), ci_length=numeric(), ci_coverage=numeric(),
                    stringsAsFactors=FALSE)
dat15[1,] <- list(15, "unadjusted", bias15[6], bias15[7], MSE15["samp"], mean(resCI15$samplength), sum(resCI15$sampcoverage)/length(resCI15$sampcoverage))
dat15[2,] <- list(15, "PS", bias15[8], bias15[9], MSE15["PS"], NA, NA)
dat15[3,] <- list(15, "Rake", bias15[10], bias15[11], MSE15["rake"], NA, NA)
dat15[4,] <- list(15, "PR", bias15[12], bias15[13], MSE15["PR"], NA, NA)
dat15

res15 <- subset(gather(restrial15$res), key != "popMean")
hist15 <- ggplot(res15) +
  geom_vline(xintercept = mean(pop15$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  #ggtitle("Fig 15. Income & Ins Samp, Pop A") +
  xlim(50000, 100000) +
  ylim(0, 75)
hist15


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
resbias16$biasSamp <- resbias16$sampMean-resbias16$popMean
resbias16$biasSampPercent <- 100 * resbias16$biasSamp / resbias16$popMean
resbias16$biasPS <- resbias16$sampPSmean-resbias16$popMean
resbias16$biasPSPercent <- 100 * resbias16$biasPS / resbias16$popMean
resbias16$biasrake <- resbias16$samprakemean-resbias16$popMean
resbias16$biasrakePercent <- 100 * resbias16$biasrake / resbias16$popMean
resbias16$biasPR <- resbias16$sampPRmean-resbias16$popMean
resbias16$biasPRPercent <- 100 * resbias16$biasPR / resbias16$popMean
bias16<- apply(resbias16,2,mean)

resMSE16 <- resbias16[,c("biasSamp", "biasPS", "biasrake", "biasPR")]
resMSE16$samp <- resMSE16$biasSamp^2
resMSE16$PS <- resMSE16$biasPS^2
resMSE16$rake <- resMSE16$biasrake^2
resMSE16$PR <- resMSE16$biasPR^2
MSE16 <- apply(resMSE16, 2, mean)

resCI16 <- resbias16[,1:5]
resCI16$sampL <- resCI16$sampMean - 1.96 * (sd(pop16$income)/sqrt(400))
resCI16$sampU <- resCI16$sampMean + 1.96 * (sd(pop16$income)/sqrt(400))
resCI16$samplength <- 2*1.96*(sd(pop16$income)/sqrt(400))
resCI16 <- resCI16 %>% mutate(sampcoverage = ifelse(popMean > sampL & popMean < sampU, 1, 0)) 

dat16 <- data.frame(trial=numeric(), method=character(), bias=numeric(), percentBias=numeric(), MSE=numeric(), ci_length=numeric(), ci_coverage=numeric(),
                    stringsAsFactors=FALSE)
dat16[1,] <- list(16, "unadjusted", bias16[6], bias16[7], MSE16["samp"], mean(resCI16$samplength), sum(resCI16$sampcoverage)/length(resCI16$sampcoverage))
dat16[2,] <- list(16, "PS", bias16[8], bias16[9], MSE16["PS"], NA, NA)
dat16[3,] <- list(16, "Rake", bias16[10], bias16[11], MSE16["rake"], NA, NA)
dat16[4,] <- list(16, "PR", bias16[12], bias16[13], MSE16["PR"], NA, NA)
dat16

res16 <- subset(gather(restrial16$res), key != "popMean")
hist16 <- ggplot(res16) +
  geom_vline(xintercept = mean(pop16$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  # ggtitle("Income X, Pop A") +
  xlab("Income") +
  xlim(50000, 110000)
hist16


# Trial 16: 4 Var Strat, Pop A, Ins Y ----------------------------------
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
resbias17$biasSamp <- resbias17$sampMean-resbias17$popMean
resbias17$biasSampPercent <- 100 * resbias17$biasSamp / resbias17$popMean
resbias17$biasPS <- resbias17$sampPSmean-resbias17$popMean
resbias17$biasPSPercent <- 100 * resbias17$biasPS / resbias17$popMean
resbias17$biasrake <- resbias17$samprakemean-resbias17$popMean
resbias17$biasrakePercent <- 100 * resbias17$biasrake / resbias17$popMean
resbias17$biasPR <- resbias17$sampPRmean-resbias17$popMean
resbias17$biasPRPercent <- 100 * resbias17$biasPR / resbias17$popMean
bias17<- apply(resbias17,2,mean)

resMSE17 <- resbias17[,c("biasSamp", "biasPS", "biasrake", "biasPR")]
resMSE17$samp <- resMSE17$biasSamp^2
resMSE17$PS <- resMSE17$biasPS^2
resMSE17$rake <- resMSE17$biasrake^2
resMSE17$PR <- resMSE17$biasPR^2
MSE17 <- apply(resMSE17, 2, mean)

resCI17 <- resbias17[,1:5]
resCI17$sampL <- resCI17$sampMean - 1.96 * (sd(pop17$income)/sqrt(400))
resCI17$sampU <- resCI17$sampMean + 1.96 * (sd(pop17$income)/sqrt(400))
resCI17$samplength <- 2*1.96*(sd(pop17$income)/sqrt(400))
resCI17 <- resCI17 %>% mutate(sampcoverage = ifelse(popMean > sampL & popMean < sampU, 1, 0)) 

dat17 <- data.frame(trial=numeric(), method=character(), bias=numeric(), percentBias=numeric(), MSE=numeric(), ci_length=numeric(), ci_coverage=numeric(),
                    stringsAsFactors=FALSE)
dat17[1,] <- list(17, "unadjusted", bias17[6], bias17[7], MSE17["samp"], mean(resCI17$samplength), sum(resCI17$sampcoverage)/length(resCI17$sampcoverage))
dat17[2,] <- list(17, "PS", bias17[8], bias17[9], MSE17["PS"], NA, NA)
dat17[3,] <- list(17, "Rake", bias17[10], bias17[11], MSE17["rake"], NA, NA)
dat17[4,] <- list(17, "PR", bias17[12], bias17[13], MSE17["PR"], NA, NA)
dat17

res17 <- subset(gather(restrial17$res), key != "popMean")
hist17 <- ggplot(res17) +
  geom_vline(xintercept = mean(pop17$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), bins=100) +
  #ggtitle("Income Y, Pop A") +
  xlab("Income") +
  xlim(50000, 110000) +
  ylim(0, 75)
hist17


### save stuff 

allplots <- list(hist1, hist2, hist3, hist4, hist5, hist6, hist7, hist8, hist9,
                 hist10, hist11, hist12,hist13, hist14, hist15, hist16, hist17)
save(allplots, file="/Users/joylee/Research Project/ConditionalRaking/CRplots.rda")

alldat <- rbind(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9, dat10, 
                dat11, dat12, dat13, dat14, dat15, dat16, dat17)
save(alldat, file="/Users/joylee/Research Project/ConditionalRaking/CRtable.rda")

library(gridExtra)
grid.arrange(hist1, hist2, hist3, hist4, hist5, hist6, hist7, hist8, hist9, hist10, hist11, hist12, nrow=4)
grid.arrange(hist1, hist2, hist3, hist13, hist14, hist15, nrow=2)
grid.arrange(hist1, hist4, hist7, hist10, nrow=2)
grid.arrange(hist2, hist5, hist8, hist11, nrow=2)
grid.arrange(hist3, hist5, hist9, hist12, nrow=2)
