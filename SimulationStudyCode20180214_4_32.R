library(weights)
library(anesrake)
library(dplyr)
library(tidyr)
library(ggplot2)


N<-100000
set.seed(89)

expit <- function(x){
  out <- exp(x) / (1+exp(x))
  return(out)
}

# pop <- data.frame(I_age_old = rbinom(N, 1, 0.6), I_sex_F = 0, I_race_B = 0, I_ins_A = 0)
# 
# #sex
# p <- expit(0 + 0.9 * pop$I_age_old)
# pop$I_sex_F <- rbinom(N,1,prob = p)
# 
# #race
# p <- expit(1 + 0.9 * pop$I_age_old * pop$I_sex_F + 1.2 * pop$I_age_old * pop$I_race_B)
# pop$I_race_B <- rbinom(N,1,prob = p)
# 
# #Insurance
# p <- expit(0 + 1.2 * pop$I_age_old * pop$I_race_B * pop$I_sex_F)
# pop$I_ins_A <- rbinom(N,1,prob = p)
# 
# pop <- pop %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
# popFreq <- pop %>% summarise(count = n()/N)
# popFreq
#
# pop$income <- 25000 + 
#   20000 * pop$I_age_old +
#   20000 * pop$I_sex_F * pop$I_race_B + 
#   30000 * pop$I_sex_F * pop$I_age_old +  
#   30000 * pop$I_age_old * pop$I_race_B + 
#   10000 * pop$I_sex_F * pop$I_ins_A +
#   50000 * pop$I_age_old * pop$I_sex_F * pop$I_race_B * pop$I_ins_A +
#   rnorm(N,0,5000)

#boxplot(income ~ I_age_old + I_sex_F, data=pop)
#boxplot(income ~ I_ins_A + I_age_old, data=pop)
#apop <- lm(income~I_age_old+pop$I_sex_F+pop$I_race_B+pop$I_ins_A,data=pop)


#### uses completely stratified sampling with sample_n()

resfunc <- function(pop){
  set.seed(89)

  #popFreq
  
  ############################################
  #Sample statistics
  ############################################
  #k<-10000
  #simple random sample
  #ind<-sample(1:N,k)
  #samp<- pop[ind,]
  s_size <- 100
  nsim<-100
  results<-list()
  resultsLM <- list()
  resultsLM[["lmRaw"]] <- resultsLM[["lmPS"]] <- resultsLM[["lmRake"]] <- resultsLM[["lmPR"]] <- matrix(NA, ncol=5,nrow=nsim)
  for (i in 1:nsim){ 
    #print(i)
   
    sampList <- list()
    
    # ind <- sample(1:sum(pop$I_age_old==1 & pop$I_sex_F==1 & pop$I_race_B == 1), s_size, replace=FALSE)
    # sampList[[1]] <- pop[pop$I_age_old==1 & pop$I_sex_F==1 & pop$I_race_B == 1,][ind,]
    # 
    # ind <- sample(1:sum(pop$I_age_old==1 & pop$I_sex_F==1 & pop$I_race_B == 0), s_size, replace=FALSE)
    # sampList[[2]] <- pop[pop$I_age_old==1 & pop$I_sex_F==1 & pop$I_race_B == 0,][ind,]
    # 
    # ind <- sample(1:sum(pop$I_age_old==1 & pop$I_sex_F==0 & pop$I_race_B == 1), s_size, replace=FALSE)
    # sampList[[3]] <- pop[pop$I_age_old==1 & pop$I_sex_F==0 & pop$I_race_B == 1,][ind,]
    # 
    # ind <- sample(1:sum(pop$I_age_old==1 & pop$I_sex_F==0 & pop$I_race_B == 0), s_size, replace=FALSE)
    # sampList[[4]] <- pop[pop$I_age_old==1 & pop$I_sex_F==0 & pop$I_race_B == 0,][ind,]
    # 
    # ind <- sample(1:sum(pop$I_age_old==0 & pop$I_sex_F==1 & pop$I_race_B == 1), s_size, replace=FALSE)
    # sampList[[5]] <- pop[pop$I_age_old==0 & pop$I_sex_F==1 & pop$I_race_B == 1,][ind,]
    # 
    # ind <- sample(1:sum(pop$I_age_old==0 & pop$I_sex_F==1 & pop$I_race_B == 0), s_size, replace=FALSE)
    # sampList[[6]] <- pop[pop$I_age_old==0 & pop$I_sex_F==1 & pop$I_race_B == 0,][ind,]
    # 
    # ind <- sample(1:sum(pop$I_age_old==0 & pop$I_sex_F==0 & pop$I_race_B == 1), s_size, replace=FALSE)
    # sampList[[7]] <- pop[pop$I_age_old==0 & pop$I_sex_F==0 & pop$I_race_B == 1,][ind,]
    # 
    # ind <- sample(1:sum(pop$I_age_old==0 & pop$I_sex_F==0 & pop$I_race_B == 0), s_size, replace=FALSE)
    # sampList[[8]] <- pop[pop$I_age_old==0 & pop$I_sex_F==0 & pop$I_race_B == 0,][ind,]
    # samp <- do.call(rbind, sampList)

    samp <- sample_n(pop, 50, replace = FALSE)   # pop is already ordered in groups so sample_n will take 50 from each group (50*16=800!)
    
    ###### check samp makeup
    # samp <- samp %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
    # sampFreq <- samp %>% summarise(count = n()/nrow(samp))
    # sampFreq
    
    ###############################
    # post strat
    ###############################
    pop <- pop %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
    popFreq <- pop %>% summarise(count = n()/N)
    
    samp <- samp %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
    sampFreq <- samp %>% summarise(count = n()/nrow(samp))
    
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
    
    samp <- samp %>% 
      mutate(I_sex_race = ifelse(I_sex_F ==0 & I_race_B ==0, 1,
                                      ifelse(I_sex_F ==0 & I_race_B == 1, 2,
                                             ifelse(I_sex_F==1 & I_race_B ==0, 3, 4))))
    
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
    
    #samp %>% group_by(I_age_old, I_sex_F, I_race_B) %>% summarize(rake=mean(rakeweight),ps = mean(psweight),pr  = mean(prweight) ,n=n())
    
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
  
  #return(weightslist)
  return(res)
}


# res$biasPS <- res$sampPSmean-res$popMean
# res$biasrake <- res$samprakemean-res$popMean
# res$biasPR <- res$sampPRmean-res$popMean
# apply(res,2,mean)
# 
# apply(resultsLM[["lmRaw"]],2,mean)
# apply(resultsLM[["lmPS"]],2,mean)
# apply(resultsLM[["lmRake"]],2,mean)
# apply(resultsLM[["lmPR"]],2,mean)
# 
# 
# hist(res$sampMean,xlim=c(60000,100000), main = "3 by 2")
# hist(res$sampPSmean,add=TRUE,col="blue")
# hist(res$samprakemean,add=TRUE,col="green")
# hist(res$sampPRmean, add= TRUE, col= "orange")
# abline(v=mean(pop$income),col="red",lwd=3)
# 
# 
# abline(v=mean(res$sampPSmean), col = "blue")
# abline(v=mean(res$samprakemean),col="green")
# abline(v=mean(res$sampPRmean), col= "orange")
# legend("topright",inset=c(0.,0), c("Sample", "Post-Strat", "Rake", "PartialR", "Pop"), fill = c("white", "blue", "green", "orange", "red"))



################ 1: mostly stratified sampling

resbias1 <- restrial1
resbias1$biasPS <- resbias1$sampPSmean-resbias1$popMean
resbias1$biasrake <- resbias1$samprakemean-resbias1$popMean
resbias1$biasPR <- resbias1$sampPRmean-resbias1$popMean
apply(resbias1,2,mean)
pop <- data.frame(I_age_old = rbinom(N, 1, 0.6), I_sex_F = 0, I_race_B = 0, I_ins_A = 0)
#sex
p <- expit(0 + 0.9 * pop$I_age_old)
pop$I_sex_F <- rbinom(N,1,prob = p)
#race
p <- expit(1 + 0.9 * pop$I_age_old * pop$I_sex_F)
pop$I_race_B <- rbinom(N,1,prob = p)
#Insurance
p <- expit(0 + 1.2 * pop$I_age_old * pop$I_race_B * pop$I_sex_F)
pop$I_ins_A <- rbinom(N,1,prob = p)
pop <- pop %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
popFreq <- pop %>% summarise(count = n()/N)
popFreq

pop$income <- 25000 + 
  40000 * pop$I_age_old +
  25000 * pop$I_sex_F * pop$I_race_B + 
  30000 * pop$I_sex_F * pop$I_age_old +  
  40000 * pop$I_age_old * pop$I_race_B * pop$I_ins_A + 
  20000 * pop$I_age_old * pop$I_sex_F * pop$I_race_B * pop$I_ins_A +
  rnorm(N,0,5000)

restrial1 <- resfunc(pop)

res1 <- subset(gather(restrial1), key != "popMean")

p1 <- ggplot(res1) +
  geom_vline(xintercept = mean(pop3$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), binwidth=500) +
  ggtitle("3 var Stratified Samp") +
  xlim(60000, 100000)
p1

################ 2: completely stratified samp (using sample_n)
pop2 <- data.frame(I_age_old = rbinom(N, 1, 0.6), I_sex_F = 0, I_race_B = 0, I_ins_A = 0)
#sex
p <- expit(0 + 0.9 * pop2$I_age_old)
pop2$I_sex_F <- rbinom(N,1,prob = p)
#race
p <- expit(1 + 0.9 * pop2$I_age_old * pop2$I_sex_F)
pop2$I_race_B <- rbinom(N,1,prob = p)
#Insurance
p <- expit(0 + 1.2 * pop2$I_age_old * pop2$I_race_B * pop2$I_sex_F)
pop2$I_ins_A <- rbinom(N,1,prob = p)
pop2 <- pop2 %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
pop2Freq <- pop2 %>% summarise(count = n()/N)
pop2Freq
pop2$income <- 25000 + 
  40000 * pop2$I_age_old +
  25000 * pop2$I_sex_F * pop2$I_race_B + 
  30000 * pop2$I_sex_F * pop2$I_age_old +  
  40000 * pop2$I_age_old * pop2$I_race_B * pop2$I_ins_A + 
  20000 * pop2$I_age_old * pop2$I_sex_F * pop2$I_race_B * pop2$I_ins_A +
  rnorm(N,0,5000)

restrial2 <- resfunc(pop2)

resbias2 <- restrial2
resbias2$biasPS <- resbias2$sampPSmean-resbias2$popMean
resbias2$biasrake <- resbias2$samprakemean-resbias2$popMean
resbias2$biasPR <- resbias2$sampPRmean-resbias2$popMean
apply(resbias2,2,mean)

res2 <- subset(gather(restrial2), key != "popMean")
p2 <- ggplot(res2) +
  geom_vline(xintercept = mean(pop3$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), binwidth=500) +
  ggtitle("using sample_n") +
  xlim(60000, 100000)
p2


#### 3: Random ? / sample_n with ungroup 
pop3 <- data.frame(I_age_old = rbinom(N, 1, 0.6), I_sex_F = 0, I_race_B = 0, I_ins_A = 0)
#sex
p <- expit(0 + 0.9 * pop3$I_age_old)
pop3$I_sex_F <- rbinom(N,1,prob = p)
#race
p <- expit(1 + 0.9 * pop3$I_age_old * pop3$I_sex_F)
pop3$I_race_B <- rbinom(N,1,prob = p)
#Insurance
p <- expit(0 + 1.2 * pop3$I_age_old * pop3$I_race_B * pop3$I_sex_F)
pop3$I_ins_A <- rbinom(N,1,prob = p)

pop3 <- pop3 %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
pop3Freq <- pop3 %>% summarise(count = n()/N)
#pop3Freq

pop3$income <- 25000 + 
40000 * pop3$I_age_old +
  25000 * pop3$I_sex_F * pop3$I_race_B + 
  30000 * pop3$I_sex_F * pop3$I_age_old +  
  40000 * pop3$I_age_old * pop3$I_race_B * pop3$I_ins_A + 
  20000 * pop3$I_age_old * pop3$I_sex_F * pop3$I_race_B * pop3$I_ins_A +
  rnorm(N,0,5000)

ungroup(pop3)
restrial3 <- resfunc(pop3)

resbias3 <- restrial3
resbias3$biasPS <- resbias3$sampPSmean-resbias3$popMean
resbias3$biasrake <- resbias3$samprakemean-resbias3$popMean
resbias3$biasPR <- resbias3$sampPRmean-resbias3$popMean
apply(resbias3,2,mean)


res3 <- subset(gather(restrial3), key != "popMean")
p3 <- ggplot(res3) +  
  geom_vline(xintercept = mean(pop3$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), binwidth=500) +
  ggtitle("using sample_n, ungrouped (random)") +
  xlim(60000, 100000)
p3


#### 4: mostly strat samp with different income  
pop4 <- data.frame(I_age_old = rbinom(N, 1, 0.5), I_sex_F = 0, I_race_B = 0, I_ins_A = 0)
#sex
p <- expit(0 + 0.9 * pop4$I_age_old)
pop4$I_sex_F <- rbinom(N,1,prob = p)
#race
p <- expit(1 + 0.9 * pop4$I_age_old * pop4$I_sex_F)
pop4$I_race_B <- rbinom(N,1,prob = p)
#Insurance
p <- expit(0 + 1.2 * pop4$I_age_old * pop4$I_race_B * pop4$I_sex_F)
pop4$I_ins_A <- rbinom(N,1,prob = p)

pop4 <- pop4 %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)

pop4Freq <- pop4 %>% summarise(count = n()/N)
pop4Freq$realcount <- pop4Freq %>% summarise(realcount = n())
pop4Freq
chisq.test(pop4Freq$count,p = rep(1/length(pop4Freq$count), length(pop4Freq$count)))




pop4$income <- 25000 + 
  20000 * pop4$I_age_old +
  15000 * pop4$I_sex_F +
  10000 * pop4$I_race_B + 
  30000 * pop4$I_sex_F * pop4$I_age_old +  
  30000 * pop4$I_age_old * pop4$I_race_B + 
  10000 * pop4$I_sex_F * pop4$I_ins_A +
  50000 * pop4$I_age_old * pop4$I_sex_F * pop4$I_race_B * pop4$I_ins_A +
  rnorm(N,0,5000)

restrial4 <- resfunc(pop4)   #######################

resbias4 <- restrial4
resbias4$biasPS <- resbias4$sampPSmean-resbias4$popMean
resbias4$biasrake <- resbias4$samprakemean-resbias4$popMean
resbias4$biasPR <- resbias4$sampPRmean-resbias4$popMean
apply(resbias4,2,mean)

res4 <- subset(gather(restrial4), key != "popMean")
p4 <- ggplot(res4) +
  geom_vline(xintercept = mean(pop4$income), color="orange") +
  geom_histogram(aes(x = value, fill = key), binwidth=500) +
  ggtitle("Mostly Strat Samp, different income") +
  xlim(60000, 105000)
p4



################ 5: mostly strat samp w/ different pop 1
pop5 <- data.frame(I_age_old = rbinom(N, 1, 0.6), I_sex_F = 0, I_race_B = 0, I_ins_A = 0)
#sex
p <- expit(0 + 0.9 * pop5$I_age_old)
pop5$I_sex_F <- rbinom(N,1,prob = p)
#race
p <- expit(1 + 0.4 * pop5$I_age_old - 0.9 * pop5$I_age_old * pop5$I_sex_F)
pop5$I_race_B <- rbinom(N,1,prob = p)
#Insurance
p <- expit(0 + pop5$I_race_B + 1.8 * pop5$I_sex_F * pop5$I_race_B + 1.2 * pop5$I_age_old * pop5$I_race_B)
pop5$I_ins_A <- rbinom(N,1,prob = p)
pop5 <- pop5 %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
pop5Freq <- pop5 %>% summarise(count = n()/N)
pop5Freq

pop5$income <- 25000 + 
  20000 * pop5$I_age_old +
  20000 * pop5$I_sex_F * pop5$I_race_B + 
  30000 * pop5$I_sex_F * pop5$I_age_old +  
  30000 * pop5$I_age_old * pop5$I_race_B + 
  10000 * pop5$I_sex_F * pop5$I_ins_A +
  50000 * pop5$I_age_old * pop5$I_sex_F * pop5$I_race_B * pop5$I_ins_A +
  rnorm(N,0,5000)

restrial5 <- resfunc(pop5)

resbias5 <- restrial5
resbias5$biasPS <- resbias5$sampPSmean-resbias5$popMean
resbias5$biasrake <- resbias5$samprakemean-resbias5$popMean
resbias5$biasPR <- resbias5$sampPRmean-resbias5$popMean
apply(resbias5,2,mean)

res5 <- subset(gather(restrial5), key != "popMean")
p5 <- ggplot(res5) +
  geom_vline(xintercept = mean(pop5$income), color = "Orange") +
  geom_histogram(aes(x = value, fill = key), binwidth=500) +
  ggtitle("Mostly Strat Samp, Different Pop 1") +
  xlim(60000, 105000)
p5


################ mostly strat samp w/ 0s
pop6 <- data.frame(I_age_old = rbinom(N, 1, 0.5), I_sex_F = 0, I_race_B = 0, I_ins_A = 0)
#sex
p <- expit(0 + 0 * pop6$I_age_old)
pop6$I_sex_F <- rbinom(N,1,prob = p)
#race
p <- expit(0 + 0 * pop6$I_age_old + 0 * pop6$I_age_old * pop6$I_sex_F)
pop6$I_race_B <- rbinom(N,1,prob = p)
#Insurance
p <- expit(0 + 0 * pop6$I_race_B + 0 * pop6$I_sex_F * pop6$I_race_B)
pop6$I_ins_A <- rbinom(N,1,prob = p)

pop6 <- pop6 %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
pop6Freq <- pop6 %>% summarise(count = n()/N)
pop6Freq

pop6$income <- 25000 + 
  20000 * pop6$I_age_old +
  15000 * pop6$I_sex_F +
  10000 * pop6$I_race_B + 
  30000 * pop6$I_sex_F * pop6$I_age_old +  
  30000 * pop6$I_age_old * pop6$I_race_B + 
  10000 * pop6$I_sex_F * pop6$I_ins_A +
  50000 * pop6$I_age_old * pop6$I_sex_F * pop6$I_race_B * pop6$I_ins_A +
  rnorm(N,0,5000)

restrial6 <- resfunc(pop6)

resbias6 <- restrial6
resbias6$biasPS <- resbias6$sampPSmean-resbias6$popMean
resbias6$biasrake <- resbias6$samprakemean-resbias6$popMean
resbias6$biasPR <- resbias6$sampPRmean-resbias6$popMean
apply(resbias6,2,mean)

res6 <- subset(gather(restrial6), key != "popMean")
p6 <- ggplot(res6) +
  geom_vline(xintercept = mean(pop6$income), color = "orange") +
  geom_histogram(aes(x = value, fill = key), binwidth=100) +
  ggtitle("mostly strat samp, even pop") +
  xlim(60000, 105000)
p6

pop6Freq <- pop6 %>% summarise(count = n()/N)
pop6Freq$realcount <- pop6Freq %>% summarise(realcount = n())
pop6Freq

chisq.test(pop6Freq$count,p = rep(1/length(pop6Freq$count), length(pop6Freq$count)))



library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, nrow=2)
