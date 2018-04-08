library(weights)
library(anesrake)
library(dplyr)
N<-100000
set.seed(89)

expit <- function(x){
  out <- exp(x) / (1+exp(x))
  return(out)
}

pop <- data.frame(I_age_old = rbinom(N, 1, 0.6), I_sex_F = 0, I_race_B = 0, I_ins_A = 0)

#sex
p <- expit(0 + 0.9 * pop$I_age_old)
pop$I_sex_F <- rbinom(N,1,prob = p)

#race
p <- expit(0 + 0.9 * pop$I_age_old + 1.2 * pop$I_age_old * pop$I_sex_F)
pop$I_race_B <- rbinom(N,1,prob = p)

#Insurance
p <- expit(1 + 0.9 * pop$I_age_old + 
             -0.2 * pop$I_sex_F +
             -2.5 * pop$I_race_B * pop$I_sex_F)
pop$I_ins_A <- rbinom(N,1,prob = p)

pop <- pop %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
popFreq <- pop %>% summarise(count = n()/N)
#popFreq


pop$income <- 25000 + 
  50000 * pop$I_age_old + 25000 * pop$I_race_B + 10000 * pop$I_sex_F + 
  30000 * pop$I_sex_F * pop$I_race_B + 
  30000 * pop$I_sex_F * pop$I_age_old +  
  30000 * pop$I_age_old * pop$I_race_B + 
  10000 * pop$I_sex_F * pop$I_ins_A +
  50000 * pop$I_age_old * pop$I_sex_F * pop$I_race_B * pop$I_ins_A +
  rnorm(N,0,5000)
#

#apop <- lm(income~I_age_old+pop$I_sex_F+pop$I_race_B+pop$I_ins_A,data=pop)



############################################
#Sample statistics
############################################
#k<-10000
#simple random sample
#ind<-sample(1:N,k)
#samp<- pop[ind,]
nsim<-100
results<-list()
resultsLM <- list()
resultsLM[["lmRaw"]] <- resultsLM[["lmPS"]] <- resultsLM[["lmRake"]] <- resultsLM[["lmPR"]] <- matrix(NA, ncol=5,nrow=nsim)
for (i in 1:nsim){ 
  #print(i)
  
  sampList <- list()
  
  samp <= NULL
  # # dplyr's sample_n lets you grab rows. but does it replace??
  strat1 <- sample_n(pop[pop$I_age_old==0 & pop$I_sex_F == 0,], 100, weight=NULL, replace = FALSE)
  strat2 <- sample_n(pop[pop$I_age_old==0 & pop$I_sex_F == 1,], 100, weight=NULL, replace = FALSE)
  strat3 <- sample_n(pop[pop$I_age_old==1 & pop$I_sex_F == 0,], 100, weight=NULL, replace = FALSE)
  strat4 <- sample_n(pop[pop$I_age_old==1 & pop$I_sex_F == 1,], 100, weight=NULL, replace = FALSE)
  samp <- do.call(rbind, list(strat1, strat2, strat3, strat4))
  
  samp <- do.call(rbind,sampList)
  
  samp <- samp %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
  sampFreq <- samp %>% summarise(count = n()/nrow(samp))
  sampFreq
  
  #samp <- samp[sample(1:nrow(samp),100),]
  
  #poststrat
  samp <- samp %>% group_by(I_age_old, I_sex_F, I_race_B, I_ins_A)
  sampFreq<- samp %>% summarise(count = n()/nrow(samp))
  
  sampFreq$psweight <- popFreq$count/sampFreq$count
  samp<-merge(samp,sampFreq, by.x = c("I_age_old","I_sex_F","I_race_B", "I_ins_A"), by.y = c("I_age_old","I_sex_F","I_race_B", "I_ins_A"),all.x=TRUE)
  
  
  
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
  
  pop <- pop %>% 
    mutate(I_sex_race = ifelse(I_sex_F ==0 & I_race_B ==0, 1,
                               ifelse(I_sex_F ==0 & I_race_B == 1, 2,
                                      ifelse(I_sex_F==1 & I_race_B ==0, 3, 4))))
  
  
  for(j in 0:1){
    subpop <- subset(pop, I_age_old==j)
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

apply(resultsLM[["lmRaw"]],2,mean)
apply(resultsLM[["lmPS"]],2,mean)
apply(resultsLM[["lmRake"]],2,mean)
apply(resultsLM[["lmPR"]],2,mean)


res<-as.data.frame(do.call(rbind,results))

res$biasPS <- res$sampPSmean-res$popMean
res$biasrake <- res$samprakemean-res$popMean
res$biasPR <- res$sampPRmean-res$popMean
apply(res,2,mean)

hist(res$sampMean,xlim=c(57000,63000))
hist(res$sampPSmean,add=TRUE,col="blue")
hist(res$samprakemean,add=TRUE,col="green")
hist(res$sampPRmean, add= TRUE, col= "orange")
abline(v=mean(pop$income),col="red",lwd=3)






#######
library(anesrake)

sexF <- c(.5,.5)
AgeO <- c(.6, .4)
RaceB <- c(.3, .7)

trueage <- wpct(pop$I_age_old)
truesex <- wpct(pop$I_sex_F)
truerace <- wpct(pop$I_race_B)
targets <- list(trueage, truesex, truerace)
names(targets) <- c("I_age_old", "I_sex_F", "I_race_B")

#anesrakefinder(targets, samp, choosemethod = "total")

samp$caseid <- 1:nrow(samp)

samp$I_age_old <- as.factor(samp$I_age_old)
samp$I_sex_F <- as.factor(samp$I_sex_F)
samp$I_race_B <- as.factor(samp$I_race_B)

anes <- anesrake(targets, samp, caseid= samp$caseid, cap= 20, choosemethod = "total")
samp$rakeweight <- anes$weightvec 


library(dplyr)
samppr<- do.call(rbind, parts)
samppr<- group_by(samppr, I_sex_F, I_race_B)
samppr%>%summarise(wt = sum(rakeweight))
