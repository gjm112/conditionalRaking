library(weights)
library(anesrake)
N<-100000
set.seed(89)

pop <- data.frame(I_age_old = rbinom(N, 1, 0.6), I_sex_F = 0, I_race_B = 0)


pop$I_sex_F[pop$I_age_old==1] <- rbinom(sum(pop$I_age_old==1),1,0.1)
pop$I_sex_F[pop$I_age_old==0] <- rbinom(sum(pop$I_age_old==0),1,0.9)

# pop$I_race_B[pop$I_age_old==1 & pop$I_sex_F==1] <- rbinom(sum(pop$I_age_old==1 & pop$I_sex_F==1),1,0.2)
# pop$I_race_B[pop$I_age_old==1 & pop$I_sex_F==0] <- rbinom(sum(pop$I_age_old==1 & pop$I_sex_F==0),1,0.4)
# pop$I_race_B[pop$I_age_old==0 & pop$I_sex_F==1] <- rbinom(sum(pop$I_age_old==0 & pop$I_sex_F==1),1,0.9)
# pop$I_race_B[pop$I_age_old==0 & pop$I_sex_F==0] <- rbinom(sum(pop$I_age_old==0 & pop$I_sex_F==0),1,0.8)

pop$I_race_B[pop$I_age_old==1 & pop$I_sex_F==1] <- rbinom(sum(pop$I_age_old==1 & pop$I_sex_F==1),1,0.1)
pop$I_race_B[pop$I_age_old==1 & pop$I_sex_F==0] <- rbinom(sum(pop$I_age_old==1 & pop$I_sex_F==0),1,0.9)
pop$I_race_B[pop$I_age_old==0 & pop$I_sex_F==1] <- rbinom(sum(pop$I_age_old==0 & pop$I_sex_F==1),1,0.9)
pop$I_race_B[pop$I_age_old==0 & pop$I_sex_F==0] <- rbinom(sum(pop$I_age_old==0 & pop$I_sex_F==0),1,0.1)

pop$income <- 25000 + 50000 * pop$I_age_old + 25000 * pop$I_race_B + 10000 * pop$I_sex_F + 30000 * pop$I_sex_F*pop$I_race_B + 30000 * pop$I_sex_F*pop$I_age_old +  30000 * pop$I_age_old*pop$I_race_B + 50000 * pop$I_sex_F*pop$I_race_B*pop$I_age_old + rnorm(N,0,5000)
#

apop <- lm(income~I_age_old+pop$I_sex_F+pop$I_race_B,data=pop)

library(dplyr)
pop <- pop %>% group_by(I_age_old, I_sex_F, I_race_B)
popFreq <- pop %>% summarise(count = n()/N)


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
for (i in 1:nsim){print(i)
  
  sampList <- list()
  
  ind <- sample(1:sum(pop$I_age_old==1 & pop$I_sex_F==1),100,replace=FALSE)
  sampList[[1]] <- pop[pop$I_age_old==1 & pop$I_sex_F==1,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==1 & pop$I_sex_F==0),100,replace=FALSE)
  sampList[[2]] <- pop[pop$I_age_old==1 & pop$I_sex_F==0,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==0 & pop$I_sex_F==1),100,replace=FALSE)
  sampList[[3]] <- pop[pop$I_age_old==0 & pop$I_sex_F==1,][ind,]
  
  ind <- sample(1:sum(pop$I_age_old==0 & pop$I_sex_F==0),100,replace=FALSE)
  sampList[[4]] <- pop[pop$I_age_old==0 & pop$I_sex_F==0,][ind,]
  
  samp <- do.call(rbind,sampList)
  #samp <- samp[sample(1:nrow(samp),100),]
  
  #poststrat
  samp <- samp %>% group_by(I_age_old, I_sex_F, I_race_B)
  sampFreq<- samp %>% summarise(count = n()/nrow(samp))
  
  sampFreq$psweight <- popFreq$count/sampFreq$count
  samp<-merge(samp,sampFreq, by.x = c("I_age_old","I_sex_F","I_race_B"), by.y = c("I_age_old","I_sex_F","I_race_B"),all.x=TRUE)
  
  
  
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
  
  
  for(j in 0:1){
    subpop <- subset(pop, I_age_old==j)
    truesexsub <- wpct(subpop$I_sex_F)
    trueracesub <- wpct(subpop$I_race_B)
    subtargets <- list(truesexsub, trueracesub)
    names(subtargets) <- c("I_sex_F", "I_race_B")
    
    subsamp<- subset(samp, I_age_old==j)
    
    
    subsamp$caseid <-1:nrow(subsamp)
    subsamp$I_sex_F <- as.factor(subsamp$I_sex_F)
    subsamp$I_race_B <- as.factor(subsamp$I_race_B)
    
    
    anes <- anesrake(subtargets, subsamp, caseid= subsamp$caseid, cap=20, choosemethod = "total", weightvec=subsamp$prweight,center.baseweights = FALSE)
    
    samp$prweight[samp$I_age_old==j] <- anes$weightvec
    
  }
  
  #samp %>% group_by(I_age_old, I_sex_F, I_race_B) %>% summarize(rake=mean(rakeweight),ps = mean(psweight),pr  = mean(prweight) ,n=n())
  
  results[[i]]<-c(popMean=mean(pop$income), sampMean=mean(samp$income), sampPSmean=weighted.mean(samp$income,samp$psweight), samprakemean=weighted.mean(samp$income,samp$rakeweight),sampPRmean = weighted.mean(samp$income,samp$prweight))
  
  pop_model <- lm(income~I_age_old+I_sex_F+I_race_B,data=pop)
  # resultsLM[["lmRaw"]][i,] <- lm(income~I_age_old+I_sex_F+I_race_B,data=samp)$coefficients
  # resultsLM[["lmPS"]][i,] <- lm(income~I_age_old+I_sex_F+I_race_B,data=samp,weight=psweight)$coefficients
  # resultsLM[["lmRake"]][i,] <- lm(income~I_age_old+I_sex_F+I_race_B,data=samp,weight=rakeweight)$coefficients
  # resultsLM[["lmPR"]][i,] <-lm(income~I_age_old+I_sex_F+I_race_B,data=samp,weight=prweight)$coefficients
  
  resultsLM[["lmRaw"]][i,] <- lm(income~I_age_old*I_sex_F + I_age_old*I_race_B + I_sex_F*I_race_B,data=samp)$coefficients
  resultsLM[["lmPS"]][i,] <- lm(income~I_age_old*I_sex_F + I_age_old*I_race_B + I_sex_F*I_race_B,data=samp,weight=psweight)$coefficients
  resultsLM[["lmRake"]][i,] <- lm(income~I_age_old*I_sex_F + I_age_old*I_race_B + I_sex_F*I_race_B,data=samp,weight=rakeweight)$coefficients
  resultsLM[["lmPR"]][i,] <-lm(income~I_age_old*I_sex_F + I_age_old*I_race_B + I_sex_F*I_race_B,data=samp,weight=prweight)$coefficients

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

hist(res$sampMean,xlim=c(60000,120000))
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
