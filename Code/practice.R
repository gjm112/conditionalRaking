library(weights)
library(anesrake)
N<-100000
set.seed(89)
pop <- data.frame(I_age_old = rbinom(N,1,0.6), I_sex_F = 0, I_race_B = 0)
pop$I_sex_F[pop$I_age_old==1] <- rbinom(sum(pop$I_age_old==1),1, 0.1)
pop$I_sex_F[pop$I_age_old==0] <- rbinom(sum(pop$I_age_old==0),1, 0.9)   ##sex suuuper dependent on age

pop$I_race_B[pop$I_age_old ==0 & pop$I_sex_F == 0] <- rbinom(sum(pop$I_age_old ==0 & pop$I_sex_F == 0), 1, 0.8)
pop$I_race_B[pop$I_age_old ==0 & pop$I_sex_F == 1] <- rbinom(sum(pop$I_age_old ==0 & pop$I_sex_F == 1), 1, 0.9)   
pop$I_race_B[pop$I_age_old ==1 & pop$I_sex_F == 0] <- rbinom(sum(pop$I_age_old ==1 & pop$I_sex_F == 0), 1, 0.1)
pop$I_race_B[pop$I_age_old ==1 & pop$I_sex_F == 1] <- rbinom(sum(pop$I_age_old ==1 & pop$I_sex_F == 1), 1, 0.1)
##race suuuper dependent on age and sex

pop$income <- 25000 + 50000 * pop$I_age_old + 5000 * pop$I_race_B + 10000 * pop$I_sex_F + rnorm(N,0,5000) + 30000 * pop$I_sex_F * pop$I_race_B
       ## try and make huge diff in income with different ages, races, sexes, etc.
        ## check with boxplots
      ## takeing out interactions for now (for simpler model)


##stratified sampling
    #based on age and sex --> get 4 strata and then do some egregious sampling 
    ## sample 100 from each (super diff numbers in each group so you get crazy disproportionate sampling)
    #do.call rbinds everything together so that you don't have to do a loop

# # dplyr's sample_n lets you grab rows
# strat1 <- sample_n(pop[pop$I_age_old==0 & pop$I_sex_F == 0,], 100)
# strat2 <- sample_n(pop[pop$I_age_old==0 & pop$I_sex_F == 1,], 100)
# strat3 <- sample_n(pop[pop$I_age_old==1 & pop$I_sex_F == 0,], 100)
# strat4 <- sample_n(pop[pop$I_age_old==1 & pop$I_sex_F == 1,], 100)
# samp <- rbind(strat1, strat2, strat3, strat4)

# ##
#p_samp <- do.call(rbind, sample(pop[pop$I_age_old ==0 & pop$I_sex_F == 0], 10))

# logitp <- 0 - 0.00004*pop$income + 0.08 * pop$I_age_old - 0.08 * pop$I_sex_F + 0.1 * pop$I_race_B
# p <-  exp(logitp)/(1+exp(logitp))
# hist(p)


apop<-lm(income~I_age_old+pop$I_sex_F+pop$I_race_B,data=pop)

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
s_size <- 1000
nsim<-100
results<-list()
for (i in 1:nsim){#print(i)
  pop <- data.frame(I_age_old = rbinom(N,1,0.6), I_sex_F = 0, I_race_B = 0)
  pop$I_sex_F[pop$I_age_old==1] <- rbinom(sum(pop$I_age_old==1),1, 0.1)
  pop$I_sex_F[pop$I_age_old==0] <- rbinom(sum(pop$I_age_old==0),1, 0.9)   ##sex suuuper dependent on age
  
  pop$I_race_B[pop$I_age_old ==0 & pop$I_sex_F == 0] <- rbinom(sum(pop$I_age_old ==0 & pop$I_sex_F == 0), 1, 0.8)
  pop$I_race_B[pop$I_age_old ==0 & pop$I_sex_F == 1] <- rbinom(sum(pop$I_age_old ==0 & pop$I_sex_F == 1), 1, 0.9)   ##race suuuper dependent on age and sex
  pop$I_race_B[pop$I_age_old ==1 & pop$I_sex_F == 0] <- rbinom(sum(pop$I_age_old ==1 & pop$I_sex_F == 0), 1, 0.4)
  pop$I_race_B[pop$I_age_old ==1 & pop$I_sex_F == 1] <- rbinom(sum(pop$I_age_old ==1 & pop$I_sex_F == 1), 1, 0.2)
  
  pop$income <- 25000 + 50000 * pop$I_age_old + 5000 * pop$I_race_B + 10000 * pop$I_sex_F + rnorm(N,0,5000)
  mean(pop$income)
  sampList <- list()
  
  #pop[pop$I_age_old==1 & pop$I_sex_F==1,]
  ind <- sample(1:sum(pop$I_age_old==1 & pop$I_sex_F==1), s_size, replace=FALSE)
  sampList[[1]] <- pop[pop$I_age_old==1 & pop$I_sex_F==1,][ind,]
  
  #pop[pop$I_age_old==1 & pop$I_sex_F==0,]
  ind <- sample(1:sum(pop$I_age_old==1 & pop$I_sex_F==0), s_size, replace=FALSE)
  sampList[[2]] <- pop[pop$I_age_old==1 & pop$I_sex_F==0,][ind,]
  
  #pop[pop$I_age_old==0 & pop$I_sex_F==1,]
  ind <- sample(1:sum(pop$I_age_old==0 & pop$I_sex_F==1), s_size, replace=FALSE)
  sampList[[3]] <- pop[pop$I_age_old==0 & pop$I_sex_F==1,][ind,]
  
  #pop[pop$I_age_old==0 & pop$I_sex_F==0,]
  ind <- sample(1:sum(pop$I_age_old==0 & pop$I_sex_F==0), s_size, replace=FALSE)
  sampList[[4]] <- pop[pop$I_age_old==0 & pop$I_sex_F==0,][ind,]
  
  samp <- do.call(rbind, sampList)
  
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
  #pop_model<-lm(income~I_age_old+I_sex_F+I_race_B,data=pop)
  #a<-lm(income~I_age_old+I_sex_F+I_race_B,data=samp)
  #b<-lm(income~I_age_old+I_sex_F+I_race_B,data=samp,weight=psweight)
  #c<-lm(income~I_age_old+I_sex_F+I_race_B,data=samp,weight=rakeweight)
  #d<-lm(income~I_age_old+I_sex_F+I_race_B,data=samp,weight=prweight)
  ####sampprmean is only plotting a point- what did I do wrong?
}


res<-as.data.frame(do.call(rbind,results))

res$biasPS <- res$sampPSmean-res$popMean
res$biasrake <- res$samprakemean-res$popMean
res$biasPR <- res$sampPRmean-res$popMean
apply(res,2,mean)

hist(res$sampMean,xlim=c(55000,65000), main="+ interactions")
hist(res$sampPSmean,add=TRUE,col="blue")
hist(res$samprakemean,add=TRUE,col="green")
hist(res$sampPRmean, add= TRUE, col= "orange")
abline(v=mean(pop$income),col="red")

abline(v=mean(res$sampPSmean), col = "blue")
abline(v=mean(res$samprakemean),col="green")
abline(v=mean(res$sampPRmean), col= "orange")
legend("topright",inset=c(-0.2,0), c("Sample", "Post-Strat", "Rake", "PartialR", "Pop"), fill = c("white", "blue", "green", "orange", "red"))

# all_means <- list( c("Pop", "PS", "PR", "R", "S"),
#   c(mean(pop$income), mean(res$sampPSmean), mean(res$sampPRmean),mean(res$samprakemean), mean(res$sampMean)))
# all_means

# Default ended up with: (.8, .9, .4, .2)
# I_age_old I_sex_F I_race_B   count
# <int>   <dbl>    <dbl>   <dbl>
#   1         0       0        0 0.00738
# 2         0       0        1 0.03205
# 3         0       1        0 0.03675
# 4         0       1        1 0.32420
# 5         1       0        0 0.32295
# 6         1       0        1 0.21580
# 7         1       1        0 0.04884
# 8         1       1        1 0.01203

popFreq
sampFreq
#smaller joint ( )
# I_age_old I_sex_F I_race_B   count  psweight
# <int>   <dbl>    <dbl>   <dbl>     <dbl>
# I_age_old I_sex_F I_race_B   count
# <int>   <dbl>    <dbl>   <dbl>
#   1         0       0        0 0.00738
# 2         0       0        1 0.03205
# 3         0       1        0 0.03675
# 4         0       1        1 0.32420
# 5         1       0        0 0.32295
# 6         1       0        1 0.21580
# 7         1       1        0 0.05498
# 8         1       1        1 0.00589



# 
# 
# #######
# library(anesrake)
# 
# sexF <- c(.5,.5)
# AgeO <- c(.6, .4)
# RaceB <- c(.3, .7)
# 
# trueage <- wpct(pop$I_age_old)
# truesex <- wpct(pop$I_sex_F)
# truerace <- wpct(pop$I_race_B)
# targets <- list(trueage, truesex, truerace)
# names(targets) <- c("I_age_old", "I_sex_F", "I_race_B")
# 
# #anesrakefinder(targets, samp, choosemethod = "total")
# 
# samp$caseid <- 1:nrow(samp)
# 
# samp$I_age_old <- as.factor(samp$I_age_old)
# samp$I_sex_F <- as.factor(samp$I_sex_F)
# samp$I_race_B <- as.factor(samp$I_race_B)
# 
# anes <- anesrake(targets, samp, caseid= samp$caseid, cap= 20, choosemethod = "total")
# samp$rakeweight <- anes$weightvec 
# 
# 
# library(dplyr)
# samppr<- do.call(rbind, parts)
# samppr<- group_by(samppr, I_sex_F, I_race_B)
# samppr%>%summarise(wt = sum(rakeweight))