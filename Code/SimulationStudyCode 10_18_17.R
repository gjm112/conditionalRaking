N<-100000
set.seed(89)
pop <- data.frame(I_age_old = rbinom(N,1,0.6),I_sex_F = rbinom(N,1,0.5),I_race_B = rbinom(N,1,0.3))

pop$income <- 25000 + 20000 * pop$I_age_old + 5000 * pop$I_race_B + 10000 * pop$I_sex_F + rnorm(N,0,5000)

logitp <- -2 - 0.00004*pop$income + 0.08 * pop$I_age_old
p <-  exp(logitp)/(1+exp(logitp))
hist(p)

pop$disease <- rbinom(N,1,p)

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
nsim<-1000
results<-list()
for (i in 1:nsim){print(i)

#more likely to be in sample if disease = 1.  
nDis<-floor(nrow(pop[pop$disease==1,])/2)
ind<-sample(1:nrow(pop[pop$disease==1,]),nDis)
sampDis<-(pop[pop$disease==1,])[ind,]

nNonDis<-floor(nrow(pop[pop$disease==0,])/100)
ind<-sample(1:nrow(pop[pop$disease==0,]),nNonDis)
sampNonDis<-(pop[pop$disease==0,])[ind,]

samp <- rbind(sampDis,sampNonDis) 

#poststrat

samp <- samp %>% group_by(I_age_old, I_sex_F, I_race_B)
sampFreq<-samp %>% summarise(count = n()/nrow(samp))

sampFreq$psweight <- popFreq$count/sampFreq$count
samp<-merge(samp,sampFreq, by.x = c("I_age_old","I_sex_F","I_race_B"), by.y = c("I_age_old","I_sex_F","I_race_B"),all.x=TRUE)



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
#######partial raking


parts <- list()
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
  
  anes <- anesrake(subtargets, subsamp, caseid= subsamp$caseid, cap= 20, choosemethod = "total")
  subsamp$rakeweight <- anes$weightvec 
  parts[[j+1]] <- subsamp
}
library(dplyr)
samppr<- do.call(rbind, parts)

results[[i]]<-c(popMean=mean(pop$income), sampMean=mean(samp$income), sampPSmean=weighted.mean(samp$income,samp$psweight), samprakemean=weighted.mean(samp$income,samp$rakeweight))
####sampprmean is only plotting a point- what did I do wrong?
}


res<-as.data.frame(do.call(rbind,results))

hist(res$sampMean,xlim=c(35000,45000))
hist(res$sampPSmean,add=TRUE,col="blue")
hist(res$samprakemean,add=TRUE,col="green")
hist(res$subsampprmean, add= TRUE, col= "orange")
abline(v=mean(pop$income),col="red")



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
