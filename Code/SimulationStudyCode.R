N<-10000
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
#k<-250
#simple random sample
#ind<-sample(1:N,k)
#samp<- pop[ind,]
nsim<-100
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

samp <- samp %>% group_by(I_age_old, I_sex_F, I_race_B)
sampFreq<-samp %>% summarise(count = n()/nrow(samp))

sampFreq$weight <- popFreq$count/sampFreq$count
samp<-merge(samp,sampFreq, by.x = c("I_age_old","I_sex_F","I_race_B"), by.y = c("I_age_old","I_sex_F","I_race_B"),all.x=TRUE)

results[[i]]<-c(popMean=mean(pop$income), sampMean=mean(samp$income), sampPSmean=weighted.mean(samp$income,samp$weight))

}


res<-as.data.frame(do.call(rbind,results))

hist(res$sampMean,xlim=c(35000,45000))
hist(res$sampPSmean,add=TRUE,col="blue")
abline(v=mean(pop$income),col="red")



