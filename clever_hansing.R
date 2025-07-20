#Let us say that i am interested in mating behavior of young adults (College students) I have access to 300 participant that were recruited at my university
n<-300

#Questions per mating context, higher score means that the people would now like to have a relationship of that type. They answer on a likert scale from 1 to 7. As you can see, from how data are generated, there is no difference between participants and the contexts.
q <- 10

STM<-matrix(1+rbinom(n*q,size=6,prob=0.4),ncol=q)
LTM<-matrix(1+rbinom(n*q,size=6,prob=0.4),ncol=q)

#Introduce outliers
STM[sample(1:n,n/10),]<-7
LTM[sample(1:n,n/10),]<-1

#Introduce missing values
STM[sample(1:(n*q),n/10)]<-NA
LTM[sample(1:(n*q),n/10)]<-NA

#Age (these are bachelor students, so they should be about 20 years old, but there will be otliers)
age<-12+replicate(n,sum(rexp(3,rate=0.25)))
hist(age)

#Sex
sex<-sample(c("M","F"),n,replace=T)

#Sexual orientation
ori<-sample(c("Het","Hom"),n,prob=c(6,1),replace=T)

#Relationship status
stat<-sample(c("Single","Paired","Married"),n,prob=c(3,3,1),replace=T)

#Also I have data about how long they took to submit responses
resp.time<-replicate(n,sum(rexp(20,rate=5)))+rbinom(n,1,p=0.05)*rexp(n,rate=1/(60*24))


#Now I want to analyze the data
#Just a quick preliminary check
stm<-rowMeans(STM,na.rm=T)

t.test(stm~sex)
summary(lm(stm~sex))

#But is this the only way?

sum(is.na(STM))
#First of all I have some missing data and I have to think about what to do with them
#Some people would replace them with global averages or by-sex averages
STMavg<-matrix(rep(apply(STM,2,mean,na.rm=T),each=n),ncol=q)
STMsex<-t(sapply(sex,function(s){apply(STM[sex==s,],2,mean,na.rm=T)}))

STM2<-STM1<-STM

STM1[is.na(STM)]<-STMavg[is.na(STM)] #Fill by global averages
STM2[is.na(STM)]<-STMsex[is.na(STM)] #Fill by by-sex averages

#And I should do the same with a Long-Term Mating context! I will use this as a reference baseline for STM.
LTMavg<-matrix(rep(apply(LTM,2,mean,na.rm=T),each=n),ncol=q)
LTMsex<-t(sapply(sex,function(s){apply(LTM[sex==s,],2,mean,na.rm=T)}))

LTM2<-LTM1<-LTM

LTM1[is.na(LTM)]<-LTMavg[is.na(LTM)]
LTM2[is.na(LTM)]<-LTMsex[is.na(LTM)]

#And now I can try 9 options in total. 3 ways to how missing data are treated, 3 ways how (relative) preference for Short Term Mating in calculated

#1. Just average the responses per context, do not fill NAs
stm<-rowMeans(STM,na.rm=T) #Just STM
summary(lm(stm~sex))

stm<-rowMeans(STM,na.rm=T)-rowMeans(LTM,na.rm=T) #Difference between STM and LTM
summary(lm(stm~sex))

stm<-rowMeans(STM,na.rm=T)/rowMeans(LTM,na.rm=T) #Ratio of STM and LTM
summary(lm(stm~sex))


#2. Filled averages by item
stm<-rowMeans(STM1,na.rm=T)
summary(lm(stm~sex))

stm<-rowMeans(STM1,na.rm=T)-rowMeans(LTM1,na.rm=T)
summary(lm(stm~sex))

stm<-rowMeans(STM1,na.rm=T)/rowMeans(LTM1,na.rm=T)
summary(lm(stm~sex))


#3. Filled averares by item by sex
stm<-rowMeans(STM2,na.rm=T)
summary(lm(stm~sex))

stm<-rowMeans(STM2,na.rm=T)-rowMeans(LTM2,na.rm=T)
summary(lm(stm~sex))

stm<-rowMeans(STM2,na.rm=T)/rowMeans(LTM2,na.rm=T)
summary(lm(stm~sex))


#Use whatever option that had smallest p-value
stm<-rowMeans(STM2,na.rm=T)

#And create a dataset
d<-data.frame(ID=1:length(stm),stm,sex,ori,age,stat,resp.time)

#How many missing values did I actually have?
d$miss<-rowSums(is.na(cbind(STM,LTM)))

summary(as.factor(d$miss))

d1<-d #Keep all
d2<-d[d$miss<2,] #Keep some (with only one missing answer)
d3<-d[d$miss==0,] #Keep complete

summary(lm(stm~sex,data=d1))
summary(lm(stm~sex,data=d2))
summary(lm(stm~sex,data=d3))

d<-d3 #Again, a tough decision. Just to be sure, just include data point that do ot ruin your results, what can go wrong?

#Oh! And by the way, some people took too much time to complete the questionnaire! Do you think their responses are reliable
hist(d$resp.time,breaks=60)
hist(log(d$resp.time))

#Let us say that 20 questions should not take longer than an hour, right?
d1<-d #Keep all
d2<-d[d$resp.time<60,] #Keep those that took less than an hour

summary(lm(stm~sex,data=d1))
summary(lm(stm~sex,data=d2))

d<-d2 #Again, a tough decision is to be made

#And we have nice and relibale data
#But wait a minute. Some individuals may not be heterosexual and we have this information!
summary(as.factor(d$ori))

#That to do with them?
d1<-d #Keep
d2<-d[d$ori=="Het",] #Discard

#Analyze by partner sex
d3<-d
d3$sex<-ifelse(d3$ori=="Hom",d3$sex,ifelse(d3$sex=="M","F","M"))

summary(lm(stm~sex,data=d1))
summary(lm(stm~sex,data=d2))
summary(lm(stm~sex,data=d3))

d<-d1 #The tough decision

#What about the age? You wanted to study college students, right?
#Age
hist(d$age)
range(d$age) #There are some young prodigies and some relatively old people..

d1<-d #keep all
d2<-d[d$age>18,] #At least adults
d3<-d[d$age<30,] #keep young enough
d4<-d[d$age>18 & d$age<30,] #both

summary(lm(stm~sex,data=d1))
summary(lm(stm~sex,data=d2))
summary(lm(stm~sex,data=d3))
summary(lm(stm~sex,data=d4))
  
d<-d1 #The tough decision

#But what if the relationship status is playing some tricks with the preferences?
#Relationship status
d1<-d #keep all
d2<-d[d$stat=="Single",] #Keep just singles
d3<-d[d$stat=="Paired",] #Keep just paired
d4<-d[d$stat!="Married",] #Just discard married

summary(lm(stm~sex,data=d1))
summary(lm(stm~sex,data=d2))
summary(lm(stm~sex,data=d3))
summary(lm(stm~sex,data=d4))

d<-d1 #The tough decision

#What about outliers?
boxplot(d$stm)

outs <- boxplot.stats(d$stm)$out

d1 <- d #keep all
d2 <- d[!d$stm %in% outs,] #Discard outliers
d3 <- d[d$stm < 6,] #Discard just the absolute extremes

summary(lm(stm~sex,data=d1))
summary(lm(stm~sex,data=d2))
summary(lm(stm~sex,data=d3))

d<-d1 #The tough decision

#What about the other extreme?
d1 <- d #do nothing
d2 <- d[d$stm!=1,] #Discard homogeneous other extreme

#Discard all participants that had 0 variance in one of the blocks, probably they were not paying attention and are unreliable
vSTM0 <-apply(STM[d$ID,],1,sd,na.rm=T)==0
vLTM0 <-apply(LTM[d$ID,],1,sd,na.rm=T)==0

d3 <- d[!vSTM0 & !vLTM0, ] #Discard all with 0 variance in STM or LTM

summary(lm(stm~sex,data=d1))
summary(lm(stm~sex,data=d2))
summary(lm(stm~sex,data=d3))

d<-d1 #The tough decision

#But what about the shape of the distribution?
#transform
plot(density(d$stm))
plot(density(log(d$stm)))
plot(density(sqrt(d$stm)))

d->d1->d2->d3

d1<-d1 #do nothing
d2$stm <- log(d$stm) #log transform
d3$stm <- sqrt(d$stm) #sqrt transform

summary(lm(stm~sex,data=d1))
summary(lm(stm~sex,data=d2))
summary(lm(stm~sex,data=d3))

d<-d2 #Here is your final data! Congrats!

summary(lm(stm~sex,data=d))


#Ok! Now you write the paper and submit it somewhere. But there is a nitpicking reviewer that points out that your dataset comes from two universities! One technical and one oriented on humanities! He wants you to analyze the data separately, just to see what happens...

d$school<-sample(c("Tech","Hum"), nrow(d), replace = TRUE, prob = c(0.5, 0.5))

#Let us see
summary(lm(stm~sex,data=d[d$school=="Tech", ]))
summary(lm(stm~sex,data=d[d$school=="Hum", ]))

#We can get a similar picture from the model with interaction
summary(lm(stm~sex*school,data=d)) #How would you interpret the results?




