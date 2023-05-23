# Luke Beebe Bootstrap Assignment assignment partners Anish and Tyra

my.bootstrapci <- function(vec0,nboot=1000,alpha){
  # creates function, taking parameters vec0, nboot, alpha
  n0<-length(vec0) #calculates n0, length of vec0
  mean0<-mean(vec0) #calculates mean of vec0
  sd0<-sqrt(var(vec0)) #caclulates standard deviation of vec0
  bootvec<-NULL #creates empty vector bootvec
  for(i in 1:nboot){ #loops amount of nboot times
    vecb<-sample(vec0,replace=T) #samples vec0, creating vecb
    meanb<-mean(vecb) #takes mean of sample data
    sdb<-sqrt(var(vecb)) #takes standard deviation of sample data
    while(sdb==0){ #resamples data if sd=0 to avoid dividing by 0
      vecb<-sample(vec0,replace=T)
      meanb<-mean(vecb)
      sdb<-sqrt(var(vecb))
    }
    bootvec<-c(bootvec,(meanb-mean0)/(sdb/sqrt(n0))) #saves (bootvec, t-stat)
  }
  lq<-quantile(bootvec,alpha/2) #finds value of lower quantile from bootvec
  uq<-quantile(bootvec,1-alpha/2) #finds value of upper quantile from bootvec
  LB<-mean0-(sd0/sqrt(n0))*uq #creates lower bound of bootvec
  UB<-mean0-(sd0/sqrt(n0))*lq #creates upper bound of bootvec
  NLB<-mean0-(sd0/sqrt(n0))*qt(1-alpha/2,n0-1) #creates lower bound of vec0
  NUB<-mean0+(sd0/sqrt(n0))*qt(1-alpha/2,n0-1) #creates upper bound of vec0
list(bootstrap.confidence.interval=c(LB,UB),normal.confidence.interval=c(NLB,NUB))
#outputs CIs of bootstrap vector (vecb) and original vector (vec0)
}

sim.func <- function(mu.val=3,n=3,nsim=1000,alpha.sim=0.1){
  #creates function, takes parameters
  cvec.boot<-NULL #creates two vectors
  cvec.norm<-NULL
  mulnorm<-(exp(mu.val+1/2)) #e to the power of (mu.val+1/2)
  for(i in 1:nsim){
    if((i/50)==floor(i/50)){
      print(i) #prints every 50 iterations, letting us know where the program is
    }
    vec.sample<-rlnorm(n,mu.val) #creates vector of n samples (this will become vec0), with mu=mu.val
    boot.list<-my.bootstrapci(vec.sample,alpha=alpha.sim) #passes vector into my.bootstrapci, saves list output of CI values
    boot.conf<-boot.list$bootstrap.confidence.interval #saves bootstrap CI values
    norm.conf<-boot.list$normal.confidence.interval #saves normal CI values
    cvec.boot<-c(cvec.boot,(boot.conf[1]<mulnorm)*(boot.conf[2]>mulnorm))
    #if original mean is within the bootstrap CI, saves as 1. Else, 0
    cvec.norm<-c(cvec.norm,(norm.conf[1]<mulnorm)*(norm.conf[2]>mulnorm))
    #if original mean is within normal CI, saves as 1. Else, 0
  }
  list(boot.coverage=(sum(cvec.boot)/nsim),norm.coverage=(sum(cvec.norm)/nsim))
  #outputs list of true percentage of times the bootstrap and normal CIs contained the original mean
}

sim.func(n=3,nsim=1000,alpha.sim=0.1)
sim.func(n=10,nsim=1000,alpha.sim=0.1)
sim.func(n=30,nsim=1000,alpha.sim=0.1)
sim.func(n=100,nsim=1000,alpha.sim=0.1)
sim.func(n=3,nsim=1000,alpha.sim=0.05)
sim.func(n=10,nsim=1000,alpha.sim=0.05)
sim.func(n=30,nsim=1000,alpha.sim=0.05)
sim.func(n=100,nsim=1000,alpha.sim=0.05)
boot<-c(0.718,0.837,0.872,0.878,0.747,0.916,0.93,0.938)
norm<-c(0.772,0.764,0.825,0.864,0.838,0.846,0.879,0.931)
nalpha<-c("3","10","30","100","3","10","30","100")
boot.norm.mat<-cbind(boot,norm)
boot.norm.mat
matplot(boot.norm.mat,type='b',ylab="accuracy",xlab="sample size\nalpha=0.1, 0.05",main="Bootstrap (1) vs Normal (2) accuracy",xaxt='n')
axis(1, at=1:8,labels=nalpha)
?matplot
