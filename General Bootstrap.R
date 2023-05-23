# Luke Beebe Bootstrap Assignment assignment partners Anish and Tyra
source("/Users/lukebeebe/Documents/School/Rutgers/Spring 2023/Applied Statistical Learning/xy.pck")
xy.pck
special.sample

my.bootstrap<-function(vec0,statfunc,nboot=100){
  n0<-length(vec0)
  stat0<-statfunc(vec0)
  bootvec<-NULL
  for( i in 1:nboot){
    vecb<-sample(vec0,replace=T)
    statb<-statfunc(vecb)
    bootvec<-c(bootvec,statb)
  }
  list(stat0=stat0,bootmean=mean(bootvec),bootvar=var(bootvec))
}

my.bootstrapci <- function(func,vec0,nboot=1000,alpha){
  # creates function, taking parameters vec0, nboot, alpha
  n0<-length(vec0) #calculates n0, length of vec0
  stat0<-func(vec0) #calculates mean of vec0
  sd0<-sqrt(my.bootstrap(vec0,func)$bootvar) #caclulates standard deviation of vec0
  bootvec<-NULL #creates empty vector bootvec
  for(i in 1:nboot){ #loops amount of nboot times
    vecb<-sample(vec0,replace=T) #samples vec0, creating vecb
    statb<-func(vecb) #takes func of sample data
    sdb<-sqrt(my.bootstrap(vecb,func)$bootvar) #takes standard deviation of sample data
    while(sdb==0){ #resamples data if sd=0 to avoid dividing by 0
      vecb<-sample(vec0,replace=T)
      statb<-func(vecb)
      sdb<-sqrt(my.bootstrap(vecb,func)$bootvar)
    }
    bootvec<-c(bootvec,(statb-stat0)/(sdb/sqrt(n0))) #saves (bootvec, t-stat)
  }
  lq<-quantile(bootvec,alpha/2) #finds value of lower quantile from bootvec
  uq<-quantile(bootvec,1-alpha/2) #finds value of upper quantile from bootvec
  LB<-stat0-(sd0/sqrt(n0))*uq #creates lower bound of bootvec
  UB<-stat0-(sd0/sqrt(n0))*lq #creates upper bound of bootvec
  NLB<-stat0-(sd0/sqrt(n0))*qt(1-alpha/2,n0-1) #creates lower bound of vec0
  NUB<-stat0+(sd0/sqrt(n0))*qt(1-alpha/2,n0-1) #creates upper bound of vec0
list(bootstrap.ci=c(LB,UB),bootstrap.ci.range=UB-LB,bootstrap.var=var(bootvec),normal.ci=c(NLB,NUB))
}
my.bootstrapci(median, special.sample, alpha=0.05)
my.bootstrapci(mean, special.sample, alpha=0.05)

median.count=0
mean.count=0
for(i in 1:100){
  if(my.bootstrapci(median,special.sample,alpha=0.05)$bootstrap.ci.range<my.bootstrapci(mean,special.sample,alpha=0.05)$bootstrap.ci.range){
    median.count=median.count+1
  }else(mean.count=mean.count+1)
}
print(median.count)
print(mean.count)

