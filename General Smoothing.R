#Assignment 6

NOAA.new<-read.csv("/Users/lukebeebe/Documents/School/Rutgers/Spring 2023/Applied Statistical Learning/NOAAnew.csv")
source("/Users/lukebeebe/Documents/School/Rutgers/Spring 2023/Applied Statistical Learning/smoother.pck")
smoother.pck

bin.mean<-function(x,y,theta,nnn,nbin,xcol=2,do.plot=T)
{
  o1<-order(x)
  x1<-x[o1]
  y1<-y[o1]
  r1<-range(x)
  inc<-(r1[2]-r1[1])/nbin
  yvec<-NULL
  smat<-NULL
  for(i in 1:nbin){
    bin.low<-r1[1]+(i-1)*inc
    bin.high<-r1[1]+i*inc
    I1<-x1>=bin.low
    if(i<nbin){
      I2<-x1<bin.high
    }else{
      I2<-x1<=(bin.high+200)
    }
    I3<-as.logical(I1*I2)
    yval<-mean(y1[I3])
    n1<-sum(I3)
    matdum<-NULL
    for(i in 1:n1){
      matdum<-rbind(matdum,I3*1/n1)
    }
    smat<-rbind(smat,matdum)
    yvec<-c(yvec,rep(yval,n1))
  }
  n99<-length(x1)
  dferror<-length(x1)-sum(diag(2*smat-smat%*%(t(smat))))
  delta1<-sum(diag(t(diag(n99)-smat)%*%(diag(n99)-smat)))
  R<-t(diag(n99)-smat)%*%(diag(n99)-smat)
  delta2<-2*sum(diag(R%*%R))
  if(do.plot==T){lines(x1,yvec,col=xcol)}
  ypred<-y1
  ypred<-smat%*%y1
  resid<-y-ypred
  PRESS<-sum((resid/(1-diag(smat)))^2)
  list(smat=smat,df=sum(diag(smat)),dferror=dferror,delta1=delta1,delta2=delta2,resid=resid,pred=ypred,x=x,press=PRESS)
}


gauss.mean<-function(x,y,lambda,nnn,nbin,xcol=3,do.plot=T)
{
  o1<-order(x)
  x1<-x[o1]
  y1<-y[o1]
  r1<-range(x)
  smat<-NULL
  n1<-length(x1)
  for(i in 1:n1){
    v1<-dnorm(x1,x1[i],lambda)
    v1<-v1/sum(v1)
    smat<-rbind(smat,v1)
  }
  yhat<-smat%*%y1
  if(do.plot){
    lines(x1,yhat,col=xcol)
  }
  n99<-length(x1)
  dferror<-length(x1)-sum(diag(2*smat-smat%*%(t(smat))))
  delta1<-sum(diag(t(diag(n99)-smat)%*%(diag(n99)-smat)))
  R<-t(diag(n99)-smat)%*%(diag(n99)-smat)
  delta2<-2*sum(diag(R%*%R))
  resid<-y1-smat%*%y1
  ypred<-y1
  ypred[o1]<-smat%*%y1
  PRESS<-sum((resid/(1-diag(smat)))^2)
  list(smat=smat,df=sum(diag(smat)),dferror=dferror,delta1=delta1,delta2=delta2,resid=resid,pred=ypred,press=PRESS)
}

gauss.reg<-function(x,y,lambda,nnn,nbin,xcol=4,do.plot=T)
{
  o1<-order(x)
  x1<-x[o1]
  y1<-y[o1]
  r1<-range(x)
  smat<-NULL
  n1<-length(x1)
  for(i in 1:n1){
    v1<-dnorm(x1,x1[i],lambda)
    v1<-v1/sum(v1)
    H1<-my.hat.w(x1,v1)
    smat<-rbind(smat,H1[i,])
  }
  yhat<-smat%*%y1
  if(do.plot){
    lines(x1,yhat,col=xcol)
  }
  n99<-length(x1)
  dferror<-length(x1)-sum(diag(2*smat-smat%*%(t(smat))))
  delta1<-sum(diag(t(diag(n99)-smat)%*%(diag(n99)-smat)))
  R<-t(diag(n99)-smat)%*%(diag(n99)-smat)
  delta2<-2*sum(diag(R%*%R))
  resid<-y1-smat%*%y1
  ypred<-y1
  ypred[o1]<-smat%*%y1
  PRESS<-sum((resid/(1-diag(smat)))^2)
  list(smat=smat,df=sum(diag(smat)),dferror=dferror,delta1=delta1,delta2=delta2,resid=resid,pred=ypred,press=PRESS)
}

gauss.mean.trunc<-function(x,y,lambda,nnn,nbin,xcol=5,do.plot=T)
{
  o1<-order(x)
  x1<-x[o1]
  y1<-y[o1]
  r1<-range(x)
  smat<-NULL
  n1<-length(x1)
  trunc.val<-n1-nnn
  for(i in 1:n1){
    v1<-dnorm(x1,x1[i],lambda)
    o2<-order(v1)
    thresh<-v1[o2[trunc.val]]
    v1<-v1*(v1>thresh)
    v1<-v1/sum(v1)
    smat<-rbind(smat,v1)
  }
  yhat<-smat%*%y1
  if(do.plot){
    lines(x1,yhat,col=xcol)
  }
  n99<-length(x1)
  dferror<-length(x1)-sum(diag(2*smat-smat%*%(t(smat))))
  delta1<-sum(diag(t(diag(n99)-smat)%*%(diag(n99)-smat)))
  R<-t(diag(n99)-smat)%*%(diag(n99)-smat)
  delta2<-2*sum(diag(R%*%R))
  resid<-y1-smat%*%y1
  ypred<-y1
  ypred[o1]<-smat%*%y1
  PRESS<-sum((resid/(1-diag(smat)))^2)
  list(smat=smat,df=sum(diag(smat)),dferror=dferror,delta1=delta1,delta2=delta2,resid=resid,pred=ypred,press=PRESS)
}

gauss.reg.trunc<-function(x,y,lambda,nnn,nbin,xcol=6,do.plot=T)
{
  o1<-order(x)
  x1<-x[o1]
  y1<-y[o1]
  r1<-range(x)
  smat<-NULL
  n1<-length(x1)
  trunc.val<-n1-nnn
  for(i in 1:n1){
    v1<-dnorm(x1,x1[i],lambda)
    o1<-order(v1)
    thresh<-v1[o1[trunc.val]]
    v1<-v1*(v1>thresh)
    v1<-v1/sum(v1)
    H1<-my.hat.w(x1,v1)
    smat<-rbind(smat,H1[i,])
  }
  yhat<-smat%*%y1
  if(do.plot){
    lines(x1,yhat,col=xcol)
  }
  n99<-length(x1)
  dferror<-length(x1)-sum(diag(2*smat-smat%*%(t(smat))))
  delta1<-sum(diag(t(diag(n99)-smat)%*%(diag(n99)-smat)))
  R<-t(diag(n99)-smat)%*%(diag(n99)-smat)
  delta2<-2*sum(diag(R%*%R))
  resid<-y1-smat%*%y1
  ypred<-y1
  ypred[o1]<-smat%*%y1
  PRESS<-sum((resid/(1-diag(smat)))^2)
  list(smat=smat,df=sum(diag(smat)),dferror=dferror,delta1=delta1,delta2=delta2,resid=resid,pred=ypred,press=PRESS)
}

greedy.random.searchA<-function(func,x,y,theta=0,nnn=0,nbin=0,num=10){ #inputs
  nnn0<-nnn
  nbin0<-nbin
  if(nnn<10){nnn<-10} #bumpers
  if(nnn>42){nnn<-42}
  if(nbin1<1){nbin1<-1} #bumpers
  if(nbin1>9){nbin1<-9}
  press0<-func(x,y,theta,nnn,nbin,do.plot=F)$press
  press00<-press0 #original press
  inc<-0
  theta0<-theta #original theta
  press1<-NA
  while(inc<num | press00==press0){
    epsilon<-rnorm(n=1,mean=0,sd=theta*3)
    theta1<-theta+epsilon
    nnn1<-nnn+ceiling(rnorm(n=1,mean=0,sd=5))
    nbin1<-nbin+ceiling(rnorm(n=1,mean=0,sd=3))
    if(nbin1<1 | is.na(nbin1)){nbin1<-1} #bumpers
    if(nbin1>9){nbin1<-9}
    if(nnn1<10 | is.na(nnn1)){nnn1<-10} #if nnn1 is NA, then set it to 10
    if(nnn1>42){nnn1<-42}
    if(theta1<0){theta1=theta1*(-1)} #if theta is negative, make it positive
    if(theta1<0.01){theta1=theta1+1} #if theta is too low, add 1
    press1<-func(x,y,theta1,nnn1,nbin1,do.plot=F)$press #new press statistic
    if(is.na(press1)){press1=press0} #if press statistic is NA, set it to previous best
    if(press1<press0){
      nbin<-nbin1
      press0<-press1
      inc<-0
      nnn<-nnn1
      theta<-theta1
    }else{
      inc<-inc+1
    }
  }
  func(x,y,theta,nnn,nbin,do.plot=T) #add best model to the graph
  if(nbin0>0){ #outputs nbin
    list(new.nbin=nbin,new.press=press0,old.nbin=nbin0,old.press=press00)
  }else if(nnn0>0){ #outputs values including nnn 
    list(new.theta=theta,new.nnn=nnn,new.press=press0,old.theta=theta0,old.nnn=nnn0,old.press=press00)
  }else{ #outputs nbin
    list(new.theta=theta,new.press=press0,old.theta=theta0,old.press=press00)
}
}
plot(NOAA.new$delta.temp,NOAA.new$X.disaster,xlab="delta temp",ylab="disaster",main="NOAA data")
greedy.random.searchA(bin.mean,NOAA.new$delta.temp,NOAA.new$X.disaster,nbin=1,num=20)
greedy.random.searchA(gauss.mean,NOAA.new$delta.temp,NOAA.new$X.disaster,theta=1,num=20)
greedy.random.searchA(gauss.reg,NOAA.new$delta.temp,NOAA.new$X.disaster,theta=1,num=20)
greedy.random.searchA(gauss.mean.trunc,NOAA.new$delta.temp,NOAA.new$X.disaster,theta=1,nnn=20,num=50)
greedy.random.searchA(gauss.reg.trunc,NOAA.new$delta.temp,NOAA.new$X.disaster,theta=1,nnn=20,num=20)

plot(NOAA.new$delta.temp,NOAA.new$X.disaster,xlab="delta temp",ylab="disaster",main="NOAA data")
bin.mean(NOAA.new$delta.temp,NOAA.new$X.disaster,nbin=4) #PRESS 377.3719
gauss.mean(NOAA.new$delta.temp,NOAA.new$X.disaster,lambda=0.07) #PRESS 442.7254
gauss.reg(NOAA.new$delta.temp,NOAA.new$X.disaster,lambda=0.2) #PRESS 469.9459
gauss.mean.trunc(NOAA.new$delta.temp,NOAA.new$X.disaster,lambda=0.07,nnn=34) #PRESS 442.7254
gauss.reg.trunc(NOAA.new$delta.temp,NOAA.new$X.disaster,lambda=4800,nnn=33) #PRESS 462.5478
