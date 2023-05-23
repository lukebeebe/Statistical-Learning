# Assignment 8 Neural Nets

my.neuralnet<-function(X1,Y,hidden=3,output="linear") # names function, parameters, hidden=3 neurons
{
  X<-cbind(1,X1) # combines 1, X1 - first layer
  input.layer.length<-length(X[1,]) # gets length of first row
  w01<-rnorm(input.layer.length*hidden) # creates random weights
  w02<-rnorm(hidden) # second layer of random weights, neurons
  w0<-c(w01,w02) # combines weights into one variable
  print(w0) # prints weights
  myoptfunc<-function(w0){my.eval2.nnet(w0,X,Y,hidden,output)$llik} # function that saves llik to myoptfunc
  dum<-optim(w0,myoptfunc,method="CG") # optimizes via gradient descent, saves to dum
  wfinal<-dum$par # best set of parameters saved to wfinal
  ss<-dum$val # value of fn corresponding to par
  pred<-my.eval2.nnet(wfinal,X,Y,hidden,output)$pred # saves my.eval.nnet with updated wfinal
  plot(pred,Y) # plots predictions and Y values, how accurate it is
  list(ss=ss,wfinal=wfinal) # lists ss and wfinal
  my.eval1.nnet<-function(Xrow,w0,hidden,output){ # names function, parameters
    input.layer.length<-length(Xrow) # length of input layer, Xrow
    w01<-w0[c(1:(length(Xrow)*hidden))] # saves weights of w0 to w01
    w02<-w0[-c(1:(length(Xrow)*hidden))] # saves weights of w0 to w02
    w1<-matrix(w01,input.layer.length,hidden) # matrix of data=w01, nrow=input.layer.length, ncol=hidden
    print(w1) # print functions so you can see what's happening
    print(Xrow)
    print(w02)
    xhidden<-t(Xrow)%*%w1 # transpose Xrow times w1 matrix, saved to xhidden
    zhidden<-my.logistic(xhidden) # runs my.logistic on xhidden, makes it zhidden
    out<-sum(w02*zhidden) # sums zhidden * w02
    if(output=="binary"){out<-my.logistic(out)} # if binary, it changes my.logistic to give percentages of output
    out # returns out
  }
  my.eval2.nnet<-function(w0,X,Y,hidden,output){ # names function, parameters
    zfunc<-function(V){my.eval1.nnet(V,w0,hidden,output)} # passes parameters to my.eval1.nnet, saves to zfunc
    pred<-apply(X,1,zfunc) # runs my.eval1.nnet on X
    if(output=="binary"){ # if binary
      llik<-(-1)*sum(log(pred)*Y+log(1-pred)*(1-Y)) # binomial percentage
    }else{
      llik<-sum((pred-Y)^2) # regular percentage
    }
    list(llik=llik,pred=pred,y=Y) #lists outputs
  }
  my.logistic<-function(z){exp(z)/(1+exp(z))} # e^z/(1+e^z), makes function like neuron, where it fires past a certain point
}

# Multilayer - How does it generalize the first layer?
# It starts the same way as single layer, takes num.layers and lambda
# num.layers is the amount of layers you want for the neural net
# lambda is the regularization coefficient that shrinks the betas

my.neuralnet.multilayer<-function(X1,Y,hidden=3,output="linear",num.layers=1,lambda=0){
    X<-cbind(1,X1)
    input.layer.length<-length(X[1,])
    w01<-rnorm(input.layer.length*hidden+(num.layers-1)*hidden*hidden)
    w02<-rnorm(hidden)
    w0<-c(w01,w02)
    myoptfunc<-function(w0){my.eval2.nnet.ml(w0,X,Y,hidden,output,num.layers,lambda)$llik}
    dum<-optim(w0,myoptfunc,method="CG")
    wfinal<-dum$par
    ss<-dum$val
    duh<-my.eval2.nnet.ml(wfinal,X,Y,hidden,output,num.layers,lambda)
    plot(duh$pred,Y,main=paste("llik=",duh$llik,"\nSS=",ss),xlab="Prediction")
    list(ss=ss,wfinal=wfinal,ll=duh$llik)
}

my.eval1.nnet.ml<-function(Xrow,w0,hidden,output,num.layers=1){
    input.layer.length<-length(Xrow)
    w01<-w0[c(1:(length(Xrow)*hidden))]
    w0A<-w0[-c(1:(length(Xrow)*hidden))]
    w1<-matrix(w01,input.layer.length,hidden)
    xhidden<-t(Xrow)%*%w1
    zhidden<-my.logistic(xhidden)
    if(num.layers==1){
      w02<-w0A
    }
    else{
      nlayers<-num.layers
      while(nlayers>1){
        w01<-w0A[c(1:(hidden*hidden))]
        w0A<-w0A[-c(1:(hidden*hidden))]
        w01<-matrix(w01,hidden,hidden)
        xhidden<-(zhidden)%*%w01
        zhidden<-my.logistic(xhidden)
        nlayers<-nlayers-1
      }
    }
    w02<-w0A
    out<-sum(w02*zhidden)
    if(output=="binary"){
      out<-my.logistic(out)
    }
    out
}

my.eval2.nnet.ml<-function(w0,X,Y,hidden,output,num.layers,lambda)
  {
    zfunc<-function(V){my.eval1.nnet.ml(V,w0,hidden,output,num.layers)}
    pred<-apply(X,1,zfunc)
    if(output=="binary"){
      llik<-(-1)*sum(log(pred)*Y+log(1-pred)*(1-Y))
    }else{
      llik<-sum((pred-Y)^2)
    }
    loglik<-llik
    llik<-llik+lambda*sum(w0^2)
    list(llik=llik,pred=pred,y=Y,loglik=loglik)
  }

my.greedy.neuralnet<-function(X0,Y,hidden=3,output="linear",num.layers=1,lambda=0,trials=15)
{
  par(mfrow=c(4,4))
  dum1<-my.neuralnet.multilayer(X0,Y,hidden,output,num.layers,lambda)
  print(c(dum1$ss,dum1$ll))
  for(i in 1:(trials-1)){
    dum2<-my.neuralnet.multilayer(X0,Y,hidden,output,num.layers,lambda)
    print(c(dum2$ss,dum2$ll))
    if(dum2$ss<dum1$ss){
      dum1<-dum2
    }
  }
  dum0<-my.eval2.nnet.ml(dum1$wfinal,cbind(1,X0),Y,hidden,output,num.layers,lambda)
  plot(dum0$pred,Y, main=paste("llik=",dum0$llik,"\nSS=",dum1$ss),xlab="Prediction")
  dum1
}

NOAA.new<-read.csv("/Users/lukebeebe/Documents/School/Rutgers/Spring 2023/Applied Statistical Learning/NOAAnew.csv")
NOAA.newA<-read.csv("/Users/lukebeebe/Documents/School/Rutgers/Spring 2023/Applied Statistical Learning/NOAAnewA.csv")
NOAA.newB<-NOAA.newA
NOAA.newB<-rbind(NOAA.newA, c(42,4.1,NOAA.new[42,2],NOAA.new[42,3],4.1^2,NOAA.new[42,3]^2,4.1*NOAA.new[42,3]))
NOAA.newB<-rbind(NOAA.newB, c(43,4.2,NOAA.new[43,2],NOAA.new[43,3],4.2^2,NOAA.new[43,3]^2,4.2*NOAA.new[43,3]))
NOAA.newB<-NOAA.newB[,-1]
NOAA.newB
my.greedy.neuralnet(NOAA.newB[,c(-2)],NOAA.newB[,2],hidden=4,num.layers=2,lambda=0.003,63)
# Trying to predict Y, the amount of disasters, using X0, the rest of the data