# Luke Beebe with groupmates Anish Gupta and Tyra Lassiter

library(leaps)
library(lars)

Auto.mat.japan<-Auto.mat[Auto.mat[,8]==3,]
Auto.mat.germany<-Auto.mat[Auto.mat[,8]==2,]
Auto.mat.usa<-Auto.mat[Auto.mat[,8]==1,]

Auto.mat2nd<-matrix.2ndorder.make(Auto.mat[,2:7])
Auto.mat.usa2nd<-matrix.2ndorder.make(Auto.mat.usa[,2:7])
Auto.mat.germany2nd<-matrix.2ndorder.make(Auto.mat.germany[,2:7])
Auto.mat.japan2nd<-matrix.2ndorder.make(Auto.mat.japan[,2:7])

matrix.2ndorder.make<-function(x, only.quad=F){
  x0<-x
  dimn<-dimnames(x)[[2]] #extract the names of the variables
  num.col<-length(x[1,]) #how many columns
  for(i in 1:num.col){
    if(!only.quad){ #if we are doing all 2nd order
      for(j in i:num.col){
        x0<-cbind(x0,x[,i]*x[,j])
        dimn<-c(dimn,paste(dimn[i],dimn[j],sep="")) #create interaction dimnames
      }
    }
    else{ #in here only if doing only squared terms
      x0<-cbind(x0,x[,i]*x[,i])
      dimn<-c(dimn,paste(dimn[i],"2",sep="")) #squared dimmension names
    }
  }
  dimnames(x0)[[2]]<-dimn
  x0
}

#This function decides between using leaps and lars, passing parameters to leaps/lars function
leaps.lars <- function(both=FALSE,leaps=TRUE,xmat,yvec,ncheck=10,int=F){ #pass matrix, yvec=response variable
  if(both==TRUE){par(mfrow=c(2,1))}else{par(mfrow=c(1,1))} #partitions screen to produce plots
  if(leaps==TRUE|both==TRUE){ #runs leaps
    output.leaps<-leaps.then.press(xmat=xmat,yvec=yvec,ncheck=ncheck)
    output.leaps
  }
  if(leaps==FALSE|both==TRUE){ #runs lars, produces plot/correlation stat
    output.lars<-lars.select(xmat=xmat,y=yvec,ncheck=ncheck,int=int) #runs function, saves to output.lars
    print(output.lars$beta.out) #prints beta values
    plot(xmat%*%output.lars$beta.out,yvec) #Actual MPG vs predicted MPG
    lars.corr<-cor(xmat%*%output.lars$beta.out,yvec) #finds correlation between actual MPG vs predicted MPG
    print(paste("LARS CORRELATION",lars.corr)) #correlation
  }
}

regpluspress<-function(x,y){ #names function, takes two variables
  str<-lsfit(x,y) #saves ls fit output to str
  press<-sum((str$resid/(1-hat(x)))^2) #calculates, saves PRESS statistic
  str$press<-press #saves PRESS statistic to str
  str #returns str
}

leaps.then.press<-function(xmat,yvec,ncheck=10,print.ls=F) #takes parameters
{
  leaps.str<-leaps(xmat,yvec) #runs through leaps and saves output to leaps.str
  z1<-leaps.str$Cp #takes Cp value from leaps.str, saves to vector z1
  o1<-order(z1) #orders z1 based on Cp values
  matwhich<-(leaps.str$which[o1,])[1:ncheck,] #saves lowest (ncheck) Cp values
  z2<-z1[o1][1:ncheck] #saves lowest cp values to z2
  pressvec<-NULL
  for(i in 1:ncheck){ #loop from 1 to number of checks
    ls.str0<-regpluspress(xmat[,matwhich[i,]],yvec) #saves PRESS statistics from lowest Cp list 
    if(print.ls){ #prints ls.str0 of PRESS stats if called
      ls.print(ls.str0)
    }
    print(i) #prints iteration
    print(paste("Press=",ls.str0$press)) #prints PRESS stat
    parvec<-matwhich[i,] #saves matrix row to vector
    npar<-sum(parvec) #sums vector values to npar (sums 1 and 0 values) 
    print(paste("MPSE=",ls.str0$press/(length(yvec)-(npar+1)))) #prints mean squared prediction error
    print(paste("Cp=",z2[i])) #prints Cp value
    pressvec<-c(pressvec, ls.str0$press) #saves press values
    if(i==1){ #lowest PRESS Stat
      Xmat<-(xmat)[,leaps.str$which[o1[1],]] #creates Xmat from "which" variables
      coef1<-lsfit(Xmat,yvec)$coef #saves coefficients
      print(coef1) #prints coef1
      leaps.pred<-Xmat%*%coef1[-1]+coef1[1] #Xmat times coefficients (minus and add intercept)
      plot(leaps.pred,yvec) #plots predictions and actual values
      leaps.corr<-cor(leaps.pred,yvec) #finds correlation between predictions and actual values
      print(paste("LEAPS CORRELATION", leaps.corr)) #prints correlation between variables
    }
  }
  o2<-order(pressvec) #output results
  matwhich[o2,] #model indicators sorted from best press to worst in top ncheck Cp
}

sumabs<-function(x){ #sumabs function for lars.select program
  sum(abs(x)) #sum of absolute values of x
}

lars.select<-function(xmat,y,int=F,ncheck=10) #names function and parameters
{
  lasso.str<-lars(xmat,y,intercept=int) #calls lars, saves output to lasso.str
  #print(xmat) #prints xmat
  cv.str<-cv.lars(xmat,y,plot.it=F,intercept=int) #calculates cross-validated error curve for lars
  o1<-order(cv.str$cv) #orders cv values from lowest to highest
  mindex<-cv.str$index[o1][1] #index of cv values ordered
  beta<-coef(lasso.str) #saves coefficients from lasso.str to beta
  index0<-apply(beta,1,sumabs) #iterates through rows, sums absolute values of beta (sum of squares)
  index0<-index0/max(index0) #sums of beta divided by max sum of beta, percentage (0 to 1)
  o1<-order(abs(index0-mindex)) #orders values subtracted by the minimum cv value
  I1<-(abs(index0-mindex)==min(abs(index0-mindex))) #if absolute value of index-mindex is the min, add to I1
  n1<-length(beta[,1]) #saves int, length of column of coefficients
  beta.out<-beta[I1,] #beta.out is minimum rows of coefficients
  if(sum(abs(beta.out))==0){ #if all values add to zero, then sort list by the Cp value
    v1<-lasso.str$Cp
    o2<-order(v1)
    beta.out<-beta[o1[1:ncheck],]
  }
  Ind.out<-beta.out!=0 #saves Ind.out as TRUE if beta.out doesn't equal 0, FALSE otherwise
  outlist<-list(beta.out=beta.out,ind.out=Ind.out) #saves list of beta.out, ind.out values
  if(int){ #finds y-intercept value
    Int.out1<-mean(y)-mean(xmat%*%beta.out[i]) #mean of y-value minus mean of x-matrix times beta coefficients
    outlist<-list(beta.out=beta.out,ind.out=Ind.out,int.out=Int.out1)
  }       
  outlist #returns outlist to end function
}

leaps.lars(both=T,xmat=Auto.mat2nd,yvec=Auto.mat[,1],ncheck=5,int=F)
leaps.lars(both=T,xmat=Auto.mat[,-1],yvec=Auto.mat[,1],ncheck=5,int=F)

leaps.lars(both=T,xmat=Auto.mat.usa2nd,yvec=Auto.mat.usa[,1],ncheck=5,int=F)
leaps.lars(both=T,xmat=Auto.mat.usa[,-c(1,8)],yvec=Auto.mat.usa[,1],ncheck=5,int=F)

leaps.lars(both=T,xmat=Auto.mat.germany2nd,yvec=Auto.mat.germany[,1],ncheck=5,int=F)
leaps.lars(both=T,xmat=Auto.mat.germany[,-c(1,8)],yvec=Auto.mat.germany[,1],ncheck=5,int=F)

leaps.lars(both=T,xmat=Auto.mat.japan2nd,yvec=Auto.mat.japan[,1],ncheck=5,int=F)
leaps.lars(both=T,xmat=Auto.mat.japan[,-c(1,8)],yvec=Auto.mat.japan[,1],ncheck=5,int=F)
