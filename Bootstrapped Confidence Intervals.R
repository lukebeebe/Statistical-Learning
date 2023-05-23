#Luke Beebe with groupmates Tyra Lassiter and Anish Gupta

my.bootstrap1 <- function(mat=NOAA,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,nboot=10000){ #names function, takes inputs
  #function(mat=NOAA,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,do.plot=T,in.boot=T)
  #stat.out<-list(smstrmod=smstr,smstrlin=smstr.lin,resid.mod=resid1,resid.lin=resid2,dfmod=dfmod,dflin=dflin,F=Fstat,P=pvalue,n=n1)
  par(mfrow=c(1,2)) #seperates screens for two plots
  stat.out0<-my.dat.plot5(mat,i,j,zxlab,zylab,zmain,zcol,do.sqrt,do.plot=T,in.boot=F) #runs my.dat.plot5 and saves output to stat.out0
  F0<-stat.out0$F #saves f-value as F0
  resid0<-stat.out0$resid.lin #saves line residuals as resid0
  bootvec<-NULL #saves bootvec as NULL
  y0<-predict(stat.out0$smstrlin,mat[,i])$y #predicts y value of smooth spline using value mat[,i]
  matb<-mat #saves mat df as matb
  for(i1 in 1:nboot){ #loop from 1 to nboot (=10000)
    if(floor(i1/500)==(i1/500)){print(i1)} #print the number of iterations every 500 times
    residb<-sample(resid0,replace=T) #samples with replacement resid0, saves as residb
    Yb<-y0+residb #residuals plus smooth spline values 
    matb[,j]<-Yb #saves values in df 
    stat.outb<-my.dat.plot5(matb,i,j,zxlab,zylab,zmain,zcol,do.sqrt,do.plot=F,in.boot=T) #runs my.dat.plot5 and saves as stat.outb
    bootvec<-c(bootvec,stat.outb$F) #saves values of bootstrap vector and F value
  }
  pvalboot<-sum(bootvec>F0)/nboot #adds 1 if bootvec>F0, divides by nboot (=10000), saves as pvalboot
  boxplot(bootvec) #boxplot of boot vector values
  stat.out0$pvalboot<-pvalboot #saves pvalue of bootstrap values as pvalboot in stat.out0
  stat.out0 #runs and displays stat.out0
}
gui.bootstrap1 <- function(){
  library(tcltk)
  inputs <- function(){ #function named inputs, dealing with all the input values
    
    x <- tclVar("NOAA") #Preloaded inputs for textbox for this section
    y <- tclVar("3")
    z <- tclVar("2")
    w<-tclVar("\"delta temp\"") 
    wa<-tclVar("\"disasters\"")
    wb<-tclVar("\"Disasters vs warming\"")
    zc<-tclVar("2")
    qc<-tclVar("F")
    rc<-tclVar("10000")
    
    tt <- tktoplevel()
    tkwm.title(tt,"Choose parameters for new function                   ") #title
    x.entry <- tkentry(tt, textvariable=x) #Saves new inputs as variables for this section
    y.entry <- tkentry(tt, textvariable=y)
    z.entry <- tkentry(tt, textvariable=z)
    w.entry<-tkentry(tt, textvariable=w)  
    wa.entry<-tkentry(tt,textvariable=wa)
    wb.entry<-tkentry(tt,textvariable=wb)
    zc.entry<-tkentry(tt,textvariable=zc)
    qc.entry<-tkentry(tt,textvariable=qc)
    rc.entry<-tkentry(tt,textvariable=rc)
    reset <- function() #if reset, makes entries as "" (nothing) for this section
    {
      tclvalue(x)<-""
      tclvalue(y)<-""
      tclvalue(z)<-""
      tclvalue(w)<-""
      tclvalue(wa.entry)<-""
      tclvalue(wb.entry)<-""
      tclvalue(zc.entry)<-""
      tclvalue(qc.entry)<-""
      tclvalue(rc.entry)<-""
    }
    
    reset.but <- tkbutton(tt, text="Reset", command=reset) #makes button for reset
    
    submit <- function() { #if button submit is clicked, saves input as values
      x <- tclvalue(x)
      y <- tclvalue(y)
      z <- tclvalue(z)
      w<-tclvalue(w)
      wa<-tclvalue(wa)
      wb<-tclvalue(wb)
      zc<-tclvalue(zc)  
      qc<-tclvalue(qc)
      rc<-tclvalue(rc)
      e <- parent.env(environment()) # saves values to dataframe e
      e$x <- x
      e$y <- y
      e$z <- z
      e$w<-w
      e$wa<-wa
      e$wb<-wb
      e$zc<-zc
      e$qc<-qc
      e$rc<-rc
      tkdestroy(tt)
    }
    
    submit.but <- tkbutton(tt, text="start", command=submit) 
    tkgrid(tklabel(tt,text="Input data matrix"),columnspan=2) #all tkgrid lists titles for entries
    tkgrid(tklabel(tt,text="data"), x.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Input X column"),columnspan=2)
    tkgrid(tklabel(tt,text="i"), y.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Input Y column"),columnspan=2)
    tkgrid(tklabel(tt,text="j"), z.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Input xaxis label"),columnspan=2)
    tkgrid(tklabel(tt,text="Xaxis"), w.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Input yaxis label"),columnspan=2)
    tkgrid(tklabel(tt,text="Yaxis"), wa.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Input plot title"),columnspan=2)
    tkgrid(tklabel(tt,text="Plot title"), wb.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Input color code"),columnspan=2)
    tkgrid(tklabel(tt,text="Color code"),zc.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Input sqrt indicator"),columnspan=2)
    tkgrid(tklabel(tt,text="Sqrt=T"),qc.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Input nboot"),columnspan=2)
    tkgrid(tklabel(tt,text="nboot"),rc.entry, pady=10, padx =30)
    
    tkgrid(submit.but, reset.but) #names both buttons
    
    tkwait.window(tt)
    return(c(x,y,z,w,wa,wb,zc,qc,rc)) #returns a vector of all values
  }
  #Now run the function like:
  predictor_para <- inputs() #runs inputs and saves the vector above as predictor_para
  print(predictor_para) #takes predictor_para data and saves values to new variables
  mat<-eval(parse(text=predictor_para[1]))
  ind1<-eval(parse(text=predictor_para[2]))
  ind2<-eval(parse(text=predictor_para[3]))
  xlab<-eval(parse(text=predictor_para[4]))
  ylab<-eval(parse(text=predictor_para[5]))
  maintitle<-eval(parse(text=predictor_para[6]))
  zcol<-eval(parse(text=predictor_para[7]))
  zsqrt<-eval(parse(text=predictor_para[8]))
  znboot<-eval(parse(text=predictor_para[9]))
  my.bootstrap1(mat,ind1,ind2,xlab,ylab, maintitle,zcol,zsqrt,znboot) #passes new variables to my.bootstrap1
  
}

my.bootstrap2 <- function(mat=NOAA,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,nboot=10000,pred.bound=T,conf.lev=.95,pivotal=T){
  #function(mat=NOAA,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,do.plot=T,in.boot=T)
  #stat.out<-list(smstrmod=smstr,smstrlin=smstr.lin,resid.mod=resid1,resid.lin=resid2,dfmod=dfmod,dflin=dflin,F=Fstat,P=pvalue,n=n1)
  par(mfrow=c(1,1)) #sets up console to display one plot
  stat.out0<-my.dat.plot5(mat,i,j,zxlab,zylab,zmain,zcol,do.sqrt,do.plot=T,in.boot=F) #runs my.dat.plot5, saves output to stat.out0
  resid0<-stat.out0$resid.mod #saves residuals from stat.out0 to resid0
  bootmat<-NULL #creates variable bootmat as NULL
  y0<-predict(stat.out0$smstrmod,mat[,i])$y #predicts y values from smooth spline
  matb<-mat #saves df as matb (matrix b)
  for(i1 in 1:nboot){ #loop from 1 to nboot (=10000)
    if(floor(i1/500)==(i1/500)){print(i1)} #prints number of iterations every 500 times to show progress of calculations
    residb<-sample(resid0,replace=T) #samples residuals with replacement
    Yb<-y0+residb #saves y values plus residuals to Yb
    matb[,j]<-Yb #stores Yb in df matb[,j] (the copy we made)
    #print(matb)
    stat.outb<-my.dat.plot5(matb,i,j,zxlab,zylab,zmain,zcol,do.sqrt,do.plot=F,in.boot=T) #runs my.dat.plot5 using new data in matb, saves output to stat.outb
    Ybp<-predict(stat.outb$smstrmod,matb[,i])$y #saves predictions from smoothspline as values in Ybp
    if(pred.bound){ #if pred.bound=T (set to T)
      if(pivotal){ #if pivotal=T (set to T)
        bootmat<-rbind(bootmat,stat.outb$resid.mod+Ybp-y0) #combines rows of bootmat and residuals from stat.outb plus the difference smoothspline predictions from our original data and sample data
      }else{
        bootmat<-rbind(bootmat,stat.outb$resid.mod+Ybp) #combines rows of bootmat and residuals from stat.outb plus predictions from sample data
      }
    }else{
      if(pivotal){
        bootmat<-rbind(bootmat,Ybp-y0) #comines rows of bootmat and predictions of smooth spline from sample minus original smooth spline values
      }else{
        bootmat<-rbind(bootmat,Ybp) #combines rows of bootmat and predictions of smooth spline from sample
      }
    }
    
  }
  alpha<-(1-conf.lev)/2 #calculates alpha (half of 1-conf.lev, two tail test)
  my.quant<-function(x,a=alpha){quantile(x,c(a,1-a))} #function to create bounds using quantile function
  bounds<-apply(bootmat,2,my.quant) #applies 2 bounds to graph
  if(pivotal){
    bounds[1,]<-y0-bounds[1,] #saves bound values of original smooth spline minus bounds in first row
    bounds[2,]<-y0-bounds[2,] #saves bound values of original smooth spline minus bounds in second row
  }
  x<-mat[,i] #saves mat[,i] to x (for my.quant function)
  if(do.sqrt){ #saves sqrt(mat[,i]) if do.sqrt=T
    x<-sqrt(x)
  }
  o1<-order(x) #orders all values so the line is created from left to right
  lines(x[o1],bounds[1,o1],col=zcol+2) #creates line of CI
  lines(x[o1],bounds[2,o1],col=zcol+2) #creates line of CI
}
gui.bootstrap2 <- function(){
  library(tcltk)
  #,pred.bound=T,conf.lev=.95,pivotal=T
  inputs <- function(){ #function named inputs, dealing with all the input values
    
    x <- tclVar("NOAA") #Preloaded inputs for textbox for this section
    y <- tclVar("3")
    z <- tclVar("2")
    w<-tclVar("\"Delta temp\"") 
    wa<-tclVar("\"Disasters\"")
    wb<-tclVar("\"Disasters vs Warming\"")
    zc<-tclVar("2")
    qc<-tclVar("F")
    rc<-tclVar("10000")
    za<-tclVar("T")
    zb<-tclVar(".95")
    wc<-tclVar("T")
    
    tt <- tktoplevel()
    tkwm.title(tt,"Choose parameters for new function                   ") #Title
    x.entry <- tkentry(tt, textvariable=x) #Saves new inputs as variables for this section
    y.entry <- tkentry(tt, textvariable=y)
    z.entry <- tkentry(tt, textvariable=z)
    w.entry<-tkentry(tt, textvariable=w)  
    wa.entry<-tkentry(tt,textvariable=wa)
    wb.entry<-tkentry(tt,textvariable=wb)
    zc.entry<-tkentry(tt,textvariable=zc)
    qc.entry<-tkentry(tt,textvariable=qc)
    rc.entry<-tkentry(tt,textvariable=rc)
    za.entry<-tkentry(tt,textvariable=za)
    zb.entry<-tkentry(tt,textvariable=zb)
    wc.entry<-tkentry(tt,textvariable=wc)
    
    reset <- function() #if reset, makes entries as "" (nothing) for this section
    {
      tclvalue(x)<-""
      tclvalue(y)<-""
      tclvalue(z)<-""
      tclvalue(w)<-""
      tclvalue(wa.entry)<-""
      tclvalue(wb.entry)<-""
      tclvalue(zc.entry)<-""
      tclvalue(qc.entry)<-""
      tclvalue(rc.entry)<-""
      tclvalue(za.entry)<-""
      tclvalue(zb.entry)<-""
      tclvalue(wc.entry)<-""
      
    }
    
    reset.but <- tkbutton(tt, text="Reset", command=reset) #makes button for reset
    
    submit <- function() { #if button submit is clicked, saves input as values
      x <- tclvalue(x)
      y <- tclvalue(y)
      z <- tclvalue(z)
      w<-tclvalue(w)
      wa<-tclvalue(wa)
      wb<-tclvalue(wb)
      zc<-tclvalue(zc)  
      qc<-tclvalue(qc)
      rc<-tclvalue(rc)
      za<-tclvalue(za)
      zb<-tclvalue(zb)
      wc<-tclvalue(wc)
      
      e <- parent.env(environment()) #saves values to df e
      e$x <- x
      e$y <- y
      e$z <- z
      e$w<-w
      e$wa<-wa
      e$wb<-wb
      e$zc<-zc
      e$qc<-qc
      e$rc<-rc
      e$za<-za
      e$zb<-zb
      e$wc<-wc
      
      tkdestroy(tt)
    }
    
    submit.but <- tkbutton(tt, text="start", command=submit)
    tkgrid(tklabel(tt,text="Input data matrix"),columnspan=2) #all tkgrid lists titles for entries
    tkgrid(tklabel(tt,text="data"), x.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Input X column"),columnspan=2)
    tkgrid(tklabel(tt,text="i"), y.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Input Y column"),columnspan=2)
    tkgrid(tklabel(tt,text="j"), z.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Input xaxis label"),columnspan=2)
    tkgrid(tklabel(tt,text="Xaxis"), w.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Input yaxis label"),columnspan=2)
    tkgrid(tklabel(tt,text="Yaxis"), wa.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Input plot title"),columnspan=2)
    tkgrid(tklabel(tt,text="Plot title"), wb.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Input color code"),columnspan=2)
    tkgrid(tklabel(tt,text="Color code"),zc.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Input sqrt indicator"),columnspan=2)
    tkgrid(tklabel(tt,text="Sqrt=T"),qc.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Input nboot"),columnspan=2)
    tkgrid(tklabel(tt,text="nboot"),rc.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Prediction Bound?"),columnspan=2)
    tkgrid(tklabel(tt,text="Pbound=T"),za.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Conf.level"),columnspan=2)
    tkgrid(tklabel(tt,text="Confidence"),zb.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Pivotal=T"),columnspan=2)
    tkgrid(tklabel(tt,text="Pivotal"),wc.entry, pady=10, padx =30)
    
    tkgrid(submit.but, reset.but) #names both buttons
    
    tkwait.window(tt)
    return(c(x,y,z,w,wa,wb,zc,qc,rc,za,zb,wc)) #returns a vector of all values
  }
  #Now run the function like:
  predictor_para <- inputs() #saves vector of values from above to predictor_para
  print(predictor_para) #prints parameters
  mat<-eval(parse(text=predictor_para[1])) #saves predictor_para to new variables
  ind1<-eval(parse(text=predictor_para[2]))
  ind2<-eval(parse(text=predictor_para[3]))
  xlab<-eval(parse(text=predictor_para[4]))
  ylab<-eval(parse(text=predictor_para[5]))
  maintitle<-eval(parse(text=predictor_para[6]))
  zcol<-eval(parse(text=predictor_para[7]))
  zsqrt<-eval(parse(text=predictor_para[8]))
  znboot<-eval(parse(text=predictor_para[9]))
  zpred<-eval(parse(text=predictor_para[10]))
  zconf<-eval(parse(text=predictor_para[11]))
  zpivot<-eval(parse(text=predictor_para[12]))
  
  my.bootstrap2(mat,ind1,ind2,xlab,ylab, maintitle,zcol,zsqrt,znboot,zpred,zconf,zpivot) #runs my.bootstrap2 with new variables
  
}

my.bootstrap3 <- function(mat=NOAA,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,nboot=10000,pred.bound=T,conf.lev=.95,pivotal=T){ #creates this function, names parameters
  par(mfrow=c(1,1)) #creates window to display one graph
  stat.out0<-my.dat.plot5(mat,i,j,zxlab,zylab,zmain,zcol,do.sqrt,do.plot=T,in.boot=F) #runs my.dat.plot5, saves its values to stat.out0
  bootmat<-NULL #creates empty matrix
  y0<-predict(stat.out0$smstrmod,mat[,i])$y #saves y values from smooth spline from stat.out0
  matb<-mat #creates another matrix, matb
  nm<-length(matb[,1]) #saves length of matrix to nm
  for(i1 in 1:nboot){ #loop from 1 to number of bootstraps (nboot=10000)
    if(floor(i1/500)==(i1/500)){print(i1)} #prints every 500 iterations, letting us know where the function is
    zed<-sample(nm,replace=T) #samples values of nm and replaces them
    matb<-mat[zed,] #saves values from mat to matb using random sample numbers zed
    #print(matb)
    stat.outb<-my.dat.plot5a(matb,mat,i,j,zxlab,zylab,zmain,zcol,do.sqrt,do.plot=F,in.boot=T) #saves stat.outb using new sample of data matb
    Ybp<-predict(stat.outb$smstrmod,mat[,i])$y #predicts y values of smooth spline
    if(pred.bound){ #runs if pred.bound=T
      if(pivotal){ #runs if pivotal=T
        bootmat<-rbind(bootmat,stat.outb$resid.mod+Ybp-y0) #combines rows of bootmat and residuals from stat.outb plus the difference smoothspline predictions from our original data and sample data
      }else{
        bootmat<-rbind(bootmat,stat.outb$resid.mod+Ybp) #combines rows of bootmat and residuals from stat.outb plus predictions from sample data
      }
    }else{
      if(pivotal){
        bootmat<-rbind(bootmat,Ybp-y0) #comines rows of bootmat and predictions of smooth spline from sample minus original smooth spline values
      }else{
        bootmat<-rbind(bootmat,Ybp) #combines rows of bootmat and predictions of smooth spline from sample
      }
    }
    
  }
  alpha<-(1-conf.lev)/2 #calculates alpha (half of 1-conf.lev, two tail test)
  my.quant<-function(x,a=alpha){quantile(x,c(a,1-a))} #function to create bounds using quantile function
  bounds<-apply(bootmat,2,my.quant) #applies 2 bounds to graph
  if(pivotal){
    bounds[1,]<-y0-bounds[1,] #saves bound values of original smooth spline minus bounds in first row
    bounds[2,]<-y0-bounds[2,] #saves bound values of original smooth spline minus bounds in second row
  }
  x<-mat[,i] #saves mat[,i] to x (for my.quant function)
  if(do.sqrt){ #saves sqrt(mat[,i]) if do.sqrt=T
    x<-sqrt(x)
  }
  o1<-order(x) #orders all values so the line is created from left to right
  lines(x[o1],bounds[1,o1],col=zcol+2) #creates line of CI
  lines(x[o1],bounds[2,o1],col=zcol+2) #creates line of CI
}


gui.bootstrapxy <- function(){
  library(tcltk)
  #,pred.bound=T,conf.lev=.95,pivotal=T
  inputs <- function(){ #function named inputs, dealing with all the input values
    
    x <- tclVar("NOAA") #Preloaded inputs for textbox for this section
    y <- tclVar("3")
    z <- tclVar("2")
    w<-tclVar("\"delta temp\"") 
    wa<-tclVar("\"disasters\"")
    wb<-tclVar("\"Disasters vs warming\"")
    zc<-tclVar("2")
    qc<-tclVar("F")
    rc<-tclVar("10000")
    za<-tclVar("T")
    zb<-tclVar(".95")
    wc<-tclVar("T")
    
    tt <- tktoplevel()
    tkwm.title(tt,"Choose parameters for new function                   ") #title
    x.entry <- tkentry(tt, textvariable=x) #Saves new inputs as variables for this section
    y.entry <- tkentry(tt, textvariable=y)
    z.entry <- tkentry(tt, textvariable=z)
    w.entry<-tkentry(tt, textvariable=w)  
    wa.entry<-tkentry(tt,textvariable=wa)
    wb.entry<-tkentry(tt,textvariable=wb)
    zc.entry<-tkentry(tt,textvariable=zc)
    qc.entry<-tkentry(tt,textvariable=qc)
    rc.entry<-tkentry(tt,textvariable=rc)
    za.entry<-tkentry(tt,textvariable=za)
    zb.entry<-tkentry(tt,textvariable=zb)
    wc.entry<-tkentry(tt,textvariable=wc)
    
    reset <- function() #reset function
    {
      tclvalue(x)<-"" #makes new values blank in text box
      tclvalue(y)<-""
      tclvalue(z)<-""
      tclvalue(w)<-""
      tclvalue(wa.entry)<-""
      tclvalue(wb.entry)<-""
      tclvalue(zc.entry)<-""
      tclvalue(qc.entry)<-""
      tclvalue(rc.entry)<-""
      tclvalue(za.entry)<-""
      tclvalue(zb.entry)<-""
      tclvalue(wc.entry)<-""
      
    }
    
    reset.but <- tkbutton(tt, text="Reset", command=reset) #reset button
    
    submit <- function() { #submit funciton
      x <- tclvalue(x) #saves input values to variables
      y <- tclvalue(y)
      z <- tclvalue(z)
      w<-tclvalue(w)
      wa<-tclvalue(wa)
      wb<-tclvalue(wb)
      zc<-tclvalue(zc)  
      qc<-tclvalue(qc)
      rc<-tclvalue(rc)
      za<-tclvalue(za)
      zb<-tclvalue(zb)
      wc<-tclvalue(wc)
      
      e <- parent.env(environment())
      e$x <- x #makes input values part of dataframe e
      e$y <- y
      e$z <- z
      e$w<-w
      e$wa<-wa
      e$wb<-wb
      e$zc<-zc
      e$qc<-qc
      e$rc<-rc
      e$za<-za
      e$zb<-zb
      e$wc<-wc
      
      tkdestroy(tt)
    }
    
    submit.but <- tkbutton(tt, text="start", command=submit) #submit button
    tkgrid(tklabel(tt,text="Input data matrix"),columnspan=2) #all tkgrid lists titles for entries
    tkgrid(tklabel(tt,text="data"), x.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Input X column"),columnspan=2)
    tkgrid(tklabel(tt,text="i"), y.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Input Y column"),columnspan=2)
    tkgrid(tklabel(tt,text="j"), z.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Input xaxis label"),columnspan=2)
    tkgrid(tklabel(tt,text="Xaxis"), w.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Input yaxis label"),columnspan=2)
    tkgrid(tklabel(tt,text="Yaxis"), wa.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Input plot title"),columnspan=2)
    tkgrid(tklabel(tt,text="Plot title"), wb.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Input color code"),columnspan=2)
    tkgrid(tklabel(tt,text="Color code"),zc.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Input sqrt indicator"),columnspan=2)
    tkgrid(tklabel(tt,text="Sqrt=T"),qc.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Input nboot"),columnspan=2)
    tkgrid(tklabel(tt,text="nboot"),rc.entry, pady=10, padx =30)
    
    tkgrid(tklabel(tt,text="Prediction Bound?"),columnspan=2)
    tkgrid(tklabel(tt,text="Pbound=T"),za.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Conf.level"),columnspan=2)
    tkgrid(tklabel(tt,text="Confidence"),zb.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Pivotal=T"),columnspan=2)
    tkgrid(tklabel(tt,text="Pivotal"),wc.entry, pady=10, padx =30)
    
    
    tkgrid(submit.but, reset.but) #puts buttons at the bottom of the screen
    
    tkwait.window(tt)
    return(c(x,y,z,w,wa,wb,zc,qc,rc,za,zb,wc)) #returns values that were inputted on the screen
  }
  #Now run the function like:
  predictor_para <- inputs() #saves inputs to a list predictor_para
  print(predictor_para) #prints list
  mat<-eval(parse(text=predictor_para[1])) #saves objects of list to variables
  ind1<-eval(parse(text=predictor_para[2]))
  ind2<-eval(parse(text=predictor_para[3]))
  xlab<-eval(parse(text=predictor_para[4]))
  ylab<-eval(parse(text=predictor_para[5]))
  maintitle<-eval(parse(text=predictor_para[6]))
  zcol<-eval(parse(text=predictor_para[7]))
  zsqrt<-eval(parse(text=predictor_para[8]))
  znboot<-eval(parse(text=predictor_para[9]))
  zpred<-eval(parse(text=predictor_para[10]))
  zconf<-eval(parse(text=predictor_para[11]))
  zpivot<-eval(parse(text=predictor_para[12]))
  
  my.bootstrap3(mat,ind1,ind2,xlab,ylab, maintitle,zcol,zsqrt,znboot,zpred,zconf,zpivot) #runs my.bootstrap3 with variables from input()
  
}

my.dat.plot5 <- function(mat=NOAA,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,do.plot=T,in.boot=T){ #creates function and names paramaters
  if(in.boot){ #if in.boot=T, then run this (already set to T)
    do.sqrt<-F #makes do.sqrt False, meaning it won't take the sqrt root of the Y variable
    #This is set to false because in the bootstrap code, it later takes the sqrt(x) saving time and computing power
  }
  if(!do.sqrt){ #if do.sqrt=F, then run this
    
    smstr<-smooth.spline(mat[,i],mat[,j]) #save smooth.spline created from mat[,i], mat[,j] variables as smstr
    smstr.lin<-smooth.spline(mat[,i],mat[,j],df=2) #save linear line from same data as smstr.lin
    if(do.plot){ #if do.plot=T, run (already set to T)
      plot(mat[,i],mat[,j],xlab=zxlab,ylab=zylab,main=zmain) #plot values and name names retrieved from inputs in parameter
      lines(smstr,col=zcol) #plot smooth spline line
      lines(smstr.lin,col=(zcol+1)) #plot linear line, change color
    }
    resid1<-mat[,j]-predict(smstr,mat[,i])$y #saves residuals of smooth spline and data points mat[,i]
    resid2<-mat[,j]-predict(smstr.lin,mat[,i])$y #saves residuals of linear line
  }else{ #if do.sqrt = F
    smstr<-smooth.spline(mat[,i],sqrt(mat[,j])) #code does same as above, except the Y values are sqrt(Y) instead
    smstr.lin<-smooth.spline(mat[,i],sqrt(mat[,j]),df=2)
    if(do.plot){
      plot(mat[,i],sqrt(mat[,j]),xlab=zxlab,ylab=zylab,main=zmain)
      lines(smstr,col=zcol)
      lines(smstr.lin,col=(zcol+1))
    }
    resid1<-sqrt(mat[,j])-predict(smstr,mat[,i])$y
    resid2<-sqrt(mat[,j])-predict(smstr.lin,mat[,i])$y
  }
  
  dfmod<-smstr$df #df of smooth spline
  dflin<-2 #df of linear line
  ssmod<-sum(resid1^2) #adds up the square of all residuals (SSE of smooth spline)
  sslin<-sum(resid2^2) #adds up the square of all residuals (SSE of linear line)
  numss<-sslin-ssmod #SSE of linear line - SSE of smooth spline
  n1<-length(mat[,j]) #saves lenth of column mat[,j] as n1
  Fstat<-(numss/(dfmod-dflin))/(ssmod/(n1-dfmod)) #calculates Fstatistic using above variables
  pvalue<-1-pf(Fstat,dfmod-dflin,n1-dfmod) #calculates Pvalue using pf and parameters from above
  stat.out<-list(smstrmod=smstr,smstrlin=smstr.lin,resid.mod=resid1,resid.lin=resid2,dfmod=dfmod,dflin=dflin,F=Fstat,P=pvalue,n=n1)
  #creates list of output for function
}

my.dat.plot5a <- function(mat=NOAA,mat0,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,do.plot=T,in.boot=T){ #makes function, names parameters
  if(in.boot){ #if in.boot=T
    do.sqrt<-F #names do.sqrt=F because it will do it later in bootstrap function
  }
  if(!do.sqrt){ #if do.sqrt=F
    smstr<-smooth.spline(mat[,i],mat[,j]) #saves smooth spline to smstr
    pstr<-predict(smstr,mat0[,i]) #makes predictions based on sample data, mat0
    smstr$x<-pstr$x #saves sample data x values to smooth spline
    smstr$y<-pstr$y #saves sample data y values to smooth spline
    smstr.lin<-smooth.spline(mat[,i],mat[,j],df=2) #makes linear spline
    if(do.plot){ #function to create plot
      plot(mat[,i],mat[,j],xlab=zxlab,ylab=zylab,main=zmain) #makes plot with matrix values
      lines(smstr,col=zcol) #places smooth spline
      lines(smstr.lin,col=(zcol+1)) #places linear spline, changes color
    }
    resid1<-mat[,j]-predict(smstr,mat[,i])$y #residuals from matrix to updated values on smooth spline
    resid2<-mat[,j]-predict(smstr.lin,mat[,i])$y #residuals from matrix to linear spline
  }else{ #does same as above function, but if sqrt=T. takes the sqrt of mat[,j]
    smstr<-smooth.spline(mat[,i],sqrt(mat[,j]))
    pstr<-predict(smstr,mat0[,i])
    smstr$x<-pstr$x
    smstr$y<-pstr$y
    smstr.lin<-smooth.spline(mat[,i],sqrt(mat[,j]),df=2)
    if(do.plot){
      plot(mat[,i],sqrt(mat[,j]),xlab=zxlab,ylab=zylab,main=zmain)
      lines(smstr,col=zcol)
      lines(smstr.lin,col=(zcol+1))
    }
    resid1<-sqrt(mat[,j])-predict(smstr,mat[,i])$y
    resid2<-sqrt(mat[,j])-predict(smstr.lin,mat[,i])$y
  }
  
  dfmod<-smstr$df #degrees of freedom of smooth spline
  dflin<-2 #degrees of freedom of linear spline
  ssmod<-sum(resid1^2) #sum of residuals squared of updated smooth spline
  sslin<-sum(resid2^2) #sum of residuals squared of linear spline
  numss<-sslin-ssmod #difference of above residuals
  n1<-length(mat[,j]) #num of observations, length of matrix column j
  Fstat<-(numss/(dfmod-dflin))/(ssmod/(n1-dfmod)) #calculates F statistic
  pvalue<-pf(Fstat,dfmod-dflin,n1-dfmod) #calculates p-value
  stat.out<-list(smstrmod=smstr,smstrlin=smstr.lin,resid.mod=resid1,resid.lin=resid2,dfmod=dfmod,dflin=dflin,F=Fstat,P=pvalue,n=n1) #outputs stats from function
}
