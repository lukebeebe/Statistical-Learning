# Luke Beebe Assignment 9
# Benjamini Hochberg - False Discovery Rates

source("/Users/lukebeebe/Documents/School/Rutgers/Spring 2023/Applied Statistical Learning/renal.pck")
datFDR.pck

fdr <- function(pvec,Q,ind=T){ # names function, takes parameters
  par(mfrow=c(2,1)) # sets plots to 2x1
  psort<-sort(pvec) # sorts pvec
  m<-length(pvec) # length of pvec
  plot(1:m,psort,ylab="p-value",xlab="ordering",main="False Discovery Rates") # plots pvalues vs order
  if(ind==T){Qval=(Q*c(1:m))/m}else{Qval=(Q*c(1:m))/(m*(sum(1/c(1:m))))} # finds Qval values given independence and Q
  lines(c(1:m), Qval, col='red') # plots Q line
  i=1 # sets counter
  for(i in i:m){if(psort[i]<Qval[i]){n=i}} # loop from i to m. if pval < Qval, save index
  plot(1:n,psort[1:n],ylab="p-value",xlab="ordering",main="Interesting",col="blue") # plot second graph up to highest index
  lines(c(1:n), Qval[1:n], col='red') # plot Qline up to highest index
  list(interesting=names(psort[1:n]),ind=ind) # return list of interesting values and independence
}

fdr(psmall.renal,0.1,T)
fdr(plarge.renal,0.1,T)
