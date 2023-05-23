#Luke Beebe
#Groupmates Tyra Lassiter and Anish Gupta

movie.peru<-function(depth=350,depth.nw=200, dis.vec=c(8:13))
{
  library(scatterplot3d) #loads scatterplot3d
  sctstr<-scatterplot3d(Peru[,c(2,3,4)],type="n") #creates scatter structure with Peru data
  IDmat<-Peru[,24:27] #Matrix of Peru columns 24:27
  IDmat[,2]<-IDmat[,2]*2 #Multiplies column by 2
  IDmat[,3]<-IDmat[,3]*3 #Multiplies column by 3
  IDmat[,4]<-IDmat[,4]*4 #Multiplies column by 4
  ztime<-Peru[,1] #Saves column 1 to ztime (column 1 is times recorded)
  nlag=20 #sets nlag to 20
  #loop through time with window, id points pi
  nz<-length(ztime)-nlag #nz is the length of ztime - 20
  for(i in 1:nz){ #loops from 1 to nz
    v9<-c(i:(i+nlag)) #vector from 1 to i+20
    icolvec<-apply(IDmat[v9,],1,sum) #adds 1 to rows i:i+20
    #this loop sets delay in movie, produces calculations according to difference between consecutive ztimes
    time.count<-floor(1000*(ztime[i+nlag]-ztime[i+(nlag-1)])) #rounds to floor of 1000*(difference of consecutive ztimes)
    for(j in 1:(2*time.count)){ #loop from 1 to time.count
      v1<-rnorm(100) #100 normal distribution
      for(k in 1:100){ #samples normal distribution values
        v2<-sample(v1,replace=T) 
        mean(v2) #finds mean of sample
      }
    }
    #Perumat<-cbind(Peru[,c(1,2,3)],sqrt(Peru[,4]))
    sctstr<-scatterplot3d(Peru[,c(2,3,4)],type="n") #runs scatterplot3d
    sctstr$points3d(Peru[v9,c(2,3,4)],col=icolvec,type="h",lwd=((exp(Peru[v9,5]-4)))) #updates points so the graph looks "live"
    #print(c(i,nz,ztime[i],ztime[nz]))
    
  }
  print("DONE")
}

animate(mystery2) #some outliers, at one point resembles a negative linear relationship
animate(mystery9) #some outliers, mushed up points

pairs(mystery2) #3x6 looks like it has a negative linear relationship
#2 has groups of outliers

pairs(mystery9) #8x2 produces some interesting butterfly shaped plots
#lots of outliers with graphs related to 2
#8 seems to stretch the data vertical or horizontal

gui.asym() #Put trim values (0.9, 0.4) looks like butterfly
#Butterfly effect. Animate kept me from seeing it because it's locked running through each dimension in 2d
#pairs() allowed me to see the butterfly shaped plot right away, but the more dimensions, the more plots to look through, the more muddled the results can be
