library(spatstat)
meand=function(a1,b1,a2,b2){
  dist=abs(sqrt(((a1-a2)**2)+((b1-b2)**2)))
  return(dist)
}

Spatialforce=function(n,r){
  par(mfrow=c(1,2), mar=c(1,1,1,1))
  x=runif(n)
  y=runif(n)
  m=data.frame(x,y)
  tplot <- ppx(data=m)
  plot(tplot)
  for (i in 1:n) {
    for (j in 1:n) {
      if(meand(x[i],y[i],x[j],y[j])<r){
        xm=(x[i]+x[j])/2
        ym=(y[i]+y[j])/2
        x[i]=xm
        y[i]=ym
      }else{
        x=x
        y=y
      }
    }
  }
  df2=data.frame(x,y)
  tplot2 <- ppx(data=df2)
  plot(tplot2)
}
Spatialforce(500,0.09)

