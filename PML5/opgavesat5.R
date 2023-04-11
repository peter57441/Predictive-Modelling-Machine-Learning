N1<-1000
N2<-1000
N3<-1000

N<-N1+N2+N3

std<-0.4

xdata<-array(rep(0,N*20*20),dim=c(N,20,20))
ydata<-rep(NA,N)
ydata_text<-rep(NA,N)

rmat<-matrix(rep(1:20,20),ncol=20)
cmat<-matrix(rep(1:20,20),ncol=20,byrow=TRUE)

for (i in 1:N1){
  c1<-runif(1,7,13)
  c2<-runif(1,7,13)
  width1<-runif(1,2,4)
  width2<-runif(1,2,4)
  xdata[i,,]<-xdata[i,,]+(cmat<=c1+width1)*(cmat>=c1-width1)*(rmat<=c2+width2)*(rmat>=c2-width2)*1
  xdata[i,,]<-xdata[i,,]+matrix(rnorm(20*20,mean=0,sd=std),ncol=20)
}

for (i in (N1+1):(N1+N2)){
  c1<-sample(7:13,1)
  c2<-sample(7:13,1)
  radius<-runif(1,5,6)
  xdata[i,,]<-xdata[i,,]+(sqrt((rmat-c1)^2+(cmat-c2)^2)<=radius)*1
  xdata[i,,]<-xdata[i,,]+matrix(rnorm(20*20,mean=0,sd=std),ncol=20)
}

for (i in (N1+N2+1):(N1+N2+N3)){
  b<-runif(1,-1,1)
  slope<-runif(1,0.7,1.3)
  xdata[i,,]<-xdata[i,,]+(rmat<=1+slope*cmat+b+1)*(rmat>=1+slope*cmat+b-1)*1
  xdata[i,,]<-xdata[i,,]+matrix(rnorm(20*20,mean=0,sd=std),ncol=20)
}

ydata[1:N1]<-0
ydata[(N1+1):(N1+N2)]<-1
ydata[(N1+N2+1):(N1+N2+N3)]<-2

bland<-sample(1:N)
xdata<-xdata[bland,,]
ydata<-ydata[bland]
