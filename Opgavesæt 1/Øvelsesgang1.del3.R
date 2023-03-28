library(glmnet)
set.seed(1)
nsim<-1000
N<-25
beta<-c(2,0.5,-0.6,2,0.001,0,-1)


par.beta3<-matrix(rep(NA,5*nsim),ncol=5)
par.beta5<-matrix(rep(NA,5*nsim),ncol=5)

ypred.linreg<-rep(0,nsim)
ypred.ridge01<-rep(0,nsim)
ypred.ridge03<-rep(0,nsim)
ypred.lasso01<-rep(0,nsim)
ypred.lasso03<-rep(0,nsim)

xtest<-c(1,rnorm(6))
for (i in 1:nsim){
  


## x-variable
x1<-rnorm(N)
x2<-rnorm(N)
x3<-rnorm(N)
x4<-rnorm(N)
x5<-rnorm(N)
x6<-rnorm(N)

## x-matrix, y-variabel og fuldt datasæt
y<-beta[1]+beta[2]*x1+beta[3]*x2+beta[4]*x3+beta[5]*x4+beta[6]*x5+beta[7]*x6+rnorm(N)
x<-model.matrix(y~x1+x2+x3+x4+x5+x6)[,-1]

dataset<-cbind.data.frame(y,x1,x2,x3,x4,x5,x6)

## lineær regression

lin.model<-lm(y~x1+x2+x3+x4+x5+x6,data=dataset)
lin.koeff<-as.vector(lin.model$coefficients)

## ridge
ridge.mod<-glmnet(x,y,alpha=0)
ridge.koeff01<-predict(ridge.mod,type='coefficients',s=0.1)
ridge.koeff03<-predict(ridge.mod,type='coefficients',s=0.3)

## lasso

lasso.mod<-glmnet(x,y,alpha=1)
lasso.koeff01<-predict(lasso.mod,type='coefficients',s=0.1)
lasso.koeff03<-predict(lasso.mod,type='coefficients',s=0.3)


## beta3 og beta5 gemmes
par.beta3[i,]<-c(lin.koeff[4],ridge.koeff01[4],ridge.koeff03[4],lasso.koeff01[4],lasso.koeff03[4])
par.beta5[i,]<-c(lin.koeff[6],ridge.koeff01[6],ridge.koeff03[6],lasso.koeff01[6],lasso.koeff03[6])

## prædiktion af y-værdi med de 5 sæt koefficienter
ypred.linreg[i]<-sum(lin.koeff*xtest)
ypred.ridge01[i]<-sum(ridge.koeff01*xtest)
ypred.ridge03[i]<-sum(ridge.koeff03*xtest)
ypred.lasso01[i]<-sum(lasso.koeff01*xtest)
ypred.lasso03[i]<-sum(lasso.koeff03*xtest)

}


#### histogrammer, der viser estimater af beta3. Den sande værdi er 2
par(mfrow=c(1,5))
hist(par.beta3[,1],xlim=c(0.9,2.8),main='lin reg')
hist(par.beta3[,2],xlim=c(0.9,2.8),main='ridge, lambda=0.1')
hist(par.beta3[,3],xlim=c(0.9,2.8),main='ridge, lambda=0.3')
hist(par.beta3[,4],xlim=c(0.9,2.8),main='lasso, lambda=0.1')
hist(par.beta3[,5],xlim=c(0.9,2.8),main='lasso, lambda=0.3')

#### histogrammer, der viser estimater af beta5. Den sande værdi er 0
hist(par.beta5[,1],xlim=c(-0.8,0.8),main='lin reg')
hist(par.beta5[,2],xlim=c(-0.8,0.8),main='ridge, lambda=0.1')
hist(par.beta5[,3],xlim=c(-0.8,0.8),main='ridge, lambda=0.3')
hist(par.beta5[,4],xlim=c(-0.8,0.8),main='lasso, lambda=0.1')
hist(par.beta5[,5],xlim=c(-0.8,0.8),main='lasso, lambda=0.3')

#### histogrammer, der viser prædiktioner af y. Den sande forventede værdi er indtegnet
hist(ypred.linreg,main='lin reg')
abline(v=sum(beta*xtest),col='red')
hist(ypred.ridge01,main='ridge, lambda=0.1')
abline(v=sum(beta*xtest),col='red')
hist(ypred.ridge03,main='ridge, lambda=0.3')
abline(v=sum(beta*xtest),col='red')
hist(ypred.lasso01,main='lasso, lambda=0.1')
abline(v=sum(beta*xtest),col='red')
hist(ypred.lasso03,main='lasso, lambda=0.3')
abline(v=sum(beta*xtest),col='red')

#### gennemsnit og varianser af de prædikterede værdier
sum(beta*xtest)  # sand forventet værdi
mean(ypred.linreg); mean(ypred.ridge01); mean(ypred.ridge03); mean(ypred.lasso01); mean(ypred.lasso03)
var(ypred.linreg); var(ypred.ridge01); var(ypred.ridge03); var(ypred.lasso01); var(ypred.lasso03)
