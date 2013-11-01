

library(mvtnorm)
library(MASS)


########function info #####
bayes.log.2<- function(m,y,X, beta.0, Sigma.0.inv, b.start, s.start, niter=10000,
                       burnin=1000, retune=100) { 
  f=function(beta) 
    (sum(y*t(X)%*%beta-m*log(1+exp(t(X)%*%beta)))
     -(1/2)*t(beta-beta.0)%*%solve(Sigma.0.inv)%*%(beta-beta.0))
  b.update=function(b.start, s.start) {
    b.cur = b.start #start value   add new
    s = s.start  #start value    add new
    b.can=mvrnorm(1, b.cur, s)
    alpha=f(b.can)-f(b.cur)
    if (log(runif(1)) < alpha) {
      r=b.can
      acp.rate=1
    } 
    else {
      r=b.cur
      acp.rate=0  
    } 
    return(c(r,acp.rate))
  }
  draws.1=matrix(0, nrow=burnin, ncol=12)
  draws.1[1,]=c(b.0,0)
  s = s.0
  for (i in 2:burnin) {
    b.can=matrix(draws.1[i-1,1:11])
    draws.1[i,]=b.update(b.can, s)
    ### tuning sigma
    if (i%%retune == 0) {
      rate= mean(draws.1[(retune*(i/retune-1)+1):(retune*(i/retune)),12])  
      if (rate > 0.6) {
        s = 10*s
      }
      if (rate < 0.3) {
        s = rate*s
      }
    }
  }
  draws.2=matrix(0, nrow=niter, ncol=12)
  draws.2[1,]=draws.1[burnin,]
  for (j in 2:niter) {
    b.can=matrix(draws.2[j-1,1:11])
    draws.2[j,]=b.update(b.can, s)
  }
  #after tuning
  return(rbind(draws.1, draws.2))
}

######## variable info ###########
##################################
#b.data = read.table("/Users/Megan/Desktop/breast_cancer.txt", header=T)
b.data = read.table("/home/pccchen/Stuff/HW1/BayesLogit/breast_cancer.txt", header=T)
#dim(b.data) #569 x 11 
num=as.numeric(b.data$diagnosis)
new1=as.matrix((num-1), nrow=1, ncol=569) 
b.data.new =  cbind(b.data[,1:10], new1)

#####fit the baysian model (prior beta)
m=as.numeric(rep(1, 569))
y=as.numeric(b.data.new$new1); 
x.non=t(as.matrix(cbind(rep(1,569),  b.data.new[,1:10]))); 
dim(x.non) # 11 x 569

####standard X
stan = function(z) {
  x.stan = matrix(0, nrow=10, ncol=569)
  for (i in 1:10) {
    x.stan[i,] = (z[i,]-mean(z[i,]))/var(z[i,])
  }
  return(x.stan)
}
x.new=x.non[-1,]
X=rbind(x.non[1,], stan(x.new))

x.odd= data.frame(t(X[2:11,]))
dim(x.odd)
b.y = transform(x.odd, c=rep(1, 569))
bayes.bc=glm(c~.,data=b.y, family=binomial)
b.prior = bayes.bc$coefficients
### start value
b.0=matrix(b.prior, nrow=11, ncol=1)+0.01
s.0= vcov(bayes.bc)+0.01 # 11x 11

#####prior beta value
beta.0 <- matrix(rep(0, 11), nrow=11)
Sigma.0.inv <- diag(rep(1000,11))

####Posterior distribution 
bayes.2 = bayes.log.2(m,y,X, beta.0, Sigma.0.inv, b.start=b.0, s.start=s.0, niter=10000,
                       burnin=1000, retune=100)

which(bayes.2[1:1000,12]==1)
length(which(bayes.2[1:1000,12]==1))
rate.1= mean(bayes.2[1:1000,12]) 

which(bayes.2[1001:11000,12]==1) ## 
length(which(bayes.2[1001:11000,12]==1)) 
rate.2 = mean(bayes.2[1000:11000,12])
c(rate.1, rate.2)
#0.9 1.0

#### b) Comput lag-1 autocorrelation
b.2 = bayes.2[1001:11000,1:10]
n=11000-1000
#cov(b.2[,1], b.2[-1,1])
#auto = sapply(bayes.2.b[1:10], cov)

cov.b= function(r) { 
  a=c()
for (i in 1:10) {
  a[i]= cov(r[1:n-1,i], r[-1,i])
}
return(a)
}
cov.b(b.2)            

### c) autocorrelation result
#> cov.b(b.2)
#[1] 0 0 0 0 0 0 0 0 0 0
### c) 


  