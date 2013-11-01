
##
#
# Logistic regression
# 
# Y_{i} | \beta \sim \textrm{Bin}\left(n_{i},e^{x_{i}^{T}\beta}/(1+e^{x_{i}^{T}\beta})\right)
# \beta \sim N\left(\beta_{0},\Sigma_{0}\right)
#
##

library(mvtnorm)
library(coda)
library(MASS)

########################################################################################
########################################################################################
## Handle batch job arguments:

# 1-indexed version is used now.
args <- commandArgs(TRUE)

cat(paste0("Command-line arguments:\n"))
print(args)

####
# sim_start ==> Lowest simulation number to be analyzed by this particular batch job
###

#######################
sim_start <- 1000
length.datasets <- 200
#######################

if (length(args)==0){
  sinkit <- FALSE
  sim_num <- sim_start + 1
  set.seed(1330931)
} else {
  # Sink output to file?
  sinkit <- TRUE
  # Decide on the job number, usually start at 1000:
  sim_num <- sim_start + as.numeric(args[1])
  # Set a different random seed for every job number!!!
  set.seed(762*sim_num + 1330931)
}



# Simulation datasets numbered 1001-1200

########################################################################################
########################################################################################


bayes.log.1<- function(m,y,X, beta.0, Sigma.0.inv, b.start, s.start, niter=10000,
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
  draws.1=matrix(0, nrow=burnin, ncol=3)
  draws.1[1,]=c(b.0,0)
  s = s.0
  for (i in 2:burnin) {
    b.can=matrix(draws.1[i-1,1:2])
    draws.1[i,]=b.update(b.can, s)
    ### tuning sigma
    if (i%%retune == 0) {
      rate= mean(draws.1[(retune*(i/retune-1)+1):(retune*(i/retune)),3])  
      if (rate > 0.6) {
        s = 2*s
      }
      if (rate < 0.3) {
        s = rate*s
      }
    }
  }
  draws.2=matrix(0, nrow=niter, ncol=3)
  draws.2[1,]=draws.1[burnin,]
  for (j in 2:niter) {
    b.can=matrix(draws.2[j-1,1:2])
    draws.2[j,]=b.update(b.can, s)
  }
  #after tuning
  return(rbind(draws.1, draws.2))





#################################################
# Set up the specifications:
beta.0 <- matrix(c(0,0))
Sigma.0.inv <- diag(rep(1.0,2))
niter <- 10000

beta.0=as.matrix(rbind(0,0)); # 2 x1
Sigma.0.inv=diag(2) 


# etc... (more needed here)
#################################################

# Read data corresponding to appropriate sim_num:
#DIR = "data"
#file = paste("blr_data_",sim_num,".csv",sep="")
#f.p <- file.path(DIR, file)
#data = read.csv(f.p, header=TRUE)

read.file= paste("/home/pccchen/Stuff/HW1/BayesLogit/data/blr_data_", sim_num, sep="", ".csv")
data = read.csv(read.file, header=TRUE)
#par=read.csv(read.par, sim_num, header=T)


#read.par= paste("/home/pccchen/Stuff/HW1/BayesLogit/data/blr_pars_", sim_num, sep="", ".csv")
#par=read.csv(read.par, sim_num, header=T)

# Extract X and y:
m=as.numeric(data$n);
y=as.numeric(data$y); 
X=as.matrix(rbind(data$X1, data$X2)); #2x100
b.0 = rbind(-0.1, 0.1)
s.0=matrix(c(0.01, 0, 0, 0.01), nrow=2, ncol=2)


# Fit the Bayesian model:
bayes.1 = bayes.log.1(m=as.numeric(data$n), y=as.numeric(data$y), X=as.matrix(rbind(data$X1, data$X2)),
                       beta.0=as.matrix(rbind(0,0)),Sigma.0.inv=diag(2),
                       b.start=b.0, s.start=s.0 , niter=10000, burnin=1000, retune=100)
bayes.1
# Extract posterior quantiles...
p1 <- quantile((bayes.1[1001:10000,1]), probs=seq(from=0.01, to = 0.99, by =0.01))
p2 <- quantile((bayes.1[1001:10000,2]), probs=seq(from=0.01, to = 0.99, by = 0.01)) 
p = cbind(p1, p2)
# Write results to a (99 x p) csv file...



result= paste("/home/pccchen/Stuff/HW1/BayesLogit/results/blr_res_", sim_num, sep="", ".csv")

write.table(data.frame(p),file=result, quote=F, sep=",", col.names=F, row.names=FALSE)

}

# Go celebrate.
cat("done. :)\n")







