mini <- FALSE
#============================== Setup for running on Gauss... ==============================#
args <- commandArgs(TRUE)
cat("Command-line arguments:\n")
print(args)
####
# sim_start ==> Lowest possible dataset number
###

###################
sim_start <- 1000
###################

if (length(args)==0){
  sim_num <- sim_start + 1
  set.seed(121231)
} else {
  # SLURM can use either 0- or 1-indexing...
  # Lets use 1-indexing here...
  sim_num <- sim_start + as.numeric(args[1])
  sim_seed <- (762*(sim_num-1) + 121231)
}

cat(paste("\nAnalyzing dataset number ",sim_num,"...\n\n",sep=""))

# Find r and s indices:
##s_index: if the reminder = 0, then s_index is the quotient
##s_index: if the reminder !=0, then the s_index is the quotient + 1
##r_index: if the reminder =0, r_index =50, if the reminder doesn't =0, r_index equals the reminder.
if (sim_num%%50 == 0) {
  s_index= (sim_num-1000)%/%50 
  r_index= 50
} else  {
  s_index=(sim_num-1000)%/%50 + 1
  r_index=(sim_num-1000)%%50
}

#============================== Run the simulation study ==============================#

# Load packages:
library(BH)
library(bigmemory.sri)
library(bigmemory)
library(biganalytics)

# I/O specifications:
datapath <- "/home/pdbaines/data"
outpath <- "output/"

# choose the full dataset since mini is false
if (mini){
  rootfilename <- "blb_lin_reg_mini"
} else {
  rootfilename <- "blb_lin_reg_data"
}

# Filenames: paste the rootfilename with the .desc file in order to use attach.big.matrix
filename <- paste(rootfilename, ".desc", sep="")
# Set up I/O stuff: 
file <-file.path(datapath, filename)
# Attach big.matrix :
blb <-attach.big.matrix(file)
# Remaining BLB specs: set.seed for each s_index
## so that when are run the boostrap it will come from the same s_index
##each of the s_index runs 5 times
set.seed(s_index)
# Extract the subset:
##sample from n rows and draw B rows without replacement
##B equals the round number of n^0.7
g=0.7 ## gamma
n=1000000
B=round(n^g)
bootstrap.1<-function(data, N, gamma){
  B=round(n^gamma)
  m=data[sample(1:n, B, replace=FALSE), ]
  return(m)
}
newd=bootstrap.1(blb, N=n, gamma=g)
# Reset simulation seed:
##since we need to run each s_index 50 times (r=50), we need to reset the simulation seed 
## and we can simply do this by setting the seed as sim_num, since each sim_num is different
set.seed(sim_num)
# Bootstrap dataset:
##we need to generate 1 vector with size =n, and the probabbility drawing from the bootstrap is 1/B
prob=rmultinom(1, size=n, p=rep(1/B, B))

# Fit lm: 
##Fit lm with weights=prob and fit without the intercept
reg= lm(X1001~.-1, data=data.frame(newd), weights=prob)
coef <-as.matrix(reg$coefficients, nrow=length(reg$coefficients), ncol=1)

# Output file:
outfile = paste0("output/","coef_",sprintf("%02d",s_index),"_",sprintf("%02d",r_index),".txt")

# Save estimates to file:
write.table(coef, file=outfile, quote=F, sep=",", row.names=FALSE)