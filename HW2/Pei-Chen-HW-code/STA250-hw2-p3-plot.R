#### STA250 HW2-problem3 ###
means=read.table("/Users/Megan/big_mean/big_mean.txt", header=F)
var=read.table("/Users/Megan/big_var/big_var.txt", header=F)
class(means)
dim(means)
m = as.numeric(as.matrix(means, nrow=1000, ncol=1))
class(m)
v = as.numeric(as.matrix(var, nrow=1000, ncol=1))
scatterplot
plot(m, v, xlab="Means", ylab="Variance")