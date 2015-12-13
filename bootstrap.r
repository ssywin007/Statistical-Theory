l1 = rep(1, 52)
l2 = rep(2, 21)
l3 = rep(3, 11)
l4 = rep(4, 8)
l5 = rep(5, 2)
l6 = rep(6, 5)
l14 = rep(14,1)
data = c(l1, l2, l3, l4, l5, l6, l14)
#bootstrap package, the variance is the same as the sample
library(boot)
mean.fun <- function(d, i)
{    m <- mean(d[i])
n <- length(i)
v <- (n-1)*var(d[i])/n^2
c(m, v)
}

set.seed(1994)
bb = boot(data, mean.fun, R = 9999)
boot.ci(bb)
#simulate by myself, the variance is the theoretical var with p_hat
num = 9999
pstar = numeric(num)
set.seed(1994)
for (i in 1:num){
  data = rgeom(100, 100 / 213)
  pstar[i] = sum(data) + 100
  }
s = sort(pstar)
p975 = 100 / s[250] 
p025 = 100 / s[9750]

#databoot = as.matrix(bb[[2]])
#resboot = 1 / databoot[,1] - 1 / 2.13
resboot = (100 / pstar) - (1 / 2.13)
qqnorm(resboot, ylab = 'difference between practical bootstrap p and mle p', main = 'difference between practical bootstrap p and mle p')
qqline(resboot)
hist(resboot, freq = F, main = 'difference between practical bootstrap p and mle p', xlab = 'difference between p from practical bootstrap and mle p')
lines(density(resboot, bw = 0.01), lwd = 2)

simup = 100 / (rnbinom(9999, 100, 0.5) + 100) - 0.5
qqnorm(simup, ylab = 'difference between simulated p and ture p', main = 'difference between simulated p and ture p')
qqline(simup)
hist(simup, freq = F, main = 'difference between simulated p and ture p', xlab = 'difference between simulated p and ture p')
lines(density(simup, bw = 0.01), lwd = 2)
  
