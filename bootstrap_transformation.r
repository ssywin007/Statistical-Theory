l1 = rep(0, 52)
l2 = rep(1, 21)
l3 = rep(2, 11)
l4 = rep(3, 8)
l5 = rep(4, 2)
l6 = rep(5, 5)
l14 = rep(13,1)
data = c(l1, l2, l3, l4, l5, l6, l14)

library(boot)
mean.fun <- function(d, i)
{    m <- mean(d[i])
n <- length(i)
v <- var(d[i])/n
c(m, v)
}

set.seed(199442)
bb = boot(data, mean.fun, R = 9999)
ci1 = boot.ci(bb, type = 'basic')
ci2 = boot.ci(bb, h = function(u) asinh(sqrt((100 * u + 3 / 8) / 99.25)), hinv = function(v) (99.25 * sinh(v) ^ 2 - 3 / 8) / 100, type="basic")
ci3 = boot.ci(bb, h = function(u) log(100 * u + 50), hinv = function(v) (exp(v) - 50) / 100, type="basic")


#compute p_low, 1 - p_high for 3 95% CI's, generate samples theoreticcally
#when n is 100, 500, 1000, 5000, 10000, 50000, 100000
num = 100000
pstar = numeric(num)
ptlow1 = numeric(7)
pthigh1 = numeric(7)
ptlow2 = numeric(7)
pthigh2 = numeric(7)
ptlow3 = numeric(7)
pthigh3 = numeric(7)
for (i in 1:num){
  simudata = rgeom(100, 100 / 213)
  pstar[i] = sum(simudata) / 100
}
pthigh1[1] = sum(pstar[1:100] < ci1[[4]][4]) / 100
ptlow1[1] = sum(pstar[1:100] > ci1[[4]][5]) / 100
pthigh1[2] = sum(pstar[1:500] < ci1[[4]][4]) / 500
ptlow1[2] = sum(pstar[1:500] > ci1[[4]][5]) / 500
pthigh1[3] = sum(pstar[1:1000] < ci1[[4]][4]) / 1000
ptlow1[3] = sum(pstar[1:1000] > ci1[[4]][5]) / 1000
pthigh1[4] = sum(pstar[1:5000] < ci1[[4]][4]) / 5000
ptlow1[4] = sum(pstar[1:5000] > ci1[[4]][5]) / 5000
pthigh1[5] = sum(pstar[1:10000] < ci1[[4]][4]) / 10000
ptlow1[5] = sum(pstar[1:10000] > ci1[[4]][5]) / 10000
pthigh1[6] = sum(pstar[1:50000] < ci1[[4]][4]) / 50000
ptlow1[6] = sum(pstar[1:50000] > ci1[[4]][5]) / 50000
pthigh1[7] = sum(pstar[1:100000] < ci1[[4]][4]) / 100000
ptlow1[7] = sum(pstar[1:100000] > ci1[[4]][5]) / 100000

pthigh2[1] = sum(pstar[1:100] < ci2[[4]][4]) / 100
ptlow2[1] = sum(pstar[1:100] > ci2[[4]][5]) / 100
pthigh2[2] = sum(pstar[1:500] < ci2[[4]][4]) / 500
ptlow2[2] = sum(pstar[1:500] > ci2[[4]][5]) / 500
pthigh2[3] = sum(pstar[1:1000] < ci2[[4]][4]) / 1000
ptlow2[3] = sum(pstar[1:1000] > ci2[[4]][5]) / 1000
pthigh2[4] = sum(pstar[1:5000] < ci2[[4]][4]) / 5000
ptlow2[4] = sum(pstar[1:5000] > ci2[[4]][5]) / 5000
pthigh2[5] = sum(pstar[1:10000] < ci2[[4]][4]) / 10000
ptlow2[5] = sum(pstar[1:10000] > ci2[[4]][5]) / 10000
pthigh2[6] = sum(pstar[1:50000] < ci2[[4]][4]) / 50000
ptlow2[6] = sum(pstar[1:50000] > ci2[[4]][5]) / 50000
pthigh2[7] = sum(pstar[1:100000] < ci2[[4]][4]) / 100000
ptlow2[7] = sum(pstar[1:100000] > ci2[[4]][5]) / 100000

pthigh3[1] = sum(pstar[1:100] < ci3[[4]][4]) / 100
ptlow3[1] = sum(pstar[1:100] > ci3[[4]][5]) / 100
pthigh3[2] = sum(pstar[1:500] < ci3[[4]][4]) / 500
ptlow3[2] = sum(pstar[1:500] > ci3[[4]][5]) / 500
pthigh3[3] = sum(pstar[1:1000] < ci3[[4]][4]) / 1000
ptlow3[3] = sum(pstar[1:1000] > ci3[[4]][5]) / 1000
pthigh3[4] = sum(pstar[1:5000] < ci3[[4]][4]) / 5000
ptlow3[4] = sum(pstar[1:5000] > ci3[[4]][5]) / 5000
pthigh3[5] = sum(pstar[1:10000] < ci3[[4]][4]) / 10000
ptlow3[5] = sum(pstar[1:10000] > ci3[[4]][5]) / 10000
pthigh3[6] = sum(pstar[1:50000] < ci3[[4]][4]) / 50000
ptlow3[6] = sum(pstar[1:50000] > ci3[[4]][5]) / 50000
pthigh3[7] = sum(pstar[1:100000] < ci3[[4]][4]) / 100000
ptlow3[7] = sum(pstar[1:100000] > ci3[[4]][5]) / 100000

ptlow1
pthigh1
ptlow2
pthigh2
ptlow3
pthigh3


#compute p_low, 1 - p_high for 3 95% CI's, generate samples by bootstrap
#when n is 100, 500, 1000, 5000, 10000, 50000, 100000
pblow1 = numeric(7)
pbhigh1 = numeric(7)
pblow2 = numeric(7)
pbhigh2 = numeric(7)
pblow3 = numeric(7)
pbhigh3 = numeric(7)

set.seed(19940402)
testb = boot(data, mean.fun, R = 100000)
testp = testb[[2]]
testp = testp[,1]

pbhigh1[1] = sum(testp[1:100] < ci1[[4]][4]) / 100
pblow1[1] = sum(testp[1:100] > ci1[[4]][5]) / 100
pbhigh1[2] = sum(testp[1:500] < ci1[[4]][4]) / 500
pblow1[2] = sum(testp[1:500] > ci1[[4]][5]) / 500
pbhigh1[3] = sum(testp[1:1000] < ci1[[4]][4]) / 1000
pblow1[3] = sum(testp[1:1000] > ci1[[4]][5]) / 1000
pbhigh1[4] = sum(testp[1:5000] < ci1[[4]][4]) / 5000
pblow1[4] = sum(testp[1:5000] > ci1[[4]][5]) / 5000
pbhigh1[5] = sum(testp[1:10000] < ci1[[4]][4]) / 10000
pblow1[5] = sum(testp[1:10000] > ci1[[4]][5]) / 10000
pbhigh1[6] = sum(testp[1:50000] < ci1[[4]][4]) / 50000
pblow1[6] = sum(testp[1:50000] > ci1[[4]][5]) / 50000
pbhigh1[7] = sum(testp[1:100000] < ci1[[4]][4]) / 100000
pblow1[7] = sum(testp[1:100000] > ci1[[4]][5]) / 100000

pbhigh2[1] = sum(testp[1:100] < ci2[[4]][4]) / 100
pblow2[1] = sum(testp[1:100] > ci2[[4]][5]) / 100
pbhigh2[2] = sum(testp[1:500] < ci2[[4]][4]) / 500
pblow2[2] = sum(testp[1:500] > ci2[[4]][5]) / 500
pbhigh2[3] = sum(testp[1:1000] < ci2[[4]][4]) / 1000
pblow2[3] = sum(testp[1:1000] > ci2[[4]][5]) / 1000
pbhigh2[4] = sum(testp[1:5000] < ci2[[4]][4]) / 5000
pblow2[4] = sum(testp[1:5000] > ci2[[4]][5]) / 5000
pbhigh2[5] = sum(testp[1:10000] < ci2[[4]][4]) / 10000
pblow2[5] = sum(testp[1:10000] > ci2[[4]][5]) / 10000
pbhigh2[6] = sum(testp[1:50000] < ci2[[4]][4]) / 50000
pblow2[6] = sum(testp[1:50000] > ci2[[4]][5]) / 50000
pbhigh2[7] = sum(testp[1:100000] < ci2[[4]][4]) / 100000
pblow2[7] = sum(testp[1:100000] > ci2[[4]][5]) / 100000

pbhigh3[1] = sum(testp[1:100] < ci3[[4]][4]) / 100
pblow3[1] = sum(testp[1:100] > ci3[[4]][5]) / 100
pbhigh3[2] = sum(testp[1:500] < ci3[[4]][4]) / 500
pblow3[2] = sum(testp[1:500] > ci3[[4]][5]) / 500
pbhigh3[3] = sum(testp[1:1000] < ci3[[4]][4]) / 1000
pblow3[3] = sum(testp[1:1000] > ci3[[4]][5]) / 1000
pbhigh3[4] = sum(testp[1:5000] < ci3[[4]][4]) / 5000
pblow3[4] = sum(testp[1:5000] > ci3[[4]][5]) / 5000
pbhigh3[5] = sum(testp[1:10000] < ci3[[4]][4]) / 10000
pblow3[5] = sum(testp[1:10000] > ci3[[4]][5]) / 10000
pbhigh3[6] = sum(testp[1:50000] < ci3[[4]][4]) / 50000
pblow3[6] = sum(testp[1:50000] > ci3[[4]][5]) / 50000
pbhigh3[7] = sum(testp[1:100000] < ci3[[4]][4]) / 100000
pblow3[7] = sum(testp[1:100000] > ci3[[4]][5]) / 100000

pblow1
pbhigh1
pblow2
pbhigh2
pblow3
pbhigh3

1/ (1+ci1[[4]][4])
1/ (1+ci1[[4]][5])

