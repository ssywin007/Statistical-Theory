Scrabble = read.table('G:/UCHI/24410/HW/h8/scrabble5566.txt',col.names=c("Computer","Professor"))
hist(Scrabble$Professor, breaks = 45, xlim = c(200, 650))
summary(Scrabble)

lastdigit <- Scrabble[, 2] %% 10
o1 <- c(sum(lastdigit == 0), sum(lastdigit == 1), sum(lastdigit == 2), sum(lastdigit == 3), sum(lastdigit == 4), sum(lastdigit == 5), sum(lastdigit == 6), sum(lastdigit == 7), sum(lastdigit == 8), sum(lastdigit == 9))
e1 <- rep(5566 / 10, 10)
chi <- sum((e1 - o1) ^ 2 / e1)
p <- 1- pchisq(chi, 9)
  
tensdigit <- (Scrabble[, 2] %/% 10) %% 10
o2 <- c(sum(tensdigit == 0), sum(tensdigit == 1), sum(tensdigit == 2), sum(tensdigit == 3), sum(tensdigit == 4), sum(tensdigit == 5), sum(tensdigit == 6), sum(tensdigit == 7), sum(tensdigit == 8), sum(tensdigit == 9))
e2 <- rep(5566 / 10, 10)
chi <- sum((e2 - o2) ^ 2 / e2)
p <- 1- pchisq(chi, 9)

s300_400 = subset(Scrabble, Scrabble$Professor < 400 & Scrabble$Professor >= 300)
lastdigit <- s300_400[, 2] %% 10
o3 <- c(sum(lastdigit == 0), sum(lastdigit == 1), sum(lastdigit == 2), sum(lastdigit == 3), sum(lastdigit == 4), sum(lastdigit == 5), sum(lastdigit == 6), sum(lastdigit == 7), sum(lastdigit == 8), sum(lastdigit == 9))
e3 <- rep(dim(s300_400)[1] / 10, 10)
chi <- sum((e3 - o3) ^ 2 / e3)
p <- 1- pchisq(chi, 9)

tensdigit <- (s300_400[, 2] %/% 10) %% 10
o4 <- c(sum(tensdigit == 0), sum(tensdigit == 1), sum(tensdigit == 2), sum(tensdigit == 3), sum(tensdigit == 4), sum(tensdigit == 5), sum(tensdigit == 6), sum(tensdigit == 7), sum(tensdigit == 8), sum(tensdigit == 9))
e4 <- rep(dim(s300_400)[1] / 10, 10)
chi <- sum((e4 - o4) ^ 2 / e4)
p <- 1- pchisq(chi, 9)
