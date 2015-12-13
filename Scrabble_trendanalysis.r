Scrabble = read.table('G:/UCHI/24410/HW/scrabble5566.txt',col.names=c("Computer","Professor"))
winner <- ifelse(Scrabble$Computer < Scrabble$Professor, 1, 0)
n <- dim(Scrabble)[1]
num <- c(1:n)
model <- lm(winner ~ num)
p <- sum(winner) / n
#L1 <- p ^ sum(winner) * (1 - p) ^ (n - sum(winner))
p0 <- as.double(coef(model)[1])
k <- as.double(coef(model)[2])
#L2 <- prod((p0 + k * num) ^ winner * (1 - p0 - k * num) ^ (1 - winner))
LR <- prod((p / (p0 + k * num)) ^ winner * ((1 - p) / (1 - p0 - k * num)) ^ (1 - winner))
chi <- -2 * log(LR)
pvalue <- 1- pchisq(chi, 1)

modell <- glm(winner ~ num, family = 'binomial')
LR <- prod((p / modell$fitted.values) ^ winner * ((1 - p) / (1 - modell$fitted.values)) ^ (1 - winner))
chi <- -2 * log(LR)
pvalue <- 1- pchisq(chi, 1)

ks <- NULL
set.seed(199442)
computer <- Scrabble$Computer
professor <- Scrabble$Professor
for (i in 1:9999){
  tempprof <- sample(professor)
  tempwinner <- ifelse(computer < tempprof, 1, 0)
  modeltemp <- lm(tempwinner ~ num)
  ks <- c(ks, as.double(coef(modeltemp)[2]))
}
CI <- quantile(ks, c(0.025, 0.975))
