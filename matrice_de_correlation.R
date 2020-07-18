# Exercice 1

library(MASS)
n <- 80
Sigma <- matrix(c(1,-.5,-.5,0,
                  -.5,1,.5,-.5,
                  -.5,.5,1,-.5,
                  0 ,-.5,-.5,1), nc=4)
dat <- mvrnorm(n, rep(0, 4), Sigma)
dat <- cbind(dat, replicate(40, rnorm(n, 0, 1)))
colnames(dat) <- c("y", paste("x", 1:(ncol(dat)-1), sep=""))

# Adapté de : http://www.sthda.com/french/wiki/matrice-de-correlation-la-fonction-r-qui-fait-tout
# Compute the matrix of correlation p-values
library(corrplot)
cor.pmat <- function(x, ...) {
  mat <- as.matrix(x)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
res <- cor.pmat(dat) #notre matrice de corrélation (cf. question 1)
vecteur <- res["y",]
vecteur <- sort(vecteur)
vecteur[1]
vecteur[2]
vecteur[3]
vecteur[4]
vecteur[5]


