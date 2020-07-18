# Exo 2
# Adapté de https://rstudio-pubs-static.s3.amazonaws.com/187747_3eb3fc30ad7f4d8e92ad73520a0ff8f5.html
load("golub.rda")

# Question 1

gol.fac <- factor(golub.cl, levels=0:1, labels = c("ALL","AML"))

vec <- 0 # on va stocker dans vec les p-values ainsi que les noms des gènes
vec.genes <- 1:3051

for (i in 1:3051) { # pour chaque gène
res <- t.test(golub[i,] ~ gol.fac, var.equal=FALSE)$p.value # p-value de i
res <- p.adjust(res,method="bonferroni") # on applique une correction
vec = c(vec,res)
}
names(vec) <- vec.genes
vec <- sort(vec,decreasing=TRUE) # les 10 gènes les plus significatifs sont en haut
# Réponse : 827, 844, 2085, 198, 1984, 276, 151, 303, 1855, 1739

