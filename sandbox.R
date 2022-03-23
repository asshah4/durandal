# formulas
f1 <- y ~ x
f2 <- y ~ c
f3 <- x ~ c
fl <- c(f1, f2, f3)

# paths
p1 <- path.formula(f1)
p2 <- path.formula(f2)
p3 <- path.formula(f3)
pl <- c(p1, p2, p3)

rls <- list(y ~ "outcome", x ~ "exposure", c ~ "confounder")
