## boolean3 test run
## https://cran.r-project.org/web/packages/boolean3/boolean3.pDF
require(boolean3)
require(mvtnorm)
set.seed(12345)

N <- 2000
Df <- cbind(1, rmvnorm(N, mean = rep(0,5)))

## Set coefficients
beta.a <- c(-2.00, 0.33, 0.66, 1.00)
beta.b <- c(0.00, 1.50, -0.25)

## Generate path probabilities following a normal model
y.a <- as.vector(pnorm(tcrossprod(beta.a, Df[, 1:4])))
y.b <- as.vector(pnorm(tcrossprod(beta.b, Df[, c(1, 5, 6)])))

## AND and OR-model
or <- function(x, y) { x + y - x * y }
and <- function(x, y) { x * y }
y.star.OR <- or(y.a, y.b)
y.star.AND <- and(y.a, y.b)

## Observed responses
y.OR <- rbinom(N, 1, y.star.OR)
y.AND <- rbinom(N, 1, y.star.AND)

## Set up data.frame for estimation
Df <- cbind(1, Df)
Df <- as.data.frame(Df)
Df[,1] <- y.OR
Df[,2] <- y.AND
names(Df) <- c("y.OR", "y.AND", "x1", "x2", "x3", "x4", "x5")

## Before estimating, boolean models need to be specified using the
## boolprep function.
## OR model, specified to use a probit link for each causal path. This
## matches the data generating process above.
mod.OR <- boolprep(y.OR ~ (a | b), a ~ x1 + x2 + x3, b ~ x4 + x5,
    data = Df, family=list(binomial("probit")))

## IF you really want to, it's also possible to specify a different
## link function for each causal path. These should be in the same
## order as specified in the model formula.
mod.OR2 <- boolprep(y.OR ~ (a | b), a ~ x1 + x2 + x3, b ~ x4 + x5,
          data = Df, family=list(binomial("probit"),
        binomial("logit")))


(fit.OR <- boolean(mod.OR, method="nlminb", control=list(trace=1)))

## DK: Testing if we get the same estimates by estimating two separate models

m1 <- glm(y.OR ~ x1 + x2 + x3, family = binomial(link = "logit"),
          data = Df)

m2 <- glm(y.OR ~ x4 + x5, family = binomial(link = "logit"),
          data = Df)

require(broom)
tidy(m1)
tidy(m2)

invlogit <- function(B){1/(1 + exp(-B))}

## x1_a
invlogit(0.465)
##0.614

## coef for m1 x1
invlogit(0.031)
## 0.507

## coef for m2 x5
## hmm...
