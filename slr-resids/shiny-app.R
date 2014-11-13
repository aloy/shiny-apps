
## Creating a shiny app for residual plots from SLR

library(ggplot2)

scatterplot <- function(x,y, title="", xlab="", ylab="") {
  d <- data.frame(x=x,y=y)
  p <- ggplot(d, aes(x=x,y=y)) + 
    geom_point() + 
    ggtitle(title) + 
    xlab(xlab) + 
    ylab(ylab)
  return(p)
}

# model form
n <- 30
intercept <- rnorm(1, sd = 2)
slope     <- rnorm(1, sd = 2)

## Properly specified model

# discrete setting
e <- rnorm(n)
x <- rep(0:5, length.out = n)
y <- intercept + slope * x + e

mod <- lm(y ~ x)

# continuous setting
e <- rnorm(n)
x <- runif(n, min = 0, max = 5)
y <- intercept + slope * x + e

mod <- lm(y ~ x)

# fitted model
qplot(x, y, geom = c("point", "smooth"), method = "lm", se = FALSE)

# residuals vs. fitted values
scatterplot(x = fitted(mod), y = resid(mod), xlab = "Fitted values", ylab = "Residuals") +
  geom_hline(yintercept=0)

# residual vs. x
scatterplot(x = x, y = resid(mod), xlab = "x", ylab = "Residuals") +
  geom_hline(yintercept=0)

## Fitting a line to a curve
n <- 30
intercept <- rnorm(1, sd = 2)
slope     <- rnorm(1, sd = 2)
slope2 <- rnorm(1)

# discrete setting
e <- rnorm(n)
x <- rep(0:5, length.out = n)
y <- intercept + slope * x + slope2 * x^2 + e

mod <- lm(y ~ x)

# continuous setting
e <- rnorm(n)
x <- runif(n, min = 0, max = 5)
y <- intercept + slope * x + slope2 * x^2 + e

mod <- lm(y ~ x)

# fitted model
qplot(x, y, geom = c("point", "smooth"), method = "lm", se = FALSE)

# residuals vs. fitted values
scatterplot(x = fitted(mod), y = resid(mod), xlab = "Fitted values", ylab = "Residuals") +
  geom_hline(yintercept=0)

# residual vs. x
scatterplot(x = x, y = resid(mod), xlab = "x", ylab = "Residuals") +
  geom_hline(yintercept=0)

## Nonconstant variance
## Fitting a line to a curve
n <- 100
intercept <- rnorm(1, sd = 2)
slope     <- rnorm(1, sd = 2)

# discrete setting
x <- rep(0:5, length.out = n)
e <- rnorm(n) * sqrt(x + .1)
e <- rnorm(n) * sqrt(5.1 - x)
y <- intercept + slope * x + e

mod <- lm(y ~ x)

# continuous setting
x <- runif(n, min = 0, max = 5)
e <- rnorm(n) * sqrt(x + .1)
e <- rnorm(n) * sqrt(5.1 - x)
y <- intercept + slope * x + e

mod <- lm(y ~ x)


## Both curve and nonconstant variance