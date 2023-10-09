test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


## constant over time?

set.seed(5)

N <- 100
P <- 3
mu <- numeric(P)
sd <- rep(1, P)
cor <- diag(P)
levels <- rep(Inf, P)


cov <- diag(sd) %*% cor %*% diag(sd)

df <- as.data.frame(MASS::mvrnorm(n = N, mu = mu, Sigma = cov))

brks <- quantile(df$V3, seq(0, 1, length.out = 10))
ints <- findInterval(df$V3, brks, all.inside = T)
df$V3 <- (brks[ints] + brks[ints + 1]) / 2


head(df)

cor(df)
