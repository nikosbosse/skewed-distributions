library(distr)
library(purrr)

create_distribution <- function(parameters, coeff = c(0.4, 0.6)) {
  components <- lapply(parameters, function(x) {
    Norm(mean = x[1], sd = x[2])
  })

  distr <- do.call(UnivarMixingDistribution,
                   c(components, list(mixCoeff = c(0.4, 0.6))))
  return(d(distr))
}


## Construct the distribution object.
parameters <- data.frame(f1 = c(4, 1),
                         f2 = c(8, 2))


interpolate_parameters <- function(parameters1,
                                   parameters2) {
  parameters1 <- data.frame(f1 = c(0, 1))

  out <- 0

}

library(purr)
ll <- list(parameters1, parameters)
reduce(ll,`+`) / length(ll)


create_distribution(parameters)



mean11 <- 4
sd11 <- 1
mean12 <- 8
sd12 <- 2
target <- UnivarMixingDistribution(a, mixCoeff=c(0.4, 0.6))

w <- 0.5
inter <- UnivarMixingDistribution(Norm(mean=w * mean11, sd=((1-w)*1 + w * sd11)),
                                  Norm(mean=w * mean12, sd=((1-w)*1 + w * sd12)),
                                  mixCoeff=c(0.4, 0.6))

w <- 0.8
inter2 <- UnivarMixingDistribution(Norm(mean=w * mean11, sd=((1-w)*1 + w * sd11)),
                                  Norm(mean=w * mean12, sd=((1-w)*1 + w * sd12)),
                                  mixCoeff=c(0.4, 0.6))

w <- 0.2
inter3 <- UnivarMixingDistribution(Norm(mean=w * mean11, sd=((1-w)*1 + w * sd11)),
                                   Norm(mean=w * mean12, sd=((1-w)*1 + w * sd12)),
                                   mixCoeff=c(0.4, 0.6))

# density function
dtarget <- d(target)
dinter <- d(inter)
dinter2 <- d(inter2)
dinter3 <- d(inter3)

xmin = -5
xmax = 20
x <- seq(xmin, xmax, 0.05)
n = length(x)

y <- dnorm(x)
y_comp1 <- dnorm(x, mean = 5, sd = 1)
ytarget <- dtarget(x)
yinter <- dinter(x)
yinter2 <- dinter2(x)
yinter3 <- dinter3(x)

ggplot(data = data.frame(x = x, y = y), aes(x = x)) +
  geom_line(aes(y = y)) +
  geom_line(data = data.frame(y = yinter), aes(y = yinter), color = "blue") +
  geom_line(data = data.frame(y = yinter2), aes(y = yinter2), color = "blue") +
  geom_line(data = data.frame(y = yinter3), aes(y = yinter3), color = "blue") +
  geom_line(data = data.frame(y = ytarget), aes(y = ytarget), color = "red")

