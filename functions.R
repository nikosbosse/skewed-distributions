library(ggplot2)
library(zoo)


# plotting function
make_plot <- function(xmin = -5, xmax = 5, cdf = TRUE, ...) {

  x <- seq(xmin, xmax, 0.05)
  n = length(x)

  if (!cdf) {
    y <- dnorm(x)
    y1 <- transformed_pdf(x, ...)
  } else {
    y <- pnorm(x)
    y1 <- transformed_cdf(x, ...)
  }
  df <- data.frame(
    y = y,
    x = x
  )

  df_transformed <- data.frame(
    y1 = y1
  )

  # check whether the integral is actually one
  # code stolen from https://stackoverflow.com/questions/4954507/calculate-the-area-under-a-curve
  # also check mean and median
  median <- x[which(cumsum(y1/sum(y1)) > 0.5)[1]]

  message <- paste(
    paste("Integral normal: ", sum(diff(df$x)*rollmean(df$y,2))),
    paste("Integral transformed: ", sum(diff(df$x)*rollmean(df$y1,2))),
    paste("Mean transformed: ", sum(x * y1 / sum(y1))),
    paste("Median transformed: ", median),
    sep = "\n"
  )

  cat(message)
  # print(sum(diff(df$x)*rollmean(df$y1,2)))

  ggplot(data = df, aes(x = x)) +
    geom_line(aes(y = y)) +
    geom_line(data = df_transformed, aes(y = y1), color = "red")
}

# 1st: unclipped (unbounded transformation of the cdf / pdf) -------------------

# a = skewness parameter between -1 and 1
# s = kurtosis parameter
# x0 = mode of the distribution
# s skewness. Not sure what the boundary is --> integral goes > 1 for values close to 0

transformed_pdf <- function(x, x0 = 0, s = 1, a = 0) {
  k <- ifelse(x < x0, -1, 1)
  x <- dnorm((x-x0) / (s * (1-k*a))) / s
  return(x)
}

transformed_cdf <- function(x, x0 = 0, s = 1, a = 0) {
  k <- ifelse(x < x0, -1, 1)
  c <- ifelse(x < x0, 1, 0)
  out <- c + k * (1 + a) * pnorm(abs((x - x0) / s * (1 + a)))
  return(out)
}

transformed_cdf(
  x = -0.1,
  s = 1,
  a = 0.6,
  x0 = 0.0
)



make_plot(x0 = 0, a = 0.6, s = 1, xmin = -5, xmax = 5, cdf = TRUE)

# Adding functionality to invert the PDF and CDF -------------------------------
# transformed_pdf_with_invert <- function(x, x0 = 0, s = 1, a = 0, invert = FALSE) {
#   k <- ifelse(x < x0, -1, 1)
#   x <- dnorm((x-x0) / (s * (1-k*a))) / s
#   return(x)
# }

transformed_cdf_with_invert <- function(x, x0 = 0, s = 1, a = 0, invert = FALSE) {
  # when invert = TRUE, returns 1 - cdf
  # invert = TRUE means that x0 >= 0.5.

  a <- ifelse(x < x0, -a, a)

  # invert == TRUE --> c = 1
  # x < x0 --> c = 1, since ^0 is always 1
  # invert == FALSE AND x > x0 --> c = 0
  c <- ifelse(!invert, 1, 0) ^ (x > x0)

    # k is 1 if c is 0 and -1 else
  k <- 1 - 2 * c

  out <- c + k * (1 + a) * pnorm(-abs((x - x0) / s * (1 + a)))
  return(out)
}

transformed_cdf2 <- function(x, ...) {
  invert <- ifelse(x0 < 0.5, TRUE, FALSE)
  invert = FALSE

  # invert = FALSE
  y <- transformed_cdf_with_invert(x = x, invert = invert, ...)
  # ya <- transformed_cdf_with_invert(x = 0, invert = invert, ...)
  # yb <- transformed_cdf_with_invert(x = 1, invert = invert, ...)
  #
  # r = (y - ya) / (yb - ya)
  #
  # w = 1
  # low = 0
  # high = 1
  #
  # out <- w * (low + (high - low) * r)
  out <- y
  return(out)
}

make_plot(x0 = 2, a = 0.3, s = 2, xmin = -5, xmax = 5, cdf = FALSE) +
  geom_line(data = data.frame(
    y2 = transformed_pdf(
      x = seq(-5, 5, 0.05),
      x0 = 1,
      s = 1.5,
      a = 0.15
    )
  ),
  aes(y = y2),
  color = "blue"
  ) +
  geom_line(data = data.frame(
    y2 = transformed_pdf(
      x = seq(-5, 5, 0.05),
      x0 = 0.3 * x02,
      s = 0.7 * 1 + 0.3 * s2,
      a = 0.3 * a2
    )
  ),
  aes(y = y2),
  color = "blue"
  ) +
  geom_line(data = data.frame(
    y2 = transformed_pdf(
      x = seq(-5, 5, 0.05),
      x0 = 0.7 * x02,
      s = 0.3 * 1 + 0.7 * s2,
      a = 0.7 * a2
    )
  ),
  aes(y = y2),
  color = "blue"
  ) +
  geom_line(data = data.frame(
    y2 = transformed_pdf(
      x = seq(-5, 5, 0.05),
      x0 = 0.15 * x02,
      s = 0.85 * 1 + 0.15 * s2,
      a = 0.15 * a2
    )
  ),
  aes(y = y2),
  color = "blue"
  )



















# clipped (bounded transformation of the cdf / pdf) -------------------

# a = skewness parameter between -1 and 1
# s = kurtosis parameter
# x0 = mode of the distribution
# s skewness. Not sure what the boundary is --> integral goes > 1 for values close to 0

transformed_pdf_bounded <- function(x, x0 = 0, s = 1, a = 0) {
  k <- ifelse(x < x0, -1, 1)
  x <- dnorm((x-x0) / (s * (1-k*a))) / s
  return(x)
}

transformed_pdf <- function(x, a = 0, s = 1, x0 = 0, w = 1, high = 1, low = 0) {
  invert <- ifelse(x < x0, TRUE, FALSE)
  ya <- qnorm(0.000001)
  yb <- qnorm(0.999999)
  k = 1
  dy = k * dnorm((x-x0) / (s * (1-k*a))) / s
  w * ((high - low) * dy / (yb - ya))
}



make_plot(x0 = 0, a = 0, s = 1, xmin = -5, xmax = 5, cdf = TRUE)


# x0 || 0.0;
# s = s || 0.5; // Kurtosis
# w = w || 1.0;  // Weight. Used for MultiDist.
# a = a || 0.0;  // Shape (skewness). -1 to 1
# centerSkew = 1.0;  // 0 to infinity. Used to set center out of bounds.

min_s = 0.007;
max_s = 10;
low = 0.0; # I think boundaries for the CDF
high = 1.0; # I think boundaries for the CDF



# Questions:
# what's k?
# what are the exact limits for the different values?
# is there any sort of documentation to it?
# why is there a minimum and a maximum value?
# how are distributions combined?
# how do you get an ensemble
# what would be a sensible way to interpolate between different functions?
# why logistic distributions?
# how do the different knobs at the two sides work?




