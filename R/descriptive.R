#' Calculate z-score
#'
#' This function calculates the z-score (standard score) for a value or vector of values.
#'
#' @param x A numeric vector of values.
#' @param mean The mean to use for standardization. If NULL, the mean of x is used.
#' @param sd The standard deviation to use for standardization. If NULL, the sd of x is used.
#'
#' @return A numeric vector of z-scores.
#' @export
#'
#' @examples
#' z_score(c(1, 2, 3, 4, 5))
#' z_score(c(10, 20, 30), mean = 20, sd = 10)
z_score <- function(x, mean = NULL, sd = NULL) {
  if(is.null(mean)) mean <- mean(x, na.rm = TRUE)
  if(is.null(sd)) sd <- sd(x, na.rm = TRUE)
  if(sd == 0) stop("Standard deviation is zero, z-scores cannot be calculated")
  return((x - mean) / sd)
}

#' Calculate confidence interval for the mean
#'
#' This function calculates a confidence interval for the mean of a numeric vector.
#'
#' @param x A numeric vector of values.
#' @param conf.level The confidence level (default is 0.95).
#'
#' @return A numeric vector with the lower and upper bounds of the confidence interval.
#' @export
#'
#' @examples
#' confidence_interval(rnorm(100))
#' confidence_interval(1:10, conf.level = 0.99)
confidence_interval <- function(x, conf.level = 0.95) {
  n <- length(x)
  if(n < 2) stop("Need at least two data points to calculate a confidence interval")

  mean_x <- mean(x, na.rm = TRUE)
  se <- sd(x, na.rm = TRUE) / sqrt(n)
  alpha <- 1 - conf.level
  t_crit <- qt(1 - alpha/2, df = n - 1)

  ci <- c(mean_x - t_crit * se, mean_x + t_crit * se)
  names(ci) <- c("lower", "upper")
  return(ci)
}

#' Detect outliers using IQR method
#'
#' This function identifies outliers in a dataset using the Interquartile Range (IQR) method.
#'
#' @param x A numeric vector of values.
#' @param multiplier The multiplier for the IQR to define the threshold (default is 1.5).
#'
#' @return A logical vector indicating which values are outliers.
#' @export
#'
#' @examples
#' outliers <- detect_outliers(c(1, 2, 3, 4, 100))
#' outliers
#' detect_outliers(rnorm(100), multiplier = 2)
detect_outliers <- function(x, multiplier = 1.5) {
  q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - multiplier * iqr
  upper_bound <- q[2] + multiplier * iqr

  return(x < lower_bound | x > upper_bound)
}
