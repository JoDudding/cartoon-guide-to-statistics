#-------------------------------------------------------------------------------
#' chapter-06-sampling.r
#-------------------------------------------------------------------------------
#' jo dudding
#' April 2025
#' chapter 6 data and code
#-------------------------------------------------------------------------------

source('scripts/_setup.r')

# Sampling is governed by 1 / sqrt(n)

# sampling design ---------------------------------------------------------

# random
# - simple random
# - stratified
# - cluster
# - systematic
# - opportunity - prone to bias


# sample size and standard error ------------------------------------------

# bernoulli
# 1/0 success vs failure (binomial) with probability p and x trials
# probability of success - p hat = x / n
# standard deviation sqrt(p * (1 - p)) / sqrt(n)

tacks_x <- 1000
tacks_p <- 0.85

tacks_sd <- sqrt(tacks_p * (1-tacks_p)) /sqrt(tacks_x)
tacks_68_ci <- tacks_p + c(-1, 1) * tacks_sd

list(
  tacks_x = comma(tacks_x),
  tacks_p = percent(tacks_p, 0.1),
  tacks_sd = percent(tacks_sd, 0.1),
  tacks_68_ci = paste(percent(tacks_68_ci, 0.1), collapse = ' - ')
) |> 
  cli_dl()
