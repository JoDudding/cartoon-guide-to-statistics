#-------------------------------------------------------------------------------
#' chapter-07-confidence-intervals.r
#-------------------------------------------------------------------------------
#' jo dudding
#' April 2025
#' chapter 7 data and code
#-------------------------------------------------------------------------------

source('scripts/_setup.r')


# estimating confidence intervals -----------------------------------------

astute_n <- 1000
astute_p_hat <- 0.55

astute_sd = sqrt(astute_p_hat * (1-astute_p_hat)) /sqrt(astute_n)

tibble(
  alpha = c(0.2, 0.1, 0.05, 0.01),
  minus_alpha = 1 - alpha,
  alpha_div_2 = alpha / 2,
  z_alpha_div_2 = qnorm(1 - alpha_div_2)
) |> 
  print()

z95 <- qnorm(1 - ((1 - 0.95) / 2))
z99 <- qnorm(1 - ((1 - 0.99) / 2))

astute_95_ci <- astute_p_hat + c(-z95, z95) * astute_sd
astute_95_margin_of_error <- z95 * astute_sd

astute_99_ci <- astute_p_hat + c(-z99, z99) * astute_sd

astute_required_error <- 0.01

astute_sample_required <- ((z99 ^ 2) * (0.5 ^ 2)) / astute_required_error ^ 2

list(
  astute_n = comma(astute_n),
  astute_p_hat = percent(astute_p_hat, 0.1),
  astute_sd = percent(astute_sd, 0.1),
  astute_95_margin_of_error = percent(astute_95_margin_of_error, 0.1),
  astute_95_error = glue('{percent(astute_p_hat,0.1)} ± {
    percent(astute_95_margin_of_error, 0.1)}'),
  astute_95_ci = paste(percent(astute_95_ci, 0.1), collapse = ' - '),
  astute_99_ci = paste(percent(astute_99_ci, 0.1), collapse = ' - '),
  astute_required_error = percent(astute_required_error, 0.1),
  astute_sample_required = comma(astute_sample_required)
) |> 
  cli_dl()

# polls problems
# - response bias - lie or change mind
# - potential voters vs actual voters
# - non-response bias - refuse poll

# confidence intervals for μ ----------------------------------------------

penn_list <- readRDS('data/penn-state-students-weight.rda')

penn <- penn_list$data

penn_n <- nrow(penn)
penn_mean <- mean(penn$weight)
penn_sd <- sd(penn$weight)
penn_se <- penn_sd / sqrt(penn_n)
penn_95_ci <- penn_mean + c(-z95, z95) * penn_se
penn_95_error <- z95 * penn_se

list(
  penn_n = comma(penn_n),
  penn_mean = comma(penn_mean, 0.1),
  penn_sd = comma(penn_sd, 0.1),
  penn_se = comma(penn_se, 0.1),
  penn_95_error = comma(penn_95_error, 0.1),
  penn_95_error = glue('{comma(penn_mean,0.1)} ± {comma(penn_95_error, 0.1)}'),
  penn_95_ci = paste(comma(penn_95_ci, 0.1), collapse = ' - ')
) |> 
  cli_dl()


# students t --------------------------------------------------------------

# used for small samples
# degrees of freedom n - 1

cli_h3('t distribution by degrees of freedom')

tibble(
  alpha = c(0.2, 0.1, 0.05, 0.01),
  minus_alpha = 1 - alpha,
  alpha_div_2 = alpha / 2,
  t_alpha_div_2_df1 = qt(1 - alpha_div_2, 1),
  t_alpha_div_2_df10 = qt(1 - alpha_div_2, 10),
  t_alpha_div_2_df30 = qt(1 - alpha_div_2, 30),
  t_alpha_div_2_df100 = qt(1 - alpha_div_2, 100),
  t_alpha_div_2_dfinf = qt(1 - alpha_div_2, Inf)
) |> 
  #gather(-alpha, key = 'key', value = 'value') |> 
  #spread(key = alpha, value = value) |> 
  print()

chameleon <- c(150, 400, 720, 500, 930)

chameleon_n <- length(chameleon)
chameleon_mean <- mean(chameleon)
chameleon_sd <- sd(chameleon)

t_95_4df <- qt(1 - ((1 - 0.95) / 2), chameleon_n - 1)

chameleon_se <- chameleon_sd / sqrt(chameleon_n)
chameleon_95_ci <- chameleon_mean + c(-t_95_4df, t_95_4df) * chameleon_se
chameleon_95_error <- t_95_4df * chameleon_se

list(
  chameleon_n = comma(chameleon_n),
  chameleon_mean = dollar(chameleon_mean, 0.1),
  chameleon_sd = dollar(chameleon_sd, 0.1),
  chameleon_se = dollar(chameleon_se, 0.1),
  chameleon_95_error = dollar(chameleon_95_error, 0.1),
  chameleon_95_error = glue('{dollar(chameleon_mean,0.1)} ± {dollar(chameleon_95_error, 0.1)}'),
  chameleon_95_ci = paste(dollar(chameleon_95_ci, 0.1), collapse = ' - ')
) |> 
  cli_dl()

cli_h3('t distribution by degrees of freedom')

tibble(
  alpha = c(0.2, 0.1, 0.05, 0.01),
  minus_alpha = 1 - alpha,
  alpha_div_2 = alpha / 2,
  t_alpha_div_2_df1 = qt(1 - alpha_div_2, 1),
  t_alpha_div_2_df2 = qt(1 - alpha_div_2, 2),
  t_alpha_div_2_df3 = qt(1 - alpha_div_2, 3),
  t_alpha_div_2_df4 = qt(1 - alpha_div_2, 4),
  t_alpha_div_2_df5 = qt(1 - alpha_div_2, 5)
) |> 
  #gather(-alpha, key = 'key', value = 'value') |> 
  #spread(key = alpha, value = value) |> 
  print()
