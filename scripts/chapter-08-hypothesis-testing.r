#-------------------------------------------------------------------------------
#' chapter-08-hypothesis-testing.r
#-------------------------------------------------------------------------------
#' jo dudding
#' April 2025
#' chapter 8 data and code
#-------------------------------------------------------------------------------

source('scripts/_setup.r')

# null hypothesis - result purely by chance
# alternate hypothesis - real effect

p_val_alpha <- 0.05

jurors_pop_pct <- 0.5
jurors_panel_n <- 80
jurors_panel_x <- 4

jurors_panel_pct <- jurors_panel_x / jurors_panel_n

# TODO check this
jurors_prob <- pbinom(jurors_panel_x, jurors_panel_n, jurors_pop_pct)
jurors_signif <- jurors_prob <= p_val_alpha

list(
  jurors_pop_pct = percent(jurors_pop_pct, 0.1),
  jurors_panel_n = comma(jurors_panel_n),
  jurors_panel_x = comma(jurors_panel_x),
  jurors_panel_pct = percent(jurors_panel_pct, 0.1),
  jurors_prob = as.character(jurors_prob),
  jurors_signif = jurors_signif
) |> 
  cli_dl()

# large sample significance test for proportions --------------------------

astute_pop_pct <- 0.5
astute_n <- 1000
astute_pct <- 0.55
astute_x <- astute_n * astute_pct

astute_test_stat <- (astute_pct - astute_pop_pct) / 
  (sqrt(astute_pop_pct * astute_pop_pct) / sqrt(astute_n))

astute_test_p <- pnorm(astute_test_stat, lower.tail = FALSE)
astute_signif <- astute_test_p <= p_val_alpha

list(
  astute_pop_pct = percent(astute_pop_pct),
  astute_n = comma(astute_n),
  astute_x = comma(astute_x),
  astute_pct = percent(astute_pct),
  astute_test_p = comma(astute_test_p, 0.00001),
  astute_signif = astute_signif
) |> 
  cli_dl()


# large sample test for the population mean -------------------------------

granola_pop_mean <- 16
granola_samp_n <- 49
granola_samp_mean = 15.90
granola_samp_sd = 0.35

granola_test_stat <- (granola_samp_mean - granola_pop_mean) / 
  (granola_samp_sd / sqrt(granola_samp_n))

granola_test_p <- pnorm(granola_test_stat, lower.tail = TRUE)
granola_signif <- granola_test_p <= p_val_alpha

list(
  granola_pop_mean = comma(granola_pop_mean, 0.1),
  granola_samp_mean = comma(granola_samp_mean, 0.1),
  granola_samp_sd = comma(granola_samp_sd, 0.01),
  granola_samp_n = comma(granola_samp_n),
  granola_test_p = comma(granola_test_p, 0.00001),
  granola_signif = granola_signif
) |> 
  cli_dl()

# small sample test for the population mean -------------------------------

