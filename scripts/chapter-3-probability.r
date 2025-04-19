#-------------------------------------------------------------------------------
#' chapter-3-probability.r
#-------------------------------------------------------------------------------
#' jo dudding
#' April 2025
#' chapter 3 data and code
#-------------------------------------------------------------------------------

source('scripts/_setup.r')  


# parameters --------------------------------------------------------------

n_people <- 1000
p_disease <- 0.001
p_pos_d <- 0.99
p_pos_n <- 0.02


# probability table -------------------------------------------------------

bayes <- tibble(
  disease = factor(c(0, 0, 1, 1)),
  test_pos = factor(c(0, 1, 0, 1)),
  outcome = c('TN', 'FP', 'FN', 'TP') |> 
    fct_inorder(),
  pct = c(
    (1 - p_disease) * (1 - p_pos_n),
    (1 - p_disease) * (p_pos_n),
    (p_disease) * (1 - p_pos_d),
    (p_disease) * (p_pos_d)
  ),
  n = round(pct * n_people)
) |> 
  print()


# confusion matrix --------------------------------------------------------

bayes_conf <- bayes |> 
  uncount(n) |> 
  yardstick::conf_mat(truth = disease, estimate = test_pos) |> 
  print()

conf_pal <-c('green', 'red4', 'red3', 'green4')
names(conf_pal) <- levels(bayes$outcome)
  
#show_col(conf_pal)

bayes |> 
  mutate(
    Truth = if_else(disease == 1, 'Yes', 'No'),
    Prediction = if_else(test_pos == 1, 'Yes', 'No'),
    label = glue("{outcome}\n{comma(n)}\n{percent(pct, 0.1)}")
  ) |> 
  ggplot(aes(Truth , Prediction, label = label, fill = outcome)) +
  geom_tile() +
  geom_text(colour = 'white', size = 4) +
  scale_fill_manual(values = conf_pal) +
  scale_x_discrete(
    expand = c(0, 0), 
    position = 'top'
  ) +
  scale_y_discrete(
    expand = c(0, 0)
  ) +
  labs(title = 'Confusion matrix') +
  guides(fill = 'none') +
  theme_cartoon(base_size = 12) +
  theme(
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.text = element_text(size = 4 * .pt),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.key.size = unit(0.8, 'lines'), 
    legend.key.width = unit(3.5, 'lines')
  )

gg_save(1, 'conf-matrix')
