#-------------------------------------------------------------------------------
#' chapter-09-comparing-two-populations.r
#-------------------------------------------------------------------------------
#' jo dudding
#' April 2025
#' chapter 9 data and code
#-------------------------------------------------------------------------------

source('scripts/_setup.r')


# comparing success rates for two populations -----------------------------

aspirin <- tibble(
  attack = c("Attack", "Attack", "No attack", "No attack") |>
    fct_inorder(),
  group = c("Placebo", "Aspirin", "Placebo", "Aspirin") |>
    fct_inorder(),
  n = c(239, 139, 10795, 10898),
) |>
  group_by(group) |>
  mutate(pct = n / sum(n)) |>
  ungroup() |>
  print()

aspirin_event <- aspirin |>
  group_by(group) |>
  summarise(
    event = sum((attack == "Attack") * n),
    tot_n = sum(n),
    event_pct = event / tot_n,
  ) |>
  ungroup() |>
  mutate(
    event_sd = (event_pct * (1 - event_pct)) / tot_n,
    relative_risk = case_when(
      group == "Placebo" ~ event_pct / (sum(event_pct) - event_pct)
    ),
    difference = first(event_pct) - last(event_pct),
    se = sqrt(sum(event_sd)),
    pooled_pct = sum(event) / sum(tot_n),
    se_pooled = sqrt(
      first(pooled_pct) * 
      (1 - (pooled_pct)) * 
      ((1 / first(tot_n)) + (1 / last(tot_n)))
    ),
    z = difference / se_pooled
  ) |>
  print()

confidence_level <- 0.95
direction <- "two.sided" # 'greater', 'less

aspirin_test <- prop.test(
  x = aspirin_event$event,
  n = aspirin_event$tot_n,
  conf.level = confidence_level,
  alternative = direction,
  correct = FALSE
) |>
  broom::tidy() |>
  rename(p_value = p.value) |>
  mutate(
    across(starts_with("estimate"), ~percent(.x, 0.01)),
    p_value_format = percent(p_value, 0.00001),
    interpret = case_when(
      p_value > (1 - confidence_level) ~ "Insufficient evidence of difference",
      TRUE ~ "Evidence of difference"
    ),
    confidence_level = percent(confidence_level)
  ) |>
  glimpse() |>
  as.list()
