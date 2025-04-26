#-------------------------------------------------------------------------------
#' 00-data.r
#-------------------------------------------------------------------------------
#' jo dudding
#' April 2025
#' read in the datasets
#-------------------------------------------------------------------------------

source('scripts/_setup.r')

# read penn data ----------------------------------------------------------

midpoints <- seq(95, 215, by = 15)

penn <- tibble(raw = readLines('inputs/penn-state-students-weight.txt', warn = FALSE)) |> 
  mutate(
    gender = case_when(str_sub(raw, 1, 1) %in% c("M", "F") ~ raw),
    weight = case_when(! str_sub(raw, 1, 1) %in% c("M", "F") ~ str_split(raw, '\\s+')),
  ) |> 
  fill(gender) |> 
  unnest_longer(weight) |> 
  transmute(gender, weight = as.integer(weight)) |> 
  filter(! is.na(weight)) |> 
  mutate(
    midpoint = cut(weight, c(0, midpoints - 7.5, Inf), labels = c(0, midpoints)),
    z_score = (weight - mean(weight)) / sd(weight),
    z_band = case_when(
      abs(z_score) <= 1 ~ 1,
      abs(z_score) <= 2 ~ 2,
      TRUE ~ 3
    ) |> 
      factor()
  ) |> 
  group_by(gender) |> 
  mutate(
    z_score_gender = (weight - mean(weight)) / sd(weight)
  ) |> 
  ungroup()