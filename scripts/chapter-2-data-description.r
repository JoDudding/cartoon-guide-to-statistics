#-------------------------------------------------------------------------------
#' chapter-2-data-description.r
#-------------------------------------------------------------------------------
#' jo dudding
#' April 2025
#' chapter 1 data and code
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


# common text -------------------------------------------------------------

penn_text <- 'Penn State students weight in pounds'
penn_caption <- 'Source: The cartoon guide to statistics'

# assign colours ----------------------------------------------------------

gender_pal <- plasma_pal[c(2, 8, 5)]
names(gender_pal) <- c(unique(penn$gender), 'Total')

#show_col(gender_pal)

z_pal <- plasma_pal[c(3, 5, 7)]
names(z_pal) <- c(1, 2, 3)

#show_col(z_pal)

# save --------------------------------------------------------------------

list(
  data = penn,
  gender_pal = gender_pal,
  z_pal = z_pal,
  penn_text = penn_text,
  penn_caption = penn_caption
) |> 
  saveRDS('data/penn-state-students-weight.rda')

# create dotplot ----------------------------------------------------------

penn |> 
  ggplot(aes(weight, fill = z_band)) +
  geom_dotplot(binwidth = 1, dotsize = 2) +
  #geom_function(fun = dnorm, args = list(mean = mean(penn$weight), 
  #  sd = sd(penn$weight))) +
  scale_x_continuous(breaks = c(50, 100, 150)) +
  scale_y_continuous(expand = expansion(mult = c(0, .05))) +
  scale_fill_manual(values = z_pal) +
  labs(
    x = 'Weight in Pounds',
    y = NULL,
    title = glue('Dotplot of {penn_text}'),
    subtitle = 'Colour by z-score band',
    caption = penn_caption
  ) +
  guides(y = 'none', fill = 'none')

gg_save(3, 'dotplot')

# summarise by weight bands -----------------------------------------------

penn_freq <- penn |> 
  count(midpoint, name = 'frequency') |> 
  transmute(
    class_interval = glue('{as.numeric(as.character(midpoint)) - 7.5
      }-{as.numeric(as.character(midpoint)) + 7.4}'),
    midpoint,
    frequency,
    relative_frequency = frequency / sum(frequency)
  ) |> 
  print()

# summarise by z-score band -----------------------------------------------

penn |> 
  count(z_band, name = 'frequency') |> 
  mutate(
    relative_frequency = frequency / sum(frequency),
    cum_relative_frequency = cumsum(relative_frequency)
  ) |> 
  print()

# relative frequency histogram --------------------------------------------

penn_freq |> 
  ggplot(aes(midpoint, relative_frequency)) +
  geom_col(width = 1, colour = 'white') +
  scale_y_continuous(expand = expansion(mult = c(0, .05)), label = percent) +
  labs(
    x = 'Weight in Pounds',
    y = NULL,
    title = glue('Relative frequency histogram of {penn_text}'),
    caption = penn_caption
  ) +
  guides(y = 'none')

gg_save(3, 'freq-histogram')

# histogram ---------------------------------------------------------------

penn |> 
  ggplot(aes(weight, fill = gender)) +
  geom_histogram(binwidth = 15, center = 95, colour = 'white') +
  scale_x_continuous(breaks = midpoints) +
  scale_y_continuous(expand = expansion(mult = c(0, .05))) +
  scale_fill_manual(values = gender_pal) +
  labs(
    x = 'Weight in Pounds',
    y = NULL,
    fill = NULL,
    title = glue('Histogram of {penn_text}'),
    caption = penn_caption,
    subtitle = glue('Coloured by gender - {colour_text("Males", gender_pal["Males"])
      } vs {colour_text("Females", gender_pal["Females"])}')
  ) +
  guides(y = 'none', fill = 'none') +
  theme(
    plot.subtitle = ggtext::element_markdown(
      size = rel(1.05),
      hjust = 0
    )
  )

gg_save(3, 'histogram')

# summary statistics ------------------------------------------------------

penn |> 
  summaryx(weight) |> 
  glimpse()

# boxplot -----------------------------------------------------------------

penn |> 
  select(-gender) |> 
  bind_rows(penn) |> 
  mutate(gender = coalesce(gender, 'Total')) |> 
  ggplot(aes(gender, weight, fill = gender, colour = gender)) +
  geom_boxplot() +
  scale_fill_manual(values = gender_pal) +
  scale_colour_manual(values = gender_pal) +
  labs(
    x = NULL,
    y = 'Weight in Pounds',
    fill = NULL,
    colour = NULL,
    title = glue('Boxplot of {penn_text}'),
    caption = penn_caption
  ) +
  guides(fill = 'none', colour = 'none')

gg_save(3, 'boxplot')

# stem and leaf plot ------------------------------------------------------

penn |> 
  mutate(stem = fct_rev(factor(weight %/% 10)), leaf = factor(weight %% 10)) |> 
  group_by(stem) |> 
  arrange(leaf) |> 
  mutate(row = row_number()) |> 
  ungroup() |> 
  ggplot(aes(row, stem, label = leaf, colour = z_band)) +
  geom_text(size = 4) +
  scale_colour_manual(values = z_pal) +
  labs(
    x = NULL, y = NULL, 
    title = glue('Stem and leaf plot of {penn_text}'),
    caption = penn_caption
  ) +
  guides(x = 'none', colour = 'none') +
  theme(axis.text = element_text(size = 4 * .pt))

gg_save(3, 'stem-and-leaf')
  
#-------------------------------------------------------------------------------

