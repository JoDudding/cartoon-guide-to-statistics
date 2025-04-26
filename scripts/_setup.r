#-------------------------------------------------------------------------------
#' _setup.r
#-------------------------------------------------------------------------------
#' jo dudding
#' April 2025
#' common setup
#-------------------------------------------------------------------------------

library(tidyverse)
library(glue)
library(scales)
library(cli)
library(tidymodels)

# functions ---------------------------------------------------------------

summaryx <- function(data, var) {
  data |> 
    summarise(
      min = min({{ var }}, na.rm = TRUE),
      p1 = quantile({{ var }}, probs = 0.01, na.rm = TRUE, names = FALSE),
      p5 = quantile({{ var }}, probs = 0.05, na.rm = TRUE, names = FALSE),
      p10 = quantile({{ var }}, probs = 0.1, na.rm = TRUE, names = FALSE),
      p25 = quantile({{ var }}, probs = 0.25, na.rm = TRUE, names = FALSE),
      median = median({{ var }}, na.rm = TRUE),
      p75 = quantile({{ var }}, probs = 0.75, na.rm = TRUE, names = FALSE),
      p90 = quantile({{ var }}, probs = 0.9, na.rm = TRUE, names = FALSE),
      p95 = quantile({{ var }}, probs = 0.95, na.rm = TRUE, names = FALSE),
      p99 = quantile({{ var }}, probs = 0.99, na.rm = TRUE, names = FALSE),
      max = max({{ var }}, na.rm = TRUE),
      iqr = quantile({{ var }}, probs = 0.75, na.rm = TRUE, names = FALSE) -
        quantile({{ var }}, probs = 0.25, na.rm = TRUE, names = FALSE),
      n_obs = n(),
      sum = sum({{ var }}, na.rm = TRUE),
      mean = mean({{ var }}, na.rm = TRUE),
      sd = sd({{ var }}, na.rm = TRUE),
      lci_95 = mean({{ var }}, na.rm = TRUE) - 
        qnorm(0.975) * sd({{ var }}, na.rm = TRUE),
      uci_95 = mean({{ var }}, na.rm = TRUE) + 
        qnorm(0.975) * sd({{ var }}, na.rm = TRUE),
      n_miss = sum(is.na({{ var }})),
      n_zero = sum({{ var }} == 0, na.rm = TRUE),
      pct_miss = mean(is.na({{ var }})),
      .groups = "drop"
    ) |>
    mutate(
      pct_zero = n_zero / n_obs
    )
}

#-------------------------------------------------------------------------------
# ggplot2 theme
#-------------------------------------------------------------------------------

# font

#systemfonts::register_font(
#  name = "Montserrat",
#  plain      = list(
#    "assets/Montserrat/static/Montserrat-Regular.ttf", 0),
#  bold       = list(
#    "assets/Montserrat/static/Montserrat-Bold.ttf", 0),
#  italic     = list(
#    "assets/Montserrat/static/Montserrat-Italic.ttf", 0),
#  bolditalic = list(
#    "assets/Montserrat/static/Montserrat-BoldItalic.ttf", 0)
#)

# colour palette

cartoon <- function(n = 9, begin = 0.1, end = 0.85, ...) {

  viridisLite::plasma(n = n, begin = begin, end = end, ...) 
}  

cartoon_pal <- cartoon(9) 

# show_col(cartoon_pal)

scale_fill_cartoon_d <- function(...) {
  scale_fill_viridis_d(option = 'plasma', begin = 0.3, end = 0.8, ...)
}

scale_fill_cartoon_c <- function(...) {
  scale_fill_viridis_c(option = 'plasma', begin = 0.3, end = 0.8, ...)
}

scale_colour_cartoon_d <- function(...) {
  scale_colour_viridis_d(option = 'plasma', begin = 0.3, end = 0.8, ...)
}

scale_colour_cartoon_c <- function(...) {
  scale_colour_viridis_c(option = 'plasma', begin = 0.3, end = 0.8, ...)
}

purple_pal <- grDevices::colorRampPalette(c('#6800A8FF', '#E1CEFFFF'))

#show_col(purple_pal(9))

orange_pal <- grDevices::colorRampPalette(c('#502A00FF', '#F9973FFF', '#FFD6C1FF'))

#show_col(orange_pal(9))

# change default colours etc for geoms ------------------------------------

base_size <- 8

update_geom_defaults("col", list(fill = cartoon_pal[3], colour = NA))
update_geom_defaults("bar", list(fill = cartoon_pal[3], colour = NA))
update_geom_defaults("rect", list(fill = cartoon_pal[3], colour = NA))
update_geom_defaults("violin", list(fill = cartoon_pal[3], colour = cartoon_pal[2], 
  alpha = 0.5))
update_geom_defaults("boxplot", list(fill = cartoon_pal[3], colour = cartoon_pal[2], 
  alpha = 0.5))
update_geom_defaults("dotplot", list(fill = cartoon_pal[3], colour = NA))
update_geom_defaults("point", list(colour = cartoon_pal[2]))
update_geom_defaults("line", list(colour = cartoon_pal[2]))
update_geom_defaults("step", list(colour = cartoon_pal[2]))
update_geom_defaults("path", list(colour = cartoon_pal[2]))
update_geom_defaults("text", list(lineheight = 0.85, 
  size = base_size / .pt * 0.9, family = "Montserrat"))
update_geom_defaults("label", list(lineheight = 0.85, 
  size = base_size / .pt * 0.9, family = "Montserrat"))
update_geom_defaults("vline", list(colour = cartoon_pal[9], linetype = 3))
update_geom_defaults("hline", list(colour = cartoon_pal[9], linetype = 3))
update_geom_defaults("abline", list(colour = cartoon_pal[9], linetype = 3))
update_geom_defaults("smooth", list(fill = cartoon_pal[6], colour = cartoon_pal[6], 
  alpha = 0.5))


# theme_cartoon -----------------------------------------------------------

theme_cartoon <- function(
    base_size = 8, 
    base_family = "Montserrat"
) {
  
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      line = element_line(
        linewidth = 0.75,
        linetype = 1,
        lineend = "butt"
      ),
      rect = element_rect(
        fill = "white",
        colour = "white",
        linewidth = 0.1,
        linetype = 1
      ),
      text = element_text(
        family = base_family,
        face = "plain",
        size = base_size,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        lineheight = 0.85,
        margin = ggplot2::margin(),
        debug = FALSE
      ),
      
      axis.line.x = element_line(linewidth = 0.25),
      axis.line.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_text(
        size = rel(0.8), 
        face = 'bold'
      ),
      
      #plot.background = element_rect(fill = NA, colour = NA),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(
        size = rel(1.15),
        colour = cartoon_pal[2],
        face = "bold",
        hjust = 0
      ),
      plot.subtitle = element_text(
        size = rel(0.9),
        hjust = 0
      ),
      plot.caption = element_text(
        hjust = 0
      ),
      
      legend.position = 'top',
      legend.justification = 0
    )
}

theme_set(theme_cartoon())


# colour section of text for use in ggtext --------------------------------

colour_text <- function(x, colour) {
  glue::glue("<span style='color:{colour}'>{x}</span>")
}


# gg_save -----------------------------------------------------------------

gg_save <- function(
    chapter,
    chart_name, 
    device = "png", 
    width = 4.667, 
    height = 3.30,
    dpi = 150, 
    ...
) {
  name_path <- glue("outputs/ch{chapter}_{chart_name}.png")
  
  ggplot2::ggsave(
    name_path,
    device = device,
    width = width,
    height = height,
    dpi = dpi,
    ...
  )
  
  cli::cli_alert_info('saved {name_path}')
}

#-------------------------------------------------------------------------------

