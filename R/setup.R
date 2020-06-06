
## ---- packages --------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)

library(wesanderson)

library(knitr)
library(kableExtra)

## ---- custom themes ---------------------------------------------------------

# plot theme
theme_set(
  theme_minimal() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.spacing = unit(1, "lines"),
      strip.text.y = element_blank(),
      legend.position = "bottom"
    )
)
# table theme
kable_theme <- function(data){
  kable(data) %>%
    kable_styling(
      bootstrap_options = "condensed",
      full_width = FALSE,
      position = "center",
      font_size = 10
    )
}

grid_plot <- function(dat, x, y, fill){
  p <- ggplot(dat) + geom_point( aes({{x}}, {{y}}, color = {{fill}}), size = 4) +
    scale_color_gradientn(colours = wes_palette("Zissou1", 1000, type = "continuous"))
}
