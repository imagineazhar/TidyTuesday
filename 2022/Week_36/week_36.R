library(tidyverse)
library(showtext)


# Set the Stage ------------------------------------------------

font <- "Ubuntu"
font_add_google(font, font)
showtext_auto(enable = TRUE)
theme_set(theme_minimal(base_family = font))
bg <- "#F0EBE3"
txt_col <- 'black'


# read data-----------------------------------------------------

tt_data <- tidytuesdayR::tt_load(2022, week=36)


# data wrangling-------------------------------------------------



# plot 1 ----------------------------------------------------------

df |>  labs(title = "2000")+
  theme(
    panel.grid = element_blank(),
    axis.title  = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(size=20, color=txt_col, hjust=.5,
                              face="bold", margin=margin(0,0,30,0)),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(0,0,0,0),
    legend.title = element_text(size=14),
    legend.text = element_text(size=10))



# Save plot ---------------------------------------------------------- 

showtext_opts(dpi = 320) 

ggsave("week_35.png", height = 8, width = 10, dpi=320)  

showtext_auto(FALSE)
