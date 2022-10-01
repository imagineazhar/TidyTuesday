library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(usmap)
library(MetBrewer)

# Set the Stage ------------------------------------------------

font <- "Lobster"
font_add_google(family=font, font,db_cache = FALSE)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <-  "#F0EBE3"
txt_col <- "black"

# read data-----------------------------------------------------

tt <- tt_load(2022, week=39)
artists <- tt$artists
# data wrangling-------------------------------------------------

df <- artists |>
  filter(type=="Musicians")|>
  select(state, race, artists_n, artists_share, location_quotient)|>
  drop_na()

# plot  ----------------------------------------------------------

p1 <- plot_usmap(data = df, values = "artists_n")+
  scale_fill_met_c('Tam', type = 'continuous',limits=c(0,100))+


  labs( title="Musicians in the USA",
        subtitle = "",
        caption = "Muhammad Azhar | #TidyTuesday Week 39 | Data: arts.gov")+
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

ggsave("week_39.png", height = 10, width = 10, dpi=320)  

showtext_auto(FALSE)
