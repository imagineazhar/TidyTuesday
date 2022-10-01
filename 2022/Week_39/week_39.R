library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(usmap)
library(MetBrewer)

# Set the Stage ------------------------------------------------

font <- "Kanit"
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
  scale_fill_met_c('VanGogh3', type = 'continuous', limits=c(1500,15000))+
  labs( title="Musicians in the USA",
  caption = "Muhammad Azhar | #TidyTuesday Week 39 | Data: arts.gov")+
  theme(
    panel.grid = element_blank(),
    axis.title  = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(size=30, color=txt_col, hjust=.5,
                              face="bold", margin=margin(0,0,30,0)),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(10,0,0,0),
    legend.position = 'bottom',
    legend.background = element_rect(color = bg, fill=bg),
    legend.justification = "center",
    legend.title = element_text(size=12),
    legend.text = element_text(size=10))+
      guides(fill=guide_legend(title="Number of Musicians",
                               title.position = "top",
                               title.hjust = .5,
                               keyheight = unit(3, units = "mm"),
                               keywidth=unit(14, units = "mm"),
                               label.position = "bottom"))
p1

# Plot 2 -------------------------------------------------------------

df |> group_by(race)|>
  arrange( desc(artists_n))|>
  ungroup()|>
  ggplot(aes(y=as.factor(state), x=artists_n))+
  geom_col(width = 0.7)+
  facet_wrap(~race)

# Save plot ---------------------------------------------------------- 

showtext_opts(dpi = 320) 

ggsave("week_39.png", height = 10, width = 10, dpi=320)  

showtext_auto(FALSE)
