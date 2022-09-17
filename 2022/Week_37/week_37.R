library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(hrbrthemes)
library(patchwork)
library(lubridate)
library(ggtext)

# Set the Stage ------------------------------------------------

font <- "Special Elite"
font_add_google(font, font)
showtext_auto(enable = TRUE)
theme_set(theme_ipsum(base_family = font))
bg <- "#F0EBE3"
txt_col <- "black"



# read data-----------------------------------------------------

tt <- tt_load(2022, week=37)
bigfoot <- tt$bigfoot

# data wrangling-------------------------------------------------

df <- bigfoot|>
  select(county, state, season, latitude, longitude, date, classification)|>
  mutate(year=year(date)) |>
  filter(year >= 1950)|>
  mutate(season = na_if(season, "Unknown"))


# plot 1 ----------------------------------------------------------

p1 <- df|> count(year)|> ggplot(aes(x=year, y=n))+
  geom_line(size=1.2, color='#636175') +
  geom_point(size=1.3, color='#636175')+
  coord_cartesian(clip="off")+
  labs(
    title = 'Bigfoot sightings over the years',
    subtitle = "2004 onwards Bigfoot Sightings have decreased significantly.",
    y = "# of sightings",
    x = " ")+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(hjust = 0.5),
    plot.title = element_text(size=20, color=txt_col,
                              face="bold", margin=margin(0,0,10,0)),
    plot.subtitle =  element_text(size=12, color='grey50',
                              face="bold", margin=margin(0,0,30,0)),
    
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0),
                                size=10, color='grey20', face="plain"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.title.position = "plot",
    legend.position = 'hidden')
p1

# plot 2 ----------------------------------------------------------

top_5 <- df |> count(state, sort = TRUE)|>
  head(5) |>
  ggplot(aes(n, y= reorder(state, n)))+
  geom_col(width = 0.5, fill='#636175')+
  scale_y_discrete()+
  labs(title = "Top 5 states with most Sightings")+
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size=10, color='grey50'),
    plot.title = element_text(size=11, color=txt_col,
                              face="bold", margin=margin(0,0,5,0)),
    plot.background = element_rect(color=bg, fill=bg),
    plot.title.position = "plot",
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    legend.position = 'hidden')

# Patchwork ----------------------------------------------------------

p1 + inset_element( top_5, 
                    left = 0.1, bottom = 0.4,
                    right = 0.6, top = 1,
                    align_to = 'panel')+
  plot_annotation(caption = "Muhammad Azhar | #TidyTuesday Week 37 | Data: Data.World/BFRO") &
  theme(plot.caption = element_text(size=12, color=txt_col,face = "plain", hjust=0.5,
                                    margin=margin(0,0,0,0), lineheight = 1.4),
        plot.margin = margin(30,30,30,30),
        plot.background = element_rect(color=bg, fill=bg))

# Save plot ---------------------------------------------------------- 

showtext_opts(dpi = 320) 

ggsave("week_37.png", height = 8, width = 10, dpi=320)  

showtext_auto(FALSE)
