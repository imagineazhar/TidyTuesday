library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(hrbrthemes)
library(patchwork)
library(lubridate)

# Set the Stage ------------------------------------------------

font <- "Special Elite"
font_add_google(font, font)
showtext_auto(enable = TRUE)
theme_set(theme_ipsum(base_family = font))
bg <- "#F0EBE3"
text_color <- "black"



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
  geom_line(size=1) +
  geom_point(size=1.6)+
  coord_cartesian(clip="off")+
  labs(
    title = '1990-2004 saw an exponential increase in Bigfoot sightings.',
    subtitle = " 2004 onwards Bigfoot Sightings have decreased significantly.",
    y = "# of sightings",
    x = " "
    )+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(hjust = 0.5),
    plot.title = element_text(size=20, color=text_color,
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
p2 <- df |>
  filter(state!="Alaska", longitude>-130)|>
  ggplot(aes(longitude, latitude, color="red"))+
  geom_point()+
  borders("state")+
  ggthemes::theme_map()+
  coord_map()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(hjust = 0.5),
    plot.title = element_text(size=20, color=text_color,
                              face="bold", margin=margin(0,0,10,0)),
    plot.subtitle =  element_text(size=14, color='grey50',
                                  face="bold", margin=margin(0,0,30,0)),
    
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0),
                                size=10, color='grey20', face="plain"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.title.position = "plot",
    legend.position = 'hidden')

# Patchwork ----------------------------------------------------------
(p1 / p2) + plot_annotation(
  title = 'Finding BigFoot',
  #  subtitle = 'The amount awarded to each recipient more than doubled.',
  caption = "Muhammad Azhar | #TidyTuesday Week37 | Data: BFRO",
  theme = theme(
    plot.title = element_text(size=34, face="bold",
                              margin=margin(10,0,10,0)),
    plot.subtitle = element_text(size=16, face="bold",color = "grey40"),
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0),
                                size=12, face="bold"),
    plot.title.position = "plot",
#    plot.margin = margin(30,10,30,10),
    plot.background = element_rect(color=bg, fill=bg)))

# Save plot ---------------------------------------------------------- 

showtext_opts(dpi = 320) 

ggsave("week_37.png", height = 10, width = 8, dpi=320)  

showtext_auto(FALSE)
