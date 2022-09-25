library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(hrbrthemes)
library(patchwork)
library(lubridate)
library(ggtext)

# Set the Stage ------------------------------------------------

font <- "Dosis"
font_add_google(font, font)
showtext_auto(enable = TRUE)
theme_set(theme_ipsum(base_family = font))
bg <- "#F0EBE3"
txt_col <- "black"



# read data-----------------------------------------------------

tt <- tt_load(2022, week=38)
hydro <- tt$HydroWASTE_v10

# data wrangling-------------------------------------------------

df <- hydro |>
  filter(CNTRY_ISO == "PAK") |>
  select(WASTE_ID, LAT_WWTP, LON_WWTP, LAT_OUT, LON_OUT, POP_SERVED, WASTE_DIS,
         HYRIV_ID, RIVER_DIS)


# plot  ----------------------------------------------------------

p1 <- df|> count(year)|> ggplot(aes(x=year, y=n))+
  geom_line(size=1.2, color='#636175') +
  geom_point(size=1.3, color='#636175')+
  coord_cartesian(clip="off")+
  labs(
    
  )+
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

# Save plot ---------------------------------------------------------- 

showtext_opts(dpi = 320) 

ggsave("week_37.png", height = 8, width = 10, dpi=320)  

showtext_auto(FALSE)
