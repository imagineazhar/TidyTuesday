library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(maps)
library(lubridate)
library(ggtext)

# Set the Stage ------------------------------------------------

font <- "Mukta"
font_add_google(family=font, font,db_cache = FALSE)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <-  "#F0EBE3"
txt_col <- "black"

# read data-----------------------------------------------------

tt <- tt_load(2022, week=38)
hydro <- tt$HydroWASTE_v10

# data wrangling-------------------------------------------------

df <- hydro |>
  filter(CNTRY_ISO == "PAK") |>
  select(WASTE_ID, LAT_WWTP, LON_WWTP, LAT_OUT, LON_OUT, POP_SERVED, WASTE_DIS)


map <- as_tibble(map_data("world")) |>
  filter(region =="Pakistan")

# plot  ----------------------------------------------------------

map %>% 
  ggplot() + 
  geom_polygon(aes(x=long, y=lat, group=group),fill="#576E68", color="black", size=.1) +
  geom_point(data=df, aes(x=LON_WWTP, y=LAT_WWTP), color = "#e34a33", size=2) +
  coord_map(projection = "mercator", xlim=c(60,80)) +
  labs( title="Wastewater Plants in Pakistan.",
        caption = "Muhammad Azhar | #TidyTuesday Week 38 | Data: Macedo et al (2022)")+
  theme(
    panel.grid = element_blank(),
    axis.title  = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(size=40, color=txt_col, hjust=0.5,lineheight=1,
                              face="bold", margin=margin(0,0,30,0)),
    plot.caption = element_text(size=12, color=txt_col,face = "plain",
                                    hjust=0.5, margin=margin(0,0,0,0),
                                    lineheight = 1.4),
    plot.background = element_rect(color=bg, fill=bg),
    plot.title.position = "panel",
    plot.margin = margin(20,20,20,20),
    legend.position ="bottom"
  )

# Save plot ---------------------------------------------------------- 

showtext_opts(dpi = 320) 

ggsave("week_38.png", height = 10, width = 10, dpi=320)  

showtext_auto(FALSE)
