library(tidytuesdayR)
library(showtext)
library(tidyverse)
library(janitor)
library(hrbrthemes)
library(MetBrewer)


# Set the Stage ------------------------------------------------
font <- "Mouse Memoirs"
font_add_google(font, font)
showtext_auto(enable = TRUE)
theme_set(theme_minimal(base_family = font))
bg <- "#F0EBE3"

# read data-----------------------------------------------------
tt_data <- tt_load(2022, week=31)
frogs <- tt_data$frogs |> clean_names()


# data wrangling-------------------------------------------------

df <- frogs |> group_by(hab_type, structure) |>
  summarise(count=n()) |>
  mutate(percent=round(count/sum(count)*100))

# plot----------------------------------------------------------

df|> ggplot()+
  geom_bar(aes(x=percent, y=hab_type, fill=structure), stat = "identity",
           position = "stack")+
  scale_x_continuous(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  scale_fill_met_d(name = "Isfahan1", direction = -1)+
  
  labs(
    title = "Oregon Frogs prefer Herbaceous Vegetation",
    subtitle = "percentage of frogs spotted",
    caption = "Muhammad Azhar | #TidyTuesday Week 31 | Data: USGS") +
  
  coord_cartesian(clip="off") +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size=12, color="black"),
    axis.text.y = element_text(size=18, color="black"),
    plot.title = element_text(size=30, face="bold",
                              margin=margin(10,0,10,0)),
    plot.subtitle = element_text(size=20, color="grey50", face="bold",
                                 margin=margin(0,0,20,0)),
    plot.caption = element_text(size=14, color="black", face="plain", hjust=0.5,
                                margin=margin(20,0,0,0)),
    plot.background = element_rect(color = bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.title = element_blank(),
    legend.text = element_text(size=12),
    legend.position = "top",
    legend.justification="left"
  )

# Save plot ---------------------------------------------------------- 

showtext_opts(dpi = 320) 

ggsave("week_31.png", height = 7, width = 7, dpi=320,)  

showtext_auto(FALSE)
