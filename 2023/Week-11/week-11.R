library(tidyverse)
library(ggtext)
library(ggrepel)
library(showtext)

# ------ Get Data ------ 

numbats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-07/numbats.csv')

# ------ Data Wrangling ------ 

 

# ------ Typography ------ 

font_add_google("Outfit", "title_font")
font_add_google("Roboto Condensed", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Set theme ------ 

bg <-  "#FFFFFF"

theme_set(theme_minimal(base_size = 19, base_family = "Roboto"))
theme_update(
  text = element_text(color = "grey12"),
  axis.title = element_blank(),
  axis.text.x = element_text(family = body_font),
  axis.text.y = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank(),
  plot.margin = margin(20, 5, 10, 10),
  plot.subtitle = element_textbox_simple(family = body_font, size = 14,
                                         lineheight = 1.6),
  plot.title.position = "plot",
  plot.caption = element_text(family = body_font, color = "grey12", 
                              hjust = .5, size = 10, 
                              margin = margin(35, 0, 0, 0))
  )

# ------ Plot ------
p1 <- ggplot(sf_oz) + 
  geom_sf() +
  geom_point(data = df_map, aes(x=decimalLongitude, y=decimalLatitude),
             color='#b40059', size=1.5, alpha=0.8)+
  facet_wrap(~factor(thresh, levels = c("Before 2020", "2020 to 2022", "As of 2023")))+
  labs(title = "Where are <b style='color:#b40059'>Numbats</b> found?",
       subtitle = 'Numbats were widely distributed across Southern Australia, from Western Australia to New South Wales. The species has survived only in two small patches of land in the Western Australia.',
       caption = "Graphic: Muhammad Azhar | Twitter: @imagineazhar | Data: Atlas of Living Australia")+
  theme(
    strip.text.x = element_text(family = title_font, face='bold',
                                size = 13, colour = "grey10"),
    plot.title = element_markdown(family = title_font, face = 'bold',
                                  size = 28,color = "black",
                                  margin = margin(5, 35, 15, 35)),
    plot.subtitle = element_textbox_simple(margin = margin(5, 35, 15, 35)),
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(),
    plot.background = element_rect(color=bg, fill=bg)
  )
p1


# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("week-10.png",dpi=320,
       width = 10, height = 6)
showtext_auto(FALSE)

