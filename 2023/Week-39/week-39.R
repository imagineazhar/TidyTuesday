library(tidyverse)
library(ggtext)
library(ggrepel)
library(showtext)

# ------ Get Data ------ 

richmondway <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-26/richmondway.csv')|>
  janitor::clean_names()


# ------ Data Wrangling ------ 

richmondway$season <- factor(data$x, levels=rev(levels(richmondway$season)))


# ------ Typography ------ 

font_add_google("Outfit", "title_font")
font_add_google("Roboto Condensed", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Set theme ------ 

bg <-  "#FFFFFF"

# ------ Plot ------
richmondway|> ggplot(aes(y=season, x=as.factor(episode), label = f_count_rk,
                         fill=f_count_rk)) +
  geom_tile(color="white") +
  geom_text(aes(label=f_count_rk))+
  scale_fill_gradient(low = '#9bf6ff', high = "#ff6b6b") +
  scale_y_reverse(breaks = 1:12) +
  coord_equal()+
  theme_minimal()+
  labs(title = "FUCKS GIVEN!",
       subtitle = "BY ROY KENT IN EACH EPISODE.",
       x= "EPISODE",
       y = "SEASON",
       caption = "Graphic: Muhammad Azhar | Twitter: @imagineazhar | Data:richmondway {R package}")+
  theme(
    
    legend.position = "none",
    plot.title = element_markdown(family = title_font, face = 'bold',
                                  size = 28,color = "black",
                                  margin = margin(0, 0, 5, 0)),
    plot.subtitle = element_textbox_simple(family = body_font, color="grey50",
                                           size=16, face="bold",
                                           margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(family = body_font, color = "grey12", 
                                hjust = .5, size = 10, 
                                margin = margin(35, 0, 0, 0)),
    axis.text = element_text(family = body_font, color = "grey30", size = 10, 
                             margin = margin(0, 2, 2, 0), face = "bold"),
    axis.title = element_text(family = body_font, margin = margin(5, 5, 5, 5),
                              size = 12, color = "grey30", face = "bold"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(50, 10, 50, 10)
  )



# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("week-39.png",dpi=320,
       width = 10, height = 6)
showtext_auto(FALSE)

