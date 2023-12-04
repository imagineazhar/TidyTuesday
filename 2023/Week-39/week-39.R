library(tidyverse)
library(ggtext)
library(ggrepel)
library(showtext)

# ------ Get Data ------ 

richmondway <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-26/richmondway.csv')|>
  janitor::clean_names()

<<<<<<< HEAD
=======

# ------ Data Wrangling ------ 

richmondway$season <- factor(data$x, levels=rev(levels(richmondway$season)))


>>>>>>> 9e214cfc35b441eece28f9e12cdccce6f3dcf79a
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
<<<<<<< HEAD
  geom_text(aes(label=f_count_rk), color="grey50")+
=======
  geom_text(aes(label=f_count_rk))+
>>>>>>> 9e214cfc35b441eece28f9e12cdccce6f3dcf79a
  scale_fill_gradient(low = '#9bf6ff', high = "#ff6b6b") +
  scale_y_reverse(breaks = 1:12) +
  coord_equal()+
  theme_minimal()+
  labs(title = "FUCKS GIVEN!",
<<<<<<< HEAD
       subtitle = "Roy Kent is a prominent character in the Apple TV+ series 'Ted Lasso'.\nThis visualization depicts the number of times Roy Kent uses the word 'F**K' in each episode.",
       x = "EPISODE",
=======
       subtitle = "BY ROY KENT IN EACH EPISODE.",
       x= "EPISODE",
>>>>>>> 9e214cfc35b441eece28f9e12cdccce6f3dcf79a
       y = "SEASON",
       caption = "Graphic: Muhammad Azhar | Twitter: @imagineazhar | Data:richmondway {R package}")+
  theme(
    
    legend.position = "none",
<<<<<<< HEAD
    plot.title = element_text(family = title_font, face = 'bold',
                                  size = 28,color = "black",
                                  margin = margin(0, 0, 5, 0)),
    plot.subtitle = element_text(family = body_font, color="grey30",
                                           size=14, face="plain",
=======
    plot.title = element_markdown(family = title_font, face = 'bold',
                                  size = 28,color = "black",
                                  margin = margin(0, 0, 5, 0)),
    plot.subtitle = element_textbox_simple(family = body_font, color="grey50",
                                           size=16, face="bold",
>>>>>>> 9e214cfc35b441eece28f9e12cdccce6f3dcf79a
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
<<<<<<< HEAD
    plot.margin = margin(30, 10, 30, 10)
=======
    plot.margin = margin(50, 10, 50, 10)
>>>>>>> 9e214cfc35b441eece28f9e12cdccce6f3dcf79a
  )



# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("week-39.png",dpi=320,
       width = 10, height = 6)
showtext_auto(FALSE)

