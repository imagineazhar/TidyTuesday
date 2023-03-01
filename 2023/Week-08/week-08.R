library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(showtext)

# ------ Get Data ------ 

tt_data <- tt_load(2023, week=08)
readme(tt_data)
bob_ross <- tt_data$bob_ross

# ------ Data Wrangling ------ 
df <- bob_ross|>
  select(painting_index, painting_title,
         season, episode, num_colors,
         color_hex)
# df$season_string <- paste("season", bob_ross$season, sep = " ")

# ------ Typography ------ 

font_add_google("Catamaran", "title_font")
font_add_google("Chivo", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Set theme ------ 

bg <-  "#F0EBE3"
txt_col <- "black"

# ------ Texts ------ 

title_text <- "Colors of Bob Ross"
subtitle_text <- "This visualization presents the number of colors used in each episode of the television series, The Joy of Painting, hosted by Bob Ross. The show aired for 31 seasons, running from 1983 to 1994. The x-axis represents the episode number, while the y-axis shows the number of colors used, depicted as the height of each bar."
caption_text <- "Created by: Muhammad Azhar         Twitter: @imagineazhar"


# ------ Plot ------

df |> ggplot(aes(x=as.factor(episode), y=as.integer(num_colors),))+
  geom_bar(stat="identity", fill='#7286D3', width = 0.8)+
  geom_text(aes(label = num_colors), vjust = 1.5, colour = "black")+
  facet_wrap(vars(season), 
             ncol = 4,
             labeller = labeller(season = function(x) {ifelse(x >= 1, paste("Season", x), " ")}))+
  scale_x_discrete()+
  scale_y_continuous()+
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text,
       x="",
       y="")+
  ggthemes::theme_tufte()+
  theme(
    panel.spacing = unit(1.2, "lines"),
    strip.text.x = element_text(size = 14, 
                                family = body_font,
                                face= 'bold',
                                color = 'grey25'),
    axis.text.x = element_text(family = body_font,
                               size = 9,
                               face = 'plain',
                               color = 'grey30'),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    # Legend
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.height= unit(0.5, 'cm'),
    legend.key.width= unit(2, 'cm'),
    legend.text = element_text(family = body_font,
                               size=12,
                               face = 'plain',
                               color = txt_col),
    # TITLE
    plot.title.position = "plot",
    plot.title = element_text(family = title_font,
                              face = "bold",
                              color = txt_col,
                              size = 40,
                              lineheight = 1,
                              margin = margin(20,0,10,0)),
    # Subtitle
    plot.subtitle = element_textbox(family=body_font,
                                    face = "plain",
                                    width = unit(58, "lines"),
                                    size = 18,
                                    color = "grey20",
                                    lineheight = 1,
                                    margin = margin(5,0,20,0)),
    # Caption
    plot.caption = element_text(family=body_font,
                                face="plain",
                                size=13, 
                                color="grey40",
                                hjust=.5,
                                margin=margin(20,0,0,0)),
    
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,60,30,60)
    )

# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("week-08.png", height = 15,
       width = 13, dpi=320)  
showtext_auto(FALSE)
