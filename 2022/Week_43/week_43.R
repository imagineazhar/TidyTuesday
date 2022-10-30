library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(MetBrewer)
library(ggtext)


# ------ Load TidyTuesday data ------ 

bakers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-25/bakers.csv')


# ------ Data wrangling ------ 


df <- bakers |>
  group_by(baker_full)|>
  select(baker_full, age, series_winner)|>
  ungroup()

df2 <- df|>
  mutate( winner = case_when(series_winner > 0 ~ "winner",
                             series_winner == 0 ~ "participant"))|>
  pivot_wider(names_from = winner, values_from = age, values_fill=0)

# ------ Texts ------ 

title_text <- "The Great British Bake Off: Age Distribution"
subtitle_text <- "GBBO is a British television baking competition."
caption_text <- "Muhammad Azhar | Twitter: @imagineazhar | Data: {bakeoff}"


# ------ Typography ------ 

font_add_google("Hind", "title_font")
font_add_google("cabin", "body_font")
showtext_auto()
theme_set(hrbrthemes::theme_ipsum(base_family = body_font))
title_font <- "title_font"
body_font <- "body_font"

# ------ Set theme ------ 

bg <-  "#F0EBE3"
txt_col <- "black"


# ------ Plot ------

df2|> ggplot(aes(x=x))+
  geom_density(aes(x=winner, y= ..density..), fill="#FF731D")+
  geom_label( aes(x=55, y=0.035, label="Winners"), color="#FF731D") +
  geom_density( aes(x = participant, y = -..density..), fill= "#5F9DF7") +
  geom_label( aes(x=55, y=-0.035, label="Other Participants"), color="#5F9DF7") +
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text,
       x = "Age",
       y = "Density")+
  theme(
    # Legend
    legend.position = "none",
    # TITLE
    plot.title.position = "plot",
    plot.title = element_text(family = title_font,
                              face = "bold",
                              color = txt_col,
                              size = 32,
                              lineheight = 1,
                              margin = margin(20,0,20,0)),
    # Subtitle
    plot.subtitle = element_text(family=body_font,
                                 face = "plain",
                                 color = "grey50",
                                 size = 14,
                                 lineheight = 1,
                                 margin = margin(0,0,10,0)),
    # Caption
    plot.caption = element_text(family=body_font,
                                face="plain",
                                size=12, 
                                color=txt_col,
                                hjust=.5,
                                margin=margin(20,0,0,0)),
    axis.title.y = element_text(hjust = 0.5),
    axis.title.x = element_text(hjust = 0.5),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30)
    )



# ------ Save Plot ------ 

showtext_opts(dpi = 320) 
ggsave("week_43.png",height = 10, width = 10, dpi=320)  
showtext_auto(FALSE)
