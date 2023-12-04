library(tidytuesdayR)
library(tidyverse)
library(ggforce)
library(ggtext)
library(glue)
library(prismatic)
library(showtext)


# ------ Get Data ------ 

tuesdata <- tt_load(2023, week = 48)

drwho_episodes <- tuesdata$drwho_episodes
drwho_directors <- tuesdata$drwho_directors
drwho_writers <- tuesdata$drwho_writers

# ------ Data Wrangling ------ 

df <- left_join(drwho_episodes, drwho_directors, by="story_number")|>
  filter(type=="episode")

df <- left_join(df, drwho_writers, by="story_number")|>
  select(season_number, episode_number, uk_viewers, rating, duration)|>
  group_by(season_number)




# ------ Typography ------ 

font_add_google("Fraunces", "title_font")
font_add_google("Chivo", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Set theme ------ 

bg <-  "#FFFFFF"
txt_col <- "black"

# ------ Texts ------ 

title_text <- "Dr. WHO?"
subtitle_text <- "Doctor Who episodes rating by UK viewers."
caption_text <- "Graphic: Muhammad Azhar | @imagineazhar"

# ------ Plot ------

df |> ggplot(aes(y=uk_viewers, x=rating))+
  geom_point(size=3, shape=20, alpha=0.4)+
  geom_point(shape=1, size=3)+
  facet_wrap(vars(season_number), ncol = 3,
             labeller = labeller(season_number = function(x) {ifelse(x >= 1,
                                                              paste("Season", x),
                                                              " ")}),
             scales = "fixed")+
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text,
       y="Viewers (millions)",
       x="Rating")+
  hrbrthemes::theme_ipsum()+
  theme(
    strip.text.x = element_text(family = title_font,
                                face = 'bold',
                                size = 15, colour = "grey20"),
    axis.text.y = element_text(family = body_font,
                               face = 'plain',
                               size = 9, colour = "grey40"),
    axis.ticks.y = element_blank(),
    
    axis.text.x = element_text(family = body_font,
                              face = 'plain',
                              size = 9, colour = "grey60"),
    axis.ticks.x = element_blank(),
    
    axis.title.x = element_text(family = body_font,
                                face = 'bold', hjust = 0.5,
                                size = 11, colour = "grey60",
                                margin = margin(10, 0, 0, 0)),
    axis.title.y = element_text(family = body_font,
                                face = 'bold', hjust = 0.5,
                                size = 11, colour = "grey40",
                                margin = margin(0, 20, 20, 0)),
    
    # TITLE
    plot.title.position = "panel",
    plot.title = element_text(margin = margin(20, 0, 10, 0),
                              size = 40,
                              family = title_font),
    # Subtitle
    plot.subtitle = element_textbox(family=body_font,
                                    face = "plain",
                                    width = unit(60, "lines"),
                                    size = 20,
                                    color = "grey20",
                                    margin = margin(10,0,20,0)),
    # Caption
    plot.caption = element_text(family=body_font,
                                face="plain",
                                size=14, 
                                color="grey40",
                                hjust=.5,
                                margin=margin(20,0,0,0)),
    
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30, 50, 30, 50)
  )

# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("week-48.png",dpi=320,
       width = 10, height = 12)  
showtext_auto(FALSE)

