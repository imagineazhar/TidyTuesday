library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(showtext)

# ------ Get Data ------ 

tt_data <- tt_load(2023, week=09)
readme(tt_data)

sentiment <- tt_data$afrisenti
languages <- tt_data$languages
lang_scripts <- tt_data$language_scripts
lang_conturies <- tt_data$language_countries
country_region <- tt_data$country_regions

# ------ Data Wrangling ------ 

df <- left_join(sentiment, languages, by="language_iso_code")|>
  select(language_iso_code, label, language)|>
  group_by(language_iso_code, label, language)|>
  summarise(n=n())|>
  ungroup()|>
  group_by(language_iso_code)|>
  mutate(total=sum(n))|>
  mutate(perc_of_total= round(100*n/total, 1))

# TODO 
# github.com/gkaramanis/tidytuesday/blob/master/2020/2020-week11/diversity-school2020.R


# ------ Typography ------ 

font_add_google("Outfit", "title_font")
font_add_google("Chivo", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Set theme ------ 

bg <-  "#F0EBE3"
txt_col <- "black"

# ------ Texts ------ 

title_text <- "Languages of Africa"
subtitle_text <- ""
caption_text <- "Created by: Muhammad Azhar         Twitter: @imagineazhar"

# ------ Plot ------

df |> ggplot(aes(y=language, x=perc_of_total, fill=label))+
  geom_col() +
  geom_text(aes(label = perc_of_total),
            position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels = perc_of_total) +
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text,
       x="",
       y="")+
  theme_minimal()+
  theme(
    # LEGEND
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.direction = "horizontal",
    legend.key.width = unit(1.5, "cm"),
    
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
    plot.caption = element_textbox_simple(family=body_font,
                                face="plain",
                                size=13, 
                                color="grey40",
                                hjust=.5,
                                margin=margin(20,0,0,0)),
    
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(60,60,60,60)
    )

# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("week-09.png", height = 15,
       width = 13, dpi=320)  
showtext_auto(FALSE)
