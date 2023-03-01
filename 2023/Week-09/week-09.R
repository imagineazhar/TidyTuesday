library(tidyverse)
library(ggforce)
library(ggtext)
library(glue)
library(prismatic)
library(showtext)

# ------ Get Data ------ 

sentiment <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/afrisenti.csv')
languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/languages.csv')


# ------ Data Wrangling ------ 

df <- left_join(sentiment, languages, by="language_iso_code")|>
  select(language_iso_code, label, language)|>
  group_by(language_iso_code, label, language)|>
  summarise(n=n())|>
  ungroup()|>
  group_by(language_iso_code)|>
  mutate(total=sum(n),
         perc_of_total= round(100*n/total, 1),
         category_nr = row_number())|>
  ungroup()

f = 0.5  # change to change shape of the "balloon"

df_shapes <- df |>
  rowwise()|>
  mutate(
    # Calculate points on circle for the "balloons", we need 4 x-y pairs for geom_bspline_closed
    x = list(c(0,
               f * perc_of_total * sin(category_nr * 2 * pi / 9 - pi/4),
               perc_of_total * sin(category_nr * 2 * pi / 9), # real percentage for main "radius"
               f * perc_of_total * sin(category_nr * 2 * pi / 9 + pi/5),
               0
    )),
    y = list(c(0,
               f * perc_of_total * cos(category_nr * 2 * pi / 9 - pi/5),
               perc_of_total * cos(category_nr * 2 * pi / 9), # real percentage for main "radius"
               f * perc_of_total * cos(category_nr * 2 * pi / 9 + pi/4),
               0
    ))
  )|>
  ungroup()|>
  pivot_wider(id_cols = c(language_iso_code, language), names_from = category_nr,
              values_from = c(x,y))|>
  unnest(x_1:y_3)

# Category colors
pal <- c("#EB455F","#BAD7E9","#469990")

# Pull categories from the dataset
cat <- df|>
  distinct(label) |>
  pull()

# Join colors with categories
pal_df <- data.frame(c = pal, l = cat)

  
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

title_text <- "Sentiment of African Languages"
subtitle_text <- "Showing percentage of tweets classified as positive, negative, or neutral."
caption_text <- "Graphic: Muhammad Azhar | Twitter: @imagineazhar | Data: AfriSenti"

# ------ Plot ------

df_shapes |> ggplot()+
  geom_bspline_closed(aes(x_1, y_1, group = language, fill = pal[1]), alpha = 0.8)+
  geom_bspline_closed(aes(x_2, y_2, group = language, fill = pal[2]), alpha = 0.8)+
  geom_bspline_closed(aes(x_3, y_3, group = language, fill = pal[3]), alpha = 0.8)+
  scale_fill_identity(guide = guide_legend(title = "", nrow = 2, 
                                           override.aes = list(alpha = 0.7, shape = 2)),
                      breaks = pal, labels = pal_df$l) +
  guides(fill = guide_legend(nrow = 1,
                             label.position = "top"))+
  coord_fixed() +
  facet_wrap(vars(language), ncol = 5,
             labeller = labeller(language = label_wrap_gen(width = 10)))+
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text,
       x="",
       y="")+
  hrbrthemes::theme_ipsum()+
  theme(
    strip.text.x = element_text(family = title_font,
                                face = 'bold',
                                size = 15, colour = "grey20"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    # Legend
    legend.position = "top",
    legend.title = element_blank(),
    legend.key.height= unit(0.5, 'cm'),
    legend.key.width= unit(2.5, 'cm'),
    legend.spacing = unit(1, 'cm'),
    legend.text = element_text(family = body_font,
                               size=15,
                               face = 'plain',
                               color = txt_col),

    
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
ggsave("week-09.png",dpi=320,
       width = 14, height = 15)  
showtext_auto(FALSE)

