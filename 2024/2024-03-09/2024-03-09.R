library(ggplot2)
library(showtext)
library(tidyverse)
library(ggtext)
library(camcorder)
library(glue)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 10, units = "in", dpi = 320)

# ------ Get Data ------

trashwheel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-05/trashwheel.csv')


# ------ Data Wrangling ------

df <- trashwheel|>
  filter(Year==2023)|>
  select(Name, PlasticBottles:CigaretteButts,
         PlasticBags, Wrappers)|>
  pivot_longer(cols = PlasticBottles:Wrappers,
               names_to = "trash_type",
               values_to = "num_items")|>
  filter(num_items>0)|>
  group_by(Name, trash_type)|>
  summarise(total=sum(num_items))|>
  mutate(col_id = row_number(),
         fraction = total/sum(total),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)),
         label_pos = (ymax + ymin)/2,
         label = paste0(round(fraction*100, digits = 0),"%")) |>
  ungroup()

# ------ Typography ------

font_add_google("Outfit", "title_font")
font_add_google("Chivo", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Texts ------

title_text <- glue(" The <span style = 'color: #ef476f;'> Cigarette Butt </span>: Baltimore's Trash Wheels' Most Prevalent Catch in 2023")
subtitle_text <- glue("Highlighting the Percentage Distribution of Collected Item Types")
caption_text <- glue("Graphic: Muhammad Azhar | Data: mrtrashwheel.com")

# ------ Plot ------
ggplot(df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=trash_type)) +
  geom_rect() +
  geom_text( x=3.5, aes(y=label_pos, label=label), size=9) +
  scale_y_continuous()+
  scale_fill_manual(values =c("#ef476f", "#e9ecef", "#6c757d","#adb5bd", "#495057" ))+
  coord_polar(theta="y") +
  xlim(c(1.5, 4)) +
  facet_wrap(vars(Name))+
  labs(title = title_text, subtitle = subtitle_text, caption = caption_text)+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    
    strip.text = element_text(family=title_font, size = 24,
                              face = "bold", color = "grey10"),
    
    # Legend
    legend.position = "top",
    legend.title = element_blank(),
    legend.spacing = unit(0.5, 'cm'),
    legend.key.height= unit(0.5, 'cm'),
    legend.key.width= unit(0.7, 'cm'),
    legend.text = element_text(family = body_font,
                               size=20,
                               face = 'plain',
                               color = "grey10"),
    # TITLE
    plot.title.position = "plot",
    plot.title = element_textbox(margin = margin(20, 0, 5, 0), size = 40,
                                 family = title_font, face = "bold",
                                 width = unit(75, "lines")),
    
    # SUB-TITLE
    plot.subtitle = element_textbox(margin = margin(5, 0, 30, 0), size = 34,
                                    family = body_font, face = "plain",
                                    colour = "grey20",
                                    width = unit(75, "lines")),
    
    plot.caption = element_text(family = body_font, face = 'plain',
                                size = 24, colour = 'grey30', hjust = 0.5),
    
    plot.background = element_rect(color="white", fill="white"),
    plot.margin = margin(20, 40, 20, 40))
