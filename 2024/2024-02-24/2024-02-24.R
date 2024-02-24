library(ggplot2)
library(showtext)
library(forcats)  
library(tidyverse)
library(ggtext)

# ------ Get Data ------

isc_grants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-20/isc_grants.csv')

# ------ Data Wrangling ------

df <- isc_grants|>
  filter(year==2023)|>
  arrange(desc(funded))
df$label_text <- paste(df$title, "(",df$proposed_by,")")

# ------ Typography ------

 font_add_google("Outfit", "title_font")
 font_add_google("Outfit", "body_font")
 showtext_auto()

 title_font <- "title_font"
 body_font <- "body_font"
 
 # ------ Texts ------

 title_text <- "R Consortium\nInfrastructure Steering Committee (ISC) Grant Program 2023"
 subtitle_text <- "ISC funds projects contributing to the R community’s technical and social infrastructures."
 caption_text <- "Graphic: Muhammad Azhar | Data: R Consortium"

 # ------ Plot ------

ggplot(df, aes(x=funded, y=title))+
   geom_col(aes(fill = proposed_by == "Jon Harmon"), width = 0.8) +
   facet_wrap(~ reorder(label_text, -funded), ncol = 1, scales = "free_y")+
   scale_x_continuous( name = "Funded Amount", expand = c(0, 0), 
   labels = scales::label_currency(prefix = "$", scale_cut=c(0, k=1e3)))+
   scale_y_discrete(guide = "none", expand = expansion(add = c(.8, .6))) +
   scale_fill_manual(values = c("grey50", "#525CEB"), guide = "none") +
   geom_text(aes(label = paste0(funded, "$", "    "), color=funded>15000, 
                 hjust=1.1),
             size = 4,
             fontface = "bold",
             family = body_font) +
   scale_x_continuous(guide = "none", name = NULL, expand = c(0, 0))+
   scale_color_manual(values = c("black", "white"), guide = "none")+
   labs(title = title_text,
      subtitle = subtitle_text,
      caption = caption_text)+
   theme_void()+
   theme(
     strip.text = element_text(hjust = 0, margin = margin(1, 0, 0.5, 0),
                               size = rel(1.1), face = "plain",
                               color = "grey10",
                               family = body_font),
     axis.title.x = element_blank(),
     axis.title.y = element_blank(),
     axis.text.x = element_text(family = body_font,
                                face = "bold",
                                size=12),
     axis.text.y = element_text(family = body_font,
                                face = "bold",
                                size=12),
     # Legend
     legend.position = "none",
     
      # TITLE
     plot.title.position = "plot",
     plot.title = element_textbox(margin = margin(20, 0, 5, 0),
                                  size = 20,
                                  family = title_font,
                                  face = "bold",
                                  width = unit(33, "lines")),
     # SUB-TITLE
     plot.subtitle = element_textbox(margin = margin(5, 0, 30, 0),
                                     size = 14,
                                     family = body_font,
                                     face = "plain",
                                     width = unit(35, "lines")),
     # Caption
     plot.caption = element_text(family=body_font,
                                 face="plain",
                                 size=12, 
                                 color="grey50",
                                 margin=margin(10,0,0,0)),
     plot.background = element_rect(color="white", fill="white"),
     plot.margin = margin(30, 50, 30, 50))
 
 # ------ Save Plot ------ 
 
 showtext_opts(dpi = 320)
 ggsave("Week08.png", height = 10,
        width = 8, dpi=320)  
 showtext_auto(FALSE)
 