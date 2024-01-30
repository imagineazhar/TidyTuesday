library(ggplot2)
library(showtext)
library(tidyverse)
library(ggtext)


# ------ Get Data ------ 

tt_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-23/english_education.csv')


# ------ Data Wrangling ------ 
df <- tt_data|>
  filter(!size_flag %in% c("Not BUA", "Other Small BUAs")) |>
  mutate(
    size = case_when(
      str_detect(tolower(size_flag), "london") ~ "Inner and outer London",
      size_flag == "City" ~ "Cities (excluding London)",
      TRUE ~ size_flag
    ),
    income = case_when(
      str_detect(income_flag, "deprivation") ~ income_flag,
      TRUE ~ NA
    )
  ) |>
  mutate(
    size = fct_inorder(size),
    income = fct_relevel(income, 
                         "Higher deprivation towns",
                         "Mid deprivation towns",
                         "Lower deprivation towns",
    )
  )

# ------ Typography ------ 

font_add_google("Outfit", "title_font")
font_add_google("Outfit", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

## custom colors
my_pal <- rcartocolor::carto_pal(n = 8, name = "Bold")

# ------ Texts ------ 

title_text <- "North East of England has the lowest education attainment score. "
subtitle_text <- "Educational attainment score by region. This score estimates your odds of acheiving a higher educational degree."
caption_text <- "Graphic: Muhammad Azhar         Data: UK Office of National Statistics. "


# ------ Plot ------

ggplot(df, aes(education_score, fct_rev(rgn11nm))) + 
  coord_cartesian(clip = "off") +
  scale_y_discrete(expand = c(.07, .07)) +
  scale_color_manual(values = my_pal, guide = "none") +
  scale_fill_manual(values = my_pal, guide = "none") +
  ggridges::stat_density_ridges(
    quantile_lines = TRUE, quantiles = 2,
    rel_min_height = 0.01, fill = "#92C7CF", alpha = .8, size = 1.5) +
  labs(title = title_text,
       subtitle = subtitle_text,
       x = paste0("← Lower attainment", strrep(" ", 30), "Educational attainment index score", strrep(" ", 30), "Higher attainment →"),
       caption = caption_text)+
  theme_minimal()+
  theme(
    axis.title.x  = element_text(hjust = 0.32, color = "grey30"),
    axis.title.y  = element_blank(),
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
                                 size = 24,
                                 family = title_font,
                                 face = "bold",
                                 width = unit(75, "lines")),
    # SUB-TITLE
    plot.subtitle = element_textbox(margin = margin(5, 0, 30, 0),
                                    size = 14,
                                    color = "grey30",
                                    family = body_font,
                                    width = unit(70, "lines")),
    
    # Caption
    plot.caption = element_text(family=body_font,
                                face="plain",
                                size=9, 
                                color="grey",
                                margin=margin(10,0,0,0)),
    
    plot.background = element_rect(color="#FBF9F1", fill="#FBF9F1"),
    plot.margin = margin(20, 20, 20, 20)
  )

# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("week-04.png", height = 8,
       width = 12, dpi=320)  
showtext_auto(FALSE)
