library(tidyverse)
library(showtext)
library(hrbrthemes)


# Set the Stage ------------------------------------------------

font <- "Righteous"
font_add_google(font, font)
showtext_auto(enable = TRUE)
theme_set(theme_ipsum(base_family = font))
bg <- "#F0EBE3"
txt_col <- 'black'


# read data-----------------------------------------------------

sets <-  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')
themes <-  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/themes.csv.gz')


# data wrangling-------------------------------------------------

df <- sets |>
  left_join(themes |>
              select(id, theme_name=name),
            by=c(theme_id="id"))|>
  group_by(name, year, theme_name)|>
  summarise(n_sets=n(), num_parts)

# plot ----------------------------------------------------------

df |> ggplot(aes(year, n_sets, fill= 'orange' ))+
  geom_col()+
  labs(
    title = 'LEGO SETS',
    subtitle = "The number of sets is increasing.",
    caption = "Muhammad Azhar | #TidyTuesday Week36 | Data:LEGO database",
    y = "# of LEGO sets",
    x = " "
    )+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(hjust = 0.5),
    plot.title = element_text(size=20, color=txt_col,
                              face="bold", margin=margin(0,0,10,0)),
    plot.subtitle =  element_text(size=14, color='grey60',
                              face="bold", margin=margin(0,0,30,0)),
    
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0),
                                size=10, color='grey20', face="plain"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = 'hidden')

# Save plot ---------------------------------------------------------- 

showtext_opts(dpi = 320) 

ggsave("week_36.png", height = 8, width = 10, dpi=320)  

showtext_auto(FALSE)
