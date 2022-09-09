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

colours <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/colors.csv.gz')
inventory_parts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_parts.csv.gz')
inventories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
inventory_sets <-  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_sets.csv.gz')
sets <-  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')
themes <-  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/themes.csv.gz')


# data wrangling-------------------------------------------------

df <- sets |>
  left_join(themes |>
              select(id, theme_name=name),
            by=c(theme_id="id"))|>
  group_by(name, year, theme_name)|>
  summarise(n_sets=n())


# plot ----------------------------------------------------------
df |> ggplot(aes(year, n_sets, fill= 'orange' ))+
  geom_col()+
  labs(
    title = "The Number of LEGO sets over the years.",
    y = "# of LEGO sets",
    x = " "
    )+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(hjust = 0.5),
    plot.title = element_text(size=20, color=txt_col,
                              face="bold", margin=margin(0,0,30,0)),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = 'hidden')




# Save plot ---------------------------------------------------------- 

showtext_opts(dpi = 320) 

ggsave("week_36.png", height = 8, width = 10, dpi=320)  

showtext_auto(FALSE)
