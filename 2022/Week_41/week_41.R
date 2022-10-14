library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(MetBrewer)

# Set the Stage ------------------------------------------------

font <- "Satisfy"
font_add_google(family=font, font,db_cache = FALSE)
showtext_auto(enable = TRUE) 
theme_set(theme_void(base_family = font))
bg <-  "#F0EBE3"
txt_col <- "black"

# read data-----------------------------------------------------

tt <- tt_load(2022, week=41)
readme(tt)
yarn <- tt$yarn


# data wrangling-------------------------------------------------

df <- yarn |> 
  select(yarn_company_name, rating_average, rating_count, yarn_weight_name)|>
  drop_na()

df <- df |>
  group_by(yarn_company_name, yarn_weight_name)|>
  summarise(average_rating = mean(rating_average),
            average_count = mean(rating_count))|>
  ungroup()

# Text ----------------------

title_text <- "Spooky Threads"
caption_text <- "Muhammad Azhar | #TidyTuesday Week41 | Data: Ravelry.com"



# plot  ----------------------------------------------------------

df |> ggplot(aes(x=average_count, y=average_rating, colour=yarn_weight_name))+
  geom_line(size=1) +
  scale_x_log10(limits =c(1,1e5)) +
  scale_colour_manual(values = met.brewer(name = "Derain", n = 15)) +
  coord_polar()+
  
  labs(title = title_text,
       caption = caption_text,
       x = " ",
       y = " ")+
  theme(
    legend.position = 'None',

    # Title
    plot.title = element_text(size=34,
                              color=txt_col,
                              lineheight=1,
                              hjust=0.5,
                              face="bold"),
    # Caption
    plot.caption = element_text(hjust=.5,
                                size=12, 
                                color=txt_col,
                                face="bold",
                                margin=margin(10,0,0,0)),
    plot.margin = margin(20,20,20,20),
    plot.background = element_rect(color=bg, fill=bg))

# Save plot ---------------------------------------------------------- 

showtext_opts(dpi = 320) 

ggsave("week_41.png", height = 10, width = 10, dpi=320)  

showtext_auto(FALSE)
