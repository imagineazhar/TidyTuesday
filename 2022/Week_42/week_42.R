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

tt <- tt_load(2022, week=42)
readme(tt)
episodes <- tt$episodes
all_dialogue <- tt$stranger_things_all_dialogue

# data wrangling-------------------------------------------------


# Text ----------------------

title_text <- "Stranger Things"
caption_text <- "Muhammad Azhar | #TidyTuesday Week 42 | Data: 8flix.com"



# plot  ----------------------------------------------------------

df |> 
  
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

ggsave("week_42.png", height = 10, width = 10, dpi=320)  

showtext_auto(FALSE)
