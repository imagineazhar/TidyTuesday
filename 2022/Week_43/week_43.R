library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(MetBrewer)


# ------ Load TidyTuesday data ------ 

tt <- tt_load(2022, week=43)
readme(tt)

gbbo_challenges <- tt$challenges

# ------ Data wrangling ------ 



# ------ Texts ------ 

title_text <- "Stranger Things"
subtitle_text <- ""
caption_text <- "Muhammad Azhar | Twitter: @imagineazhar | Data: {bakeoff}"


# ------ Typography ------ 

font_add_google("Vollkorn", "title_font")
font_add_google("Chivo", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Set theme ------ 

bg <-  "#F0EBE3"
txt_col <- "black"


# ------ Plot ------ 


  coord_fixed()+
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text)+
  theme_void()+
  theme(
    # Legend
    legend.position = "none",
    legend.key.width = unit(0.5, 'in'),
    legend.text = element_text(family = body_font,
                               face = 'plain',
                               color = txt_col,
                               size = 10,
                               hjust = 0.5,
                               margin = margin(20,0,20,0)),
    # TITLE
    plot.title.position = "plot",
    plot.title = element_text(family = title_font,
                              face = "bold",
                              color = txt_col,
                              size = 40,
                              lineheight = 1,
                              margin = margin(20,0,20,0)),
    # Subtitle
    plot.subtitle = element_textbox(family=body_font,
                                    face = "plain",
                                    width = unit(32, "lines"),
                                    size = 14,
                                    lineheight = 1,
                                    margin = margin(0,0,10,0)),
    # Caption
    plot.caption = element_text(family=body_font,
                                face="plain",
                                size=12, 
                                color=txt_col,
                                hjust=.5,
                                margin=margin(20,0,0,0)),
    
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30)
    )



# ------ Save Plot ------ 

showtext_opts(dpi = 320) 
ggsave("week_43.png", height = 10,
       width = 8, dpi=320)  
showtext_auto(FALSE)
