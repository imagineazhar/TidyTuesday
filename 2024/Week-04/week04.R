library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(MetBrewer)
library(showtext)

# ------ Get Data ------ 

tt_data <- tt_load(2024, week=04)
readme(tt_data)


# ------ Data Wrangling ------ 



# ------ Typography ------ 

font_add_google("PT Serif", "title_font")
font_add_google("Chivo", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Set theme ------ 

bg <-  "#F0EBE3"
txt_col <- "black"

# ------ Texts ------ 

title_text <- ""
subtitle_text <- ""
caption_text <- "Muhammad Azhar         Twitter: @imagineazhar         Data: "


# ------ Plot ------

df |> 
  theme_minimal()+
  theme(
    axis.text.x = element_text(family = body_font,
                               size = 12,
                               face = 'plain',
                               color = txt_col),
    
    # Legend
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.height= unit(0.5, 'cm'),
    legend.key.width= unit(2, 'cm'),
    legend.text = element_text(family = body_font,
                               size=12,
                               face = 'plain',
                               color = txt_col),
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
                                    width = unit(65, "lines"),
                                    size = 16,
                                    color = "grey20",
                                    lineheight = 1,
                                    margin = margin(0,0,10,0)),
    # Caption
    plot.caption = element_text(family=body_font,
                                face="plain",
                                size=12, 
                                color="grey40",
                                hjust=.5,
                                margin=margin(20,0,0,0)),
    
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,100,30,100)
    )

# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("week-04.png", height = 10,
       width = 16, dpi=320)  
showtext_auto(FALSE)
