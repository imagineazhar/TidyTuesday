library(tidytuesdayR)
library(showtext)
library(tidyverse)
library(janitor)
library(glue)
library(hrbrthemes)
library(ggtext)


#read data
tt_data <- tt_load(2022, week=31)
frogs <- tt_data$frogs |> clean_names()


#data cleaning


# Set the Stage
font <- "Noto Serif"
font_add_google(family=font, font)
font_add_google("Roboto", "roboto")
bg <- "#F0EBE3"
txt_col <- "black"
col <- "#0F3D3E"
showtext_auto(enable = TRUE)

#plot
ggplot(df, aes(x=year, y=value, color=metric))+
  geom_line(aes(color=metric),size=1)+
  labs() +
  coord_cartesian(clip="off") +
  theme_ipsum_tw()+
  theme(
    panel.grid = element_blank(),
    axis.title.x  = element_blank(),
    axis.title.y  = element_text(color=txt_col, size=10),
    axis.text = element_text(color=txt_col, size=10),
    axis.line = element_line(),
    plot.title = element_markdown(size=20, color=txt_col, hjust=.5,lineheight=1,
                              family = font,
                              face="bold", margin=margin(0,0,10,0)),
    plot.subtitle = element_text(size=12, family="Roboto",
                                 color = "grey50",hjust=.5,
                                 lineheight=1, face = "plain",
                                 margin = margin(0,0,20,0)),
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8,
                                color=txt_col, face="plain"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.title.position = "panel",
    plot.margin = margin(30,30,30,30),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.justification = "center",
  ) 

#Save plot  
showtext_opts(dpi = 320) 

ggsave("week31.png",
       height = 7,
       width = 10,
       dpi=320,
       
)  

showtext_auto(FALSE)
