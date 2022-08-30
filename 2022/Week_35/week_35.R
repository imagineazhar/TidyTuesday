library(tidytuesdayR)
library(tidyverse)
library(MetBrewer)
library(patchwork)
library(showtext)

# Set the Stage ------------------------------------------------

font <- "Roboto"
font_add_google(font, font)
theme_set(hrbrthemes::theme_ipsum(base_family = font))
showtext_auto(enable = TRUE)
bg <- "#F0EBE3"

# read data-----------------------------------------------------

pell_grants <- readxl::read_xlsx("pellinst1718.xlsx")

# data wrangling-------------------------------------------------

df <- pell_grants[(-(1:3)),]


# plot----------------------------------------------------------


df |> ggplot()+
  labs(
    title = " ",
    caption = "Muhammad Azhar | #TidyTuesday Week 35 | Data: U.S. Department of Education",
    x = " ",
    y = " ") +
  coord_cartesian(clip="off") +
  
  theme(
        axis.ticks = element_blank(), 
        axis.text.x = element_text(size=10, color="grey40", face='plain'),
        axis.text.y = element_text(size=10,color="grey40", face='plain'),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=10,color="grey40", face='plain', hjust=0.5),
        plot.title = element_text(size=16, face="bold"),
        plot.background = element_rect(color = bg, fill=bg),
        plot.margin = margin(20,30,0,30),
        plot.title.position = "plot",
        legend.position = "bottom",
        legend.title = element_blank())



# Save plot ---------------------------------------------------------- 

showtext_opts(dpi = 320) 

ggsave("week_35.png", height = 10, width = 8, dpi=320)  

showtext_auto(FALSE)
