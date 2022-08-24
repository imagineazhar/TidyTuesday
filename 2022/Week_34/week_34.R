library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(MetBrewer)
library(hrbrthemes)

# Set the Stage ------------------------------------------------
font <- "Roboto"
font_add_google(font, font)
theme_set(theme_ipsum(base_family = font))
showtext_auto(enable = TRUE)
bg <- "#F0EBE3"

# read data-----------------------------------------------------
tt_data <- tt_load(2022, week=34)

chips <- tt_data$chips


# data wrangling-------------------------------------------------

df <- chips |>
  select(year, transistors_million)



# plot----------------------------------------------------------

df |> ggplot(aes(x=year, y=transistors_million))+
  geom_line()+
  labs(
    title = "Transistor Production ",                
    subtitle = "2000 - 2021",
    caption = "Muhammad Azhar | #TidyTuesday Week 34 | Data: ",
    x = "",
    y = "") +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme(
        axis.ticks = element_blank(), 
        axis.text.x = element_text(size=12, color="grey60", face='plain'),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size=34, face="bold",
                                  margin=margin(10,0,5,0)),
        plot.subtitle = element_text(size=22, face="bold",color = "grey50",
                                     margin=margin(0,0,20,0)),
        plot.background = element_rect(color = bg, fill=bg),
        plot.margin = margin(30,50,30,50),
        plot.caption = element_text(size=12, color="black", face="plain",
                                    hjust=0.5,
                                    margin=margin(20,0,0,0)),
        legend.position = "none")

# Save plot ---------------------------------------------------------- 

showtext_opts(dpi = 320) 

ggsave("week_34.png", height = 8, width = 10, dpi=320)  

showtext_auto(FALSE)
