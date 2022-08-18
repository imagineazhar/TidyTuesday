library(dplyr)
library(ggtext) 
library(ggplot2)
library(tidytuesdayR)
library(showtext)
library(hrbrthemes)


#default font from show text
font_add_google("Noto Serif", "Noto Serif")
font_add_google("Roboto", "roboto")
showtext_auto()

#read data
tt_data <- tt_load(2022, week=27)
rent <- tt_data$rent

#clean Data
rent_clean <- rent |> filter(city=='san francisco') |>
  drop_na() 

rent_sum <- rent_clean |> group_by(year, nhood)|>
  mutate(year=factor(year,levels = c('2014','2015','2016','2017','2018'))) |> 
  summarise(price=mean(price))

#plot
rent_sum|> ggplot(aes(x=price))+
  geom_histogram( binwidth=1000, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  labs(x,title = "Price distribution of San Francisco Rentals",
     caption = "Data: Kate Pennington | Graphic: @imagineazhar") +
  theme_ipsum() +
  theme(
    plot.background = element_rect(fill = "#F0EBE3"),
    panel.background = element_rect(fill = "#F0EBE3"),
    plot.title = element_text( face = "bold", size=20.8,
                               margin = margin(t=2), family = "Noto Serif"),
    plot.subtitle = element_text(face = "plain", size=12, color = "grey50", 
                                 margin = margin(t=5,b=15),
                                 family = "Roboto", lineheight = 1.2),
    plot.caption = element_text(color = "grey50", vjust = 0, size = 11),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

#Save plot  
ggsave("2022_W27_SF-rentals.png", 
       units = 'in', width = 5, height = 4, dpi = 100)
