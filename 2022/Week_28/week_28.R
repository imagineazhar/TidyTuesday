library(tidyverse)
library(dplyr)
library(ggtext) 
library(ggplot2)
library(tidytuesdayR)
library(showtext)


#default font from showtext
font_add_google("Noto Serif", "Noto Serif")
font_add_google("Roboto", "roboto")
showtext_auto()

#read data
tt_data <- tt_load(2022, week=28)
flights <- tt_data$flights

#clean data
total <- flights|>group_by(STATE_NAME)|>
  ungroup()|>
  group_by(STATE_NAME, YEAR, MONTH_NUM)|>
  summarise(total_flights = sum(FLT_TOT_1))|>
  ungroup()|>
  unite(col = "yearmonth", YEAR, MONTH_NUM, remove = FALSE)|>
  distinct()|>
  group_by(STATE_NAME)|>
  mutate(share = total_flights/sum(total_flights, na.rm = TRUE)*100)|>
  ungroup()|>
  mutate(State2=STATE_NAME)

#plot
total |> ggplot(aes(x=yearmonth, y=total_flights, group=1))+
  geom_line(data = total|> dplyr::select(-STATE_NAME), aes(group=State2),
            color="grey", size=0.5, alpha=0.5)+
  geom_line(aes(color=STATE_NAME), color="#DF7861", size=1.2 )+
  
  labs(title = "Commercial Flights in Europe",
       subtitle = "Each line is associated to a European country with complete flight data from January 2016 to May 2022. The y-axis represents the total flights from and to each country.",
       caption = "Data Source:Eurocontrol | Created by @imagineazhar",
       x = "",
       y = "")+
  theme_void()+
  theme(
    plot.background = element_rect(fill = "#F0EBE3"),
    panel.background = element_rect(fill = "#F0EBE3"),
    plot.title = element_text( face = "bold", size=24.4,
                               margin = margin(t=25), family = "Noto Serif"),
    plot.subtitle = element_text(face = "plain", size=12, color = "grey50", 
                                     margin = margin(t=5,b=15),
                                     family = "Roboto", lineheight = 1.2),
    plot.caption = element_text(color = "grey50", vjust = 0, size = 11),
    plot.margin = unit(c(1,1,1,1), "cm"))+
  facet_wrap(~STATE_NAME)
  
#Save plot  
ggsave("european-flights.png", 
       units = 'in', width = 7, height = 5, dpi = 300)
