library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(MetBrewer)
library(hrbrthemes)

# Set the Stage ------------------------------------------------
font <- "Concert One"
font_add_google(font, font)
theme_set(theme_ipsum(base_family = font))
showtext_auto(enable = TRUE)
bg <- "#F0EBE3"

# read data-----------------------------------------------------
tt_data <- tt_load(2022, week=33)
show_name = "Friends"
characters <- tt_data$characters |> filter(uni_name==show_name)
myer_briggs <- tt_data$myers_briggs |> filter(uni_name==show_name)
stats <- tt_data$psych_stats |> filter(uni_name==show_name)

traits <- c("romantic", "driven", "curious", "moody", "gossiping", "chatty")

# data wrangling-------------------------------------------------

df <- characters|>
  select(char_name=name)|>
  left_join(
    stats|>
      filter(personality %in% traits)|>
      select(char_name, personality, avg_rating)
  )



# plot----------------------------------------------------------

df |>ggplot(aes(y=as.factor(char_name), x=avg_rating, fill=char_name))+
  geom_col(width = 0.7)+
  facet_wrap(~personality)+
  scale_fill_met_d(name = "Veronese", direction = 1)+
  labs(
    title = "F.R.I.E.N.D.S",                
    subtitle = "Personality traits of the main characters.",
    caption = "Muhammad Azhar | #TidyTuesday Week 33 | Data: openpsychometrics.org",
    x = "",
    y = "") +
  scale_x_continuous(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(
    aes(0, y = char_name, label = char_name),
    hjust = 0, nudge_x = 0.3, colour = "white", size = 3.5, fontface='bold')+
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
        plot.caption = element_text(size=12, color="black", face="plain", hjust=0.5,
                                    margin=margin(20,0,0,0)),
        strip.text = element_text(size = 18),
        legend.position = "none")

# Save plot ---------------------------------------------------------- 

showtext_opts(dpi = 320) 

ggsave("week_33.png", height = 8, width = 10, dpi=320)  

showtext_auto(FALSE)
