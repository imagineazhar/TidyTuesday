library(tidytuesdayR)
library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)
library(hrbrthemes)

# Set the Stage ------------------------------------------------
font <- "Poppins"
font_add_google(font, font)
theme_set(theme_ipsum(base_family = font))
showtext_auto(enable = TRUE)
bg <- "#F0EBE3"

# read data-----------------------------------------------------
tt_data <- tt_load(2022, week=26)
paygap <- tt_data$paygap |> clean_names()


# data wrangling-------------------------------------------------

df <- paygap |> filter(substr(due_date,1,4)=="2022",
                       employer_size=="20,000 or more") |> na.omit()
# plot----------------------------------------------------------

p1 <- df|> ggplot()+
  geom_point(aes(x=female_top_quartile, y=female_lower_quartile, size=3))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+

    labs(
    title = "UK Gender Pay Gap | 2022",
    subtitle = " ",
    caption = "Muhammad Azhar | #TidyTuesday Week 26 | Data: ons.gov.uk") +
  
  coord_cartesian(clip="off") +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size=11, color="black"),
    axis.text.y = element_text(size=11, color="black"),
    plot.title = element_text(size=26, face="bold",
                              margin=margin(10,0,10,0)),
    plot.subtitle = element_text(size=20, color="grey50", face="bold",
                                 margin=margin(0,0,20,0)),
    plot.caption = element_text(size=11, color="black", face="plain", hjust=0.5,
                                margin=margin(20,0,0,0)),
    plot.background = element_rect(color = bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.title = element_blank(),
    legend.text = element_blank()
  )

# Save plot ---------------------------------------------------------- 

showtext_opts(dpi = 320) 

ggsave("week26.png", height = 7, width = 7, dpi=320,)  

showtext_auto(FALSE)
