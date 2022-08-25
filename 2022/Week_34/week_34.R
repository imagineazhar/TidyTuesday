library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(MetBrewer)
library(patchwork)

# Set the Stage ------------------------------------------------

font <- "Share Tech"
font_add_google(font, font)
theme_set(hrbrthemes::theme_ipsum(base_family = font))
showtext_auto(enable = TRUE)
bg <- "#F0EBE3"

# read data-----------------------------------------------------

raw_chips <- read.csv("chip_dataset.csv")


# data wrangling-------------------------------------------------

chips <- raw_chips|>
  janitor::clean_names()|>
  mutate(year = lubridate::year(as.Date(release_date)))|>
  select(product, year, type, process_size_nm, tdp_w, die_size_mm_2,
         transistors_million, freq_m_hz, vendor)|>
  drop_na()



# plot----------------------------------------------------------

p1 <- chips |> ggplot(aes(x=year, y=transistors_million, color=type))+
  geom_point(size = 3.5, alpha=0.25)+
  scale_color_manual(values=met.brewer("Egypt"))+
  scale_y_log10()+
  geom_smooth(method = 'loess', se=FALSE)+
  labs(
    title = "The number of transistors on a chip is still increasing exponentially over time, especially for GPUs.",
    x = "Year",
    y = "Tansistors (millions)") +
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
        legend.position = "top",
        legend.title = element_blank())

# plot 2 ------------------------------------------------------

p2 <- chips |> ggplot(aes(x=year, y=die_size_mm_2))+
  geom_point(size=2)+
  scale_color_manual(values=met.brewer("Egypt"))+
  geom_smooth(method = 'loess', se=FALSE)+
  facet_wrap(~type)+
  labs(
    title = "Die density (number of transistors per unit area) is increasing for GPUs,driving the increase in transistor count.",
    x = "Year",
    y = "Die size (mm^2)") +
  coord_cartesian(clip="off")+
  theme(
    axis.ticks = element_blank(), 
    axis.text.x = element_text(size=10, color="grey40", face='plain'),
    axis.text.y = element_text(size=10,color="grey40", face='plain'),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=12,color="grey40", face='plain', hjust=0.5),
    plot.title = element_text(size=16, face="bold"),
    plot.background = element_rect(color = bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    plot.title.position = "plot",
    legend.position = "none")

# PATCHWORK ----------------------------------------------------------

(p1/p2) + 
  plot_annotation(
    title = stringr::str_to_upper("Does Moore's law still hold?"),
    subtitle = "Moore’s Law predicted that the number of transistors on a chip would double every 18 months.",
    caption = "Muhammad Azhar | #TidyTuesday Week 34 | Data: CHIP dataset ",
    theme = theme(
      plot.title = element_text(size=34, face="bold",
                                margin=margin(10,0,5,0)),
      plot.subtitle = element_text(size=18, face="bold",color = "grey40"),
      plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0),
                                  size=12, face="bold"),
      plot.title.position = "plot",
      plot.margin = margin(30,30,30,30),
      plot.background = element_rect(color=bg, fill=bg)))

# Save plot ---------------------------------------------------------- 

showtext_opts(dpi = 320) 

ggsave("week_34.png", height = 10, width = 12, dpi=320)  

showtext_auto(FALSE)
