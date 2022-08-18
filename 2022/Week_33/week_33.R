library(tidytuesdayR)
library(tidyverse)
library(janitor)
library(showtext)
library(MetBrewer)

# Set the Stage ------------------------------------------------
font <- "Mukta"
font_add_google(font, font)
theme_set(theme_minimal(base_family = font))
showtext_auto(enable = TRUE)
bg <- "#F0EBE3"

# read data-----------------------------------------------------
tt_data <- tt_load(2022, week=33)
paygap <- tt_data$paygap |> clean_names()


# data wrangling-------------------------------------------------


# plot----------------------------------------------------------

df |> ggplot( aes(diff_mean_hourly_percent, fct_rev(employer_size),
                         color=employer_size))+
     ggridges::stat_density_ridges(
       aes(fill = stat(quantile)),
       geom = "density_ridges_gradient",
       quantile_lines = TRUE, quantiles = 4, 
       color = "black", alpha = .8, size = 0.8
     ) +
  scale_fill_met_d(name = "Morgenstern", direction = 1)+
  scale_x_continuous(expand = c(0,-1), limits = c(-105, 105)) + 
  scale_y_discrete( expand = c(0,0), limits=c('Not Provided',
                                              'Less than 250', '250 to 499',
                                              '500 to 999', '1000 to 4999',
                                              '5000 to 19,999',
                                              '20,000 or more'))+
  labs(
    title = "UK Gender Pay Gap",                
    subtitle = "Mean % difference between male and female hourly pay.
(negative = women's mean hourly pay is higher) ",
    caption = "Muhammad Azhar | #TidyTuesday Week 26 | Data: ONS.gov.uk",
    x = "Mean % difference hourly pay",
    y = "Compnay Size --->") +
  
  coord_cartesian(clip="off") +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_text (size=11, color="grey20",
                                     margin = margin(10,0,0,0) ),
    axis.title.y = element_text (size=11, color="grey20", hjust=0.4,
                                 margin = margin(r=5)),
    axis.text.x = element_text(size=11, color="black"),
    axis.text.y = element_text(size=11, color="black"),
    
    plot.title = element_text(size=24, face="bold",
                              margin=margin(10,0,10,0)),
    plot.title.position = 'plot',
    plot.subtitle = element_text(size=16, color="grey50", face="plain",
                                 margin=margin(0,0,10,0)),
    plot.caption = element_text(size=11, color="black", face="plain", hjust=0.5,
                                margin=margin(20,0,0,0)),
    plot.background = element_rect(color = bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.title = element_text("Quantiles"),
    legend.text = element_text(size=12),
    legend.position = "top",
    legend.justification="right"
  )

# Save plot ---------------------------------------------------------- 

showtext_opts(dpi = 320) 

ggsave("week_33.png", height = 7, width = 8, dpi=320)  

showtext_auto(FALSE)
