library(tidytuesdayR)
library(showtext)
library(tidyverse)
library(janitor)
library(patchwork)
library(hrbrthemes)


# Set the Stage
font <- "Noto Serif"
font_add_google(family=font, font)
font_add_google("Roboto", "roboto")
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <- "#F0EBE3"
txt_col <- "black"
col <- "#0F3D3E"
annotation_color <- "#B20600"

#read data
tt_data <- tt_load(2022, week=29)
tech <- tt_data$technology

codes <- read_csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv") |>
clean_names()


#data cleaning
technology <- tech |> 
  left_join(
    codes |> select(iso3c=alpha_3, region, sub_region)
  )

df <- technology|>
  filter(variable %in% c("steel_production", "steel_demand") & iso3c=='PAK' & year>1990)|>
  select(iso3c, year, label, value) |> group_by(year, label) |>
  rename(metric=label) |> ungroup()


#plot
ggplot(df, aes(x=year, y=value, color=metric))+
  geom_line(aes(color=metric),size=1)+
  labs(
    title = "Supply & Demand of Steel in Pakistan",
    subtitle = "Steel production & demand measured in thousand metric tons,\n 1991-2019",
    caption = "Muhammad Azhar | #TidyTuesday Week 29 | Data: data.nber.org",
    y= "thousand metric tons"
  ) +
  coord_cartesian(clip="off") +
  theme_ipsum()+
  theme(
    panel.grid = element_blank(),
    axis.title.x  = element_blank(),
    axis.title.y  = element_text(color=txt_col, size=10),
    axis.text = element_text(color=txt_col, size=10),
    axis.line = element_line(),
    plot.title = element_text(size=20, color=txt_col, hjust=.5,lineheight=1,
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

ggsave("week29.png",
       height = 7,
       width = 10,
       dpi=320,
       
)  

showtext_auto(FALSE)
