library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(usmap)

# Set the Stage ------------------------------------------------

font <- "Roboto"
font_add_google(family=font, font,db_cache = FALSE)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <-  "#F0EBE3"
txt_col <- "black"

# read data-----------------------------------------------------

tt <- tt_load(2022, week=39)
artists <- tt$artists
# data wrangling-------------------------------------------------

df <- artists |>
  filter(type=="Musicians")|>
  group_by(state)|>
  select(state, race, artists_n, artists_share, location_quotient)|>
  drop_na() |>
  ungroup()

# plot  ----------------------------------------------------------

plot_usmap(data = df, values = "artists_n")+
  scale_fill_continuous(low = "#ecf39e", high = "#007200", limits=c(0,18000),
                        name="# of Musicians",
                        guide=guide_legend(
                          keyheight = unit(3, units = "mm"),
                          keywidth=unit(12, units = "mm"), 
                          label.position = "bottom",
                          title.position = 'top', title.hjust=0.5, nrow=1))+
  labs( title = "Musicians in United States",
        subtitle = "California has more than 15000 musicians.",
        caption = "Muhammad Azhar | #TidyTuesday Week 39 | Data: arts.gov"
  )+
  
  theme(legend.position = 'bottom',
        legend.background = element_rect(color = bg, fill=bg),
        legend.justification = "center",
        plot.title = element_text(size=34, color=txt_col,lineheight=1,
                                  hjust=0.5,face="bold",
                                  margin=margin(10,0,10,0)),
        plot.subtitle = element_text(size=20, color="grey50", face='bold',
                                     hjust=0.5, margin=margin(0,0,15,0)),
        plot.caption = element_text(hjust=.5, margin=margin(10,0,0,0),
                                    size=12, color=txt_col, face="bold"),
        plot.margin = margin(20,20,20,20),
        plot.background = element_rect(color=bg, fill=bg))

# Save plot ---------------------------------------------------------- 

showtext_opts(dpi = 320) 

ggsave("week_39.png", height = 10, width = 10, dpi=320)  

showtext_auto(FALSE)
