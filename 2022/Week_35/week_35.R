library(tidyverse)
library(showtext)
library(usmap)
library(hrbrthemes)
library(patchwork)

# Set the Stage ------------------------------------------------

font <- "Lora"
font_add_google(font, font)
theme_set(theme_ipsum(base_family = font))
showtext_auto(enable = TRUE)
bg <- "#F0EBE3"

# read data-----------------------------------------------------

tt_data <- tidytuesdayR::tt_load(2022, week=35)
pell <- tt_data$pell

# data wrangling-------------------------------------------------

df <- pell |> 
  group_by(STATE, YEAR) |> 
  summarise(sumAward = sum(AWARD),
            sumRecipient = sum(RECIPIENT))|>
  mutate(award_per_recipient = sumAward / sumRecipient )|>
  janitor::clean_names()|> ungroup()

df2000 <- df |> filter(year==2000)
df2017 <- df |> filter(year==2017)

# plot 1 ----------------------------------------------------------

p1 <- plot_usmap(data = df2000, values = "award_per_recipient")+
  scale_fill_continuous(low = "#F5F5F5", high = "#210cae",limits=c(1e3,5000),
                        name="Total Pell Grant Funding (Million USD$)",
                        labels = scales::dollar_format())+
  labs(title = "2000")+
  theme(
    panel.grid = element_blank(),
    axis.title  = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(size=20, color='black', hjust=.5, face="bold", margin=margin(0,0,30,0)),
    plot.background = element_rect(color=bg, fill=bg),
    plot.title.position = "panel",
    plot.margin = margin(0,0,0,0),
    legend.position = "bottom"
    )+ 
  guides(colour = guide_legend(title.position = "top"))
  
p1


# plot 2 ----------------------------------------

p2 <-  plot_usmap(data = df2017, values = "award_per_recipient")+
  scale_fill_continuous(low = "#F5F5F5", high = "#210cae",limits=c(1e3,5000),
                        name="Total Pell Grant Funding (Million USD$)",
                        labels = scales::dollar_format())+
  labs(title = "2017")+
  theme(
    panel.grid = element_blank(),
    axis.title  = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(size=20, color='black', hjust=.5, face="bold", margin=margin(0,0,30,0)),
    plot.background = element_rect(color=bg, fill=bg),
    plot.title.position = "panel",
    plot.margin = margin(0,0,0,0),
    legend.position = "bottom"
  )+ 
  guides(colour = guide_legend(title.position = "top"))

p2

# PATCHWORK ----------------------------------------------------------

(p1+ p2) +
  plot_annotation(
  title = 'Pell Grants',
  caption = "Muhammad Azhar | #TidyTuesday Week 35 | Data: U.S. Department of Education",
  theme = theme(
    plot.title = element_text(size=34, face="bold", hjust=.5,
                              margin=margin(0,0,30,0)),
    plot.subtitle = element_text(size=16, face="bold",color = "grey40"),
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0),
                                size=12, face="bold"),
    plot.title.position = "plot",
    plot.margin = margin(30,30,30,30),
    plot.background = element_rect(color=bg, fill=bg),
    legend.position = "bottom")
  ) + plot_layout(guides = "collect")



# Save plot ---------------------------------------------------------- 

showtext_opts(dpi = 320) 

ggsave("week_35.png", height = 8, width = 10, dpi=320)  

showtext_auto(FALSE)
