library(tidyverse)
library(showtext)
library(usmap)
library(patchwork)

# Set the Stage ------------------------------------------------

font <- "Ubuntu"
font_add_google(font, font)
showtext_auto(enable = TRUE)
theme_set(theme_minimal(base_family = font))
bg <- "#F0EBE3"
txt_col <- 'black'
color_low <- "#FFFFFF"
color_high <- "#3a0ca3"

# read data-----------------------------------------------------

tt_data <- tidytuesdayR::tt_load(2022, week=35)
pell <- tt_data$pell

# data wrangling-------------------------------------------------

df <- pell |> 
  group_by(STATE, YEAR) |> 
  summarise(total_award = sum(AWARD),
            total_recipients = sum(RECIPIENT))|>
  mutate(award_per_recipient = total_award / total_recipients )|>
  janitor::clean_names()|> ungroup()

df2000 <- df |> filter(year==2000)
df2017 <- df |> filter(year==2017)

# plot 1 ----------------------------------------------------------

p1 <- plot_usmap(data = df2000, values = "award_per_recipient")+
  scale_fill_continuous(low = color_low, high = color_high, limits=c(1000,6000),
                        name="Award per Recipient ($)",
                        guide=guide_legend(
                          keyheight = unit(3, units = "mm"),
                          keywidth=unit(12, units = "mm"), 
                          label.position = "bottom",
                          title.position = 'top', title.hjust=0.5, nrow=1))+
  labs(title = "2000")+
  theme(
    panel.grid = element_blank(),
    axis.title  = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(size=20, color=txt_col, hjust=.5,
                              face="bold", margin=margin(0,0,30,0)),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(0,0,0,0),
    legend.title = element_text(size=14),
    legend.text = element_text(size=10))
p1
# plot 2 ----------------------------------------

p2 <-  plot_usmap(data = df2017, values = "award_per_recipient")+
  scale_fill_continuous(low = color_low, high = color_high, limits=c(1000,6000),
                        name="Award per Recipient ($)",
                        guide=guide_legend(
                          keyheight = unit(3, units = "mm"),
                          keywidth=unit(12, units = "mm"), 
                          label.position = "bottom",
                          title.position = 'top', title.hjust=0.5, nrow=1))+
  labs(title = "2017")+
  theme(
    panel.grid = element_blank(),
    axis.title  = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(size=20, color=txt_col, hjust=.5,
                              face="bold", margin=margin(0,0,30,0)),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(0,0,0,0),
    legend.title = element_text(size=14),
    legend.text = element_text(size=10))

# PATCHWORK ----------------------------------------------------------

p1 + p2 + plot_annotation(
  title = 'PELL GRANTS',
  subtitle = 'The amount awarded to each recipient more than doubled.',
  caption = "Muhammad Azhar | #TidyTuesday Week 35 | Data: U.S. Department of Education",
) + plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom',
        legend.background = element_rect(color = bg, fill=bg),
        legend.justification = "center",
        plot.title = element_text(size=34, color=txt_col,lineheight=1,
                                  hjust=0.5,face="bold",
                                  margin=margin(10,0,10,0)),
        plot.subtitle = element_text(size=20, color="grey50", face='bold',
                                     hjust=0.5, margin=margin(0,0,15,0)),
        plot.caption = element_text(hjust=.5, margin=margin(0,0,0,0),
                                    size=12, color=txt_col, face="bold"),
        plot.margin = margin(20,20,20,20),
        plot.background = element_rect(color=bg, fill=bg)
  )


# Save plot ---------------------------------------------------------- 

showtext_opts(dpi = 320) 

ggsave("week_35.png", height = 8, width = 10, dpi=320)  

showtext_auto(FALSE)
