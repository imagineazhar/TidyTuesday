library(tidytuesdayR)
library(showtext)
library(tidyverse)
library(janitor)
library(patchwork)
library(maptools)
library(broom)
library(ggmap)
library(scico)


#default font from show text
font_add_google("Noto Serif", "Noto Serif")
font_add_google("Roboto", "roboto")
showtext_auto()

#read data
tt_data <- tt_load(2022, week=29)
tech <- tt_data$technology

codes <- read_csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv") %>% 
clean_names()


#data cleaning
technology <- tech |> 
  left_join(
    codes |> select(iso3c=alpha_3, region, sub_region)
  )

df <- technology|>
  filter(variable=='pctimmunizmeas' & region=='Asia')|>
  select(iso3c, year, label, value)


data("wrld_simpl")
asia <-wrld_simpl[wrld_simpl$REGION==150,]

asia_2 <- tidy(asia)

df2 <- asia_2 |> rename(iso3c=id) |> left_join(df)

