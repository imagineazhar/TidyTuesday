new_week <- function() {
  
  # Get today's date and format it
  todays_date <- format(Sys.Date(), "%Y-%m-%d")
  
  # Set the base path for file creation
  base_path <- "D:/Projects/TidyTuesday/2024"
  
  # Check if the folder already exists and delete it if so
  full_folder_path <- file.path(base_path, todays_date)
  if (dir.exists(full_folder_path)) {
    unlink(full_folder_path, recursive = TRUE)  # Delete existing folder and contents
  }
  
  # Create the folder with today's date in the specified path
  dir.create(full_folder_path)
  
  # Get the year and week number
  current_year <- format(Sys.Date(), "%Y")
  week_number <- strftime(Sys.Date(), "%V")
  
  # Create the README.md file with Markdown content
  file_path <- file.path(full_folder_path, "README.md")
  cat(paste("#", current_year, "\n"), file = file_path)
  cat(paste("# Week Number:", week_number), file = file_path, append = TRUE)
  
  # Create the R script file with the provided code snippet
  r_script_path <- file.path(full_folder_path, paste0(todays_date, ".R"))
  cat(paste("library(ggplot2)\n",
            "library(showtext)\n",
            "library(tidyverse)\n",
            "library(ggtext)\n\n",
            "# ------ Get Data ------\n\n",
            "# ------ Data Wrangling ------\n\n",
            "# ------ Typography ------\n\n",
            "font_add_google(\"Outfit\", \"title_font\")\n",
            "font_add_google(\"Outfit\", \"body_font\")\n",
            "showtext_auto()\n\n",
            "title_font <- \"title_font\"\n",
            "body_font <- \"body_font\"\n\n",
            "# ------ Texts ------\n\n",
            "title_text <- \" \"\n",
            "subtitle_text <- \"\"\n",
            "caption_text <- \"Graphic: Muhammad Azhar Data:  \"\n\n",
            "# ------ Plot ------\n\n",
            "ggplot(df, aes()) +\n\n\n",
            "labs(title = title_text,\n",
            "subtitle = subtitle_text,\n",
            "caption = caption_text)+ \n",
            "theme_minimal()+\n",
            "theme(\n",
            "axis.title.x = element_text(hjust = 0.32, color = \"grey30\"),\n",
            "axis.title.y = element_blank(),\n",
            "axis.text.x = element_text(family = body_font,\n",
            "face = \"bold\",\n",
            "size=12),\n",
            "axis.text.y = element_text(family = body_font,\n",
            "face = \"bold\",\n",
            "size=12),\n",
            "\n",
            "# Legend\n",
            "legend.position = \"none\",\n",
            "\n",
            "# TITLE\n",
            "plot.title.position = \"plot\",\n",
            "plot.title = element_textbox(margin = margin(20, 0, 5, 0),\n",
            "size = 24,\n",
            "family = title_font,\n",
            "face = \"bold\",\n",
            "width = unit(75, \"lines\")),\n\n",
            "# SUB-TITLE\n",
            "plot.subtitle = element_textbox(margin = margin(5, 0, 30, 0),\n",
            "size = 16,\n",
            "family = body_font,\n",
            "face = \"bold\",\n",
            "width = unit(75, \"lines\")),\n\n",
            "plot.background = element_rect(color=, fill=),\n",
            "plot.margin = margin(20, 20, 20, 20)))\n"
            ),
            file = r_script_path)
}
