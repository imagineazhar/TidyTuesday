library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(MetBrewer)
library(ggtext)
library(tidytext)
library(ggraph)
library(tidygraph)

# read data-----------------------------------------------------

tt <- tt_load(2022, week=42)
#readme(tt)
all_dialogue <- tt$stranger_things_all_dialogue

# data wrangling-------------------------------------------------

# get bigrams from the text
df <- all_dialogue|>
  unnest_tokens(output=bigram,
                input=dialogue,
                token = "ngrams",
                n=2)

# remove stop words
df_bigram <- df|>
  tidyr::separate(col = bigram,
                  into = c("word_1", "word_2"),
                  sep = " ")|>
  filter(!word_1 %in% stop_words$word,
         !word_2 %in% stop_words$word)

# filter by Adjectives (at least one)

vec_idx_adjectives <- which(parts_of_speech$pos == "Adjective")
vec_words_adjectives <- parts_of_speech$word[vec_idx_adjectives]

df_bigram_adj <- df_bigram|>
  filter(word_1 %in% vec_words_adjectives | word_2 %in% vec_words_adjectives)

df_bigram_adj_count <- df_bigram_adj|>
  count(word_1, word_2, sort = T)

df_viz <- df_bigram_adj_count|>
  rename(freq = n)|>
  filter(freq >= 5)|>
  mutate(color_encoding = case_when( freq <= quantile(freq, 0.7) ~ 1,
                                     freq > quantile(freq, 0.7) & freq <= quantile(freq, 0.85) ~ 2,
                                     freq > quantile(freq, 0.85) & freq <= quantile(freq, 0.95) ~ 3,
                                     freq > quantile(freq, 0.95) & freq <= quantile(freq, 0.97) ~ 4,
                                     T ~ 5))

# ------ CREATE NETWORK ------ 

graph_bigram <- df_viz |>
  igraph::graph_from_data_frame()|>
  as_tbl_graph()|>
  activate(nodes)|>
  mutate(degree_score = centrality_degree())|>
  rename(word = name)|>
  activate(edges) |>
  mutate(color_encoding = ordered(color_encoding,
                                  levels=c(1,2,3,4,5),
                                  labels= c("frequency < 10",
                                            "frequency >= 10 and < 12",
                                            "frequency >= 12 and < 20",
                                            "frequency >= 20 and < 50",
                                            "frequency > 50")))
# ------ Texts ------ 

title_text <- "Stranger Things"
subtitle_text <- "Stranger Things is an American sci-fi television series created by the Duffer Brothers. It was first released as a Netflix original series on July 15, 2016.<br><br> Each dot represents a word, and when they are connected by a line, a bigram (a pair of words) is formed.The entire network displays the most frequently used bigrams throughout all seasons of the show.<br>"
caption_text <- "Muhammad Azhar | Twitter: @imagineazhar | Data: 8flix.com"


# ------ Typography ------ 

font_add_google("Vollkorn", "title_font")
font_add_google("Chivo", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Set theme ------ 

bg <-  "#F0EBE3"
txt_col <- "black"


# ------ Plot ------ 

network<- graph_bigram|>
  ggraph(layout = 'fr')+
  geom_edge_arc(aes(edge_color=color_encoding),
                edge_width = 1.5,
                strength = 0.3,
                end_cap = circle(5, 'pt'))+
  geom_node_point(aes(size=degree_score),
                  colour = "#E0144C")+
  ggrepel::geom_label_repel(aes(x=x, y=y, label=word),
                            force = 10,
                            force_pull = 0.01,
                            colour = "#42032C",
                            family = body_font,
                            fontface = "plain",
                            size = 5)+
  scale_alpha_identity(guide = "none")+
  scale_edge_color_manual(values = met.brewer("OKeeffe2", 5, direction = 1),
                          labels = ~ stringr::str_wrap(.x, width = 15),
                          guide = guide_legend(title = NULL,
                                               label.position = "top"))+
  guides(size="none")+
  coord_fixed()+
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text)+
  theme_void()+
  theme(
    # Legend
    legend.position = "none",
    legend.key.width = unit(0.5, 'in'),
    legend.text = element_text(family = body_font,
                               face = 'plain',
                               color = txt_col,
                               size = 10,
                               hjust = 0.5,
                               margin = margin(20,0,20,0)),
    # TITLE
    plot.title.position = "plot",
    plot.title = element_text(family = title_font,
                              face = "bold",
                              color = txt_col,
                              size = 40,
                              lineheight = 1,
                              margin = margin(20,0,20,0)),
    # Subtitle
    plot.subtitle = element_textbox(family=body_font,
                                    face = "plain",
                                    width = unit(32, "lines"),
                                    size = 14,
                                    lineheight = 1,
                                    margin = margin(0,0,10,0)),
    # Caption
    plot.caption = element_text(family=body_font,
                                face="plain",
                                size=12, 
                                color=txt_col,
                                hjust=.5,
                                margin=margin(20,0,0,0)),
    
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30)
    )
network


# ------ Save Plot ------ 

showtext_opts(dpi = 320) 
ggsave("week_42.png", height = 10,
       width = 8, dpi=320)  
showtext_auto(FALSE)
