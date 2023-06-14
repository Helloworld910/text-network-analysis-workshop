#Importing Libraries
library(tidyverse)
library(udpipe)
library(ggtext)
library(magrittr)
library(igraph)
library(ggraph)

#Environment Setup
rm(list = ls())
setwd("e:/charles university/semantic network workshop/text-network-analysis-workshop")
getwd()
windowsFonts(georgia = windowsFont("georgia"))

#Uploading Data as Tibble
anno_df <- read_csv("data/annotated_rightwing.csv")

#Inspecting the Uploaded Data
names(anno_df)

#Extracting the Distribution of the Parts of Speeches
pos_dis <- anno_df %>%
  count(upos)

#Assigning Full Forms
abbr <- c("ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "INTJ", "NOUN", "NUM", "PART", "PRON", "PROPN", "PUNCT", "SCONJ", "SYM", "VERB", "X")
full <- c("Adjective", "Adposition", "Adverb", "Auxiliary", "Coordinating Conjunction", "Determiner", "Interjection", "Noun", "Numeral", "Particle", "Pronoun", "Proper Noun", "Punctuation", "Subordinating Conjunction", "Symbol", "Verb", "Other")
tib_full <- tibble(abbr, full)

#Joining Full Forms
pos_dis %<>%
  left_join(tib_full, by = c("upos"="abbr")) %>%
  select(upos,full,n)

#Plotting POS Distribution
pos_dis %>%
  ggplot() +
  geom_col(aes(x = n, y = reorder(full,n)), fill = "#262626") +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+
  labs(x = "Frequency", y = "Parts of Speech") +
  theme_classic() +
  theme(
    axis.text.x = element_text(family = "georgia", angle = 0, hjust = 1),
    axis.text.y = element_text(family = "georgia"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "#0D0D0D"),
    legend.position = "none",
    plot.title = element_textbox(face = "bold", family = "georgia", size = 23, hjust = 0.5),
    axis.title = element_textbox(face = "bold", family = "georgia", size = 11),
    axis.text = element_textbox(size = 13),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "#F0F0F2"),
    plot.background = element_rect(colour = "#0D0D0D", size = 1),
  ) +
  ggtitle("Distribution of Parts of Speech")


#Extracting 5 most Frequent Nouns
freq_nouns <- anno_df %>%
  filter(upos=="NOUN")%>%
  count(lemma, sort=TRUE)%>%
  slice(1:5)

#Displaying the most Frequent Nouns
freq_nouns


#Extracting 5 most Frequent Adjectives
freq_adj <- anno_df %>%
  filter(upos=="ADJ")%>%
  count(lemma, sort=TRUE)%>%
  slice(1:5)

#Displaying the most Frequent Adjectives
freq_adj

#Load Libraries
library(igraph)

#Create Co-occurrences Tibble
cooc <- cooccurrence(x = subset(anno_df, upos %in% c("NOUN", "ADJ", "VERB")), 
                     term = "lemma", 
                     group = c("doc_id", "paragraph_id", "sentence_id"))
cooc <- as_tibble(cooc)

#Extracte Semantic Network
sem_net <- graph_from_data_frame(cooc)

sem_net
vcount(sem_net)
rm(cooc)
gc()

#Trimming Semantic Network for Ego Network Extraction
sem_net_trimmed <- delete_edges(sem_net, E(sem_net)[cooc < 40])

#Extracting the Ego-Network of Immigrant
ego_network <- ego(sem_net_trimmed, node = "immigrant", order=1, mode="all")
ego_network <- ego_network[[1]]
induced_ego <- induced_subgraph(sem_net_trimmed, ego_network)


vcount(induced_ego)
ecount(induced_ego)

#Finding Communities in Ego Network
members <- cluster_walktrap(induced_ego)

#Assigning Communities
V(induced_ego)$community <- members$membership


#Drawing Ego Network with Communities
set.seed(358)
ggraph(induced_ego, layout="fr") +
  geom_edge_link2(aes(alpha = cooc,colour = as_factor(node.community)), arrow = arrow(length = unit(4, 'mm')), 
                  end_cap = circle(5, 'mm'), width = 2, show.legend = FALSE)+
  geom_node_text(aes(label = name), colour = "#0D0D0D", size = 5, family="georgia")+
  ggtitle("Degree 1 Ego Network of Immigrant") +
  theme(plot.title = element_textbox(face = "bold", color = "#0D0D0D", 
                                     family = "georgia", size = , hjust = 0.5),
        
        plot.background = element_rect(color = "#0D0D0D", size = 1),
        panel.background = element_rect(fill = "#F0F0F2"))

#Trimming Semantic Network for Ego Network Extraction
sem_net_trimmed <- delete_edges(sem_net, E(sem_net)[cooc < 7])

#Extracting the Ego-Network of Woman
ego_network <- ego(sem_net_trimmed, node = "woman", order=1, mode="all")
ego_network <- ego_network[[1]]
induced_ego <- induced_subgraph(sem_net_trimmed, ego_network)


vcount(induced_ego)
ecount(induced_ego)

#Finding Communities in Ego Network
members <- cluster_walktrap(induced_ego)

#Assigning Communities
V(induced_ego)$community <- members$membership


#Drawing Ego Network with Communities
set.seed(358)
ggraph(induced_ego, layout="fr") +
  geom_edge_link2(aes(alpha = cooc,colour = as_factor(node.community)), arrow = arrow(length = unit(4, 'mm')), 
                  end_cap = circle(5, 'mm'), width = 2, show.legend = FALSE)+
  geom_node_text(aes(label = name), colour = "#0D0D0D", size = 5, family="georgia")+
  ggtitle("Degree 1 Ego Network of Woman") +
  theme(plot.title = element_textbox(face = "bold", color = "#0D0D0D", 
                                     family = "georgia", size = , hjust = 0.5),
        
        plot.background = element_rect(color = "#0D0D0D", size = 1),
        panel.background = element_rect(fill = "#F0F0F2"))

