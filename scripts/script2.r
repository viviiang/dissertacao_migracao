#Script2
#Lematizacao
#Visuzalizacao grafica
#Rede de coocorrencias

#Autora_Vivian Gomes Monteiro Souza
#Colaborador_Rodrigo Lima Lopes

#Carregando pacotes

library(udpipe)
library(ggplot2)
library(data.table)
library(word2vec)
library(psych)
library(quanteda)
library(dplyr)
library(uwot)
library(ggrepel)
library(rvest)
library(purrr)
library(xml2) 
library(stringr)
library(abjutils)
library(tidytext)
library(tidyverse)
library(stm)
library(tm)
library(formattable)
library(ggridges)
library(ggraph)

udmodel <- udpipe_download_model(language = "portuguese")
udmodel <- udpipe_load_model(file = 'portuguese-bosque-ud-2.5-191206.udpipe')

#Carregando dados

AC <- textreadr::read_document("ACPiloto.txt")

FBV <- textreadr::read_document("FBVPiloto.txt")

G1 <- textreadr::read_document("G1Piloto.txt")

UOL <- textreadr::read_document("UOLPiloto.txt")

#Transformando os dados em data frame e listas

AC.df <- data.frame(text = AC, stringsAsFactors = F)

cont.AC <- 0
for(i in 1:length(AC.df$text)) {
  if( startsWith(AC.df[i,"text", drop=T],"Jornal") ) {
    cont.AC <- cont.AC + 1
  }
  AC.df$doc_id[i] <- paste(c("AC", cont.AC), collapse = ".")
}


AC.df <- aggregate(text ~ doc_id, AC.df, FUN = paste, collapse = ' ')

AC.df$jornal <- "AC"

AC.lista  <- data.frame(text = AC.df$text, 
                        stringsAsFactors = F) |>
  unnest_tokens(word, text) |>
  mutate(jornal = "AC")

AC.lista$word <- rm_accent(AC.lista$word)

#O looping foi criado para indicar quais trechos do texto pertenciam a mesma noticia. Es-se processo foi feito para todos os jornais, pois essa marcacao estava feita apenas no texto corrido (.txt).

--------------------------------------
  
FBV.df <- data.frame(text = FBV, stringsAsFactors = F)

cont.FBV <- 0
for(i in 1:length(FBV.df$text)) {
  if( startsWith(FBV.df[i,"text", drop=T],"Jornal") ) {
    cont.FBV <- cont.FBV + 1
  }
  FBV.df$doc_id[i] <- paste(c("FBV", cont.FBVFBV), collapse = ".")
}

FBV.df <- aggregate(text ~ doc_id, FBV.df, FUN = paste, collapse = ' ')

FBV.df$jornal <- "FBV"

FBV.lista  <- data.frame(text = FBV.df$text, 
                         stringsAsFactors = F) |>
  unnest_tokens(word, text) |>
  mutate(jornal = "FBV")

FBV.lista$word <- rm_accent(FBV.lista$word)

-------------------------------------
  
G1.df <- data.frame(text = G1, stringsAsFactors = F)

cont.G1 <- 0
for(i in 1:length(G1.df$text)) {
  if( startsWith(G1.df[i,"text", drop=T],"Jornal") ) {
    cont.G1 <- cont.G1 + 1
  }
  G1.df$doc_id[i] <- paste(c("G1", cont.G1), collapse = ".")
}

G1.df <- aggregate(text ~ doc_id, G1.df, FUN = paste, collapse = ' ')

G1.df$jornal <- "G1"

G1.lista  <- data.frame(text = G1.df$text, 
                        stringsAsFactors = F) |>
  unnest_tokens(word, text) |>
  mutate(jornal = "G1")

G1.lista$word <- rm_accent(G1.lista$word)

------------------------------------
  
UOL.df <- data.frame(text = UOL, stringsAsFactors = F)

cont.UOL <- 0
for(i in 1:length(UOL.df$text)) {
  if( startsWith(UOL.df[i,"text", drop=T],"Jornal") ) {
    cont.UOL <- cont.UOL + 1
  }
  UOL.df$doc_id[i] <- paste(c("UOL", cont.UOL), collapse = ".")
}

UOL.df <- aggregate(text ~ doc_id, UOL.df, FUN = paste, collapse = ' ')

UOL.df$jornal <- "UOL"

UOL.lista  <- data.frame(text = UOL.df$text, 
                         stringsAsFactors = F) |>
  unnest_tokens(word, text) |>
  mutate(jornal = "UOL")

UOL.lista$word <- rm_accent(UOL.lista$word)

---------------------------------

#Etiquetagem de arquivos 

AC.tag <- udpipe_annotate(object = udmodel,  
                          x = AC.df$text, 
                          doc_id = AC.df$doc_id) |>
  as.data.frame()

FBV.tag <- udpipe_annotate(object = udmodel,  
                           x = FBV.df$text, 
                           doc_id = FBV.df$doc_id) |>
  as.data.frame()

G1.tag <- udpipe_annotate(object = udmodel,  
                          x = G1.df$text, 
                          doc_id = G1.df$doc_id) |>
  as.data.frame()

UOL.tag <- udpipe_annotate(object = udmodel,  
                           x = UOL.df$text, 
                           doc_id = UOL.df$doc_id) |>
  as.data.frame()

jornais.tag <- udpipe_annotate(object = udmodel,  
                               x = jornais.df$text, 
                               doc_id = jornais.df$doc_id) |>
  as.data.frame()

jornais.tag <- jornais.tag %>%
  anti_join(lista.2.stopwords, by = c("lemma" = "palavra")) 

jornais.tag <- jornais.tag[-c(118636, 25854, 26006, 99202, 3400, 3568, 4289, 4530, ), ]


#Analise de co-ocorrencia de verbos

AC.CO.Verb <- cooccurrence(x = subset(AC.tag, upos %in% c("VERB", "VERB")), 
                            term = "lemma", 
                            group = c("doc_id", "paragraph_id", "sentence_id"))

FBV.CO.Verb <- cooccurrence(x = subset(FBV.tag, upos %in% c("VERB", "VERB")), 
                             term = "lemma", 
                             group = c("doc_id", "paragraph_id", "sentence_id"))

G1.CO.Verb <- cooccurrence(x = subset(G1.tag, upos %in% c("VERB", "VERB")), 
                            term = "lemma", 
                            group = c("doc_id", "paragraph_id", "sentence_id"))

UOL.CO.Verb <- cooccurrence(x = subset(UOL.tag, upos %in% c("VERB", "VERB")), 
                             term = "lemma", 
                             group = c("doc_id", "paragraph_id", "sentence_id"))

jornais.CO.Verb <- cooccurrence(x = subset(jornais.tag, upos %in% c("VERB", "VERB")), 
                            term = "lemma", 
                            group = c("doc_id", "paragraph_id", "sentence_id"))


wordnetwork <- head(jornais.CO.Verb, 150)

ggraph(wordnetwork,  layout = 'kk') +
  geom_edge_arc(color="gray", curvature=0.3) +            
  geom_node_point(color="#de4e96", aes(size= igraph::degree(igraph::graph_from_data_frame(wordnetwork)))) +
  scale_size(range = c(2, 8)) +
  geom_node_text(aes(label = name), size=4, color="black", repel=T) +
  theme_void()

