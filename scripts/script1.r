#Script1
#Modelagem de topicos
#Concordancias
#Contagem de tokens e types
#Visuzalizacao grafica

#Autora_Vivian Gomes Monteiro Souza
#Colaborador_Rodrigo Lima Lopes

#Carregando pacotes 

library(abjutils)
library(tidytext)
library(tidyverse)
library(stm)
library(tm)
library(ggridges)
library(formattable)
library(dbplyr)
library(ggplot2)
library(ggridges)

#Carregando dados

AC <- textreadr::read_document("Jornal A crítica Piloto_22.txt")

FBV <- textreadr::read_document("Jornal Folha Boa Vista Piloto_22 2.txt")

G1 <- textreadr::read_document("Jornal G1 Piloto_22.txt")

UOL <- textreadr::read_document("Jornal UOL Piloto_22.txt")

#Transformando os dados em data frame

AC.df <- data.frame(text = AC, stringsAsFactors = F)

AC.df$jornal <- "AC"

cont.AC <- 0
for(i in 1:length(AC.df$text)) {
  if( startsWith(AC.df[i,"text", drop=T],"Jornal") ) {
    cont.AC <- cont.AC + 1
  }
  AC.df$doc_id[i] <- paste(c("AC", cont.AC), collapse = ".")
}

AC.df <- aggregate(text ~ doc_id, AC.df, FUN = paste, collapse = ' ')

AC.df$jornal <- "AC"

AC.df$text <- tolower(AC.df$text)

#O looping foi criado para indicar quais trechos do texto pertenciam a mesma noticia. Esse processo foi feito para todos os jornais, pois essa marcacao estava feita apenas no texto corrido (.txt).

--------------------------------------
  
FBV.df <- data.frame(text = FBV, stringsAsFactors = F)

FBV.df$jornal <- "FBV"

cont.FBV <- 0
for(i in 1:length(FBV.df$text)) {
  if( startsWith(FBV.df[i,"text", drop=T],"Jornal") ) {
    cont.FBV <- cont.FBV + 1
  }
  FBV.df$doc_id[i] <- paste(c("FBV", cont.FBV), collapse = ".")
}

FBV.df <- aggregate(text ~ doc_id, FBV.df, FUN = paste, collapse = ' ')

FBV.df$jornal <- "FBV"

FBV.df$text <- tolower(FBV.df$text)

-------------------------------------
  
G1.df <- data.frame(text = G1, stringsAsFactors = F)

G1.df$jornal <- "G1"

cont.G1 <- 0
for(i in 1:length(G1.df$text)) {
  if( startsWith(G1.df[i,"text", drop=T],"Jornal") ) {
    cont.G1 <- cont.G1 + 1
  }
  G1.df$doc_id[i] <- paste(c("G1", cont.G1), collapse = ".")
}

G1.df <- aggregate(text ~ doc_id, G1.df, FUN = paste, collapse = ' ')

G1.df$jornal <- "G1"

G1.df$text <- tolower(G1.df$text)

------------------------------------
  
UOL.df <- data.frame(text = UOL, stringsAsFactors = F)

UOL.df$jornal <- "UOL"

cont.UOL <- 0
for(i in 1:length(UOL.df$text)) {
  if( startsWith(UOL.df[i,"text", drop=T],"Jornal") ) {
    cont.UOL <- cont.UOL + 1
  }
  UOL.df$doc_id[i] <- paste(c("UOL", cont.UOL), collapse = ".")
}

UOL.df <- aggregate(text ~ doc_id, UOL.df, FUN = paste, collapse = ' ')

UOL.df$jornal <- "UOL"

UOL.df$text <- tolower(UOL.df$text)

#Organizacao Dados

Jornais <- rbind(AC.df,FBV.df,G1.df, UOL.df)

metadata.jornais <- Jornais[ ,-3]

Jornais$text <- rm_accent(Jornais$text)

---------------------------------
#Carregando lista de stopwords 
  
lista.stopwords <- data.frame(
    readr::read_csv("Stopwordsportugues_22csv.csv", col_names = FALSE),stringsAsFactors = FALSE
)

colnames(lista.stopwords) <- "palavra"

lista.stopwords$palavra <- rm_accent(lista.stopwords$palavra)


#Criacao Corpus

AC.Corpus <- corpus(AC.df, text_field = 'text')
FBV.Corpus <- corpus(FBV.df, text_field = 'text')
G1.Corpus <- corpus(G1.df, text_field = 'text')
UOL.Corpus <- corpus(UOL.df, text_field = 'text')

noticias.df <- rbind(AC.df, FBV.df, G1.df, UOL.df)
noticias.corpus <- corpus(noticias.df, text_field = 'text')

#Criacao Tokens

AC.tokens  <- tokens(AC.Corpus,
                     what = "word",
                     remove_punct = TRUE,
                     remove_symbols = TRUE,
                     remove_numbers = TRUE,
                     remove_url = TRUE,
                     split_hyphens = FALSE,
                     include_docvars = TRUE,
                     padding = FALSE,
                     verbose = TRUE
)

FBV.tokens  <- tokens(FBV.Corpus,
                      what = "word",
                      remove_punct = TRUE,
                      remove_symbols = TRUE,
                      remove_numbers = TRUE,
                      remove_url = TRUE,
                      split_hyphens = FALSE,
                      include_docvars = TRUE,
                      padding = FALSE,
                      verbose = TRUE
)

G1.tokens  <- tokens(G1.Corpus,
                     what = "word",
                     remove_punct = TRUE,
                     remove_symbols = TRUE,
                     remove_numbers = TRUE,
                     remove_url = TRUE,
                     split_hyphens = FALSE,
                     include_docvars = TRUE,
                     padding = FALSE,
                     verbose = TRUE
)

UOL.tokens  <- tokens(UOL.Corpus,
                      what = "word",
                      remove_punct = TRUE,
                      remove_symbols = TRUE,
                      remove_numbers = TRUE,
                      remove_url = TRUE,
                      split_hyphens = FALSE,
                      include_docvars = TRUE,
                      padding = FALSE,
                      verbose = TRUE
)

noticias.tokens  <- tokens(noticias.corpus,
                           what = "word",
                           remove_punct = TRUE,
                           remove_symbols = TRUE,
                           remove_numbers = TRUE,
                           remove_url = TRUE,
                           split_hyphens = FALSE,
                           include_docvars = TRUE,
                           padding = FALSE,
                           verbose = TRUE
)

#Contagem de tokens e types

n.token.AC <- ntoken(AC.tokens)

n.token.FBV <- ntoken(FBV.tokens)  

n.token.G1 <- ntoken(G1.tokens) 

N.token.UOL <- ntoken(UOL.tokens)

N.token.not <- ntoken(noticias.tokens)

sum(n.token.FBV)

sum(n.token.AC) 

sum(n.token.G1) 

sum(N.token.UOL)

sum(N.token.not) 


n.type.AC <- ntype(AC.tokens)

n.type.FBV <- ntype(FBV.tokens)

n.type.G1 <- ntype(G1.tokens) 

n.type.UOL <- ntype(UOL.tokens) 

n.type.not <- ntype(noticias.tokens) 

sum(n.type.AC) 

sum(n.type.FBV) 

sum(n.type.G1) 

sum(n.type.UOL) 

sum(n.type.not) 

#Analise concordancia

 kwic(noticias.corpus, pattern = "venez*", 
       valuetype = "regex", window = 15) |> 
View()

#Analise por topico

proc <- 
  stm::textProcessor(Jornais$text, metadata = metadata.jornais,
                     language = "portuguese",
                     customstopwords = lista.stopwords$palavra,
                     stem = FALSE,
                     verbose = TRUE)
out <- 
  stm::prepDocuments(proc$documents,
                     proc$vocab, 
                     proc$meta,
                     lower.thresh = 3,
                     verbose = TRUE)
storage <-
  stm::searchK(out$documents,
               out$vocab, K = c(3:10),
               data = out$meta)

plot.searchK(storage)

fit <- stm::stm(documents = out$documents,
                vocab = out$vocab, data = out$meta,  K = 5,
                max.em.its = 75, init.type = "LDA",
                verbose = TRUE)

plot.STM(fit, type = "labels",labeltype = "prob")
plot.STM(fit, type = "labels",labeltype = "frex")
plot.STM(fit, type = "labels",labeltype = "lift")
plot.STM(fit, type = "labels",labeltype = "score")

plot.STM(fit, type = "perspectives",topics = c(2,3))

plot.STM(fit, type = "hist")

stm::labelTopics(fit)
View(fit$theta)

topicos <- c("Adaptação de migrantes e refugiados", "Ações governamentais", "Status legal de migrantes e refugiados", "Intervenção política", "Migração venezuelana")
  
jornais.topicos <- topicos[apply(fit$theta, 1, which.max)]

prob <- apply(fit$theta, 1, max)

df_topicos <- Jornais  |> 
  mutate(best_prob = prob,
         topico = jornais.topicos)

#Visualizacao grafica

colour <- "#717D7E"

df_topicos  |> 
  count(topico) |> 
  mutate(topico = forcats::fct_reorder(topico, n)) |> 
  ggplot(aes(x = topico, y = n)) +
  theme(text = element_text(size=25)) +
  geom_col(fill = colour) +
  #theme_minimal() + 
  labs(x = "Tópicos", y = "Quantificação de reportagens",
       title = NULL) +
  coord_flip()

df_topicos$jornal <- as.factor(df_topicos$jornal)
df_topicos$topico <- as.factor(df_topicos$topico)

cruzamento <- xtabs(~ jornal + topico, df_topicos)

xtabs(~ jornal + topico, df_topicos) |> 
  as.data.frame() |>
  ggplot(aes(jornal, topico)) + 
  theme(text = element_text(size=25)) +
  geom_tile(aes(fill = Freq)) +
  scale_fill_gradient(low="#CCD1D1", high="#717D7E")+
  geom_text(aes(label = round(Freq, 1)), size=7) +
  labs(x = "Jornal", y = "Tópicos",
       title = NULL)
