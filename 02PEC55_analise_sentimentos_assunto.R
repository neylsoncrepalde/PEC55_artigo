### PEC 55 - Novas Análises
### Maria Alice e Neylson
### Script: Neylson
### Análise de sentimentos
###########################

source("01PEC55_classifica_posts.R")
library(descr)
library(tidytext)

# mbl_text_completo e une_text_completo

#---------------------------------------
### UNE

#########################
# Fazendo o merge das classificações
une_text_completo$comment_treated = une_text_completo$comment_message %>% 
  tolower %>% removePunctuation %>% removeWords(., stopwords("pt")) %>% 
  removeNumbers
une_text_completo$comment_treated = une_text_completo$comment_treated %>% 
  removeWords(., "abaafceaaacef")

# Cria id único para cada comentário
une_text_completo %<>% mutate(comment_id = row_number())

# Coloca cada termo em uma linha
une_unnested = une_text_completo %>% unnest_tokens("term", "comment_message")

# Faz o merge com as classificações dos léxicos
une_unnested = une_unnested %>%
  left_join(op30, by = "term") %>% 
  left_join(sent %>% select(term, lex_polarity = polarity), by = "term")

une_unnested %<>% select(-polarity_revision, -type)
une_unnested %>% select(term, polarity, lex_polarity)

###########################################################
# investigando a inclinação dos comentários por assunto de postagem

# Tabela dos assuntos
freq(une_text_completo$une_subject, plot=T)
library(forcats)
ggplot(une_text_completo, aes(fct_rev(fct_infreq(une_subject))))+geom_bar()+coord_flip()+
  labs(x="Assuntos", y="Frequência", title="Comentários por assunto - UNE")

# Inclinação de sentimentos por assunto
une_class = une_unnested %>% group_by(une_subject) %>%
  summarise(
    sentiment_op30 = sum(polarity, na.rm=T),
    sentiment_lex  = sum(lex_polarity, na.rm=T),
    n_words = n()
  )

# Exibe os resultados
une_class
sapply(une_class, class)

# Plota os resultados
ggplot(une_class, aes(x=une_subject, y=sentiment_op30))+
  geom_col()+
  scale_y_continuous(limits = c(min(une_class$sentiment_op30), max(une_class$sentiment_op30)))+
  coord_flip()+
  labs(x="Assunto", y="Inclinação dos comentários", title="Inclinação dos comentários - UNE - OPLexicon 3.0")

ggplot(une_class, aes(x=une_subject, y=sentiment_lex))+
  geom_col()+
  scale_x_discrete()+
  coord_flip()+
  labs(x="Assunto", y="Inclinação dos comentários", title="Inclinação dos comentários - UNE - SentiLex")

#--------------------------------------------------
### MBL

mbl_text_completo$comment_treated = mbl_text_completo$comment_message %>% 
  tolower %>% removePunctuation %>% removeWords(., stopwords("pt")) %>% 
  removeNumbers
mbl_text_completo$comment_treated = mbl_text_completo$comment_treated %>% 
  removeWords(., "pra")



# Cria id único para cada comentário
mbl_text_completo %<>% mutate(comment_id = row_number())

# Coloca cada termo em uma linha
mbl_unnested = mbl_text_completo %>% unnest_tokens("term", "comment_message")

# Faz o merge com as classificações dos léxicos
mbl_unnested = mbl_unnested %>%
  left_join(op30, by = "term") %>% 
  left_join(sent %>% select(term, lex_polarity = polarity), by = "term")

mbl_unnested %<>% select(-polarity_revision, -type)
mbl_unnested %>% select(term, polarity, lex_polarity)

###########################################################
# investigando a inclinação dos comentários por assunto de postagem

# Tabela dos assuntos
freq(mbl_text_completo$mbl_subject, plot=T)
ggplot(na.omit(mbl_text_completo), aes(fct_rev(fct_infreq(mbl_subject))))+geom_bar()+coord_flip()+
  labs(x="Assuntos", y="Frequência", title="Comentários por assunto - MBL")

# Inclinação de sentimentos por assunto
mbl_class = mbl_unnested %>% group_by(mbl_subject) %>%
  summarise(
    sentiment_op30 = sum(polarity, na.rm=T),
    sentiment_lex  = sum(lex_polarity, na.rm=T),
    n_words = n()
  )

# Exibe os resultados
mbl_class
sapply(mbl_class, class)

# Plota os resultados
ggplot(na.omit(mbl_class), aes(x=mbl_subject, y=sentiment_op30))+
  geom_col()+
  scale_y_continuous(limits = c(min(mbl_class$sentiment_op30), max(mbl_class$sentiment_op30)))+
  coord_flip()+
  labs(x="Assunto", y="Inclinação dos comentários", title="Inclinação dos comentários - MBL - OPLexicon 3.0")

ggplot(na.omit(mbl_class), aes(x=mbl_subject, y=sentiment_lex))+
  geom_col()+
  scale_x_discrete()+
  coord_flip()+
  labs(x="Assunto", y="Inclinação dos comentários", title="Inclinação dos comentários - MBL - SentiLex")
