### PEC 55 - Novas Análises
### Maria Alice e Neylson
### Script: Neylson
### Análise de sentimentos
###########################

source("01PEC55_classifica_posts.R")
library(descr)
library(tidytext)

# mbl_text_completo e une_text_completo

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

# Inclinação de sentimentos por assunto
une_class = une_unnested %>% group_by(une_subject) %>%
  summarise(
    sentiment_op30 = sum(polarity, na.rm=T),
    sentiment_lex  = sum(lex_polarity, na.rm=T),
    n_words = n()
  )

# Exibe os resultados
une_class
purrr::map_chr(une_class, class)

# Plota os resultados
ggplot(une_class, aes(x=une_subject, y=sentiment_op30, fill=sentiment_op30))+
  geom_col()+
  scale_y_continuous(limits = c(min(une_class$sentiment_op30), max(une_class$sentiment_op30)))+
  labs(x="Assunto", y="Inclinação dos comentários", title="Reações dos comentários aos posts da UNE - OPLexicon 3.0")

ggplot(une_class, aes(x=une_subject, y=sentiment_lex, fill=sentiment_lex))+
  geom_col()+
  scale_x_discrete()+
  labs(x="Assunto", y="Inclinação dos comentários", title="Reações dos comentários aos posts da UNE - SentiLex")





