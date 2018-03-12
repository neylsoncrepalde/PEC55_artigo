### PEC 55 - Novas Análises
### Maria Alice e Neylson
### Script: Neylson
###########################

library(readr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tm)
library(wordcloud)
library(lexiconPT)
library(magrittr)

# Verifica os dados nas pastas
dir(paste0(getwd(),"/UNE"))
dir(paste0(getwd(),"/MBL"))

# Lê os dados
UNE <- read_tsv(paste0(getwd(),"/UNE/page_241149405912525_2017_02_16_01_53_23_comments.tab"))
MBL <- read_tsv(paste0(getwd(),"/MBL/page_204223673035117_2017_02_17_15_59_08_topcomments.tab"))

# Separa no banco UNE apenas as colunas comuns com a MBL
UNE <- UNE %>% select(names(MBL))

#------------------------------------------------------
# Análise de sentimentos dos comentários
data("oplexicon_v3.0")
data("sentiLex_lem_PT02")

op30 <- oplexicon_v3.0
sent <- sentiLex_lem_PT02
rm(oplexicon_v3.0, sentiLex_lem_PT02)


# UNE
# Limpa
une_text = UNE %>% select(post_id, comment_message)
une_text$comment_message = une_text$comment_message %>% tolower %>% removePunctuation %>%
  removeWords(., stopwords("pt")) %>% removeNumbers
une_text$comment_message = une_text$comment_message %>% removeWords(., "abaafceaaacef")

# Dá id único aos comentários
une_text %<>% mutate(comment_id = row_number())
head(une_text)

# Coloca cada termo em uma linha
une_text_unnested = une_text %>% tidytext::unnest_tokens("term", "comment_message")
head(une_text_unnested)

# Fazendo merge com as polaridades
une_text_unnested = une_text_unnested %>%
  left_join(op30, by = "term") %>% 
  left_join(sent %>% select(term, lex_polarity = polarity), by = "term") %>% 
  select(post_id, comment_id, term, polarity, lex_polarity)

une_text_unnested[1:100,]

# Cria bd com os dois léxicos
une_class = une_text_unnested %>% 
  group_by(post_id) %>% 
  summarise(
    comment_sentiment_op = sum(polarity, na.rm = T),
    comment_sentiment_lex = sum(lex_polarity, na.rm = T),
    n_words = n()
  )
  
une_class[1:50,]
# Plota OPLexicon
ggplot(na.omit(une_class %>% select(post_id, comment_sentiment_op)), 
       aes(x=post_id, y=comment_sentiment_op))+
  geom_col()+scale_y_continuous(limits = c())+
  scale_x_discrete(labels = NULL)+
  labs(x="Post", y="Inclinação dos comentários", title="Reações dos comentários aos posts da UNE - OPLexicon 3.0")

# Plota Sentilex
ggplot(na.omit(une_class %>% select(post_id, comment_sentiment_lex)), 
       aes(x=post_id, y=comment_sentiment_lex))+
  geom_col()+scale_y_continuous(limits = c())+
  scale_x_discrete(labels = NULL)+
  labs(x="Post", y="Reação dos comentários", title="Reações dos comentários aos posts da UNE - SentiLex")



# MBL
mbl_text = MBL %>% select(post_id, comment_message)
mbl_text$comment_message = mbl_text$comment_message %>% tolower %>% removePunctuation %>%
  removeWords(., stopwords("pt")) %>% removeNumbers
mbl_text$comment_message = mbl_text$comment_message %>% removeWords(., "pra")

# Dá id único aos comentários
mbl_text %<>% mutate(comment_id = row_number())
head(mbl_text)

# Coloca cada termo em uma linha
mbl_text_unnested = mbl_text %>% tidytext::unnest_tokens("term", "comment_message")
head(mbl_text_unnested)

# Fazendo merge com as polaridades
mbl_text_unnested = mbl_text_unnested %>%
  left_join(op30, by = "term") %>% 
  left_join(sent %>% select(term, lex_polarity = polarity), by = "term") %>% 
  select(post_id, comment_id, term, polarity, lex_polarity)

mbl_text_unnested[1:100,]

# Cria bd com os dois léxicos
mbl_class = mbl_text_unnested %>% 
  group_by(post_id) %>% 
  summarise(
    comment_sentiment_op = sum(polarity, na.rm = T),
    comment_sentiment_lex = sum(lex_polarity, na.rm = T),
    n_words = n()
  )

mbl_class[1:50,]
# Plota OPLexicon
ggplot(na.omit(mbl_class %>% select(post_id, comment_sentiment_op)), 
       aes(x=post_id, y=comment_sentiment_op))+
  geom_col()+scale_y_continuous(limits = c())+
  scale_x_discrete(labels = NULL)+
  labs(x="Post", y="Inclinação dos comentários", title="Reações dos comentários aos posts da MBL - OPLexicon 3.0")

# Plota Sentilex
ggplot(na.omit(mbl_class %>% select(post_id, comment_sentiment_lex)), 
       aes(x=post_id, y=comment_sentiment_lex))+
  geom_col()+scale_y_continuous(limits = c())+
  scale_x_discrete(labels = NULL)+
  labs(x="Post", y="Reação dos comentários", title="Reações dos comentários aos posts da MBL - SentiLex")

