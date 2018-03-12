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
une_text = UNE$comment_message %>% tolower %>% removePunctuation %>%
  removeWords(., stopwords("pt")) %>% removeNumbers
une_text = une_text %>% removeWords(., "abaafceaaacef")
une_text = as.data.frame(une_text, stringsAsFactors=F)

# Dá id único aos comentários
une_text %<>% mutate(comment_id = row_number())
head(une_text)

# Coloca cada termo em uma linha
une_text_unnested = une_text %>% tidytext::unnest_tokens("term", "une_text")
head(une_text_unnested)

# Fazendo merge com as polaridades
une_text_unnested = une_text_unnested %>%
  left_join(op30, by = "term") %>% 
  left_join(sent %>% select(term, lex_polarity = polarity), by = "term") %>% 
  select(comment_id, term, polarity, lex_polarity)

une_text_unnested[1:100,]

# Cria bd com os dois léxicos
une_class = une_text_unnested %>% 
  group_by(comment_id) %>% 
  summarise(
    comment_sentiment_op = mean(polarity, na.rm = T),
    comment_sentiment_lex = mean(lex_polarity, na.rm = T),
    n_words = n()
  )
  
une_class[1:50,]
# Plota
ggplot(na.omit(une_class %>% select(comment_id, comment_sentiment_op)), 
       aes(x=comment_id, y=comment_sentiment_op))+
  geom_col()+scale_y_continuous(limits = c())

# Verifica
une_class


# MBL
mbl_text = MBL$comment_message %>% tolower %>% removePunctuation %>%
  removeWords(., stopwords("pt"))
mbl_text = mbl_text %>% removeWords(., c("pra"))