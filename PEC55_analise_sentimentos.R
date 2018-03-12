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

# UNE
une_text = UNE$comment_message %>% tolower %>% removePunctuation %>%
  removeWords(., stopwords("pt")) %>% removeNumbers
une_text = une_text %>% removeWords(., "abaafceaaacef")



# MBL
mbl_text = MBL$comment_message %>% tolower %>% removePunctuation %>%
  removeWords(., stopwords("pt"))
mbl_text = mbl_text %>% removeWords(., c("pra"))