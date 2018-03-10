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

# Verifica os dados nas pastas
dir(paste0(getwd(),"/UNE"))
dir(paste0(getwd(),"/MBL"))

# Lê os dados
UNE <- read_tsv(paste0(getwd(),"/UNE/page_241149405912525_2017_02_16_01_53_23_comments.tab"))
MBL <- read_tsv(paste0(getwd(),"/MBL/page_204223673035117_2017_02_17_15_59_08_topcomments.tab"))

# Separa no banco UNE apenas as colunas comuns com a MBL
UNE <- UNE %>% select(names(MBL))

#----------------------------------------------------
### COMENTARIOS

# Contando os comentários
length(unique(UNE$comment_id))
length(unique(MBL$comment_id))

une_comments <- round_date(unique(UNE$comment_published), "day")
une_comment_dates = as.data.frame(table(une_comments), stringsAsFactors = F)
ggplot(une_comment_dates, aes(x=as_date(une_comments), y=Freq))+geom_path(lwd=1)+
  labs(x='', y='', title='Comentários na página UNE por dia')

mbl_comments <- round_date(unique(MBL$comment_published), "day")
mbl_comment_dates = as.data.frame(table(mbl_comments), stringsAsFactors = F)
ggplot(mbl_comment_dates, aes(x=as_date(mbl_comments), y=Freq))+geom_path(lwd=1)+
  labs(x='', y='', title='Principais comentários na página MBL por dia')

#-----------------------------------
# Análise textual

# UNE
une_text = UNE$comment_message %>% tolower %>% removePunctuation %>%
  removeWords(., stopwords("pt")) %>% removeNumbers
une_text = une_text %>% removeWords(., "abaafceaaacef")
pal <- brewer.pal(9, "RdBu")[1:4]
wordcloud(enc2native(une_text), min.freq = 5, max.words = 100, 
          random.order = F, colors = pal)

corpus <- Corpus(VectorSource(une_text))
tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, sparse = 0.98)
df <- as.data.frame(as.matrix(tdm))
#dim(df)
df.scale <- scale(df)
d <- dist(df.scale, method = "euclidean")
fit.ward2 <- hclust(d, method = "ward.D2")
plot(fit.ward2)

# MBL
mbl_text = MBL$comment_message %>% tolower %>% removePunctuation %>%
  removeWords(., stopwords("pt"))
mbl_text = mbl_text %>% removeWords(., c("pra"))

wordcloud(enc2native(mbl_text), min.freq = 2, max.words = 100, 
          random.order = F, colors = pal)

corpus <- Corpus(VectorSource(mbl_text))
tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, sparse = 0.98)
df <- as.data.frame(as.matrix(tdm))
#dim(df)
df.scale <- scale(df)
d <- dist(df.scale, method = "euclidean")
fit.ward2 <- hclust(d, method = "ward.D2")
plot(fit.ward2)





