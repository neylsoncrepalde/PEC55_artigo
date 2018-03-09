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

#--------------------------------------------------
### POSTAGENS
# Contando as postagens

length(unique(UNE$post_id))
length(unique(MBL$post_id))

# Fluxo das postagens
une_posts <- round_date(unique(UNE$post_published), "day")
une_posts_dates = as.data.frame(table(une_posts), stringsAsFactors = F)
ggplot(une_posts_dates, aes(x=as_date(une_posts), y=Freq))+geom_path(lwd=1)+
  labs(x='', y='', title='Postagens da página UNE por dia')+
  scale_y_continuous(breaks = 0:max(une_posts_dates$Freq))

mbl_posts <- round_date(unique(MBL$post_published), "day")
mbl_posts_dates = as.data.frame(table(mbl_posts), stringsAsFactors = F)
ggplot(mbl_posts_dates, aes(x=as_date(mbl_posts), y=Freq))+geom_path(lwd=1)+
  labs(x='', y='', title='Postagens da página MBL por dia')+
  scale_y_continuous(breaks = 0:max(mbl_posts_dates$Freq))

#----------------------------------------------
# Análise textual

# UNE
une_text = UNE$post_text %>% tolower %>% removePunctuation %>%
  removeWords(., stopwords("pt"))
pal <- brewer.pal(9, "RdBu")[1:4]
wordcloud(enc2native(une_text), min.freq = 2, max.words = 100, 
          random.order = F, colors = pal)

corpus <- Corpus(VectorSource(une_text))
tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, sparse = 0.87)
df <- as.data.frame(as.matrix(tdm))
#dim(df)
df.scale <- scale(df)
d <- dist(df.scale, method = "euclidean")
fit.ward2 <- hclust(d, method = "ward.D2")
plot(fit.ward2)

# MBL
mbl_text = MBL$post_text %>% tolower %>% removePunctuation %>%
  removeWords(., stopwords("pt"))
wordcloud(enc2native(mbl_text), min.freq = 2, max.words = 100, 
          random.order = F, colors = pal)

corpus <- Corpus(VectorSource(mbl_text))
tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, sparse = 0.95)
df <- as.data.frame(as.matrix(tdm))
#dim(df)
df.scale <- scale(df)
d <- dist(df.scale, method = "euclidean")
fit.ward2 <- hclust(d, method = "ward.D2")
plot(fit.ward2)
