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
une_text = UNE %>% select(post_id, post_text, comment_message)
une_posts_assunto = unique(une_text$post_text)
une_posts_assunto = as.data.frame(une_posts_assunto, stringsAsFactors=F)

une_subject = c("teatro","heróis","bienal da UNE", "ocupacao", "artes visuais", "bienal da UNE", "bienal da UNE", "bienal da UNE", "bienal da UNE", "heróis",
                "heróis", "detidos", "PEC55", "PEC55", "PEC55", "PEC55", "MP Ens Med", "presidente", "ocupacao", "foto",
                "PEC55", "ocupacao", "PEC55", "ocupacao", "PEC55", "ocupacao", "brasilia", "ocupacao", "PEC55", "PEC55",
                "PEC55", "PEC55", "ocupacao", "ocupacao", "heróis", "PEC55", "ocupacao", "PEC55", "ocupacao", "ocupacao",
                "ocupacao", "ocupacao", "ocupacao", "debate", "ocupacao", "ocupacao", "ocupacao", "ocupacao", "ocupacao", "ocupacao",
                "ocupacao", "ocupacao", "ocupacao", "ocupacao", "ocupacao", "ocupacao", "bienal da UNE", "testemunho", "ocupacao", "ocupacao",
                "ocupacao", "ocupacao", "PEC55", "ocupacao", "ocupacao", "PEC55", "ocupacao")
une_posts_assunto = cbind(une_posts_assunto, une_subject)
names(une_posts_assunto)[1] = "post_text"

une_text_completo = left_join(une_text, une_posts_assunto)

une_text_completo

# Segue para as análises #################


# MBL
mbl_text = MBL %>% select(post_id, post_text, comment_message)
mbl_posts_assunto = unique(mbl_text$post_text)
mbl_posts_assunto = as.data.frame(mbl_posts_assunto, stringsAsFactors=F)

mbl_subject = c("alvaro dias", "jean wyllys", "lula", "kim kata", "corrupcao", "alvaro dias", "abuso autoridade", "jean wyllys", "PT", NA,
                "pavinatto", "gleisi hoff", "heróis", "esquerda", "esquerda", "politicos esquerda", "renan calheiros", "carta capital", "jose medeiros", "politicos esquerda",
                "renan calheiros", "reforma previdencia", "roberto justus", "sergio moro", "politicos esquerda", "corrupcao", "fernando holiday", "esquerda", "vergonha", "ciro gomes",
                "pavinatto", "renan calheiros", "renan calheiros", "PT", "coletiva", "renan calheiros", "renan calheiros", "pavinatto", "kim kata", "pavinatto",
                "magno malta", "magno malta", "renan calheiros", "super salarios", "cobertura", "danilo gentili", "renan calheiros", "lula", "gleisi hoff", "manifestacao",
                "renan calheiros", "cobertura", "cobertura", "cobertura", "cobertura", "cobertura", "kim kata", "cobertura", "cobertura", "cobertura",
                "cobertura", "cobertura", "cobertura", "danilo gentili", "esquerda", "manifestacao", "lindbergh", "renan calheiros", "renan calheiros", "ocupacoes",
                "sergio moro", "PEC55", "renan calheiros", "abuso autoridade", "fernando holiday", "pavinatto", "camara deputados", "manifestacao", "lava jato", "manifestacao",
                "manifestacao", "imposto sindical", "foro privilegiado", "foro privilegiado", "PEC55", "medidas corrupcao", "jean wyllys", "esquerda", "esquerda", "PT",
                "esquerda", "fascistas", "jean wyllys", "medidas corrupcao", "medidas corrupcao", "esquerda", "esquerda", "esquerda", "globo", "heróis",
                "jose serra", "esquerda", "desigualdade", "pavinatto", "fidel castro", "fidel castro", "fidel castro", "manifestacao", "abuso autoridade", "live",
                "ocupacoes", "ocupacoes", "pavinatto", "manifestacao", "lula", "medidas corrupcao", "sergio moro", "sostenes cavalcante", "danilo gentili", "globo",
                "foto", "medidas corrupcao", "onyx lorenzoni", "medidas corrupcao", "luiz carlos santos", "lula", "pavinatto", "corrupcao", "armas", "congresso MBL",
                "congresso MBL", "congresso MBL", "lula", "esquerda")
mbl_posts_assunto = cbind(mbl_posts_assunto, mbl_subject)
names(mbl_posts_assunto)[1] = "post_text"

mbl_text_completo = left_join(mbl_text, mbl_posts_assunto)

mbl_text_completo

#############################

# removendo supérfluos
rm(mbl_posts_assunto, mbl_text, une_posts_assunto, une_text)
