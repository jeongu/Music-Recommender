# Import
setwd("C:/Users/user/Desktop/Statistics/Bigdata_analysis/Project")

library(data.table)
library(tidyverse)

Melon <- fread('bind_data_Melon/Melon.csv')[,-1]

# Preprocessing
Melon$flac[Melon$flac == ""] <- "No_Flac"
Melon$like <- gsub("[∞°-∆R]", "", Melon$like)
Melon$like <- gsub(",", "", Melon$like)
Melon$reply <- gsub("[∞°-∆R]", "", Melon$reply)
Melon$reply <- gsub(",", "", Melon$reply)
Melon <- Melon[!(Melon$lyric == "")]

Melon$artist <- as.factor(Melon$artist)
Melon$flac <- as.factor(Melon$flac)
Melon$like <- as.numeric(Melon$like)
Melon$reply <- as.numeric(Melon$reply)

# genre π¸¡÷ ¡Ÿ¿Ã±‚
genre_list <- data.frame(table(Melon$genre)[table(Melon$genre)>10])$Var1
genre_list <- genre_list[-c(14, 17, 21:27)]

Melon <- Melon %>% filter(genre %in% genre_list)

## genre ∫∞∑Œ µ•¿Ã≈Õ ªÏ∆Ï∫∏±‚
temp1 <- Melon %>% filter(genre == "New Age")
temp2 <- Melon %>% filter(genre == "Pop")
temp3 <- Melon %>% filter(genre == "Rock")
temp[24,]$lyric

CompressGenre <- function(x){
  y <- c()
  for(i in 1:length(x)){
    if(x[i] %in% c("Animation", "Game")) y <- c(y, "Animation / Game")
    else if(x[i] %in% c("Crossover", "Musical")) y <- c(y, "Crossover / Musical")
    else if(x[i] %in% c("Drama", "Korean Movie")) y <- c(y, "Drama / Korean Movie")
    else if(x[i] %in% c("Blues", "Jazz", "New Age")) y <- c(y, "Blues / Jazz / New Age")
    else if(x[i] %in% c("Electronica", "Rock", "Electronica,Rock")) y <- c(y, "Electronica / Rock")
    else y <- c(y, x[i])
  }
  return(y)
}

Melon$genre <- CompressGenre(Melon$genre)
Melon$genre <- as.factor(Melon$genre)
data.frame(table(Melon$genre))
pie(table(Melon$genre))
barplot(table(Melon$genre))

# ∞Ë¿˝, ø˘, ø¨µµ ∏∏µÈ±‚
Melon$Year <- substr(Melon$date, 1, 4)
Melon$Year <- as.factor(Melon$Year)

Melon$Mon <- substr(Melon$date, 6, 7)
Melon$Mon <- as.factor(Melon$Mon)

Mon2Season <- function(x){
  y <- c()
  for(i in 1:length(x)){
    if(x[i] %in% c("03", "04", "05")) y <- c(y, "spring")
    else if(x[i] %in% c("06", "07", "08")) y <- c(y, "summer")
    else if(x[i] %in% c("09", "10", "11")) y <- c(y, "fall")
    else y <- c(y, "winter")
  }
  return(y)
}

Melon$Season <- Mon2Season(Melon$Mon)
Melon$Season <- as.factor(Melon$Season)


# replyøÕ like¿« ªÛ∞¸∞¸∞Ë ªÏ∆Ï∫∏±‚
temp <- Melon %>% filter(complete.cases(Melon))
temp$title # º’¿∏∑Œ √§øÏ±‚
summary(Melon)
data.frame(table(Melon$genre))

barplot(Melon$reply)
barplot(Melon$like)

cor(temp$like, temp$reply)
plot(temp$like, temp$reply)

temp %>% 
  ggplot(aes(like, reply))+
  geom_point()+
  facet_wrap(~ genre)


# ¿€ªÁ, ¿€∞Ó, ∆Ì∞Ó
temp1 <- Melon %>% filter(lyricist == "")
temp2 <- Melon %>% filter(composer == "")
temp3 <- Melon %>% filter(arranger == "")
temp4 <- Melon %>% filter(lyricist == "")%>% filter(composer == "")%>% filter(arranger == "")

temp <- data.frame(table(Melon$artist))
temp2 <- temp[order(temp$Freq, decreasing = T),]


# « ø‰ æ¯¥¬ ∫Øºˆ ¡¶∞≈
temp <- Melon %>% select(-date, -Mon, -lyricist, -composer, -arranger)
write.csv(temp, "Melon_tidy.csv")


temp <- fread("word_count_temp.txt", header = F)
temp <- data.frame(t(temp))
temp <- temp[-38260,]
temp <- data.frame(cnt = temp)
barplot(temp)
temp_vec <- which((temp < 50), temp)

Melon_tidy <- fread('bind_data_Melon/Melon_tidy.csv')[,-1]

temp2 <- Melon_tidy[temp_vec,]

summary(Melon_tidy$like)
