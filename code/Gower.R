
library(data.table)
library(dplyr)
library(cluster)
library(plyr)

# Top 10 추출
setwd("C:/Users/user/Desktop/Statistics/Bigdata_analysis/Project/bind_data_melon/list_up")
list <- list.files()

## 80개의 노래들 각각 리스트업
data <- lapply(list, fread)
num <- c()
for(i in 1:80){
  num <- c(num, str_sub(list[i], 1, length(list[i])-6))
}

data2 <- list()
for(i in 1:80){
  temp <- data[[i]]
  temp <- temp[,-1]
  
  ID <- temp$ID[order(temp$Similarity, decreasing = T)][2:11]
  Similarity <- sort(temp$Similarity, decreasing = T)[2:11]
  data2[[i]] <- data.frame(ID = ID, LS = Similarity)
}

## 80개의 노래들 각각 Top 10 매칭
setwd('C:/Users/user/Desktop/Statistics/Bigdata_analysis/Project/bind_data_melon')
Melon2_df <- fread('Melon2_df.csv')[,-1]

data3 <- list()
for(i in 1:80){
  temp <- data2[[i]]
  data3[[i]] <- Melon2_df %>% filter(Melon2_df$ID %in% temp$ID) %>% select(-Cnt, -Title, -Album, -Lyric)
}

## Gower Distance
library(stringr)
str(Melon2_df)
str(data3[[1]])

data4 <- list()
for(i in 1:80){
  temp2 <- Melon2_df %>% filter(ID == num[i]) %>% select(-Cnt, -Title, -Album, -Lyric)
  
  temp <- data3[[i]]
  temp <- rbind(temp2, temp)
  temp$Artist <- as.factor(temp$Artist)
  temp$Genre <- as.factor(temp$Genre)
  temp$Flac <- as.factor(temp$Flac)
  temp$Year <- as.factor(temp$Year)
  temp$Season <- as.factor(temp$Season)
  
  temp3 <- as.matrix(daisy(temp[,-1] , metric = 'gower'))
  ID <- temp$ID[-1]
  Gower_Similarity <- c(temp3[c(2:11),1])
  data4[[i]] <- data.frame(ID=ID, GS=Gower_Similarity)
  names(data4[i]) <- num[i]
}


## Number of Play
temp <- Melon2_df %>% filter(ID %in% num)
vec <- temp$Cnt
P_Score <- temp$Cnt/sum(vec)
M_Score <- (vec-min(vec))/(max(vec)-min(vec))
id <- temp$ID
data5 <- data.frame(ID=id, P_Score=P_Score, M_Score=M_Score)

## 평가 지표 합치기
data6 <- list()
for(i in 1:80){
  temp <- left_join(data2[[i]], data4[[i]])
  data6[[i]] <- cbind(temp, P_Score=data5$P_Score[i], M_Score=data5$M_Score[i])
}
data6_df <- ldply(data6)
length(unique(data6_df$ID))

str(data6_df)
data6_df$Result.P <- data6_df$LS*data6_df$GS*data6_df$P_Score
data6_df$Result.M <- data6_df$LS*data6_df$GS*data6_df$M_Score

tempP <- data6_df[order(data6_df$Result.P, decreasing = T),][c(1:20),] %>% select(ID, Result.P)
tempM <- data6_df[order(data6_df$Result.M, decreasing = T),][c(1:20),] %>% select(ID, Result.M)

tempP2 <- Melon2_df %>% filter(ID %in% tempP$ID) %>% select(ID, Title, Artist)
tempM2 <- Melon2_df %>% filter(ID %in% tempM$ID) %>% select(ID, Title, Artist)

for(i in 1:20){
  temp <- tempP2 %>% filter(ID == tempP$ID[i])
  tempP2 <- rbind(tempP2, temp)
}
tempP2 <- tempP2[-c(1:20),]

for(i in 1:20){
  temp <- tempM2 %>% filter(ID == tempM$ID[i])
  tempM2 <- rbind(tempM2, temp)
}
tempM2 <- tempM2[-c(1:20),]


## Cnt를 숫자 그대로 넣어보기
data7$Cnt = data7$P_Score*sum(vec)
data7$Result.C <- data7$LS*data7$GS*data7$Cnt
tempC <- data7[order(data7$Result.C, decreasing = T),][c(1:20),] %>% select(ID, Result.C)
tempC2 <- Melon2_df %>% filter(ID %in% tempC$ID) %>% select(ID, Title, Artist)
for(i in 1:20){
  temp <- tempC2 %>% filter(ID == tempC$ID[i])
  tempC2 <- rbind(tempC2, temp)
}
tempC2 <- tempC2[-c(1:20),]

## Like MinMax Score
temp <- Melon2_df %>% filter(ID %in% data6_df$ID) %>% select(ID, Like)
vec <- temp$Like
temp$LM_Score <- (vec-min(vec))/(max(vec)-min(vec))

data7 <- inner_join(data6_df, temp)
summary(data7)

tempP3 <- data7[order(data7$Result.P, decreasing = T),][c(1:20),] %>% select(ID, Result.P)
tempM3 <- data7[order(data7$Result.M, decreasing = T),][c(1:20),] %>% select(ID, Result.M)

tempP4 <- Melon2_df %>% filter(ID %in% tempP3$ID) %>% select(ID, Title, Artist)
tempM4 <- Melon2_df %>% filter(ID %in% tempM3$ID) %>% select(ID, Title, Artist)

for(i in 1:20){
  temp <- tempP4 %>% filter(ID == tempP3$ID[i])
  tempP4 <- rbind(tempP4, temp)
}
tempP4 <- tempP4[-c(1:20),]

for(i in 1:20){
  temp <- tempM4 %>% filter(ID == tempM3$ID[i])
  tempM4 <- rbind(tempM4, temp)
}
tempM4 <- tempM4[-c(1:20),]

## 재생횟수 제거
data7$Result.L <- data7$LS*data7$GS*data7$LM_Score

tempL <- data7[order(data7$Result.L, decreasing = T),][c(1:20),] %>% select(ID, Result.L)
tempL2 <- Melon2_df %>% filter(ID %in% tempL$ID) %>% select(ID, Title, Artist)
for(i in 1:20){
  temp <- tempL2 %>% filter(ID == tempL$ID[i])
  tempL2 <- rbind(tempL2, temp)
}
tempL2 <- tempL2[-c(1:20),]
  

temp <- Melon2_df %>% filter(ID %in% num) %>% select(ID, Cnt)
hist(temp$Cnt, breaks = 30)
range(temp$Cnt)

Melon2_df %>% filter(ID == '22763')
which(list == '2549')

