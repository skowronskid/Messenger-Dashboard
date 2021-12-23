source("wczytanie_json.R")
library(reshape2)
library(ggplot2)
library(dplyr)

df <- load_all_conversations()

photos_videos <- function(big_df){
  photos_nr <- big_df %>% filter(photos != "NULL") %>%  group_by(sender_name) %>% summarise(photo = n())
  videos_nr <- big_df %>% filter(videos != "NULL") %>% group_by(sender_name) %>% summarise(video = n())
  DF <- inner_join(photos_nr,videos_nr, by="sender_name") %>% arrange(-photo)
  DF
}

#przykladowy wykres
#df1 <- melt(photos_videos(df) %>% head(10), id.vars="sender_name")
#ggplot(df1, aes(x=sender_name, y=value, fill=variable)) +
#  geom_bar(stat='identity', position='dodge')+
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

