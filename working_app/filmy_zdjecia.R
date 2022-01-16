setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("wczytanie_json.R")
library(reshape2)


df <- load_all_conversations()

photos_videos <- function(big_df){
  photos_nr <- big_df %>% filter(photos != "NULL") %>%  group_by(sender_name) %>% summarise(photo = n())
  videos_nr <- big_df %>% filter(videos != "NULL") %>% group_by(sender_name) %>% summarise(video = n())
  DF <- inner_join(photos_nr,videos_nr, by="sender_name") %>% arrange(-photo)
  DF
}



