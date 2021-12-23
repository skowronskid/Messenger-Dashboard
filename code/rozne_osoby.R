setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("wczytanie_json.R")


df <- load_all_conversations()

rozne_osoby <- function(big_df, receiver){
  #receiver - string bedacy sender_name jednego z nas
  df1 <- big_df %>% group_by(Date_Y_M_D) %>% filter(sender_name!=receiver) %>% summarise(il_osob = n_distinct(sender_name))
  df1
}

#1. opcja na przestrzeni lat ile osob danego dnia
#ggplot(rozne_osoby(df), aes(Date_Y_M_D, il_osob)) +
#  geom_line()

#2. topka dni
#ggplot(rozne_osoby(df) %>% arrange(-il_osob) %>%  head(10), aes(reorder(as.character(Date_Y_M_D), -il_osob), il_osob))+
#  geom_col() 
