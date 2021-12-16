#TODO ogarn¹æ, ¿eby polskie znaki dzia³a³y...., jak jakos sie nie uda encodingiem to pewnie mozna robic str_replace_all i bêdzie git  


# //www.r-bloggers.com/2018/06/working-with-your-facebook-data-in-r/e  
# ^^^^^^^^^^^^^  ten link mocno pomogl ogarnac co i jak 

#nie jestem pewien, które z nich na pewno sa potrzebne, wiec zostawiam wszystko na razie
library(purrr)
library(tidyverse)
library(jsonlite)
library(anytime)
library(dplyr)
library(stringr)


#¿eby znormalizowaæ dzia³anie zak³adam, ¿e masz ten skrypt w folderze z rozpakowanym zipem od fb
#zeby dzia³a³o wszystko fajnie bez podawania dlugich sciezek ustawiam workind directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

current_path <- str_replace_all(getwd(), pattern = "[/]", replacement = "\\\\")



## PRZYK£ADY UZYCIA


#weŸmy, ¿e chcemy 
path <- paste0(current_path,"\\messages\\inbox\\danonki_pdzoaidjcq") #tutaj wiadomo ta ostatnia czêœæ to jak siê u ciebie nazywa ten folder akurat
files <- dir(path, pattern = "*.json")


rm(mess_df)
for (file in files) {
  text <- paste0(path, "\\", file)
  full <-   fromJSON(txt = text)
  if(!exists("mess_df")){
    mess_df <-  full$messages
  } else {
    mess_df <- bind_rows(mess_df,full$messages)
  }
}


mess_df$timestamp_ms <- anytime(mess_df$timestamp_ms/1000) #ogarniêcie daty

mess_df %>% group_by(sender_name) %>% count() #po prostu ilosci wiadomosci na szybko





# teraz dla wszystkich konwersacji - wszystkich folderów w inbox
path <-paste0(current_path,"\\messages\\inbox\\")
dirs <-  dir(path)


rm(mess_df)
for(d in dirs){
  dir_path <-  paste0(path, d)
  print(dir_path)
  files <- dir(dir_path, pattern = "*.json")
  print(files)
  for (file in files) {
    text <- paste0(dir_path, "\\", file)
    full <-   fromJSON(txt = text)
    if(!exists("mess_df")){
      mess_df <-  full$messages
    } else {
      mess_df <- bind_rows(mess_df,full$messages)
    }
  }
}
#moze chwile potrwac bo troche tego jest, ale dzia³a na pewno

temp <- mess_df

mess_df$timestamp_ms <- anytime(mess_df$timestamp_ms/1000)

#³¹czna iloœæ wiadomoœci
all_count <- mess_df %>% group_by(sender_name) %>% count() %>% arrange(desc(n))


#wyszukiwanie ile razy ³¹cznie siê powiedzia³o jakieœ s³owo
my <- temp %>% filter(sender_name == "Damian Skowro\u00c5\u0084ski") 
slowo_count <- my$content %>% str_count(pattern = "bruh") %>% na.omit() %>% sum() #mam 684



  

