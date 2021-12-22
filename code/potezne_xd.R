setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("liczenie_slow.R")


df <- load_all_conversations()

get_potezne_xd <- function(df,sender,before = 5,after = 5){
  my <- make_my_df(df,sender)
  
  string_xd <- str_extract_all(my$content, "\\b[xX]+[dD]+\\b")
  xd_uni <- unique(na.omit(unlist(string_xd)))
  potezne_xd <- xd_uni[which.max(nchar(xd_uni))]
  one <- which(grepl(df$content,pattern = potezne_xd))
  two <- which(df$sender_name == sender)
  index <- one[one %in% two]
  wynik <- df[(index-10):(index+10),] %>% select(timestamp_ms,sender_name,content)
  wynik
}

moje_xd <- get_potezne_xd(df,"Damian Skowro\u00c5\u0084ski")


#jakos to sie zaprezentuje fajnie