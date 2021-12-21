setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("wczytanie_json.R")

mess_df <- load_all_conversations(show_progress = T)

make_my_df <- function(big_df,sender){
  # tylko swoje wiadomosci
  my <- big_df %>% filter(sender_name == sender) 
  my
}

my <- make_my_df(mess_df,"Damian Skowro\u00c5\u0084ski") #robie dla siebie

get_nr_of_messages <- function(df){
  #zwraca ilosc wiadomosci w zaleznosci od osoby w formie ramki danych
  all_count <- df %>% group_by(sender_name) %>% count() %>% arrange(desc(n))
  as.data.frame(all_count)
}

#przyklady uzycia
#get_nr_of_messages(mess_df)
#get_nr_of_messages(load_one_conversation(dir_name = "danonki_pdzoaidjcq"))

 

substring_count <- function(my_df, slowa){
  #zaznaczam, że tu dzieje nie szukamy slowa w calosci tylko zaliczy tez czesc slowa
  # na przyklad jesli szukam xd to znajde tez xd w xdd, czaisz
  # ze substring tez policzy
  searching <- str_extract_all(my_df$content, paste(slowa, collapse="|")) 
  count_df <- as.data.frame(table(na.omit(unlist(searching)))) %>% arrange(desc(Freq))
  count_df
}

# bruh_i_xd_count <- substring_count(my,c("bruh","xd"))
# siema_count <- substring_count(my,"siema")


word_count <- function(my_df,slowa){
  # to samo co w poprzednim, ale tym razem szukam calych slow, to znaczy jest nie biore pod uwage substringow
  search_by <-  paste0(rep("\\b",length = length(slowa)), slowa,rep("\\b",length = length(slowa)), collapse="|")
  searching <- str_extract_all(my_df$content,search_by) 
  count_df <- as.data.frame(table(na.omit(unlist(searching)))) %>% arrange(desc(Freq))
  count_df
}
#word_count(my,slowa = c("bruh", "siema","ok"))



words_timeline <- function(df, words){
#uzywane slowo na przestrzeni czasu
  final_df <- data.frame()
  for(word in words){
    slowo_trends <- df[grepl(word,df$content),] 
    slowo_trends <- slowo_trends %>% 
      group_by(month = floor_date(Date_Y_M_D,"month")) %>% 
      count() 
    slowo_trends$word <- word
    final_df <- bind_rows(final_df,slowo_trends)
  }
  final_df <- final_df %>%
    pivot_wider(names_from = month, values_from = n,values_fill = 0) %>%
  pivot_longer(cols = -word, names_to = "month" ,values_to = "n") %>% 
    mutate(month = as.Date(month))
  plt <- ggplot(final_df,aes(x = month,y = n,group = word,color = word))+
    geom_line() +
    scale_x_date(date_breaks = "1 year",date_labels = "%Y")
  plt
}

# words_timeline(my,c("xd","bruh"))

## ogarniacie timelinu dla najczesciej uzywanych xd i haha
# string_xd <- str_extract_all(my$content, "\\b[xX]+[dD]+\\b") 
# xd_df <- as.data.frame(table(na.omit(unlist(string_xd)))) %>% arrange(desc(Freq))
# do_por_xd <- as.character(xd_df$Var1[1:5])
# 
# words_timeline(my,do_por_xd)
# 
# string_haha <- str_extract_all(my$content, r"(\b(?:a*(?:ha)+h?|h*ha+h[ha]*\b)")
# haha_df <- as.data.frame(table(na.omit(unlist(string_haha)))) %>% arrange(desc(Freq))
# do_por_haha <- as.character(haha_df$Var1[1:5])
# 
# words_timeline(my,do_por_haha)




xd_haha_comparison <- function(df,xd,haha){
  final_df <- data.frame()
  for(word in xd){
    slowo_trends <- df[grepl(word,df$content),] 
    slowo_trends <- slowo_trends %>% 
      group_by(month = floor_date(Date_Y_M_D,"month")) %>% 
      count() 
    slowo_trends$word <- "xd"
    final_df <- bind_rows(final_df,slowo_trends)
  }
  for(word in haha){
    slowo_trends <- df[grepl(word,df$content),] 
    slowo_trends <- slowo_trends %>% 
      group_by(month = floor_date(Date_Y_M_D,"month")) %>% 
      count() 
    slowo_trends$word <- "haha"
    final_df <- bind_rows(final_df,slowo_trends)
  }
  final_df <- final_df %>% group_by(word,month) %>% 
    summarise(n = sum(n))
  final_df <- final_df %>%
    pivot_wider(names_from = month, values_from = n,values_fill = 0) %>%
    pivot_longer(cols = -word, names_to = "month" ,values_to = "n") %>%
    mutate(month = as.Date(month))
  plt <- ggplot(final_df,aes(x = month,y = n,group = word,color = word))+
    geom_line() +
    scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
    labs(x = "year", y = "number of words", color = "roznego rodzaju")
  plt
}

# plt1 <-xd_haha_comparison(my,do_por_xd,do_por_haha)


messages_sent <- function(df,start_date,end_date){ #robie to, ze bedzie mozna w shiny dac slidera na przyklad z datami
  #funckja podlicza laczna ilosc wiadomosci w kazdym miesiacu od start_date do end_date
  df <- df %>% 
    group_by(month = floor_date(Date_Y_M_D,"month")) %>% 
    count() %>% 
    filter(month > start_date,month <end_date) 
  df
}

##tu mozna splotowac wyniki poprzedniej funkcji 
# plt2 <- messages_sent(my,as.Date("2013/01/01"),as.Date("2022/01/01")) %>% 
#   ggplot(aes(x = month,y=n))+
#   geom_line()+
#   scale_x_date(date_labels = "%Y")+
#   labs(x = "year", y ="number of messages")
# plt2

# library(patchwork)
# plt1/plt2
