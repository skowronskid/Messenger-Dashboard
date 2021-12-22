setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("liczenie_slow.R")

# data_frame <- load_all_conversations()
# my <- make_my_df(data_frame,"Damian Skowro\u00c5\u0084ski")



top_slowa <- function(df,min= 0){
  slowa <- as.data.frame(table(tolower(unlist(str_split(df$content, pattern = " "))))) %>% 
    mutate(Var1 = as.character(Var1)) %>% 
    filter(nchar(Var1)>min-1) %>% 
    arrange(desc(Freq))
}

# test <- top_slowa(my,5)
# head(test,20) #kurwa dopiero na 16, wiedzialem ze nie jestem wulgarnym czlowiekiem


top_reakcje <- function(df,sender){
  #nie jestem pewien czy to bedzie git
  reakcje <- unlist(df$reactions,recursive = F)
  n = length(reakcje)
  reaction <- "nic"
  actor <- "nikt"
  for(i in 1:n){
    if(i%%2 == 1){
      reaction <- c(reaction,reakcje[[i]])
    }
    else{
      actor <- c(actor,reakcje[[i]])
    }
  }
  reakcje <- as.data.frame(list("reaction" = reaction,"actor" = actor)) %>% 
    filter(actor == sender) %>%  
    group_by(reaction) %>% 
    count() %>% as.data.frame() %>% 
    arrange(desc(n))
  reakcje
}

 # top_reakcje(my,"Damian Skowro\u00c5\u0084ski")
