# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# source("liczenie_slow.R")





top_slowa <- function(df,min= 0){
  slowa <- as.data.frame(table(tolower(unlist(str_split(df$content, pattern = " "))))) %>% 
    mutate(Var1 = as.character(Var1)) %>% 
    filter(nchar(Var1)>min-1) %>% 
    arrange(desc(Freq))
}

# test <- top_slowa(my,5)
# head(test,20) #kurwa dopiero na 16, wiedzialem ze nie jestem wulgarnym czlowiekiem


suppressWarnings(suppressMessages(emoji <- read.csv("emoji-test.csv",encoding = "UTF-8")))
top_reakcje <- function(df,act){
  suppressWarnings(
    react_df <- df %>% select(reactions,actor) %>% filter(!is.na(reactions))  %>% 
    mutate(actor = strsplit(actor, ", "),reactions = strsplit(reactions, ", ")) %>% 
    unnest(actor,reactions) %>% filter(actor == act) %>% table() %>% as.data.frame() %>% slice_max(Freq,n = 20) %>% 
    left_join(emoji,by = c("reactions" = "utf"))
  )
  react_df
}



