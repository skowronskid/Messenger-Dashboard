


#nie uzyta

# top_slowa <- function(df,min= 0){
#   slowa <- as.data.frame(table(tolower(unlist(str_split(df$content, pattern = " "))))) %>% 
#     mutate(Var1 = as.character(Var1)) %>% 
#     filter(nchar(Var1)>min-1) %>% 
#     arrange(desc(Freq))
# }

# test <- top_slowa(my,5)
# head(test,20) 




# fwrite(emoji_df,"emoji_df.csv")



emojis <-data.table:: fread("emoji_df.csv",encoding = "UTF-8")

top_reakcje <- function(df,act,isMike = F){

  df$actor <- enc2native(df$actor)
  react_df <- df %>% select(reactions,actor) %>% filter(!is.na(reactions))  %>%
    mutate(actor = strsplit(actor, ", "),reactions = strsplit(reactions, ", "))
  if(isMike){ # u mike'a sie dzieja szalone rzeczy w tych wierszach 
    react_df <- react_df[-4717,]
    react_df <- react_df[-17876,]
    react_df <- react_df[-17877,]
    react_df <- react_df[-17880,]
  } 
  react_df <- react_df %>% unnest(actor,reactions) %>% 
    filter(actor == act) %>% table() %>% as.data.frame() %>% na.omit() %>% slice_max(Freq,n = 20)
  react_df <- merge(react_df,emojis, by.x = "reactions", by.y = "raw")[,-1]
  react_df[,-1]
}


