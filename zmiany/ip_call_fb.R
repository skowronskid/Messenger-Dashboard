library('rgeolocate')
library(ggplot2)
library('tidytext')
library(dplyr)
library(tidyr)
library(corrplot)
library(RColorBrewer)
library(gifski)
library(png)

#party plot
party_cor<- function(df){
wordKeb<-c("keb")
wordDrink<-c("cola","pepsi","sprite","fanta")
wordAlk<-c("wodk","vodk","w�dk","gin","whiskey","�ych","piw","alko","wino")
wordImp<-c("impr","melan","party","18stka","osiemnastk","domowka","dom�wk")
keb<-unique(df[grepl(wordKeb,df$content),Date_Y_M_D])
alc<-unique(df[grepl(paste0(wordAlk,collapse="|"),df$content),Date_Y_M_D])
imp<-unique(df[grepl(paste0(wordImp,collapse="|"),df$content),Date_Y_M_D])
dri<-unique(df[grepl(paste0(wordDrink,collapse="|"),df$content),Date_Y_M_D])

df_coor<-data.frame("day"=unique(df$Date_Y_M_D),
                    "kebab"=0,
                    "party"=0,
                    "alcohol"=0,
                    "coca_cola"=0,
                    stringsAsFactors=FALSE)

df_coor$kebab[which(df_coor$day %in% keb)]<-1
df_coor$coca_cola[which(df_coor$day %in% dri)]<-1
df_coor$party[which(df_coor$day %in% imp)]<-1
df_coor$alcohol[which(df_coor$day %in% alc)]<-1
df_coor$weekday<-as.numeric(strftime(as.Date(df_coor$day, "%d-%m-%Y"),"%u"))
df_coor<-df_coor %>% filter(kebab!=0|party!=0|alcohol!=0)

corrplot(cor(df_coor[, c(2:6)]),order = 'AOE',method='square', cl.pos = 'n',tl.pos = 'd',
         col = c('red', 'blue'), bg = 'black',tl.cex =0.8)
 
}



#plot violin depending on letter
violin_od_litery<- function(df){
  WHITE_TEXT = "#CDCDCD"
  GRAY_DARK = "#343E48"
  GRAY_LIGHT= "#44505A"
  BLUE = "#038FFF"
  SALMON = "#FF586A"
#wazna ramka
word_df<- df %>%
  select(content) %>% 
  unnest_tokens(input=content,output='word',format='text',drop=TRUE, to_lower=TRUE) %>% 
  mutate(first= substr(word, 1, 1),n= nchar(word))

first_letters<-word_df %>% select(first,n) %>% filter(first %in% letters & n<25)
first_letters %>% 
  ggplot(aes(factor(first), n))+
  geom_violin(fill=BLUE)+
  labs(y="word length",title = "Length of words depending on first letter")+
  dark_theme_gray(base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 90),axis.title.x = element_blank(),
        plot.background = element_rect(fill = GRAY_DARK),
        panel.background = element_rect(fill =GRAY_LIGHT),
        plot.title = element_text(hjust = 0.5, size = 20))
}






#word unique plot
unique_words<- function(df){
  WHITE_TEXT = "#CDCDCD"
  GRAY_DARK = "#343E48"
  GRAY_LIGHT= "#44505A"
  BLUE = "#038FFF"
  SALMON = "#FF586A"
  word<- df %>% 
  unnest_tokens(input=content,output='word',format='text',drop=TRUE, to_lower=TRUE) %>% 
    mutate(year = format(timestamp_ms, format="%Y")) %>%  
  group_by(year,word) %>% 
  count()

new_words <- word  %>% ungroup() %>% arrange(year) %>% distinct(word, .keep_all = TRUE)

new_words %>% group_by(year) %>% summarise(ni=n())  %>% 
  ggplot(aes(x=year, y=ni, group = 1))+
  geom_line(color=BLUE,size=1.5)+
  labs(y ="",title = "New words used")+
  dark_theme_gray(base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 90),axis.title.x = element_blank(),
        plot.background = element_rect(fill = GRAY_DARK),
        panel.background = element_rect(fill =GRAY_LIGHT),
        plot.title = element_text(hjust = 0.5, size = 20))
}

#getting ip
get_ip<- function(df){
ip_df<-df %>% 
  group_by(ip) %>% 
  summarise(n=n()) %>% 
  na.omit() %>% 
  arrange(-n) %>% head(15)
ip_vec<-ip_df$ip
db_ip(ip_vec, 'free')}

#calls

get_call<- function(df){
  WHITE_TEXT = "#CDCDCD"
  GRAY_DARK = "#343E48"
  GRAY_LIGHT= "#44505A"
  BLUE = "#038FFF"
  SALMON = "#FF586A"
  
df$call_duration<-as.numeric(df$call_duration)
df_call<-df %>% 
  filter(call_duration!="NA" & call_duration>3) 
df_call %>% 
ggplot(aes(x = Date_Y_M_D,y=call_duration/60))+
  geom_point(color=SALMON)+
  labs(y ="Duration in minutes",title = "Calls Timeline")+
  scale_x_date(date_labels = "%m-%Y",date_breaks = "6 months",expand = c(0.02, 0.05))+
  scale_y_continuous(expand = expansion(mult = c(0, 0.05), add = c(0.1, 0))) +
  dark_theme_gray(base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 90),axis.title.x = element_blank(),
        plot.background = element_rect(fill = GRAY_DARK),
        panel.background = element_rect(fill =GRAY_LIGHT),
        plot.title = element_text(hjust = 0.5, size = 20))
} 
#call df
get_dfcall<- function(df,who,option){
  df$call_duration<-as.numeric(df$call_duration)
  df_call<-df %>%filter(call_duration!="NA" & sender_name!=who)
  if(option=="suma"){
    df_call<-df_call %>% group_by(sender_name) %>% summarise(n= (ceiling(sum(call_duration)/36))/100) %>% arrange(-n)}
  if(option=="max"){
    df_call<-df_call %>% group_by(sender_name) %>% summarise(n= (ceiling(max(call_duration)/36))/100)%>% arrange(-n)}
  if(option=="nieodebrane"){
    df_call<-df_call[grepl("You missed a call from", df_call$content),] %>%  group_by(sender_name) %>% summarise(n= n())%>% arrange(-n)}
  df_call

}



