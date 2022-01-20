library(shiny)
library(shinydashboard)
library(plotly)
library(ggdark)
library(DT)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)



source("liczenie_slow.R",encoding = "UTF-8")
source("top_slowa_reakcje.R",encoding = "UTF-8")
source("potezne_xd.R",encoding = "UTF-8")
source("dashboard_theme.R",encoding = "UTF-8")
source("rozne_osoby.R",encoding = "UTF-8")
source("filmy_zdjecia.R",encoding = "UTF-8")
source("ip_call_fb.R",encoding = "UTF-8")

#wczytuje kilka rzeczy wczesniej zeby szybciej dzialalo
diego_all<- data.table::fread("diego.csv",encoding = "UTF-8") %>% select(-users,-sticker.uri,-files,-share.share_text,-ip,-share.link)
mike_all <- data.table::fread("mike.csv",encoding="UTF-8") %>% select(-users,-sticker.uri,-files,-share.share_text,-ip,-share.link)
john_all <- data.table::fread("john.csv",encoding = "UTF-8") %>% select(-users,-sticker.uri,-files,-share.share_text,-ip,-share.link)

diego <- diego_all %>% filter(sender_name == "Damian Skowroński")
mike <- mike_all %>% filter(sender_name == "Michał Mazuryk")
john <- john_all %>% filter(sender_name == "Janek Kruszewski")

mess_nr_mike <- get_nr_of_messages(mike_all)
mess_nr_diego <- get_nr_of_messages(diego_all)
# mess_nr_john <- get_nr_of_messages(john_all)

react_diego <- top_reakcje(diego_all,act = "Damian Skowronski")
react_mike <- top_reakcje(mike_all,act = "Michal Mazuryk",isMike=T)
react_john <- top_reakcje(john_all,act = "Janek Kruszewski")


words_mike <- df_words_timeline(mike,c(  "XD",   "xd",   "xD" ,  "XDDD", "XDD" ))
words_diego <- df_words_timeline(diego,c( "xd","XD","xD","Xd","xdd"))
words_john <- df_words_timeline(john,c("xd","Xd","Xddd","xddd","Xdddd"))

pv_mike <- photos_videos(mike_all) 
pv_diego <- photos_videos(diego_all)
pv_john <- photos_videos(john_all)


#### KOLORY
WHITE_TEXT = "#CDCDCD"
GRAY_DARK = "#343E48"
GRAY_LIGHT= "#44505A"
BLUE = "#038FFF"
SALMON = "#FF586A"



########################################################################################### 
#                              START UI                                                   #
########################################################################################### 





ui <- dashboardPage(
  header = dashboardHeader(title="Messenger"),
  sidebar = dashboardSidebar(collapsed = FALSE,
                             sidebarMenu(
                               selectInput(
                                 inputId = "fighter",
                                 label = "Choose your fighter:",
                                 choices = c("Big Diego","Magic Mike","Smooth John"),
                                 width = 200),
                               menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                               menuItem("Messaging tendencies", icon = icon("th"), tabName = "tendencies"),
                               menuItem("Words database", icon = icon("th"), tabName = "wordscard"),
                               menuItem("Bonus page", icon = icon("warning-sign", lib = "glyphicon"), tabName = "trailerpark"))),
  body = dashboardBody(
    customTheme,
    tabItems(
      tabItem(tabName = "dashboard",
        box(splitLayout( cellWidths = c("25%","150%"),
                         box(
                           imageOutput("gif",width = 150,
                                       height = 249),
                         ),box(verbatimTextOutput("info")),width = 40)),
        
        box(  
          plotlyOutput("messages_timeline_plot")
        ),
        tabBox(
          tabPanel("Plot",plotlyOutput("conversations")),
          tabPanel("Table",DTOutput("conversations_dt"))
        ),
        tabBox(
          tabPanel("Plot",plotlyOutput("reactions")),
          tabPanel("Table",DTOutput("reactions_dt"))
        )
      ), #end dashboard tab
      
      tabItem(tabName = "tendencies",
                box(
                  plotlyOutput("hours")
                ),
              box(
                plotlyOutput("haha_xd_plot")
              ),
              tabBox(width=12,
                tabPanel("Plot",plotlyOutput("xd_timeline")),
                tabPanel("Table",DTOutput("xd_table"))
              ),
              
              box(
                plotlyOutput("rozne_osoby_plot")
              ),
              tabBox(
                tabPanel("Photos", plotlyOutput("zdjecia_plot")),
                tabPanel("Videos",plotlyOutput("videos_plot")),
                tabPanel("Table", DTOutput("zdjecia_dt")),
              )
      ),    #end tendencies tab
      
      tabItem(tabName = "wordscard",
              fluidRow(
                box(width=6,
                    textInput("ch_word",label="Choose word to see it's timeline", value="lol"),
                    plotlyOutput("ch_word_timeline")
                ),
                
                box(width=6,
                    imageOutput("gif_plot"),height=495),
                
                box(
                  plotlyOutput("unique_words")
                )
                
              ),
              box(  width=12,
                    
                    plotlyOutput("yes_or_no")),
              
              box(width=12,
                  plotlyOutput("violin_word"),
              ),
              
              box(  width=6,
                    h3("What about your calls?"),
                    radioButtons("opt_call",label="",
                                 choices = c("most hours talked","the longest calls","most missed calls")
                    ),
                    DTOutput("get_dfcall")),
              box(  width=6,
                    plotlyOutput("get_call"))),
      tabItem(tabName = "trailerpark",shinyjs::useShinyjs(),
              fluidRow(shinyjs::useShinyjs(),
                       
                       
                       box(id="locked",actionButton("btn3", "over"),width=12,
                           actionButton("btn2", "am"),
                           actionButton("btn4", "18"),
                           actionButton("btn1", "I"),
                           imageOutput("giftrail",width = 300,height = 300)),
                       
                       box(height=495,id="bonus2",splitLayout( cellWidths = c("25%","150%"),
                                                    box(
                                                      imageOutput("gifbonus",width = 150,
                                                                  height = 249),
                                                    ),box(verbatimTextOutput("infobonus")),width = 40)),
                       
                       box(id="bonus3",h3("Party correlation starterpack:"),h6("Correlation between using specific words on the same day."), plotOutput("gifcorr")),
                       box(width=12,id="bonus1",h5("How many times you talked about:"),
                           valueBoxOutput("partyBox"),
                           valueBoxOutput("alkoBox"),
                           valueBoxOutput("trailerBox")),
                       
                       tabBox(id="bonus4",
                              tabPanel("Check",shinyjs::useShinyjs(),actionButton("ref", "check"),
                                       checkboxGroupInput(
                                "alco",
                                label="Which one did you mention:",
                                choices = c("piwo", "wódka","wino","whiskey","gin",'tequila','brandy','rum') 
                              )),
                              tabPanel("Table",DTOutput("alco_dt"))
                       ),
                       tabBox(id="bonus5",
                              tabPanel("Check1",shinyjs::useShinyjs(),actionButton("ref1", "check"),
                                       checkboxGroupInput(
                                         "alco1",
                                         label="Which one did you mention:",
                                         choices = c("ku***" ,"ch***" ,"pi***" ,"sh***", "fu***", "je***") 
                                       )),
                              tabPanel("Table",DTOutput("alco_dt1"))
                       )
                       
                       
                       
                       
              ))#end trailpark
    )
  ) #end dashboardBody
) #end ui





########################################################################################### 
#                                END UI                                                   #
########################################################################################### 
#                              START SEVER                                                #
########################################################################################### 





server <- function(input, output,session) {
  data <- reactiveValues(all = mike_all,my = mike,mess = mess_nr_mike,react = react_mike,words = words_mike, pv = pv_mike, me="Michał Mazuryk") 
  observeEvent(input$fighter,{
    if(input$fighter == "Magic Mike"){
      data$my <- mike
      data$all <- mike_all
      data$mess <- mess_nr_mike
      data$react <- react_mike
      data$words <- words_mike
      data$pv <- pv_mike
      data$me="Michał Mazuryk"
    }
    else if(input$fighter == "Big Diego"){
      data$my <- diego
      data$all <- diego_all
      data$mess <- mess_nr_diego
      data$react <- react_diego
      data$words <- words_diego
      data$pv  <- pv_diego
      data$me="Damian Skowroński"
    }
    else{
      data$my <- john
      data$all <- john_all
      data$mess <- mess_nr_john
      data$react <-react_john
      data$words <- words_john
      data$pv  <- pv_john
      data$me="Janek Kruszewski"
    }
  })
  
  output$gif <- renderImage({
    list(src = paste0("gify/",input$fighter, "2.gif"),
         contentType = 'image/gif',
         width = 157,
         height = 249)
  },deleteFile = F) 
 
  
  output$info <- renderText({

    
    ph<-table(!is.na(data$my$photos))[2]
    gf<-table(!is.na(data$my$gifs))[2]
    rc<-sum(data$react$Freq)
    aud<-table(!is.na(data$my$audio_files))[2]
    ms<-length(!is.na(data$my$content))
    mmw <- data$mess %>% arrange(desc(nr_of_messages))
    person <- sub("(\\w+).*", "\\1", mmw[2,1])
    pop_hour <-  data$my %>% mutate(hour = format(data$my$timestamp_ms, format="%H")) %>% 
       group_by(hour) %>% 
       summarise(n=n()) %>% 
       arrange(-n)
    pop_hour <- pop_hour[1,1]
    paste0("You selected: ", input$fighter,'\n',
          " Total number of text messages: ",ms,'\n',
          " Total number of photos: ",ph,'\n',
          " Total number of gifs: ",gf,'\n',
          " Total number of reactions: ",rc,'\n',
          " Total number of audio files: ",aud,'\n',
          " Most messages with: ",person," (",mmw[2,2],')\n',
          " Most common messaging hour: ",pop_hour,'\n',
          " Longest 'xd' length: ",nchar(get_potezne_xd(data$my)),'\n',
          " Longest 'haha' length: ",nchar(get_potezne_haha(data$my))
          
    )
  }) %>% bindCache(input$fighter)
  
  
    

  output$messages_timeline_plot <- renderPlotly({
    
    plt2 <- messages_sent(data$my,as.Date("2013/01/01"),as.Date("2022/01/01")) %>% 
      rename("Number of messages" = "n", "Month" = "month") %>% 
      ggplot(aes(x = Month,y=`Number of messages`))+
      geom_line()+
      scale_x_date(date_labels = "%m-%Y",date_breaks = "6 months",expand = c(0,0))+
      scale_y_continuous(expand = expansion(mult = c(0, 0.05), add = c(0.1, 0))) +
      labs(y ="Number of messages",title = "Messages sent timeline")+
      dark_theme_gray(base_family = "Arial") +
      theme(axis.text.x = element_text(angle = 90),axis.title.x = element_blank(),
            plot.background = element_rect(fill = GRAY_DARK),
            panel.background = element_rect(fill =GRAY_LIGHT),
            plot.title = element_text(hjust = 0.5, size = 20)) 
    ggplotly(plt2) %>% config(displayModeBar = F)
  }) %>% bindCache(input$fighter)
  
  
  
  
  
  
  output$conversations <- renderPlotly({
    
    df <- data$mess[-1,] %>% 
      slice_max(nr_of_messages,n=10) %>% 
      mutate(sender_name = forcats::fct_reorder(sender_name,nr_of_messages)) %>% 
      rename("Sender" = "sender_name", "Number of messages" = "nr_of_messages")
    
    plt <- ggplot(df,aes(x = Sender,y=`Number of messages`)) +
      geom_col() +
      scale_x_discrete(labels = function(x) sub("(\\w+).*", "\\1",x)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05), add = c(0.1, 0))) +
      labs(y = "Number of messages",title = "Most messages received from") +
      dark_theme_gray(base_family = "Arial") +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_text(size = 12),
            plot.background = element_rect(fill = GRAY_DARK),
            panel.background = element_rect(fill = GRAY_LIGHT),
            plot.title = element_text(hjust = 0.5, size = 20),
            panel.grid.major.y = element_blank())  +
      coord_flip()
    ggplotly(plt,tooltip = "Number of messages") %>% config(displayModeBar = F)
  }) %>% bindCache(input$fighter)
  
  output$conversations_dt <- renderDT({

    
    df <- data$mess %>%  mutate(sender_name = sub("(\\w+).*", "\\1",sender_name)) %>%
      select("Sender" = sender_name,"Number of messages" = nr_of_messages)
    df[1,1] <- input$fighter
    df 
  },options = list(pageLength=8),server = F) %>% bindCache(input$fighter)

  
  
  
  
  
  output$reactions <- renderPlotly({


    df <- data$react %>% slice_max(Freq,n = 10) %>% mutate(emoji = forcats::fct_reorder(emoji,Freq)) %>% 
      rename("Emoji"="emoji","Times used" = "Freq","Emoji name" = "emoji_names")
    plt <- ggplot(df,aes(x = Emoji,y = `Times used`)) +
      geom_col() +
      labs(y = "Times used", title = "Most used reactions") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05), add = c(0.1, 0))) +
      dark_theme_gray(base_family = "Arial") +
      theme(axis.text.y = element_text(size = 15),
            axis.title.y = element_blank(),
            plot.background = element_rect(fill = GRAY_DARK),
            panel.background = element_rect(fill =GRAY_LIGHT),
            plot.title = element_text(hjust = 0.5, size = 20),
            panel.grid.major.y = element_blank()) +
      coord_flip()
    ggplotly(plt,tooltip = c("Times used")) %>% config(displayModeBar = F)
  }) %>% bindCache(input$fighter)

 output$reactions_dt <- renderDT({
  
  data$react %>% arrange(desc(Freq)) %>% 
    rename("Emoji"="emoji","Times used" = "Freq")
},options = list(pageLength=8),server = F) %>% bindCache(input$fighter)



   
   ########################################################################################### 
   #                                END DASHBOARD TAB                                        #
   ########################################################################################### 
   #                              START TENDENCIES TAB                                       #
   ########################################################################################### 
   
  
   
   output$hours <- renderPlotly({

     df <- table(as.numeric(format(data$my$timestamp_ms,'%H'))) %>% as.data.frame()
     colnames(df) <- c("Hour","Messages")
     plt <- ggplot(df,aes(x=Hour,y=Messages,group = 1)) +
       geom_col(fill=SALMON) +
       scale_y_continuous(expand = expansion(mult = c(0, 0.05), add = c(0.1, 0))) +
       ggtitle("Messages sent by hour") +
       ylab("Nr of messages") +
       dark_theme_gray(base_family = "Arial") +
       theme(plot.background = element_rect(fill = GRAY_DARK),
             panel.background = element_rect(fill = GRAY_LIGHT),
             plot.title = element_text(hjust = 0.5, size = 20),
             panel.grid.major.x = element_blank())

     ggplotly(plt) %>% config(displayModeBar = F)
   })  %>% bindCache(input$fighter)
   
  
   
   
 output$xd_timeline <- renderPlotly({

    
    df <- words_timeline(data$words) %>% 
       rename("Times written" = "n","Month" = "month","Word" = "word")
    
    
    plt <- ggplot(df,mapping = aes(x = Month,y = `Times written`,group = Word,color = Word))+
       geom_line(size = 1) +
       scale_x_date(expand = c(0,0)) +
       scale_y_continuous(expand = expansion(mult = c(0, 0.15), add = c(0.5, 0))) +
       labs(x = "Year", y = "Times written",title = "Most used \"XD\"s")+
       dark_theme_gray(base_family = "Arial") + 
       theme(plot.background = element_rect(fill = GRAY_DARK),
             panel.background = element_rect(fill = GRAY_LIGHT),
             plot.title = element_text(hjust = 0.5, size = 20),
             legend.title = element_blank(),
             legend.position = "top",
             legend.background = element_rect(fill = GRAY_LIGHT, color = GRAY_DARK)) +
       scale_colour_brewer(palette = "Set1")
    
    ggplotly(plt,tooltip = c("Month","Times written")) %>% 
       layout(legend = list(
          title = "",
          orientation = "h",
          x = 0,
          y = 1.01,
          xanchor = "left"
       )) %>% config(displayModeBar = F)
    
    
 })%>% bindCache(input$fighter)
 
 output$xd_table <- renderDT({
    my <- data$my
    words <- data$words
    
    df1 <- words %>% group_by(word) %>% summarise(n=sum(n)) %>% arrange(desc(n))
    colnames(df1) <- c("Word","Times written")
    df1
 },server = F) %>% bindCache(input$fighter)

   
   
   
   output$haha_xd_plot <- renderPlotly({
     
     if(input$fighter == "Magic Mike"){
       xd = c(  "XD",   "xd",   "xD" ,  "XDDD", "XDD" )
       haha =c( "hahaha",   "haha" ,    "hahah" ,   "hahahaha", "hahaah" )
     }
     else if(input$fighter == "Big Diego"){
       xd = c( "xd","XD","xD","Xd","xdd")
       haha =c("hahaha", "hahahah","hahah", "hahahha","haha" )
       }
     else{
       xd = c("xd","Xd","Xddd","xddd","Xdddd")
     haha =c("haha","hah", "hahaha","hahah", "hahha"  )
     }
     ggplotly(  
       xd_haha_comparison(data$my,xd,haha)+ 
         labs(x = "Month",y = "Times used", title = "Usage of \"XD\"s and \"haha\"s") +
         scale_x_date(expand = c(0,0)) +
         scale_y_continuous(expand = expansion(mult = c(0, 0.15), add = c(0.5, 0))) +
         dark_theme_gray(base_family = "Arial") + 
         theme(plot.background = element_rect(fill = GRAY_DARK),
               panel.background = element_rect(fill = GRAY_LIGHT),
               plot.title = element_text(hjust = 0.5, size = 20),
               legend.title = element_blank(),
               legend.position = "top",
               legend.background = element_rect(fill = GRAY_LIGHT, color = GRAY_DARK))+
         scale_color_manual(values=c(BLUE,SALMON)),tooltip = c("Month","Times used")) %>%
       layout(legend = list(
         title = "",
         orientation = "h",
         x = 0,
         y = 1.01,
         xanchor = "left"
       )) %>% config(displayModeBar = F)
       
   }) %>% bindCache(input$fighter)
   
   
   ########################################################################################### 
   #                                END TENDENCIES TAB                                       #
   ########################################################################################### 
   #                              START NEW TAB                                              #
   ###########################################################################################
   
   output$rozne_osoby_plot <- renderPlotly({
      
     plt3 <- ggplot(rozne_osoby(data$all,data$me), aes(month, il_osob)) +
       geom_line(size = 0.2, linejoin = "round")+
       scale_x_date(date_labels = "%m-%Y",date_breaks = "6 months",expand = c(0,0))+
       scale_y_continuous(expand = expansion(mult = c(0, 0.05), add = c(0.1, 0))) +
       labs(y ="Number of people",title = "People writing to us timeline")+
       dark_theme_gray(base_family = "Arial") +
       theme(axis.text.x = element_text(angle = 90),axis.title.x = element_blank(),
             plot.background = element_rect(fill = GRAY_DARK),
             panel.background = element_rect(fill =GRAY_LIGHT),
             plot.title = element_text(hjust = 0.5, size = 20)) 
     ggplotly(plt3) %>% config(displayModeBar = F)
   }) %>% bindCache(input$fighter)
   
   
   
   

   output$zdjecia_plot <- renderPlotly({
     
     df1 <- data$pv %>% slice_max(photo,n=11)
     df1 <- df1[df1$sender_name!=data$me,] %>% mutate(sender_name = forcats::fct_reorder(sender_name,photo))

     plt4 <- ggplot(df1, aes(x = sender_name, y = photo))+
       geom_col()+
       labs(y = "Number of photos", title = "Top photo senders") +
       scale_x_discrete(labels = function(x) sub("(\\w+).*", "\\1",x)) +
       scale_y_continuous(expand = expansion(mult = c(0, 0.05), add = c(0.1, 0))) +
       dark_theme_gray(base_family = "Arial") +
       theme(axis.text.y = element_text(size = 12),
             axis.title.y = element_blank(),
             plot.background = element_rect(fill = GRAY_DARK),
             panel.background = element_rect(fill =GRAY_LIGHT),
             plot.title = element_text(hjust = 0.5, size = 20),
             panel.grid.major.y = element_blank()) +
       coord_flip()
     ggplotly(plt4) %>% config(displayModeBar = F)
   }) %>% bindCache(input$fighter)


   output$videos_plot <- renderPlotly({
     
     df1 <-data$pv %>% slice_max(video,n=11)
     df1 <- df1[df1$sender_name!=data$me,] %>% mutate(sender_name = forcats::fct_reorder(sender_name,video))

     plt5 <- ggplot(df1, aes(x = sender_name, y = video))+
       geom_col()+
       labs(y = "Number of videos", title = "Top video senders") +
       scale_x_discrete(labels = function(x) sub("(\\w+).*", "\\1",x)) +
       scale_y_continuous(expand = expansion(mult = c(0, 0.05), add = c(0.1, 0))) +
       dark_theme_gray(base_family = "Arial") +
       theme(axis.text.y = element_text(size = 12),
             axis.title.y = element_blank(),
             plot.background = element_rect(fill = GRAY_DARK),
             panel.background = element_rect(fill =GRAY_LIGHT),
             plot.title = element_text(hjust = 0.5, size = 20),
             panel.grid.major.y = element_blank()) +
       coord_flip()
     ggplotly(plt5) %>% config(displayModeBar = F)
   }) %>% bindCache(input$fighter)

   output$zdjecia_dt <- renderDT({
      pv <- data$pv  %>% mutate(sender_name = sub("(\\w+).*", "\\1",sender_name)) %>% arrange(desc(photo))
      pv[1,1] <- input$fighter
      pv
   },options = list(pageLength=8),server = F) %>% bindCache(input$fighter)

   
   ########################################################################################### 
   #                                END NEW TAB                                             #
   ########################################################################################### 
   #                              START WORDSCARD TAB                                       #
   ###########################################################################################
   
   
   output$unique_words<- renderPlotly({

     
     plt<-unique_words(data$my)
     ggplotly(plt) %>% config(displayModeBar = F)
   }) 
   
   
   output$ch_word_timeline <- renderPlotly({
     plt<-ch_word_timeline(data$my,input$ch_word)
     ggplotly(plt) %>% config(displayModeBar = F)
   })# %>% bindCache(input$fighter,input$ch_word)
   
   output$yes_or_no <- renderPlotly({
     
     plt<-yes_or_no(data$my)
     ggplotly(plt) %>% config(displayModeBar = F)
   }) %>% bindCache(input$fighter)
   
   
   
   output$violin_word <- renderPlotly({
     
     plt<-violin_od_litery(data$my)
     ggplotly(plt) %>% config(displayModeBar = F)
   }) %>% bindCache(input$fighter)
   
   output$get_call <- renderPlotly({
     
     plt<-get_call(data$my)
     ggplotly(plt) %>% config(displayModeBar = F)
   }) %>% bindCache(input$fighter)
   
   output$gif_plot <- renderImage({
     list(src = paste0("gify/",input$fighter, "3.gif"),
          contentType = 'image/gif',
          width = 500,
          height = 450)
   },deleteFile = F) 
   
   output$get_dfcall <- renderDT({

     
     if(input$opt_call == "most hours talked")
     {df<-get_dfcall(data$all,data$me,'suma')}
     if(input$opt_call == "the longest calls")
     {df<-get_dfcall(data$all,data$me,'max')}
     if(input$opt_call == "most missed calls")
     {df<-get_dfcall(data$all,data$me,'nieodebrane')}
     df
     
   } ,server = F) %>% bindCache(input$fighter) 
  
   ########################################################################################### 
   #                                END WORDSCARD TAB                                        #
   ########################################################################################### 
   #                              START TRAILPARKBOYS TAB                                       #
   ###########################################################################################
   output$gifbonus <- renderImage({
        list(src = paste0("gify/",input$fighter, ".gif"),
          contentType = 'image/gif',
          width = 157,
          height = 249)

   },deleteFile = F)

   output$gifcorr<- renderPlot({
     party_cor(data$my)
   }) %>% bindCache(input$fighter)

   output$trailerBox <- renderValueBox({
     valueBox(
       substring_count(data$my,c("trail","barak","rick","julian","bubbles","randy","bebech","lahey","polega odpowiedzialno","trinity")) %>% summarise(n = sum(Freq)),
       "Trailer Park Boys", icon = icon("thumbs-up", lib = "glyphicon"),
       color = "yellow"
     )
   })

   output$alkoBox <- renderValueBox({
     valueBox(
       substring_count(data$my,c("wodk","vodk","wódk","gin","whiskey","łych","piw","alko","wino")) %>% summarise(n = sum(Freq)),
       "Alcohol", icon = icon("thumbs-up", lib = "glyphicon"),
       color = "green"
     )
   })

   output$partyBox <- renderValueBox({
     valueBox(
       substring_count(data$my,c("impr","melan","party","18stka","osiemnastk","domowka","domówk")) %>% summarise(n = sum(Freq)),
       "Party", icon = icon("thumbs-up", lib = "glyphicon"),
       color = "red"
     )
   })

   output$giftrail <- renderImage({
     list(src = "gify/locked.gif",
          contentType = 'image/gif',
          width = 300,
          height = 300)
   },deleteFile = F)

   
   
   output$alco_dt<-renderDT({

     df_alc<-word_count(data$my,slowa = c("piwo", "wódka","wino","whiskey","gin",'tequila','brandy','rum'))
     colnames(df_alc)<-c("type","amount")
     df_alc
   } ,server = F) %>% bindCache(input$fighter)

   output$alco_dt1<-renderDT({
     #przepraszam za wyrazenia:(
     df_alc<-word_count(data$my,slowa = c("kurwa","fuck","chuj","jebał","shit","pierdole"))
     colnames(df_alc)<-c("type","amount")
     str_sub(df_alc$type,start=3)<-'***'

     df_alc
   } ,server = F) %>% bindCache(input$fighter)


   shinyjs::hide("bonus1")
   shinyjs::hide("bonus2")
   shinyjs::hide("bonus3")
   shinyjs::hide("bonus4")
   shinyjs::hide("bonus5")
   shinyjs::hide("alco")
   shinyjs::hide("ref")
   shinyjs::hide("alco1")
   shinyjs::hide("ref1")
   shinyjs::hide("infobonus")
#
   mi<<-0
   observeEvent(input$btn1, {
     mi<<-1

   })
   observeEvent(input$btn2, {
     if(mi==1){mi<<-2

     }else{mi<<-0}
   })
   observeEvent(input$btn3, {
     if(mi==2){mi<<-3

     }else{mi<<-0}
   })
   observeEvent(input$btn4, {
      if(mi==3){shinyjs::show("bonus1");
         shinyjs::show("bonus2",anim = TRUE,animType = "slide",time = 0.5)
         shinyjs::show("bonus3")
         shinyjs::hide("locked")
         shinyjs::show("bonus4")
         shinyjs::show("bonus5")
         shinyjs::show('alco')
         shinyjs::show('ref')
         shinyjs::show('alco1')
         shinyjs::show('ref1')
         shinyjs::show("infobonus")
      }else{mi<<-0}
   })
   observeEvent(input$ref,{
     vec<-word_count(data$my,slowa = c("piwo", "wódka","wino","whiskey","gin",'tequila','brandy','rum'))$Var1;
     updateCheckboxGroupInput(session, "alco",selected=vec)})
   observeEvent(input$ref1,{
     #przepraszam za wyrazenia:(
     vec<-word_count(data$my,slowa = c("kurwa","fuck","chuj","jebac","shit","pierdole"))$Var1;
     str_sub(vec,start=3)<-'***'
     updateCheckboxGroupInput(session, "alco1",selected=vec)})

   output$infobonus <- renderText({

   
     ph<-table(!is.na(data$my$photos))[2]
     gf<-table(!is.na(data$my$gifs))[2]
     ms<-length(!is.na(data$my$content))
   

     przeklenstwa <-  substring_count(data$my,c("kurw","huj","jeb","fuck","shit","pierdol","fiut","cip")) %>% summarise(n = sum(Freq)) #boze wybacz mi ten kod
     paste0("You selected: ", input$fighter,'\n',
            " Total number of text messages: ",ms,'\n',
            " Total number of photos: ",ph,'\n',
            " Total number of gifs: ",gf,'\n',
            " Longest 'xd' length: ",nchar(get_potezne_xd(data$my)),'\n',
            " Longest 'haha' length: ",nchar(get_potezne_haha(data$my)),'\n',
            " Longest 'k***a' length: ",nchar(get_potezne_k(data$my)),'\n',
            " Longest 'f**k' length: ",nchar(get_potezne_f(data$my)),'\n',
            " Profanity counter: ",przeklenstwa[[1]]
     )
   }) %>% bindCache(input$fighter)
}

shinyApp(ui, server)
