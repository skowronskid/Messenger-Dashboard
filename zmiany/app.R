library(shiny)
library(shinydashboard)
library(plotly)
library(ggdark)
library(DT)
library(shinyjs)
library(gganimate) 

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #jedna linkijka przy deployowaniu aplikacji, a tyle wkurwiania
source("wczytanie_json.R")
source("liczenie_slow.R")
source("day_streak.R")
source("top_slowa_reakcje.R")
source("potezne_xd.R")
source("dashboard_theme.R")
source("ip_call_fb.R")


#diego_all<- fread("diego.csv",encoding = "UTF-8")
#tymczasowo
mike_all <- fread("mike.csv",encoding = "UTF-8")
diego_all <- mike_all 
john_all <- diego_all 

# john_all <- fread("john.csv",encoding = "UTF-8")

diego <- diego_all %>% filter(sender_name == "Damian Skowro≈Ñski")
mike <- mike_all %>% filter(sender_name == "Micha≥ Mazuryk")
john <- john_all %>% filter(sender_name == "Janek Kruszewski")


#### KOLORY
WHITE_TEXT = "#CDCDCD"
GRAY_DARK = "#343E48"
GRAY_LIGHT= "#44505A"
BLUE = "#038FFF"
SALMON = "#FF586A" #nie wiem jak sie nazywa ten kolor xd





########################################################################################### 
#                              START UI                                                   #
########################################################################################### 





ui <- dashboardPage(
  header = dashboardHeader(title="Messenger"),useShinyjs(),
  sidebar = dashboardSidebar(collapsed = FALSE,
                             sidebarMenu(
                               selectInput(
                                 inputId = "fighter",
                                 label = "Choose your fighter:",
                                 choices = c("Magic Mike","Big Diego","Smooth John"),
                                 width = 200),
                               menuItem("Dashboard", tabName = "dashboard", icon = icon("calendar")),
                               menuItem("Messaging tendencies", icon = icon("th"), tabName = "tendencies"),
                               menuItem("Words database", icon = icon("th"), tabName = "wordscard"),
                             menuItem("Bonus page", icon = icon("warning-sign", lib = "glyphicon"), tabName = "trailpark"))),
  body = dashboardBody(
    customTheme,
    tabItems(
      tabItem(tabName = "dashboard",
        box(splitLayout( cellWidths = c("20%","160%"),
                         box(
                           imageOutput("gif",width = 157,
                                       height = 249), width = 50
                         ),box(verbatimTextOutput("info")),width = 100)),
        
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
              fluidRow(
              box(
                plotlyOutput("hours")
              ),
              tabBox(
                tabPanel("Plot",plotlyOutput("xd_timeline")),
                tabPanel("Table",tableOutput("xd_table"))
              )),
              box(
                plotlyOutput("haha_xd_plot")
              )
             
              ),#end tendencies tab
      tabItem(tabName = "wordscard",
              fluidRow(
                        box(width=6,
                          textInput("ch_word",label="Choose word to see it's timeline", value="lol"),
                          plotlyOutput("ch_word_timeline")
                        ),
                        box(width=6,
                            imageOutput("gif_plot",width = 500,
                                        height = 400)),
                        
                        box(width=12,
                            plotlyOutput("violin_word"),
                        ),
                        box(  width=12,
                          h3("Are you more of a \"yes\" or \"no\" person"),
                          plotlyOutput("yes_or_no")),
                        box(  width=12,
                          h3("What about your calls?"),
                          radioButtons("opt_call",label="",
                          choices = c("most hours talked","the longest calls","most missed calls")
                          ),
                          DTOutput("get_dfcall")),
                        box(  width=6,
                          plotlyOutput("get_call")),
                         
                        box(
                          plotlyOutput("unique_words")
                        )
                
                )),#end wordscard
      tabItem(tabName = "trailpark",useShinyjs(),
              fluidRow(useShinyjs(),
                
                
                box(id="locked",actionButton("btn3", "over"),width=12,
                    actionButton("btn2", "am"),
                    actionButton("btn4", "18"),
                    actionButton("btn1", "I"),
                    imageOutput("giftrail",width = 300,height = 300)),
                
                box(id="bonus2",h3("Your ultra fighter:"),
                  imageOutput("gifbonus",width = 157,
                              height = 249)),
                box(id="bonus3",h3("Party correlation starterpack:", plotOutput("party_cor"))),
                box(width=12,id="bonus1",h5("How many times you talked about:"),valueBoxOutput("partyBox"),valueBoxOutput("alkoBox"),valueBoxOutput("trailBox")),
                
                tabBox(id="bonus4",
                      tabPanel("Check",useShinyjs(),checkboxGroupInput(
                        "alco",
                        label="Which one did you mention:",
                        choices = c("piwo", "wÛdka","wino","whiskey","gin",'tequila','brandy','rum') 
                      ),actionButton("ref", "refresh")),
                      tabPanel("Table",DTOutput("alco_dt"))
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
  all_data <- reactiveValues(df = mike_all) 
  my_data <- reactiveValues(df = mike)
  
  observeEvent(input$fighter,{
    if(input$fighter == "Magic Mike"){
      my_data$df <- mike
      all_data$df <- mike_all
    }
    else if(input$fighter == "Big Diego"){
      my_data$df <- diego
      all_data$df <- diego_all
    }
    else{
      my_data$df <- john
      all_data$df <- john_all
    }
  })
  
  output$gif <- renderImage({
    list(src = paste0("gify/",input$fighter, "2.gif"),
         contentType = 'image/gif',
         width = 157,
         height = 249)
  },deleteFile = F) 
 
  
  output$info <- renderText({
    my <- my_data$df
    all <- all_data$df
    
    ph<-table(!is.na(my$photos))[2]
    gf<-table(!is.na(my$gifs))[2]
    rc<-table(!is.na(my$reactions))[2]
    aud<-table(!is.na(my$audio_files))[2]
    ms<-length(!is.na(my$content))
    mmw <- get_nr_of_messages(all) %>% arrange(desc(nr_of_messages))
    pop_hour <- table(as.numeric(format(my$timestamp_ms,'%H'))) %>% as.data.frame() %>% arrange(desc(Freq))
    pop_hour <- pop_hour[1,1]
    przeklenstwa <-  substring_count(my,c("kurw","chuj","huj","jeb","fuck","shit","pierdol","fiut")) %>% summarise(n = sum(Freq)) #mocny kod xdddd
    paste("You have selected: ", input$fighter,'\n',
          " Total number of messages sent: ",ms,'\n',
          " Total number of photos:",ph,'\n',
          " Total number of gifs:",gf,'\n',
          " Total number of reactions:",rc,'\n',
          " Total number of audio files:",aud,'\n',
          " Most messages with: ",mmw[2,1],"(",mmw[2,2],')\n',
          " Most common messaging hour: ",pop_hour,'\n',
          " More messages send in private or group chats: ",'\n',
          " Longest 'xd' length: ",get_potezne_xd(my),'\n',
          " Longest 'haha' length: ",get_potezne_haha(my),'\n',
          " How many times have you cursed: ",przeklenstwa[[1]],'\n',
          " How many people you ve ghosted: "#to jest calkiem trudne do zrobienia zeby szybko dzialo
    )
  }) %>% bindCache(input$fighter)
    
  
  
  
  
  output$messages_timeline_plot <- renderPlotly({
    my <- my_data$df
    plt2 <- messages_sent(my,as.Date("2013/01/01"),as.Date("2022/01/01")) %>% #potem zeby modyfikowaƒá daty sliderem
      ggplot(aes(x = month,y=n))+
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
    all <- all_data$df
    df <- get_nr_of_messages(all)[-1,] %>% 
      slice_max(nr_of_messages,n=10) %>% 
      mutate(sender_name = forcats::fct_reorder(sender_name,nr_of_messages)) 
    plt <- ggplot(df,aes(x = sender_name,y=nr_of_messages)) +
      geom_col() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05), add = c(0.1, 0))) +
      labs(y = "Number of messages",title = "Most messages received from") +
      dark_theme_gray(base_family = "Arial") +
      theme(axis.title.y = element_blank(),
            plot.background = element_rect(fill = GRAY_DARK),
            panel.background = element_rect(fill = GRAY_LIGHT),
            plot.title = element_text(hjust = 0.5, size = 20),
            panel.grid.major.y = element_blank())  +
      coord_flip()
    ggplotly(plt) %>% config(displayModeBar = F)
  }) %>% bindCache(input$fighter)
  
  output$conversations_dt <- renderDT({
    all <- all_data$df
    df <- get_nr_of_messages(all) %>% select("Sender name" = sender_name,"Nr of messages" = nr_of_messages)
    df
  },options = list(pageLength=8),server = F) %>% bindCache(input$fighter)
  
  
  
  
  
  
  output$reactions <- renderPlotly({
    all <- all_data$df

    # na razie takie rozwiazenie :(((
    if(input$fighter == "Magic Mike"){actor = "Micha\u623c\u3e33 Mazuryk"}
    else if(input$fighter == "Big Diego"){actor = "Damian Skowro\u663c\u3e31ski"}
    else{actor = "Janek Kruszewski"}

    df <- top_reakcje(all,actor)  %>% na.omit()
    df <- df %>% slice_max(Freq,n = 10) %>% mutate(emoji = forcats::fct_reorder(emoji,Freq))
    plt <- ggplot(df,aes(x = emoji,y = Freq)) +
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
    ggplotly(plt) %>% config(displayModeBar = F)
  }) %>% bindCache(input$fighter)

 output$reactions_dt <- renderDT({
  all <- all_data$df

  # na razie takie rozwiazenie :(((
  if(input$fighter == "Magic Mike"){actor = "Micha\u623c\u3e33 Mazuryk"}
  else if(input$fighter == "Big Diego"){actor = "Damian Skowro\u663c\u3e31ski"}
  else{actor = "Janek Kruszewski"}

  df <- top_reakcje(all,actor)  %>% na.omit()

  df_dt <- df %>% select("Emoji" = emoji, "Description" = desc, "Times used" = Freq)
},options = list(pageLength=8),server = F) %>% bindCache(input$fighter)

  
   
   ########################################################################################### 
   #                                END DASHBOARD TAB                                        #
   ########################################################################################### 
   #                              START TENDENCIES TAB                                       #
   ########################################################################################### 
   
  
   
   output$hours <- renderPlotly({
     my <- my_data$df
     df <- table(as.numeric(format(my$timestamp_ms,'%H'))) %>% as.data.frame() 
     colnames(df) <- c("Hour","Messages")
     plt <- ggplot(df,aes(x=Hour,y=Messages,group = 1)) +
       geom_col() +
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
     my <- my_data$df
     
     df1 <- df_words_timeline(my,c( "xd","XD","xD","Xd","xdd"))
     df <- words_timeline(df1)
         
     
     plt <- ggplot(df,aes(x = month,y = n,group = word,color = word))+
       geom_line() +
       scale_x_date(expand = c(0,0)) +
       scale_y_continuous(expand = expansion(mult = c(0, 0.15), add = c(0.5, 0))) +
       labs(x = "Year", y = "Times written",title = "Most used \"XD\"s")+
       dark_theme_gray(base_family = "Arial") + 
       theme(plot.background = element_rect(fill = GRAY_DARK),
             panel.background = element_rect(fill = GRAY_LIGHT),
             plot.title = element_text(hjust = 0.5, size = 20),
             legend.title = element_blank(),
             legend.position = "top",
             legend.background = element_rect(fill = GRAY_LIGHT, color = GRAY_DARK))
     
     ggplotly(plt) %>% 
       layout(legend = list(
         title = "",
         orientation = "h",
         x = 0,
         y = 1.01,
         xanchor = "left"
       )) %>% config(displayModeBar = F)
     
     
   })%>% bindCache(input$fighter)
   
   output$xd_table <- renderTable({
     my <- my_data$df
     
     df1 <- df_words_timeline(my,c( "xd","XD","xD","Xd","xdd")) %>% group_by(word) %>% summarise(n=sum(n)) %>% arrange(desc(n))
     colnames(df1) <- c("Word","Times written")
     df1
   }) %>% bindCache(input$fighter) 
   

   
   
   
   output$haha_xd_plot <- renderPlotly({
     my <- my_data$df
     ## tutaj akurat sa moje najbardziej uzywane, ale jak bede mial wasze ramki to moge dla waszych zrobic tez spersonalizowane, nie robie tego tu bo to dlugo trwa
     ggplotly(  
       xd_haha_comparison(my,c( "xd","XD","xD","Xd","xdd"),c("hahaha", "hahahah","hahah", "hahahha","haha" ))+ 
         labs(x = "Year",y = "Times used", title = "Usage of all kinds of \"XD\"s and \"haha\"s") +
         scale_x_date(expand = c(0,0)) +
         scale_y_continuous(expand = expansion(mult = c(0, 0.15), add = c(0.5, 0)),) +
         dark_theme_gray(base_family = "Arial") + 
         theme(plot.background = element_rect(fill = GRAY_DARK),
               panel.background = element_rect(fill = GRAY_LIGHT),
               plot.title = element_text(hjust = 0.5, size = 20),
               legend.title = element_blank(),
               legend.position = "top",
               legend.background = element_rect(fill = GRAY_LIGHT, color = GRAY_DARK))+
         scale_color_manual(values=c(BLUE,SALMON))) %>%
       layout(legend = list(
         title = "",
         orientation = "h",
         x = 0,
         y = 1.01,
         xanchor = "left"
       )) %>% config(displayModeBar = F)
       
   }) %>% bindCache(input$fighter)
   
   
 # m√≥g≈Çbym tu jeszcze troche pododawaƒá pewnie, ale na razie zostawiam jak jest
   ########################################################################################### 
   #                                END TENDENCIES TAB                                        #
   ########################################################################################### 
   #                              START WORDSCARD TAB                                       #
   ########################################################################################### 
   
   output$unique_words<- renderPlotly({
     plt<-unique_words(my_data$df)
     ggplotly(plt) %>% config(displayModeBar = F)
   })
   output$ch_word_timeline <- renderPlotly({
     plt<-df_words_timeline(my_data$df,c(input$ch_word)) %>% 
       ggplot(aes(x = month,y = n))+
       geom_line(size=1.5) +
       scale_x_date(expand = c(0,0)) +
       scale_y_continuous(expand = expansion(mult = c(0, 0.15), add = c(0.5, 0))) +
       labs(x = "Year", y = "Times written",title = paste0("\"",input$ch_word,"\""," timeline"))+
       dark_theme_gray(base_family = "Arial") + 
       theme(plot.background = element_rect(fill = GRAY_DARK),
             panel.background = element_rect(fill = GRAY_LIGHT),
             plot.title = element_text(hjust = 0.5, size = 20),
             legend.title = element_blank(),
             legend.position = "top",
             legend.background = element_rect(fill = GRAY_LIGHT, color = GRAY_DARK))
     ggplotly(plt) %>% config(displayModeBar = F)
   })
   
   output$yes_or_no <- renderPlotly({
     plt<-yes_or_no(my_data$df)
     ggplotly(plt) %>% config(displayModeBar = F)
   })
   
   
   
   output$violin_word <- renderPlotly({
     plt<-violin_od_litery(my_data$df)
     ggplotly(plt) %>% config(displayModeBar = F)
   })
   
   output$get_call <- renderPlotly({
     plt<-get_call(my_data$df)
     ggplotly(plt) %>% config(displayModeBar = F)
   })
   output$gif_plot <- renderImage({
     list(src = paste0("gify/",input$fighter, "3.gif"),
          contentType = 'image/gif',
          width = 157,
          height = 249)
   },deleteFile = F) 
   
   output$get_dfcall <- renderDT({
     who <-"Micha≥ Mazuryk"
     if(input$fighter == "Magic Mike"){
       who <-"Micha≥ Mazuryk"
     }
     else if(input$fighter == "Big Diego"){
       who <-"Damian SkowroÒski"
     }
     else{
       who <-"Janek Kruszewski"
     }
     if(input$opt_call == "most hours talked")
     {df<-get_dfcall(all_data$df,who,'suma')}
     if(input$opt_call == "the longest calls")
     {df<-get_dfcall(all_data$df,who,'max')}
     if(input$opt_call == "most missed calls")
     {df<-get_dfcall(all_data$df,who,'nieodebrane')}
      df
       
   })

   ########################################################################################### 
   #                                END WORDSCARD TAB                                        #
   ########################################################################################### 
   #                              START TRAILPARKBOYS TAB                                       #
   ###########################################################################################
   output$gifbonus <- renderImage({
     if(input$fighter == "Magic Mike"){
       list(src = "gify/Magic Mike.GIF",
            contentType = 'image/gif',
            width = 157,
            height = 249)
     }
     else if(input$fighter == "Big Diego"){
       list(src = "gify/Magic Mike.GIF",
            contentType = 'image/gif',
            width = 157,
            height = 249)
     }
     else{
       list(src = "gify/Smooth John.GIF",
            contentType = 'image/gif',
            width = 157,
            height = 249)
     }
     
   },deleteFile = F) 
   
   output$party_cor<- renderPlot({
     party_cor(all_data$df)
     
   })
   
   output$trailBox <- renderValueBox({
     valueBox(
       substring_count(my_data$df,c("trail","barak","rick","julian","bubbles","randy","bebech","lahey","polega odpowiedzialno","trinity")) %>% summarise(n = sum(Freq)),
       "Trail Park Boys", icon = icon("thumbs-up", lib = "glyphicon"),
       color = "yellow"
     )
   })
   output$alkoBox <- renderValueBox({
     valueBox(
       substring_count(my_data$df,c("wodk","vodk","wÛdk","gin","whiskey","≥ych","piw","alko","wino")) %>% summarise(n = sum(Freq)),
       "Alcohol", icon = icon("thumbs-up", lib = "glyphicon"),
       color = "green"
     )
   })
   output$partyBox <- renderValueBox({
     valueBox(
       substring_count(my_data$df,c("impr","melan","party","18stka","osiemnastk","domowka","domÛwk")) %>% summarise(n = sum(Freq)),
       "Party", icon = icon("thumbs-up", lib = "glyphicon"),
       color = "red"
     )
   })
   output$giftrail <- renderImage({
     list(src = "gify/locked.gif",
          contentType = 'image/gif',
          width = 200,
          height = 200)
   },deleteFile = F)

              
                            
  
   output$alco_dt<-renderDT({
     df_alc<-word_count(my_data$df,slowa = c("piwo", "wÛdka","wino","whiskey","gin",'tequila','brandy','rum'))
     colnames(df_alc)<-c("type","amount")
     df_alc
   })
   hide("bonus1")
   hide("bonus2")
   hide("bonus3")
   hide("bonus4")
   hide("alco")
   hide("ref")
   
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
     if(mi==3){show("bonus1");show("bonus2",anim = TRUE,animType = "slide",time = 0.5);show("bonus3")
       hide("locked");show("bonus4");
      show('alco');show('ref')
       }else{mi<<-0}
   })
   observeEvent(input$ref,{
     vec<-word_count(mike,slowa = c("piwo", "wÛdka","wino","whiskey","gin",'tequila','brandy','rum'))$Var1;
     updateCheckboxGroupInput(session, "alco",selected=vec)})
   
    
}

shinyApp(ui, server)
