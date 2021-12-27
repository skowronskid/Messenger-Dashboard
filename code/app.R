library(shiny)
library(dplyr)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(shinydashboard)
#lokalizacja po ip
#call_duration

df_m<- read.csv('mess_all')
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
      box(
        selectInput(
          inputId = "fighter",
          label = "Choose your fighter:",
          choices = c("Magic Mike", "Big Diego", "Smooth John")
        )
      ),
      box(
        verbatimTextOutput("info"),
      ),
      box(
        img(src = "pl.png",width='35px',height='50px')
        #nasza fota lub zdj
      )
      
    )
)

server <- function(input, output) {
  
  
  
  output$info <- renderText({ 
    #zmiana bazy danych w zaleznosci od fighter
    my_data<- df_m %>% filter(sender_name == "Micha\u00c5\u0082 Mazuryk")
    ph<-table(my_data$photos!="NULL")[2]
    gf<-table(my_data$gifs!="NULL")[2]
    rc<-table(my_data$reactions!="NULL")[2]
    aud<-table(my_data$audio_files!="NULL")[2]
    ms<-nrow(my_data)-ph-gf-rc-aud
    #godzina
    pop_hour<-my_data %>% 
      group_by(hour) %>% 
      summarise(n=n()) %>% 
      arrange(-n) %>% 
      head(1)
    

    paste("You have selected: ", input$fighter,'\n',
          " Total number of messages: ",ms,'\n',
          " Total number of photos:",ph,'\n',
          " Total number of gifs:",gf,'\n',
          " Total number of reactions:",rc,'\n',
          " Total number of audio files:",aud,'\n',
          " Most messages with: ",'\n',
          " Most common hour for you to write: ",pop_hour,'\n',
          " More messages send in private or group chats: ",'\n',
          " Longest 'xd' length: ",'\n',
          " Longest 'haha' length: ",'\n',
          " How much do you course: ",'\n',
          " How many persons you ve ghosted: "
          )
  })
}

shinyApp(ui, server)
