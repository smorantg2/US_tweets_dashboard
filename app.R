
# Especificamos las librerías necesarias en esta lista
packages1 = c("shiny","tidyverse","plotly","rjson","rio","viridis","lubridate","RColorBrewer","usmap","remotes", "shinydashboard","corrplot")
packages2 = c("fontawesome")
options(warn = -1)

package.check1 <- lapply(packages1, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
    }
})

package.check2 <- lapply(packages2, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
        remotes::install_github(paste0("rstudio/",x))
        library(x, character.only = TRUE)
    }
})

#Importacion de los datos
governors<-rio::import("./data/Governors.RData")
tweets<-rio::import("./data/Tweets.RData")

Tweets_plot <-rio::import("./data/Tweets_plot.RData")

governors$user <- gsub(pattern = "@", governors$user, replace= "")
names(governors)[names(governors)=="State"] <- "full"

df.aux<-statepop %>% left_join(governors, by = "full")
df.aux[is.na(df.aux$Party),match("Party", names(df.aux))]<-"Democratic"

df.Vars <- read.csv("./data/dfVars.csv",sep=",",header=T)
df.Vars$Fecha<-as.Date(df.Vars$Fecha)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
df.cor <- df.Vars %>% dplyr::select(-Fecha)

ui <- fluidPage(
    '',
    includeCSS("www/styles.css"),
    navbarPage(h4("DASHBOARD", icon("bar-chart-o"), class="h4-title"),
               tabPanel(h4("Tweets", class="h4"),
                        sidebarLayout(
                          
                          sidebarPanel(
                            h4("Selección de características del tweet", class = "h4"),
                            selectInput("SelectParty", choices =sort(unique(Tweets_plot$Party)), label = "Seleccione partido político"),
                            selectInput("SelectSentiment", choices = c("Negativo", "Positivo"), label = "Seleccione sentimiento"),
                            dateRangeInput("RangoFechas", "Filtrar fecha en intervalo", 
                                           start = "2020-02-01", end = "2020-06-01", min ="2020-02-01", 
                                           max = "2020-06-01", format = "yyyy-mm-dd", weekstart = 1,
                                           language = "es", separator = "a")
                          ),
                          mainPanel(
                            shinydashboard::box(
                              title = "Tweet",
                              status = "warning",
                              background = "blue",
                              
                              br(),
                              htmlOutput("picture", inline = TRUE),textOutput("govname", inline = TRUE),
                              tags$head(tags$style("#usertweet{color: blue;
                                 font-size: 14px;
                                 font-style: italic;
                                 font-family: Arial;
                                 }")),
                              h5(""),
                              textOutput("usertweet", inline = TRUE),textOutput("stategov",inline = TRUE),
                              tags$head(tags$style("#govname{color: black;
                                 font-size: 17px;
                                 font-style: bold;
                                 }")),
                              tags$head(tags$style("#stategov{color: black;
                                 font-size: 14px;
                                 font-style: bold;
                                 }")),
                              h5(""),
                              textOutput("tweet_text"),
                              tags$head(tags$style("#tweet_text{color: black;
                                 font-size: 14px;
                                 font-style: bold;
                                 }")),
                              h5(""),
                              textOutput("tweetdate"),
                              tags$head(tags$style("#tweetdate{color: gray;
                                 font-size: 12px;
                                 font-style: italic;
                                 }")),
                              br(),
                              textOutput("sentiment"),
                              tags$head(tags$style("#sentiment{color: green;
                                 font-size: 12px;
                                 font-style: bold;
                                 }")),
                              br()
                            ),
                            tags$script(HTML("$('.box').eq(0).css('border', '0px solid #00aced');"))
                          )
                        )
               ),
               
               tabPanel(h4("Variables", class="h4"),
                        sidebarLayout(
                          sidebarPanel(
                            h4("Histograma de Positividad", icon("far fa-thumbs-up"), class = "h4"),
                            selectInput("PartyHist", choices =sort(unique(Tweets_plot$Party)), label = "Seleccione partido político"),
                            br(),
                            br(),
                            h4("Correlaciones entre las variables"),
                            plotOutput("correlations"),
                            br(),
                            br(),
                            h4("Visualización de variables", icon("fas fa-angle-right"), class = "h4"),
                            selectInput("SelectVar", choices=c("Sentimiento de Demócratas",
                                                               "Sentimiento de Republicanos",
                                                               "Sentimiento del presidente Trump",
                                                               "Tweets de Demócratas",
                                                               "Tweets de Republicanos",
                                                               "Tweets del presidente Trump",
                                                               "Dólar",
                                                               "Dow Jones",
                                                               "Casos de COVID-19"
                                                               ), label = "Seleccione variable")
                          ),
                          mainPanel(
                            h3("Histograma de Positividad", align = "center"),
                            plotOutput("histoSentiment"),
                            br(),
                            downloadButton("downloadHistogram", "Download", class="btn-file"),
                            br(),
                            br(),
                            h3("Visualización de variables", align = "center"),
                            plotOutput("plotVariables"),
                            br(),
                            downloadButton("downloadVariable", "Download", class="btn-file"),
                            br(),
                            br(),
                            br()
                          )
                        )
               ),
               
               tabPanel(h4("Evolución", class="h4"),
                        sidebarLayout(
                          sidebarPanel(
                            h4("Evolución temporal", icon("fas fa-chart-line"),class = "h4"),
                            checkboxInput("separar", label = "Separar por partidos políticos", value = FALSE),
                            checkboxInput("suavizar", label = "Aplicar suavizado", value = FALSE),
                            width = 3
                          ),
                          mainPanel(
                            h3("Evolución del sentimiento", align = "center"),
                            plotOutput("evolutionSentiment"),
                            br(),
                            downloadButton("downloadSentiment", "Download", class="btn-file"),
                            br(),
                            br(),
                            h3("Evolución del número de tweets", align = "center"),
                            plotOutput("evolutionTweets"),
                            br(),
                            downloadButton("downloadTweets", "Download", class="btn-file"),
                            br(),
                            br(),
                            br()
                          )
                        )
               ),
               
               tabPanel(h4("Mapas", class="h4"),
                        sidebarLayout(
                          sidebarPanel(
                            h4("Mapas", icon("fas fa-map-marker-alt"), class = "h4"),
                            radioButtons("var.map", "Variable a visualizar:",
                                         c("Número de Tweets",
                                           "Grado de Sentimiento")
                            ),
                            dateRangeInput("RangoFechasMapa", "Filtrar fecha en intervalo", 
                                           start = "2020-02-01", end = "2020-06-01", min ="2020-02-01", 
                                           max = "2020-06-01", format = "yyyy-mm-dd", weekstart = 1,
                                           language = "es", separator = "a"),
                            width = 2
                          ),
                          mainPanel(
                            fluidRow(
                              column(width=6,
                                     h3("Mapa por partido político", align = "center"),
                                     plotOutput("politicMap"),
                                     downloadButton("downloadPoliticMap", "Download", class="btn-file")),
                              column(width=6,
                                     uiOutput("title_map"),
                                     plotOutput("varMap"),
                                     downloadButton("downloadVarMap", "Download", class="btn-file")),
                              br(),
                              br()
                            )
                          )
                        )
               )
    )
    
)

server <- function(input, output) {
  
  #================================= ZONA MAPAS ==================================
    
    #Mapa político
    output$politicMap<-renderPlot({
        usmap::plot_usmap(data = df.aux, values = "Party", color="white") + 
                     scale_fill_manual(name="Partido político",values=c("#1c5ea7ff","#c32839")) +
                     theme_void() +
                     theme(legend.position = "right")
        
    })
    
    #Download map 1
    output$downloadPoliticMap<-downloadHandler(
        filename=function() {
            paste0("Politic Map",".png")
        },
        content=function(file) {
            ggsave(filename=file,
                   plot=usmap::plot_usmap(data = df.aux, values = "Party", color="white") + 
                     scale_fill_manual(name="Partido político",values=c("#1c5ea7ff","#c32839")) +
                     ggtitle("Mapa por partido político") +
                     theme_void() +
                     theme(legend.position = "right")
                   , width = 40, height = 20, units="cm")
        }
    )
    
    #Título del mapa
    output$title_map <- renderUI({
      if(input$var.map == "Número de Tweets") {
        h3("Mapa por número de tweets", align = "center")
      }
      else {
        h3("Mapa por sentimiento", align = "center")
      }
    })
    
    DF_map <- eventReactive(c(input$var.map, input$RangoFechasMapa), {
      df.filter <- Tweets_plot %>% filter(created_at >= input$RangoFechasMapa[1] & created_at <= input$RangoFechasMapa[2])
      
      if(input$var.map == "Número de Tweets") {
        df.aux2 <- df.filter %>% count(Governor)
        df.aux2 <- governors %>% left_join(df.aux2, by = "Governor")
        df.aux2 <- statepop %>% left_join(df.aux2, by = "full")
        return(df.aux2)
      }
      else {
        df.aux2 <- df.filter %>% select(Governor, created_at, sentiment) %>% group_by(Governor) %>% 
        summarize_all(.funs = mean)
        names(df.aux2)[names(df.aux2)=="sentiment"] <- "n"
        df.aux2 <- governors %>% left_join(df.aux2, by = "Governor")
        df.aux2 <- statepop %>% left_join(df.aux2, by = "full")
        return(df.aux2)
      }
    })
    
    #Mapa por variable
    output$varMap<-renderPlot({
      if(input$var.map == "Número de Tweets") {
        usmap::plot_usmap(data = DF_map(), values = "n", color="white") + 
          scale_fill_viridis_c(name=input$var.map, option="magma", direction=-1, begin=0.2, end=0.9) +
          theme_void() +
          theme(legend.position = "right")
      }
      else {
        usmap::plot_usmap(data = DF_map(), values = "n", color="white") + 
          scale_fill_gradient2(low="seagreen4", mid="seagreen2", high="lightgreen", midpoint=0.25) +
          labs(fill = "Grado de sentimiento") +
          theme_void() +
          theme(legend.position = "right")
      }
      
    })
    
    #Download map 2
    output$downloadVarMap<-downloadHandler(
      filename=function() {
        paste0(input$var.map," Mapa",".png")
      },
      content=function(file) {
        ggsave(filename=file,
               if(input$var.map == "Número de Tweets") {
                 plot=usmap::plot_usmap(data = DF_map(), values = "n", color="white") + 
                   scale_fill_viridis_c(name=input$var.map, option="magma", direction=-1, begin=0.2, end=0.9) +
                   theme_void() +
                   theme(legend.position = "right")
               }
               else {
                 plot=usmap::plot_usmap(data = DF_map(), values = "n", color="white") + 
                   scale_fill_gradient2(low="seagreen4", mid="seagreen2", high="lightgreen", midpoint=0.25) +
                   labs(fill = "Grado de sentimiento") +
                   ggtitle(paste0("Mapa por ", input$var.map)) +
                   theme_void() +
                   theme(legend.position = "right")
               }
               , width = 40, height = 20, units="cm")
      }
    )
    
    #=========================== ZONA EVOLUCIÓN =====================================
    
    DF_evolucion <- eventReactive(c(input$separar, input$suavizar),{
      
      if (input$separar == TRUE){
        partidos <- Tweets_plot %>% select(Party, created_at, sentiment) %>% group_by(Party, created_at) %>% 
          summarize_all(.funs = mean)
        n.ts<-Tweets_plot %>% select(Party, created_at, text) %>% group_by(Party, created_at) %>% count()
        df.e<-partidos %>% left_join(n.ts, by = c("Party","created_at"))
        df.e$n <- as.numeric(df.e$n)
        df.e[df.e$Party == "Demócrata",]$n <-df.e[df.e$Party == "Demócrata",]$n/24
        df.e[df.e$Party == "Republicano",]$n <-df.e[df.e$Party == "Republicano",]$n/27
        df.e
      }
      else{
        partidos <- Tweets_plot %>% select(created_at, sentiment) %>% group_by(created_at) %>% summarize_all(.funs = mean)
        n.ts<-Tweets_plot %>% select(created_at, text) %>% group_by(created_at) %>% count()
        df.e<-partidos %>% left_join(n.ts, by = "created_at")
        df.e$n <- as.numeric(df.e$n/51)
        df.e
        
      }
      
    })
    
    output$evolutionSentiment <- renderPlot({
      
      if (dim(DF_evolucion())[2] == 4){
        
        if(input$suavizar == FALSE){
      
        ploot <- ggplot(DF_evolucion(), aes(x = created_at, y = sentiment))+
          geom_line(aes(group = Party, color = Party), alpha = 0.85, size = 1)+
          geom_point(aes(group = Party, color = Party), size = 0.8)+
          #geom_smooth(aes(group = Party, color = Party), alpha = 0.5, size = 0.75)+
          scale_color_manual(values=c("#6549D7", "#5A5A5A","#FF4040"))+
          theme(axis.text.x = element_text(angle = 45))+
          theme_minimal()+
          xlab("Fecha")+
          ylab("Positividad")+ylim(c(-0.15, 0.5))
        }
        else{
          ploot <- ggplot(DF_evolucion(), aes(x = created_at, y = sentiment))+
            #geom_line(aes(group = Party, color = Party), alpha = 0.85, size = 1)+
            #geom_point(aes(group = Party, color = Party), size = 0.8)+
            geom_smooth(aes(group = Party, color = Party), alpha = 0.5, size = 0.75, span = 0.75, se = FALSE)+
            scale_color_manual(values=c("#6549D7", "#5A5A5A","#FF4040"))+
            theme(axis.text.x = element_text(angle = 45))+
            theme_minimal()+
            xlab("Fecha")+
            ylab("Positividad")+ylim(c(-0.15, 0.5))
        }
      }
      else{
        
        if(input$suavizar == FALSE){
          ploot <- ggplot(DF_evolucion(),aes(x = created_at, y = sentiment))+
            geom_line(group = 1,alpha = 0.85, size = 1, color = "#5A5A5A")+
            theme(axis.text.x = element_text(angle = 45))+
            theme_minimal()+
            xlab("Fecha")+
            ylab("Positividad")+
            ylim(c(-0.15, 0.5))
        }
        else{
          ploot <- ggplot(DF_evolucion(),aes(x = created_at, y = sentiment))+
            geom_smooth(group = 1,alpha = 0.85, size = 1, color = "#5A5A5A",span = 0.75, se = FALSE)+
            theme(axis.text.x = element_text(angle = 45))+
            theme_minimal()+
            xlab("Fecha")+
            ylab("Positividad")+
            ylim(c(-0.15, 0.5))
        }
      }
        ploot + annotate("point", x=as.Date("2020-02-05"), y=0.45)+
          annotate("text", x=as.Date("2020-02-05"), y=0.425, label= "Trump Impeachment", size = 3)+
          annotate("point", x=as.Date("2020-02-27"), y=0.45)+
          annotate("text", x=as.Date("2020-02-27"), y=0.425, label= "10 muertes COVID-19", size = 3)+
          annotate("text", x=as.Date("2020-02-27"), y=0.4, label= "1ª caída Dow Jones", size = 3)+
          annotate("point", x=as.Date("2020-03-09"), y=0.5)+
          annotate("text", x=as.Date("2020-03-09"), y=0.475, label= "Black Monday 2020", size = 3)+
          annotate("point", x=as.Date("2020-03-13"), y=0.35)+
          annotate("text", x=as.Date("2020-03-13"), y=0.325, label= "Emergencia Nacional", size = 3)+
          annotate("point", x=as.Date("2020-04-11"), y=0.45)+
          annotate("text", x=as.Date("2020-04-11"), y=0.425, label= "1 país en Nº de casos", size = 3)+
          annotate("point", x=as.Date("2020-04-24"), y=0.5)+
          annotate("text", x=as.Date("2020-04-24"), y=0.475, label= "Polémica desinfectante", size = 3)+
          annotate("point", x=as.Date("2020-05-19"), y=0.45)+
          annotate("text", x=as.Date("2020-05-19"), y=0.425, label= "1,5M casos y 90k muertos", size = 3)+
          annotate("point", x=as.Date("2020-05-26"), y=0.5)+
          annotate("text", x=as.Date("2020-05-26"), y=0.475, label= "Asesinato policial George Floyd", size = 3)
      
    })
    
    #Download Sentiment
    output$downloadSentiment<-downloadHandler(
      
      filename=function() {
        paste0("EvolucionSentimiento",".png")
      },
      
      content=function(file) {
        
        if (dim(DF_evolucion())[2] == 4){
          
          if(input$suavizar == FALSE){
            
            ploot <- ggplot(DF_evolucion(), aes(x = created_at, y = sentiment))+
              geom_line(aes(group = Party, color = Party), alpha = 0.85, size = 1)+
              geom_point(aes(group = Party, color = Party), size = 0.8)+
              #geom_smooth(aes(group = Party, color = Party), alpha = 0.5, size = 0.75)+
              scale_color_manual(values=c("#6549D7", "#5A5A5A","#FF4040"))+
              ggtitle("Evolución del sentimiento") +
              theme(axis.text.x = element_text(angle = 45))+
              theme_minimal()+
              xlab("Fecha")+
              ylab("Positividad")+ylim(c(-0.15, 0.5))
          }
          else{
            ploot <- ggplot(DF_evolucion(), aes(x = created_at, y = sentiment))+
              #geom_line(aes(group = Party, color = Party), alpha = 0.85, size = 1)+
              #geom_point(aes(group = Party, color = Party), size = 0.8)+
              geom_smooth(aes(group = Party, color = Party), alpha = 0.5, size = 0.75, span = 0.75, se = FALSE)+
              ggtitle("Evolución del sentimiento") +
              scale_color_manual(values=c("#6549D7", "#5A5A5A","#FF4040"))+
              theme(axis.text.x = element_text(angle = 45))+
              theme_minimal()+
              xlab("Fecha")+
              ylab("Positividad")+ylim(c(-0.15, 0.5))
          }
        }
        else{
          
          if(input$suavizar == FALSE){
            ploot <- ggplot(DF_evolucion(),aes(x = created_at, y = sentiment))+
              geom_line(group = 1,alpha = 0.85, size = 1, color = "#5A5A5A")+
              ggtitle("Evolución del sentimiento") +
              theme(axis.text.x = element_text(angle = 45))+
              theme_minimal()+
              xlab("Fecha")+
              ylab("Positividad")+
              ylim(c(-0.15, 0.5))
          }
          else{
            ploot <- ggplot(DF_evolucion(),aes(x = created_at, y = sentiment))+
              geom_smooth(group = 1,alpha = 0.85, size = 1, color = "#5A5A5A",span = 0.75, se = FALSE)+
              ggtitle("Evolución del sentimiento") +
              theme(axis.text.x = element_text(angle = 45))+
              theme_minimal()+
              xlab("Fecha")+
              ylab("Positividad")+
              ylim(c(-0.15, 0.5))
          }
        }
        
        ploot + annotate("point", x=as.Date("2020-02-05"), y=0.45)+
          annotate("text", x=as.Date("2020-02-05"), y=0.425, label= "Trump Impeachment", size = 3)+
          annotate("point", x=as.Date("2020-02-27"), y=0.45)+
          annotate("text", x=as.Date("2020-02-27"), y=0.425, label= "10 muertes COVID-19", size = 3)+
          annotate("text", x=as.Date("2020-02-27"), y=0.4, label= "1ª caída Dow Jones", size = 3)+
          annotate("point", x=as.Date("2020-03-09"), y=0.5)+
          annotate("text", x=as.Date("2020-03-09"), y=0.475, label= "Black Monday 2020", size = 3)+
          annotate("point", x=as.Date("2020-03-13"), y=0.35)+
          annotate("text", x=as.Date("2020-03-13"), y=0.325, label= "Emergencia Nacional", size = 3)+
          annotate("point", x=as.Date("2020-04-11"), y=0.45)+
          annotate("text", x=as.Date("2020-04-11"), y=0.425, label= "1 país en Nº de casos", size = 3)+
          annotate("point", x=as.Date("2020-04-24"), y=0.5)+
          annotate("text", x=as.Date("2020-04-24"), y=0.475, label= "Polémica desinfectante", size = 3)+
          annotate("point", x=as.Date("2020-05-19"), y=0.45)+
          annotate("text", x=as.Date("2020-05-19"), y=0.425, label= "1,5M casos y 90k muertos", size = 3)+
          annotate("point", x=as.Date("2020-05-26"), y=0.5)+
          annotate("text", x=as.Date("2020-05-26"), y=0.475, label= "Asesinato policial George Floyd", size = 3)
        
        ggsave(filename=file,
               plot=ploot
               , width = 40, height = 20, units="cm")
      }
    )
    
    output$evolutionTweets <- renderPlot({
      
      if (dim(DF_evolucion())[2] == 4){
        
        if(input$suavizar == FALSE){
          
          ploot <- ggplot(DF_evolucion(), aes(x = created_at, y = n))+
            geom_line(aes(group = Party, color = Party), alpha = 0.85, size = 1)+
            geom_point(aes(group = Party, color = Party), size = 0.8)+
            #geom_smooth(aes(group = Party, color = Party), alpha = 0.5, size = 0.75)+
            scale_color_manual(values=c("#6549D7", "#5A5A5A","#FF4040"))+
            theme(axis.text.x = element_text(angle = 45))+
            theme_minimal()+
            xlab("Fecha")
            ylab("Número de tweets")#+
            #ylim(c(0, 100))
        }
        else{
          ploot <- ggplot(DF_evolucion(), aes(x = created_at, y = n))+
            #geom_line(aes(group = Party, color = Party), alpha = 0.85, size = 1)+
            #geom_point(aes(group = Party, color = Party), size = 0.8)+
            geom_smooth(aes(group = Party, color = Party), alpha = 0.5, size = 0.75, span = 0.75, se = FALSE)+
            scale_color_manual(values=c("#6549D7", "#5A5A5A","#FF4040"))+
            theme(axis.text.x = element_text(angle = 45))+
            theme_minimal()+
            xlab("Fecha")
            ylab("Número de tweets")#+
            #ylim(c(0, 100))
        }
      }
      else{
        
        if(input$suavizar == FALSE){
          ploot <- ggplot(DF_evolucion(),aes(x = created_at, y = n))+
            geom_line(group = 1,alpha = 0.85, size = 1, color = "#5A5A5A")+
            theme(axis.text.x = element_text(angle = 45))+
            theme_minimal()+
            xlab("Fecha")+
            ylab("Número de tweets")#+
            #ylim(c(0, 100))
        }
        else{
          ploot <- ggplot(DF_evolucion(),aes(x = created_at, y = n))+
            geom_smooth(group = 1,alpha = 0.85, size = 1, color = "#5A5A5A",span = 0.75, se = FALSE)+
            theme(axis.text.x = element_text(angle = 45))+
            theme_minimal()+
            xlab("Fecha")+
            ylab("Número de tweets")#+
            #ylim(c(0, 100))
        }
      }
      
      ploot + annotate("point", x=as.Date("2020-02-05"), y=150)+
        annotate("text", x=as.Date("2020-02-05"), y=141, label= "Trump Impeachment", size = 3)+
        annotate("point", x=as.Date("2020-02-27"), y=140)+
        annotate("text", x=as.Date("2020-02-27"), y=131, label= "10 muertes COVID-19", size = 3)+
        annotate("text", x=as.Date("2020-02-27"), y=121, label= "1ª caída Dow Jones", size = 3)+
        annotate("point", x=as.Date("2020-03-09"), y=120)+
        annotate("text", x=as.Date("2020-03-09"), y=111, label= "Black Monday 2020", size = 3)+
        annotate("point", x=as.Date("2020-03-13"), y=175)+
        annotate("text", x=as.Date("2020-03-13"), y=166, label= "Emergencia Nacional", size = 3)+
        annotate("point", x=as.Date("2020-04-11"), y=175)+
        annotate("text", x=as.Date("2020-04-11"), y=166, label= "1 país en Nº de casos", size = 3)+
        annotate("point", x=as.Date("2020-04-24"), y=135)+
        annotate("text", x=as.Date("2020-04-24"), y=126, label= "Polémica desinfectante", size = 3)+
        annotate("point", x=as.Date("2020-05-19"), y=175)+
        annotate("text", x=as.Date("2020-05-19"), y=166, label= "1,5M casos y 90k muertos", size = 3)+
        annotate("point", x=as.Date("2020-05-26"), y=155)+
        annotate("text", x=as.Date("2020-05-26"), y=146, label= "Asesinato policial George Floyd", size = 3)
      
      
    })
    
    #Download Numero de Tweets
    output$downloadTweets<-downloadHandler(
      
      filename=function() {
        paste0("EvolucionTweets",".png")
      },
      content=function(file) {
        
        if (dim(DF_evolucion())[2] == 4){
          
          if(input$suavizar == FALSE){
            
            ploot <- ggplot(DF_evolucion(), aes(x = created_at, y = n))+
              geom_line(aes(group = Party, color = Party), alpha = 0.85, size = 1)+
              geom_point(aes(group = Party, color = Party), size = 0.8)+
              #geom_smooth(aes(group = Party, color = Party), alpha = 0.5, size = 0.75)+
              ggtitle("Evolución del número de tweets") +
              scale_color_manual(values=c("#6549D7", "#5A5A5A","#FF4040"))+
              theme(axis.text.x = element_text(angle = 45))+
              theme_minimal()+
              xlab("Fecha")
            ylab("Número de tweets")#+
            #ylim(c(0, 100))
          }
          else{
            ploot <- ggplot(DF_evolucion(), aes(x = created_at, y = n))+
              #geom_line(aes(group = Party, color = Party), alpha = 0.85, size = 1)+
              #geom_point(aes(group = Party, color = Party), size = 0.8)+
              geom_smooth(aes(group = Party, color = Party), alpha = 0.5, size = 0.75, span = 0.75, se = FALSE)+
              ggtitle("Evolución del número de tweets") +
              scale_color_manual(values=c("#6549D7", "#5A5A5A","#FF4040"))+
              theme(axis.text.x = element_text(angle = 45))+
              theme_minimal()+
              xlab("Fecha")
            ylab("Número de tweets")#+
            #ylim(c(0, 100))
          }
        }
        else{
          
          if(input$suavizar == FALSE){
            ploot <- ggplot(DF_evolucion(),aes(x = created_at, y = n))+
              geom_line(group = 1,alpha = 0.85, size = 1, color = "#5A5A5A")+
              ggtitle("Evolución del número de tweets") +
              theme(axis.text.x = element_text(angle = 45))+
              theme_minimal()+
              xlab("Fecha")+
              ylab("Número de tweets")#+
            #ylim(c(0, 100))
          }
          else{
            ploot <- ggplot(DF_evolucion(),aes(x = created_at, y = n))+
              geom_smooth(group = 1,alpha = 0.85, size = 1, color = "#5A5A5A",span = 0.75, se = FALSE)+
              ggtitle("Evolución del número de tweets") +
              theme(axis.text.x = element_text(angle = 45))+
              theme_minimal()+
              xlab("Fecha")+
              ylab("Número de tweets")#+
            #ylim(c(0, 100))
          }
        }
        
        ploot + annotate("point", x=as.Date("2020-02-05"), y=150)+
          annotate("text", x=as.Date("2020-02-05"), y=141, label= "Trump Impeachment", size = 3)+
          annotate("point", x=as.Date("2020-02-27"), y=140)+
          annotate("text", x=as.Date("2020-02-27"), y=131, label= "10 muertes COVID-19", size = 3)+
          annotate("text", x=as.Date("2020-02-27"), y=121, label= "1ª caída Dow Jones", size = 3)+
          annotate("point", x=as.Date("2020-03-09"), y=120)+
          annotate("text", x=as.Date("2020-03-09"), y=111, label= "Black Monday 2020", size = 3)+
          annotate("point", x=as.Date("2020-03-13"), y=175)+
          annotate("text", x=as.Date("2020-03-13"), y=166, label= "Emergencia Nacional", size = 3)+
          annotate("point", x=as.Date("2020-04-11"), y=175)+
          annotate("text", x=as.Date("2020-04-11"), y=166, label= "1 país en Nº de casos", size = 3)+
          annotate("point", x=as.Date("2020-04-24"), y=135)+
          annotate("text", x=as.Date("2020-04-24"), y=126, label= "Polémica desinfectante", size = 3)+
          annotate("point", x=as.Date("2020-05-19"), y=175)+
          annotate("text", x=as.Date("2020-05-19"), y=166, label= "1,5M casos y 90k muertos", size = 3)+
          annotate("point", x=as.Date("2020-05-26"), y=155)+
          annotate("text", x=as.Date("2020-05-26"), y=146, label= "Asesinato policial George Floyd", size = 3)
        
        ggsave(filename=file,
               plot=ploot
               , width = 40, height = 20, units="cm")
      }
    )
    
    #============================= ZONA HISTOGRAMA ==================================
    
    DF_hist <- eventReactive(c(input$PartyHist),{
      
      hist <- Tweets_plot %>% filter(Party == input$PartyHist)
      hist
      
    })
    
    
    
    output$histoSentiment <- renderPlot({
    
      if (input$PartyHist == "Demócrata"){
        
        color_hist <- "#6549D7"
      }
      
      if (input$PartyHist == "Republicano"){
        
        color_hist <- "#FF4040"
      }
      
      if (input$PartyHist == "Presidente de EEUU"){
        
        color_hist <- "#5A5A5A"
      }
      
      
      ggplot(DF_hist(), aes(x = sentiment, fill = Party))+
        geom_histogram(aes(y = ..density..),color="#e9ecef", alpha=0.6, position = 'identity', bins = 25)+
        scale_fill_manual(values=color_hist) +
        theme_minimal() +
        labs(fill="")+
        xlim(c(-1.5,1.5))+
        ylim(c(0,2))+
        xlab("Positividad")+
        ylab("Frecuencia")
      
    })
    
    #Download Histograma
    output$downloadHistogram<-downloadHandler(
      filename=function() {
        paste0("Histograma",".png")
      },
      content=function(file) {
        
        if (input$PartyHist == "Demócrata"){
          
          color_hist <- "#6549D7"
        }
        
        if (input$PartyHist == "Republicano"){
          
          color_hist <- "#FF4040"
        }
        
        if (input$PartyHist == "Presidente de EEUU"){
          
          color_hist <- "#5A5A5A"
        }
        
        ggsave(filename=file,
               plot=ggplot(DF_hist(), aes(x = sentiment, fill = Party))+
                 geom_histogram(aes(y = ..density..),color="#e9ecef", alpha=0.6, position = 'identity', bins = 25)+
                 scale_fill_manual(values=color_hist) +
                 ggtitle("Histograma de Positividad") +
                 theme_minimal() +
                 labs(fill="")+
                 xlim(c(-1.5,1.5))+
                 ylim(c(0,2))+
                 xlab("Positividad")+
                 ylab("Frecuencia")
               , width = 40, height = 20, units="cm")
      }
    )
    
    output$correlations <- renderPlot({
      corrplot(corr = cor(x = df.cor, method = "pearson"), 
               method = "color",
               col=col(200), tl.srt=45,
               tl.col="black")
    })
    
    
    #============================ ZONA TWEETS =======================================
    
    DF_printweet <- eventReactive(c(input$SelectParty, input$RangoFechas, input$SelectSentiment), {
      
      df <- Tweets_plot %>% filter(Party == input$SelectParty)
      
      if (input$SelectSentiment == "Positivo"){
        df <- df %>% filter(created_at >= input$RangoFechas[1] & created_at <= input$RangoFechas[2])
        df <- df[df$sentiment == max(df$sentiment),]
        return(df[1,])
      }
      else if (input$SelectSentiment == "Negativo"){
        df <- df %>% filter(created_at >= input$RangoFechas[1] & created_at <= input$RangoFechas[2])
        df <- df[df$sentiment == min(df$sentiment),]
        return(df[1,])
      }
      
    })
    
    output$picture <- renderText({c('<img src="', DF_printweet()$profile_image_url,'">')})
    
    output$usertweet <- renderText(paste0("@",DF_printweet()$user)) #Usuario
    
    output$govname <- renderText(DF_printweet()$Governor)         #Nombre del gobernador
    output$stategov <- renderText(paste0(" - ",paste0(DF_printweet()$State),":")  )          #Estado del gobernador
    output$tweet_text <- renderText(DF_printweet()$text)             #Texto del Tweet
    #DF_printweet()$is_retweet       #ES RT?
    output$tweetdate <- renderText(paste0("Fecha: ",as.character(DF_printweet()$created_at)))     #Fecha
    output$sentiment <- renderText(paste0("Positividad: ",round(DF_printweet()$sentiment,2)))
    
    
    output$plotVariables <- renderPlot({

      if(input$SelectVar == "Sentimiento de Demócratas") {
        ggplot(df.Vars, aes(x=Fecha, y=dem.sent)) +
          geom_line(color="#3b3b3b") +
          ggtitle("Positividad del partido demócrata") +
          xlab("Fecha") +
          ylab("Positividad") +
          theme_minimal()
      } else if(input$SelectVar == "Sentimiento de Republicanos") {
        ggplot(df.Vars, aes(x=Fecha, y=rep.sent)) +
          geom_line(color="#3b3b3b") +
          ggtitle("Positividad del partido republicano") +
          xlab("Fecha") +
          ylab("Positividad") +
          theme_minimal()
      } else if(input$SelectVar == "Sentimiento del presidente Trump") {
        ggplot(df.Vars, aes(x=Fecha, y=trump.sent)) +
          geom_line(color="#3b3b3b") +
          ggtitle("Positividad del presidente Trump") +
          xlab("Fecha") +
          ylab("Positividad") +
          theme_minimal()
      } else if(input$SelectVar == "Tweets de Demócratas") {
        ggplot(df.Vars, aes(x=Fecha, y=dem.ntweets)) +
          geom_line(color="#3b3b3b") +
          ggtitle("Número de tweets del partido demócrata") +
          xlab("Fecha") +
          ylab("Número de tweets") +
          theme_minimal()
      } else if(input$SelectVar == "Tweets de Republicanos") {
        ggplot(df.Vars, aes(x=Fecha, y=rep.ntweets)) +
          geom_line(color="#3b3b3b") +
          ggtitle("Número de tweets del partido republicano") +
          xlab("Fecha") +
          ylab("Número de tweets") +
          theme_minimal()
      } else if(input$SelectVar == "Tweets del presidente Trump") {
        ggplot(df.Vars, aes(x=Fecha, y=trump.ntweets)) +
          geom_line(color="#3b3b3b") +
          ggtitle("Número de tweets del presidente Trump") +
          xlab("Fecha") +
          ylab("Número de tweets") +
          theme_minimal()
      } else if(input$SelectVar == "Dólar") {
        ggplot(df.Vars, aes(x=Fecha, y=dollar)) +
          geom_line(color="#3b3b3b") +
          ggtitle("Evolución temporal de la tasa de cambio del Dólar respecto al Euro") +
          xlab("Fecha") +
          ylab("Valor") +
          theme_minimal()
      } else if(input$SelectVar == "Dow Jones") {
        ggplot(df.Vars, aes(x=Fecha, y=dow.jones)) +
          geom_line(color="#3b3b3b") +
          ggtitle("Evolución temporal del Dow Jones") +
          ylab("Valor de cierre") +
          theme_minimal()
      } else {
        ggplot(df.Vars, aes(x=Fecha, y=covid.cases)) +
          geom_line(color="#3b3b3b") +
          ggtitle("Evolución temporal de los casos de COVID-19") +
          ylab("Casos") +
          theme_minimal()
      }
      
    })
    
    #Download Plot de las Variables
    output$downloadVariable<-downloadHandler(
      
      filename=function() {
        paste0(input$SelectVar,".png")
      },
      content=function(file) {
        
        if(input$SelectVar == "Sentimiento de Demócratas") {
          plotV=ggplot(df.Vars, aes(x=Fecha, y=dem.sent)) +
            geom_line(color="#3b3b3b") +
            ggtitle("Positividad del partido demócrata") +
            xlab("Fecha") +
            ylab("Positividad") +
            theme_minimal()
        } else if(input$SelectVar == "Sentimiento de Republicanos") {
          plotV=ggplot(df.Vars, aes(x=Fecha, y=rep.sent)) +
            geom_line(color="#3b3b3b") +
            ggtitle("Positividad del partido republicano") +
            xlab("Fecha") +
            ylab("Positividad") +
            theme_minimal()
        } else if(input$SelectVar == "Sentimiento del presidente Trump") {
          plotV=ggplot(df.Vars, aes(x=Fecha, y=trump.sent)) +
            geom_line(color="#3b3b3b") +
            ggtitle("Positividad del presidente Trump") +
            xlab("Fecha") +
            ylab("Positividad") +
            theme_minimal()
        } else if(input$SelectVar == "Tweets de Demócratas") {
          plotV=ggplot(df.Vars, aes(x=Fecha, y=dem.ntweets)) +
            geom_line(color="#3b3b3b") +
            ggtitle("Número de tweets del partido demócrata") +
            xlab("Fecha") +
            ylab("Número de tweets") +
            theme_minimal()
        } else if(input$SelectVar == "Tweets de Republicanos") {
          plotV=ggplot(df.Vars, aes(x=Fecha, y=rep.ntweets)) +
            geom_line(color="#3b3b3b") +
            ggtitle("Número de tweets del partido republicano") +
            xlab("Fecha") +
            ylab("Número de tweets") +
            theme_minimal()
        } else if(input$SelectVar == "Tweets del presidente Trump") {
          plotV=ggplot(df.Vars, aes(x=Fecha, y=trump.ntweets)) +
            geom_line(color="#3b3b3b") +
            ggtitle("Número de tweets del presidente Trump") +
            xlab("Fecha") +
            ylab("Número de tweets") +
            theme_minimal()
        } else if(input$SelectVar == "Dólar") {
          plotV=ggplot(df.Vars, aes(x=Fecha, y=dollar)) +
            geom_line(color="#3b3b3b") +
            ggtitle("Evolución temporal de la tasa de cambio del Dólar respecto al Euro") +
            xlab("Fecha") +
            ylab("Valor") +
            theme_minimal()
        } else if(input$SelectVar == "Dow Jones") {
          plotV=ggplot(df.Vars, aes(x=Fecha, y=dow.jones)) +
            geom_line(color="#3b3b3b") +
            ggtitle("Evolución temporal del Dow Jones") +
            ylab("Valor de cierre") +
            theme_minimal()
        } else {
          plotV=ggplot(df.Vars, aes(x=Fecha, y=covid.cases)) +
            geom_line(color="#3b3b3b") +
            ggtitle("Evolución temporal de los casos de COVID-19") +
            ylab("Casos") +
            theme_minimal()
        }
        
        ggsave(filename=file,
               plot=plotV
               ,width = 40, height = 20, units="cm")
      }
    )
    
}

shinyApp(ui = ui, server = server)
