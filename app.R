# Load R packages
library(shiny)
library(shinythemes)
library(DT)
library(readr)
library(ggplot2)
library(dplyr)
library(sf)
library(tidyr)
library(rgeos)
library(rgdal)
library(sp)
library(cartography)
library(shinyWidgets)



## Loading data needed for cartography
locations=st_read("got/data/GoTRelease/Locations.shp",crs=4326)
lakes=st_read("got/data/GoTRelease/Lakes.shp",crs=4326)
conts=st_read("got/data/GoTRelease/Continents.shp",crs=4326)
land=st_read("got/data/GoTRelease/Land.shp",crs=4326)
wall=st_read("got/data/GoTRelease/Wall.shp",crs=4326)
islands=st_read("got/data/GoTRelease/Islands.shp",crs=4326)
kingdoms=st_read("got/data/GoTRelease/Political.shp",crs=4326)
landscapes=st_read("got/data/GoTRelease/Landscape.shp",crs=4326)
roads=st_read("got/data/GoTRelease/Roads.shp",crs=4326)
rivers=st_read("got/data/GoTRelease/Rivers.shp",crs=4326)
regions=st_read("got/data/GoTRelease/Regions.shp",crs=4326)
scenesloc=st_read("got/data/GoTRelease/ScenesLocations.shp",crs=4326)
rivers=st_read("got/data/GoTRelease/Rivers.shp",crs=4326)


## setting colors
colforest="#c0d7c2"
colriver="#7ec9dc"
colriver="#87cdde"
colland="ivory"
borderland = "ivory3"  

## Loading datasets
characters = read_csv("got/data/characters.csv")
episodes = read_csv("got/data/episodes.csv")
scenes = read_csv("got/data/scenes.csv")
appearances = read_csv("got/data/appearances.csv")

## Manipulation on the datasets
#### Adding the number of scenes per location to the location dataframe to be able to represent it on a map
number_of_scenes_per_location = scenes  %>% count(subLocation) %>% rename("location" = "subLocation")
number_of_scenes_per_location2 = scenes  %>% count(location) 
nb_sc = rbind(number_of_scenes_per_location,number_of_scenes_per_location2)
C <- intersect(unique(scenes$location), unique(locations$name))
C
locations = na.omit(locations) %>% left_join(number_of_scenes_per_location, by = c("name" = "location") )

### List of the caracters
list_personnages = appearances %>% left_join(scenes)  %>% 
  group_by(name) %>% 
  summarise(screenTime=sum(duration)) %>% 
  filter(screenTime>60*60)

### Scenes stats
scenes_stats=scenes %>% left_join(episodes) %>% 
  group_by(episodeTitle,seasonNum) %>% 
  summarize(nb_scenes=n(),duration_max=max(duration),nbdeath=sum(nbdeath))


  # Define UI
  ui <- fluidPage(theme = shinytheme("sandstone"),  setBackgroundImage(
    src = "https://assets.wallpapersin4k.org/uploads/2017/04/Game-Of-Thrones-The-Wall-Wallpaper-6.jpg"
  ),
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "Game Of Thrones",
      tabPanel("Home", img(src="https://i.postimg.cc/3JG8rfF6/Game-Of-Thrones-The-Wall-Wallpaper-6.png",style="display: block; margin-left: auto; margin-right: auto;")
      ), # Navbar 1, tabPanel
      tabPanel("Data", 
      mainPanel(
        h1("The Datasets"),
        selectInput("dataset", "Choose a dataset:",
                    list("Characters", "Episodes", "Scenes","Appearances")
        ),
        #Action button to submit the select
        actionButton("Submit", "Submit"),
        
        #Text Output to show the name of the dataset chosen
        tags$br(),
        tags$h2(textOutput("result")),
        DT::dataTableOutput("characters") 
        
      ) ),
      tabPanel("Explore", sidebarPanel(
        h1("Explore our data"),
        radioButtons("stats", "What do you want to see?",
                     c("Scene's Stats" = "ss",
                       "Character's Stats" = "cs")),
        selectInput("timeapp", "Choose a character:",
                    list_personnages$name
        ),
        selectInput("season", "Choose a Season:",
                    c("Season 1", "Season 2","Season 3","Season 4","Season 5","Season 6","Season 7","Season 8","All seasons")
        ),
        actionButton("plotbtn", "Plot!"),
        
      ), # sidebarPanel
      mainPanel(
        h1(textOutput("plottext")),
        plotOutput("outputplotting", height = 700),
        plotOutput("plot2")
      )),
      tabPanel("World Map", sidebarPanel(
        selectInput("selectmap", "Show me:",
                    c("The map", "The continents","The kingdoms","Number of scenes per location")
        ),
        actionButton("worldmap", "Show me!"),
        
        
        
      ),
      mainPanel(
        plotOutput("map", height = 900)
        
      ))
  
    ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output) {
    
    
    
    
    btn1 = eventReactive(input$worldmap, {
      input$selectmap
    })
    btn2 = eventReactive(input$otherplots, {
      input$map2
    })
    
    
    output$map <- renderPlot({
      if (btn1()=="The map"){
        ggplot()+geom_sf(data=land,fill=colland,col=borderland,size=0.5)+
          geom_sf(data=islands,fill=colland,col="ivory3")+
          geom_sf(data=landscapes %>% filter(type=="forest"),fill=colforest,col=colforest)+
          geom_sf(data=rivers,col=colriver)+
          geom_sf(data=lakes,col=colriver,fill=colriver)+
          geom_sf(data=wall,col="black",size=1)+
          geom_sf_text(data= locations %>% filter(size>3,name!='Tolos'),aes(label=name),size=3,family="Palatino", fontface="bold")+
          #geom_sf_text(data= conts,aes(label=name),size=7,family="Palatino", fontface="bold")+
          theme_minimal()+coord_sf(expand = 0,ndiscr = 0)+
          theme(panel.background = element_rect(fill = colriver,color=NA)) +
          labs(x="",y="")}
      else if (btn1()=="The continents"){
        
        ggplot()+geom_sf(data=land,fill=colland,col=borderland,size=0.5)+
          
          geom_sf(data=islands,fill=colland,col="ivory3")+
          geom_sf(data=conts,aes(fill=name))+ #conts
          geom_sf_text(data= locations %>% filter(size>3,name!='Tolos'),aes(label=name),size=3,family="Palatino", fontface="bold")+
          scale_color_discrete(guide="none")+
          scale_size_area("Number of scenes",max_size = 20)+
          theme_minimal()+coord_sf(expand = 0,ndiscr = 0)+
          theme(panel.background = element_rect(fill = colriver,color=NA),legend.key = element_rect(fill="#ffffff")) +
          labs(x="",y="")  
      }
      else if (btn1()=="The kingdoms"){
        
        ggplot()+geom_sf(data=land,fill=colland,col=borderland,size=0.5)+
          
          geom_sf(data=islands,fill=colland,col="ivory3")+
          geom_sf(data=kingdoms,aes(fill=name))+ #conts
          geom_sf_text(data= locations %>% filter(size>3,name!='Tolos'),aes(label=name),size=3,family="Palatino", fontface="bold")+
          scale_color_discrete(guide="none")+
          scale_size_area("Number of scenes",max_size = 20)+
          theme_minimal()+coord_sf(expand = 0,ndiscr = 0)+
          theme(panel.background = element_rect(fill = colriver,color=NA),legend.key = element_rect(fill="#ffffff")) +
          labs(x="",y="")  
      }
      else if (btn1()=="Number of scenes per location"){
        ggplot()+geom_sf(data=land,fill=colland,col=borderland,size=0.5)+
          
          geom_sf(data=islands,fill=colland,col="ivory3")+
          geom_sf(data=locations%>% filter(!is.na(n)),aes(size=n),col = "Orange")+
          geom_sf_text(data= locations %>% filter(size>3,name!='Tolos'),aes(label=name),size=3,family="Palatino", fontface="bold")+
          scale_color_discrete(guide="none")+
          scale_size_area("Number of scenes",max_size = 20)+
          theme_minimal()+coord_sf(expand = 0,ndiscr = 0)+
          theme(panel.background = element_rect(fill = colriver,color=NA),legend.key = element_rect(fill="#ffffff")) +
          labs(x="",y="")  
        
      }
    })
      
      #########
      
      
    
    
    
    
    dataset = eventReactive(input$Submit, {
      input$dataset
    })
    
    output$characters = DT::renderDataTable({
      if (dataset() == 'Characters'){
        DT::datatable(characters, options = list(lengthMenu = c(5, 30, 50), pageLength = 20))  
      }
      else if (dataset() == 'Episodes'){
        DT::datatable(episodes, options = list(lengthMenu = c(5, 30, 50), pageLength = 20))  
      }
      else if (dataset() == 'Scenes'){
        DT::datatable(scenes, options = list(lengthMenu = c(5, 30, 50), pageLength = 20))  
      }
      else if (dataset() == 'Appearances'){
        DT::datatable(appearances, options = list(lengthMenu = c(5, 30, 50), pageLength = 20))  
      }
      
    })
    output$result <- renderText({
      paste("The ", dataset()," Dataset:")})
    
    
    
    plot = eventReactive(input$plotbtn, {
      input$stats
    })
    plot2 = eventReactive(input$plotbtn, {
      input$timeapp
    })
    plot3 = eventReactive(input$plotbtn, {
      input$season
    })
   
    
    output$outputplotting <- renderPlot({
      if (plot() == "cs"){
        output$plottext <- renderText({
          paste("Screen time for", plot2())
        })
       
        jstime = appearances %>% filter(name==plot2()) %>% 
          left_join(scenes) %>% left_join(episodes) %>%
          group_by(episodeId,seasonNum) %>% 
          summarise(time=sum(duration))
        if (input$season != "All seasons"){
          jstime = jstime %>% filter(seasonNum==strtoi(substr(plot3(),8,8)))
        }
        
        ggplot(jstime) + 
          geom_line(aes(x=episodeId,y=time))+
          theme_bw()+
          xlab("épisode")+ylab("temps")
        
      }
      else {
        
        
        if (plot3() != "All seasons"){
          
          
          labels = labels %>% filter(seasonNum==strtoi(substr(plot3(),8,8)))
          scenes_stats = scenes_stats %>% filter(seasonNum==strtoi(substr(plot3(),8,8)))
          
        }
        output$plottext <- renderText({
          paste("Scene statistics")
        })
        ggplot(scenes_stats,aes(x=nb_scenes,y=duration_max,col=factor(seasonNum)))+
          geom_point(aes(size=nbdeath))+
          geom_text(data=labels,aes(label=episodeTitle),vjust=-0.6)+
          scale_x_continuous("Number of scenes",limits = c(0,280))+
          scale_y_continuous("Duration of the longuest scene",limits = c(100,800))+
          scale_color_brewer("Season",palette ="Spectral")+
          guides(colour = "legend", size = "legend")+
          theme_bw()
        
      }})
  } # server
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)
