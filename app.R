library(shiny)
library(tidyverse)
library(readr)
library(here)
library(ggplot2)
library(packcircles)
library(shinythemes)
library(leaflet)
library(maps)
library(plotly)
library(glue)
library(scales)


data <- read_csv(here::here("project_data2.csv")) 

now <- data %>% 
  group_by(city, longitude, latitude) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count)) %>% head(19)
  
c18 <-  c("#24716F", "#972F2D", "#4E7CD8", "#83607D", "#ED458C", "#EDA145", "#125852", "#FF5733", 
           "#581845", "#FA9EA1", "#E4CD8C", "#246AB4", "#9293C2", "#C29292", "#4D3434", "#706F6F", "#454B41"
)

ui <- fluidPage(theme = shinytheme("darkly"),
    

    titlePanel("Weapons and Tragets for Iraq Terrorist Attacks"),
    helpText("'Weapon Types' and 'Target locations' would be visualized for the citites that have faced more than 50 terror attacks in Iraq (Country with most no. of terror  attacks) in 2015-2019.
             These cities are displayed below in the map (first tab).", style = "text-align:justify;font-size:15px"),
    headerPanel('Filters'),

        sidebarLayout(
        
        sidebarPanel(
            titlePanel("Barplot only"),
            selectInput("selection", "City",
                       choices = unique(data$city),
                       multiple=TRUE),
            
            

            sliderInput("yr",
                        "Year",
                        min = 2015,  max = 2019, value = c(2000, 2016), sep = ""),
            
            hr(),
            titlePanel("Bar-plot and circlepacking"),
         radioButtons("button", "Categories",
                      c("Weapon Type" = "weaptype1_txt",
                        "Target Locations/ Targets" = "targsubtype1_txt")),
            
        
        hr(),
        fluidRow(
          helpText("Note: Please wait for 60-120 seconds for the Shiny plots and map to load properly. If still the map in the first plot doesn't load, please hover and select another tab and come back to the map tab.", style = "text-align:justify;font-size:13px")
        )
        ),
        
        

        mainPanel(
            

            tabsetPanel(type = "tabs",
                        tabPanel(icon("globe-americas"),
                                 
                                 leafletOutput("map", height = "400px"),
                                 absolutePanel(top = -44, right = 9,
                                               sliderInput("range", "No. of Attacks (Map Slider)", min(now$count),
                                                           max(now$count),
                                                           value = range(now$count), step = 0.1)),
                                 hr(),
                                 fluidRow(
                                   helpText("The map displays a total of 19 cities. The centre of the circles in the map are the locations for these cities and can be viewed clearly by zooming in, out and by using the slider above the map.
                                            Please note that the size of the circle represnts the count of attacks in that city i.e. The larger the circle, more the no. of attacks in that city.
                                            Click on these circles to know accurate location and count.", style = "text-align:justify;font-size:15px")
                                 )
                        ),
                        
                        tabPanel("Analysis", icon = icon("chart-bar"), plotlyOutput("plot", height = "400px"),
                                 hr(),
                                 fluidRow(
                                                 helpText(" This bar plot shows the count of Weapon types/Target locations or targets used in various citites of Iraq for the year 2015-2019. For analysis, adjust the filters for City, Year and Category.
                                                          Use PLOTLY for features like tooltip, sparkline, zoom etc.", style = "text-align:justify;font-size:15px")
                                 ) ),
                        
                        tabPanel("Conclusion", icon=icon("lightbulb"), plotlyOutput("circle", height = "400px"),
                                 hr(),
                                 fluidRow(
                                   helpText("This circle packing plot is an overall display of 5 most used weapons/target locations or targets for all cities combined, for the year 2015-2019 altogether.
                                     The size of the circle represents the count for weapon/target e.g. 'Explosives' is most used weapon kind and 'Melee' is the least amongst the 5 weapon kinds displayed.
                                              Please USE filters on the left to view both categories and PLOTLY for features like tooltip, sparkline, zoom etc.", style = "text-align:justify;font-size:15px")
                                 )
                                 
                                 
                                 
                                 
                        ), 
                        
                        tabPanel("Summary", icon = icon("list-ul"),
                                 
                                 p("Firstly, the cities that experiened more than 50 terror attacks in Iraq (2015-2019) were visualized using a map. These were 19 cities. The further analysis was carried out for these 19 cities using a bar-plot.
                                In the analysis, we were able to discover the weapon types used and the target locations for each of the cities or citites combined for the selected years.
                                   This was then concluded by displaying (using a circlepacking plot) an overall count for Weapons and Targets for all countries and years.
                                   It was observed that most used weapon was", strong("Explosives"), "and", strong("Private citizens and properties"), "were targeted the most", ".",style = "text-align:justify;font-size:22px"),
                                 
                                 br(),
                                 
                                 br(),
                                 
                                 br(),
                                 
                                 br(),
                                 
                                 br(),
                                 
                                 p("The dataset used for the visualization has been extracted from", a("Global Terrorism Database ", href="https://www.start.umd.edu/gtd/", target="_blank"), 
                                   "which has been collected from the 'National Consortium for the Study of Terrorism and Responses to Terrorism' i.e.", a("START", href="https://www.start.umd.edu", target="_blank"),
                                   style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"))
                                 
          
                        
                        
            )
            
        )
    )
)


server <- function(input, output) {
  
  
  observeEvent(input$range, {
    now <- now %>% filter(count >= input$range[1] & count  <= input$range[2])
    map <- leaflet(data = now) %>% addProviderTiles(providers$Esri.NatGeoWorldMap) 
      output$map <- renderLeaflet({ map %>%
          addCircleMarkers(
            ~longitude,
            ~latitude,
            radius = ~now$count/10,
            popup = paste("City:",now$city,
                          "Count:", now$count),
          )
      })
  })


    
    output$plot <- renderPlotly({
        if(input$button == "weaptype1_txt") {
            
        data2 <- data %>% 
            group_by(weaptype1_txt, year, city) %>% 
            summarise(count= n()) 
        
        
        if (!is.null(input$selection)){
            data2<- data2 %>% filter(city %in% input$selection)
        }
            
    
            data2<- data2 %>%
                filter(year >= input$yr[1] & year <= input$yr[2])
            
        
            
           graph1 <- data2 %>% highlight_key(~weaptype1_txt) %>%
              plot_ly(type = "bar", x = ~count, y = ~weaptype1_txt, color = ~weaptype1_txt, 
                      colors = c("#24716F", "#972F2D", "#4E7CD8", "#83607D", "#ED458C", "#EDA145", "#125852", "#FF5733", 
                                 "#581845", "#FA9EA1", "#E4CD8C", "#246AB4", "#9293C2", "#C29292", "#4D3434", "#706F6F", "#454B41"
                                 ),
                      
                      text = paste("City:", data2$city,
                                   "<br>Year:", data2$year,
                                   "<br>Weapon:", data2$weaptype1_txt,
                                   "<br>Count:", data2$count),
                      hoverinfo = 'text') %>% highlight(on = "plotly_hover", off = "plotly_doubleclick") %>% 
             layout(title = 'Amount of Weapon Types',
                    yaxis = list(title = 'Weapon type'),
                    xaxis = list(title = 'Count'),
                    paper_bgcolor='#DFFEFB',
                    plot_bgcolor='#EDF0F0',
                    width = 0.1
                    ) 
           
           hide_legend(graph1)

       
        }
        
        else{
            data3 <- data %>%
                group_by(targtype1_txt, year, city) %>%
                summarise(count= n())


            if (!is.null(input$selection)){
                data3<- data3 %>% filter(city %in% input$selection)
            }

            data3<- data3 %>%
                filter(year >= input$yr[1] & year <= input$yr[2])

            graph2 <- data3 %>% highlight_key(~targtype1_txt) %>%
              plot_ly(type = "bar",  x = ~count, y = ~targtype1_txt, color = ~targtype1_txt, 
                      colors = c("#24716F", "#972F2D", "#4E7CD8", "#83607D", "#ED458C", "#EDA145", "#125852", "#FF5733", 
                                 "#581845", "#FA9EA1", "#E4CD8C", "#246AB4", "#9293C2", "#C29292", "#4D3434", "#706F6F", "#454B41"
                                 ),
                      text = paste("City:", data3$city,
                                   "<br>Year:", data3$year,
                                   "<br>Target:", data3$targtype1_txt,
                                   "<br>Count:", data3$count),
                      hoverinfo = 'text') %>% highlight(on = "plotly_hover", off = "plotly_doubleclick") %>% 
           
              layout(title = 'Count of Target Locations/ Targets',
                     yaxis = list(title = 'Target Location/Target'),
                     xaxis = list(title = 'Count'),
                     paper_bgcolor='#DFFEFB',
                     plot_bgcolor='#EDF0F0'
                     
                     
                     
              ) 
            
            hide_legend(graph2)
            




        }

    })
    

      
      output$circle<- renderPlotly({
        
        if(input$button == "weaptype1_txt") {
        sel <- data %>%
            group_by(weaptype1_txt) %>%
            summarise(count= n()) %>% 
        arrange(desc(count)) %>% head(5)
        
     
        
        packing <- circleProgressiveLayout(sel$count, sizetype='area')
        sel <- cbind(sel, packing)
        dat.gg <- circleLayoutVertices(packing, npoints=50)
        
        dat.gg$tooltip <- glue::glue_data(dat.gg,
                                   "\nRank: {id}"
        )
        
      p <-  ggplot() +
            geom_polygon(data = dat.gg, aes(x, y, weaptype1_txt = id, fill=as.factor(id), text = tooltip), 
                         colour = "black", 
                         alpha = 0.6) +
            geom_text(data = sel, aes(x, y, label = weaptype1_txt), 
                      position = position_dodge(width=0.9),  size=4.3)+
            # scale_fill_manual(values = c18)+
            scale_size_continuous(range = c(1,4)) +
            theme_void() +
            theme(legend.position="none") +
            coord_equal()+
            theme(plot.background = element_rect(fill="#FFD0CD"))+
        theme(panel.background = element_rect(fill = "#EDF0F0"))+
            ggtitle("Most used weapons overall")
      
      q <- ggplotly(p,
                    tooltip = "text")
      q
        
      
        
        }
        
        else{

            sel2 <-data %>%
                group_by(targtype1_txt) %>%
                summarise(count= n()) %>%
                arrange(desc(count)) %>% head(5)
            


            packing2 <- circleProgressiveLayout(sel2$count, sizetype='area')
            sel2 <- cbind(sel2, packing2)
            dat.gg2 <- circleLayoutVertices(packing2, npoints=50)
            
            dat.gg2$tooltip2 <- glue::glue_data(dat.gg2,
                                              "\nRank: {id}"
            )
            
         # r <- dat.gg2 %>% 
         #   highlight_key(~id) %>%
           
           r <- ggplot() +
         
                geom_polygon(data = dat.gg2, aes(x, y, targtype1_txt = id, fill=as.factor(id), text = tooltip2), 
                             colour = "black", alpha = 0.6) +

                geom_text(data = sel2, aes(x, y, label = targtype1_txt),
                          position = position_dodge(width=0.9),  size=4.3)+
                # scale_fill_manual(values = c18)+
                scale_size_continuous(range = c(1,4)) +
                theme_void() +
                theme(legend.position="none") +
                coord_equal()+
                theme(plot.background = element_rect(fill="#FFD0CD"))+
              theme(panel.background = element_rect(fill = "#EDF0F0"))+
                ggtitle("Most of the target locations/targets overall")


         s <- ggplotly(r,
                       tooltip = "text") 
            # %>% highlight(on = "plotly_hover", off = "plotly_doubleclick",selected = attrs_selected(showlegend = FALSE))
         s


                }

    })
    
}

runApp(shinyApp(ui, server))