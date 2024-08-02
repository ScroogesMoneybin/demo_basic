library(shiny)
library(dplyr)
library(sf)
library(tmap)
library(plotly)
library(tidyr)

alpha_value <- 1

map <- st_read("SBOE12_map_demos.csv")

map$WKT<-NULL
map <- map %>%  mutate(across(c(White, Black, Hispanic, Asian, Other_Race, Male, Female, Unknown_Sex, Age17_25, Age26_35, Age36_45, Age46_55, Age56_65, Age66_75, Age76Over), as.numeric))

df_age <- map[c("county_precinct", "Age17_25", "Age26_35", "Age36_45", "Age46_55", "Age56_65", "Age66_75", "Age76Over")]
df_age$geometry<-NULL

ui <- fluidPage(
  navbarPage(
    title="Politech.ai", id="navbar",
    
    
    tabPanel("SBOE Runoff Demographics", # Sidebar with a slider input for number of bins
             sidebarLayout(
                               sidebarPanel(id="sidebar1",
                                            radioButtons("rb1", "Demographic Data Options", c(
                                              "Male (%)" = "Male",
                                              "Female (%)" = "Female",
                                              "White (%)" = "White",
                                              "Black (%)" = "Black",
                                              "Hispanic  (%)" = "Hispanic",
                                              "Asian (%)" = "Asian",
                                              "Other Race (%)" = "Other_Race",
                                              "17 - 25 (%)" = "Age17_25", 
                                              "26 - 35 (%)" = "Age26_35", 
                                              "36 - 45 (%)" = "Age36_45", 
                                              "46 - 55 (%)" = "Age46_55", 
                                              "56 - 65 (%)" = "Age56_65", 
                                              "66 - 75 (%)" = "Age66_75", 
                                              "76+ (%)" = "Age76Over"
                                              
                                              
                                            )), 
                                            width = 3),
                           
                           
                           mainPanel(
                             width= 9,
                             tmapOutput("map9", height = "60vh"),
                           
                             plotlyOutput("bar_plotly", width="75%",height="30vh")
                             
                             
                           )
             )),

  )
  
)

# Define server logic required to draw a maps
server <- function(input, output, session) {
  
  
  
  details_displayed <- c(
    "Male (%)" = "Male",
    "Female (%)" = "Female",
    "White (%)" = "White",
    "Black (%)" = "Black",
    "Hispanic (%)" = "Hispanic",
    "Asian (%)" = "Asian",
    "Other Race (%)" = "Other_Race",
    "17 - 25 (%)" = "Age17_25", 
    "26 - 35 (%)" = "Age26_35", 
    "36 - 45 (%)" = "Age36_45", 
    "46 - 55 (%)" = "Age46_55", 
    "56 - 65 (%)" = "Age56_65", 
    "66 - 75 (%)" = "Age66_75", 
    "76+ (%)" = "Age76Over"
    )
  

  output$map9 <- renderTmap({
    map1<- tm_shape(map) + #choose dataset
      tm_borders("black", lwd=1) + #set black borders around precincts
      tm_polygons(col=input$rb1, #Select 5 different columns to make maps out of their data
                  style='quantile',
                  breaks = 5,
                  legend.format = list(fun=function(x) paste0(round(x,0))), #make legend values be a percentage with no decimals
                  title = "",
                  id = 'county_precinct', #set hover value to the precinct number of each row
                  palette = 'YlOrRd',
                  popup.vars = details_displayed,  #choose the variables from the data that appear in the popup
                  popup.format = list(fun=function(x) paste0(round(x,2))), zindex=401) +
      tm_view(alpha = alpha_value, view.legend.position = c("right", "bottom")) +
      tm_facets(as.layers = TRUE) +
      tmap_options(check.and.fix = TRUE) +
      tmap_options(max.categories = 4355) 
  })

  
  

  
  #Plotly barchart
  output$bar_plotly <- renderPlotly({
    

    df_age$county_precinct<-NULL
    
    num<-(100/ncol(df_age))
    plot_ly(df_age, x=~colnames(df_age),y=~c(num,num,num,num, num, num, num), type="bar", marker = list(color = 'gray',
                                                                                 line = list(color = 'black',
                                                                                             width = 1.5))) %>% 
      layout(title = "Click Precinct on Map to see Precinct-level results", xaxis = list(title = "  ", ticktext = list(), showticklabels = F), yaxis = list(title = "  ", ticktext = list(), showticklabels = F))
    

  })

  

  observeEvent(input$map9_shape_click, {
    click<- input$map9_shape_click
    id<- click$id
    
    df_age <- df_age %>% rowwise() %>% mutate(county_precinct=gsub(" ", "\\.", county_precinct)) %>% mutate(county_precinct=gsub("-", "\\.", county_precinct))
    
    filtered9 <-
      df_age %>% filter(county_precinct == id)

    map4<- filtered9 %>% pivot_longer(-county_precinct, names_to="group", values_to="values")
    map4$county_precinct<-NULL
    map4$group <- c("17-25","26-35","36-45","46-55","56-65","66-75","76 +")
    
    output$bar_plotly <-  renderPlotly({
      plot_ly(map4, x=~group,y=~values, type="bar", marker = list(color=~values,
                                                                  line = list(color = 'black',
                                                                              width = 1.5))) %>%
        layout(title = "Click Precinct on Map to see Precinct-level results", xaxis = list(title = "Age Groupings", ticktext = list()), yaxis = list(title = "Percentage", ticktext = list()))

    })

  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

