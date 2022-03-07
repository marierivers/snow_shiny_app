# a shiny app for snow data

# attach packages
library(shiny)
library(tidyverse)
library(paletteer)
library(shinythemes)
library(leaflet)

# load data
california <- read_csv("california_snow_data.csv")
ca_stations <- read_csv("ca_stations.csv")

# create 'ui' = "User Interface"
# widgets are things that the user interacts with to make decisions about what they want to appear as outputs
ui <- navbarPage("nav bar",
                 tabPanel("first tab",
                          h1("some giant text"),
                          p("use this map to find the name of a site near your water supply"),
                          leafletOutput(outputId = "site_map1")),
                 tabPanel("second tab",
                          sidebarLayout(
                            # sidebarPanel is where you put your widgets
                            sidebarPanel("here are my widgets: user selection options", 
                                         radioButtons(inputId = "site", 
                                                      label = "chose a site:",
                                                      choices = c("bunkerhill", "pliocenrdg", "babbittpk" = "babbittpk_", "martispeak", "schoolhous", "buzzardrst", "mazourkapk", "mt gleason" = "mt_gleason", "grapevine" = "grapevine_", "chileoflat", "laurel creek" = "laurel_crk", "pumicevall", "mono mills" = "mono_mills", "sagehenmdw", "owensgorge", "fossiltilt", "cratermark", "granitemtn", "blindsprin", "conwayroad", "lvmanzlake", "ballardrdg")),
                                         # selectInput(inputId = "color_select",
                                         #             label = "select a color",
                                         #             choices = c("fav red" = "red", 
                                         #                         "pretty purple" = "purple",
                                         #                         "ooorange" = "orange")),
                                         checkboxGroupInput(inputId = "wy_select", label = "select water years",
                                                            choices = levels(as.factor(california$water_year)),
                                                            selected = levels(as.factor(california$water_year)))),
                            mainPanel("use this map to find the name of a site near your water supply",
                                      leafletOutput(outputId = "site_map2"),
                                      "here is my graph",
                                      tabsetPanel(
                                        tabPanel("running total", plotOutput(outputId = "snow_plot")),
                                        tabPanel("mix/max"),
                                        tabPanel("snow depth", plotOutput(outputId = "depth_plot"))))
                          )
                          ),
                 tabPanel("the data",
                          p("something with a date input or date range slider" ))
  
)


# create 'server'
# the server is a function that takes in inputs which are going to be the things that the user selects and then it's going to send back outputs which the user can see.
server <- function(input, output) {
  
  
  
  # created a reactive dataframe that depends on the selection made in the site widget
  site_select <- reactive({
    california %>% 
      filter(site_name == input$site) %>% 
      filter(water_year %in% c(input$wy_select))
    })
  
  output$snow_plot <- renderPlot({
    
    # if the data that you are using is a reactive dataframe then you have to add () after data = df()
    ggplot(data = site_select(), aes(x = day_of_wy, y = running_wy_snow_accumulation_m)) +
      geom_line(aes(color = as.factor(water_year)), size = 1) +
      scale_x_continuous(breaks = c(1, 32, 62, 93, 124, 152, 183, 213, 244, 275),
                         labels = c("Oct 01", "Nov 01", "Dec 01", "Jan 01", "Feb 01", "Mar 01", "Apr 01", "May 01", "Jun 01", "Jul 01")) +
      #scale_colour_paletteer_d("khroma::smooth_rainbow") +
      theme(legend.position = "bottom") +
      labs(title = "total snow accumulation per water year", subtitle = input$site,
           x = NULL, y = "total snow (m)", color = "water year")
    })
  
  output$depth_plot <- renderPlot({
    # if the data that you are using is a reactive dataframe then you have to add () after data = df()
    ggplot(data = site_select(), aes(x = day_of_wy, y = snow_depth_m)) +
      geom_line(aes(color = as.factor(water_year)), size = 1) +
      scale_x_continuous(breaks = c(1, 32, 62, 93, 124, 152, 183, 213, 244, 275),
                         labels = c("Oct 01", "Nov 01", "Dec 01", "Jan 01", "Feb 01", "Mar 01", "Apr 01", "May 01", "Jun 01", "Jul 01")) +
      #scale_colour_paletteer_d("khroma::smooth_rainbow") +
      theme(legend.position = "bottom") +
      labs(title = "total snow accumulation per water year", subtitle = input$site,
           x = NULL, y = "total snow (m)", color = "water year")
  })
  
  output$site_map1 <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
                       ) %>% 
      addMarkers(data = ca_stations, lat = ~latitude, lng = ~longitude, popup = ~site_name)
  })
  
  output$site_map2 <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 5, maxZoom = 20)) %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE, minZoom = 5, maxZoom = 20)) %>% 
      addMarkers(data = site_select(), lat = ~latitude, lng = ~longitude, popup = ~site_name, 
                 options = markerOptions(minZoom = 5, maxZoom = 20))
  })
  
}


# Let R know that you want to combine the ui & server into an app:
shinyApp(ui = ui, server = server)