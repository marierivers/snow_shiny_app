# a shiny app for snow data

# attach packages
library(shiny)
library(tidyverse)
library(paletteer)
library(shinythemes)
library(leaflet)
library(DT)
library(ggrepel)

epsg2163 <- leafletCRS(
  crsClass = "L.Proj.CRS",
  code = "EPSG:2163",
  proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
  resolutions = 2^(16:7))

# load data
california <- read_csv("california_snow_data.csv")
ca_stations <- read_csv("ca_stations.csv")
#saveRDS(temp_month_summary, here::here(".", "myapp", "data", "temp_month_summary.rds"))
#temp_summary <- readRDS("data/temp_month_summary.rds")

# create 'ui' = "User Interface"
# widgets are things that the user interacts with to make decisions about what they want to appear as outputs
ui <- fluidPage(
  navbarPage("California Snow Data",
             # theme = bslib::bs_theme(
             #   bg = "#101010", # background color
             #   fg = "#FDF7F7", # foreground color
             #   primary = "#ED79F9"), # primary accent color
             #theme = bslib::bs_theme(bootswatch = "minty"),
             #theme = "theme_mtr.css",
             theme = shinytheme("cerulean"),
             #theme = shinytheme("cyborg"),
                 tabPanel("site locations",
                          h1("Daily Snow Depth Stations"),
                          p("This map shows California sites that contain daily snow depths estimated from GPS signal-to-noise ratios (SNRs). This dataset allows water manager to better understand the quantity of winter snowpacks. Use this map to find the name of a site near your water supply."),
                          leafletOutput(outputId = "site_map1")),
                 tabPanel("snow graphs",
                          sidebarLayout(
                            # sidebarPanel is where you put your widgets
                            sidebarPanel("user selection options", 
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
                                                            selected = 2017)),
                                                            #selected = levels(as.factor(california$water_year)))),
                            mainPanel("",
                                      leafletOutput(outputId = "site_map2"),
                                      "",
                                      tabsetPanel(
                                        tabPanel("snow depth: daily fluctuation", plotOutput(outputId = "depth_plot")),
                                        tabPanel("snow depth: running total", plotOutput(outputId = "snow_plot")),
                                        #tabPanel("mix/max"),
                                        tabPanel("snow depth: annual total", plotOutput(outputId = "wy_total_bar_chart"))),
                                      p("Citation: Larson, K. M. and E. E. Small. 2017. Daily Snow Depth and SWE from GPS Signal-to-Noise Ratios, Version 1. [Indicate subset used]. Boulder, Colorado USA. NASA National Snow and Ice Data Center Distributed Active Archive Center. doi: https://doi.org/10.5067/Z02Y1HGNFXCH. [Date Accessed]."))
                          )
                          ),
                 tabPanel("data table",
                          p("" ),
                          # data table
                          DT::dataTableOutput(outputId = "california_data"),
                          p("Citation: Larson, K. M. and E. E. Small. 2017. Daily Snow Depth and SWE from GPS Signal-to-Noise Ratios, Version 1. [Indicate subset used]. Boulder, Colorado USA. NASA National Snow and Ice Data Center Distributed Active Archive Center. doi: https://doi.org/10.5067/Z02Y1HGNFXCH. [Date Accessed]."))
  
))


# create 'server'
# the server is a function that takes in inputs which are going to be the things that the user selects and then it's going to send back outputs which the user can see.
server <- function(input, output) {
  
  
  
  # created a reactive dataframe that depends on the selection made in the site widget
  site_select <- reactive({
    california %>% 
      filter(site_name == input$site) %>% 
      filter(water_year %in% c(input$wy_select))
    })
  
  site_select_2017_2015 <- reactive({
    california %>% 
      filter(site_name == input$site) %>% 
      filter(water_year %in% c(2015, 2017))
  })
  
  # create a reactive dataframe of annual snow totals based on selected site
  annual_totals <- reactive({
    california %>% 
      filter(site_name == input$site) %>% 
      group_by(water_year) %>% 
      summarise(total_snow_wy_m = max(running_wy_snow_accumulation_m)) %>% 
      filter(water_year %in% c(input$wy_select))
  })
  
  # reactive dataframe for action thresholds
  action_thresholds <- reactive({
    california %>% 
      filter(site_name == input$site) %>% 
      group_by(water_year) %>% 
      summarise(total_snow_wy_m = max(running_wy_snow_accumulation_m))
  })

  # running total of accumulated snow  
  output$snow_plot <- renderPlot({
      # if the data that you are using is a reactive dataframe then you have to add () after data = df()
        ggplot(data = site_select(), aes(x = day_of_wy, y = running_wy_snow_accumulation_m)) +
      geom_line(aes(color = as.factor(water_year)), size = 1) +
      scale_x_continuous(breaks = c(1, 32, 62, 93, 124, 152, 183, 213, 244, 275),
                         labels = c("Oct 01", "Nov 01", "Dec 01", "Jan 01", "Feb 01", "Mar 01", "Apr 01", "May 01", "Jun 01", "Jul 01")) +
      #scale_colour_paletteer_d("khroma::smooth_rainbow") +
      theme(legend.position = "bottom") +
      labs(title = "total snow accumulation per water year", subtitle = input$site,
           x = NULL, y = "total snow (m)", color = "water year") +
      theme(text = element_text(size = 18)) +
      geom_label(aes(label = water_year), 
                 data = site_select() %>% 
                   group_by(water_year) %>% 
                   filter(water_year %in% c(2015, 2017)) %>% 
                   filter(day_of_wy == max(day_of_wy)),
                 nudge_x = 1)
    })
  
  # daily change in snow depth
  output$depth_plot <- renderPlot({
    # if the data that you are using is a reactive dataframe then you have to add () after data = df()
    ggplot(data = site_select(), aes(x = day_of_wy, y = snow_depth_m)) +
      geom_line(aes(color = as.factor(water_year)), size = 1) +
      scale_x_continuous(breaks = c(1, 32, 62, 93, 124, 152, 183, 213, 244, 275),
                         labels = c("Oct 01", "Nov 01", "Dec 01", "Jan 01", "Feb 01", "Mar 01", "Apr 01", "May 01", "Jun 01", "Jul 01")) +
      #scale_colour_paletteer_d("khroma::smooth_rainbow") +
      theme(legend.position = "bottom") +
      labs(title = "total snow accumulation per water year", subtitle = input$site,
           x = NULL, y = "snow depth (m)", color = "water year") +
      theme(text = element_text(size = 18))
  })
  
  # bar chart of total snow per water year
  output$wy_total_bar_chart <- renderPlot({
    ggplot(data = annual_totals(), aes(x = factor(water_year), y = total_snow_wy_m)) +
      geom_col(fill = "blue") +
      labs(title = "Total accumulated snow per water year",
           x = "water year", y = "total snow depth (m)") +
      theme(text = element_text(size = 18)) +
      geom_hline(yintercept = median(action_thresholds()$total_snow_wy_m), linetype = "dashed", color = "black", size = 1) +
      geom_label(aes(x = .5, y = median(action_thresholds()$total_snow_wy_m) + .2, label = "activate groundwater supply"), color = "black", fill = "white", size = 6, hjust = 0) +
      geom_hline(yintercept = quantile(action_thresholds()$total_snow_wy_m, c(0.25)), linetype = "dashed", color = "red", size = 2) +
      geom_label(aes(x = .5, y = quantile(action_thresholds()$total_snow_wy_m, c(0.25)) + .2, label = "implement water conservation measures"), color = "red", fill = "white", size = 6, hjust = 0)
  })
  
  # map of all sites
  output$site_map1 <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap) %>% 
      # addProviderTiles(providers$Stamen.TonerLite,
      #                  options = providerTileOptions(noWrap = TRUE)
      #                  ) %>% 
      addMarkers(data = ca_stations, lat = ~latitude, lng = ~longitude, label = ~site_name)
  })
  
  # map of just selected site
  output$site_map2 <- renderLeaflet({
    leaflet() %>% 
      setView(lat = 38, lng = -120, zoom = 5.25) %>% 
      addProviderTiles(providers$OpenStreetMap) %>% 
      addMarkers(data = site_select(), lat = ~latitude, lng = ~longitude, popup = ~site_name)
    #addMarkers(data = site_select(), lat = ~latitude, lng = ~longitude, popup = ~site_name, options = markerOptions(minZoom = 5, maxZoom = 20))
  })
  
  
  # render the california data table
  output$california_data <- DT::renderDataTable({
    DT::datatable(california,
                  options = list(pageLength = 50),
                  caption = tags$caption(
                    style = 'caption-side: top; text-align: left;',
                    'Table 1: ', tags$em('the data')
                  ))
  })
  
}


# Let R know that you want to combine the ui & server into an app:
shinyApp(ui = ui, server = server)