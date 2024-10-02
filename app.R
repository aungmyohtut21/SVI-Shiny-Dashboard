library(shiny)
library(leaflet)
library(viridis)
library(viridisLite)
library(DT)
library(shinythemes)
library(sf)
library(tidyverse)
library(shinythemes)
library(scales)
library(tigris)
library(htmltools)
library(ggplot2)
library(shinydashboard)
library(ggiraph)
library(rlang)
library(rsconnect)
library(patchwork)

# Load the data
svi <- st_read("portlandsvi.gpkg") %>% select(-TRACT_NUMBER) %>% 
  relocate(COUNTY, .after = CITY)
  
names(svi)

# Get the variable names excluding the geometry column
variable_choices <- names(st_drop_geometry(svi))

ui <- navbarPage( 
  title = tags$span(style = "font-weight: bold;", "SOCIAL VULNERABILITY INDEX OF PORTLAND"),
  theme = shinytheme("flatly"),  # Try a different theme
  tags$head(
    tags$style(HTML("
    .small-button {
      font-size: 8px;
      padding: 10px 10px;
      /* Remove fixed width and height to let buttons adjust */
    }
  "))
  ),
  
  ####################################################################################
  ## INTERACTIVE MAP
  ####################################################################################
  tabPanel("Interactive Map", icon = icon("map"),
           # Sidebar with a select input 
           sidebarLayout(
             sidebarPanel(
               p("This app allows you to visualize the social vulnerability index in the city of Portland.
            Please choose the variables under the drop down menu."),
               # For Variable
               selectInput(inputId = "variable",
                           label = "Variable:",
                           choices = c("Overall SVI Ranking" = "RPL_THEMES",
                                       "Socioeconomic Status Theme Ranking" = "RPL_THEME1",
                                       "Household Characteristics Theme Ranking" = "RPL_THEME2",
                                       "Racial & Ethnic Minority Status Theme Ranking" = "RPL_THEME3",
                                       "Housing Type & Transportation Theme Ranking" = "RPL_THEME4",
                                       "Population Estimate" = "E_TOTPOP",
                                       "Housing Units Estimate" = "E_HU",
                                       "Households Estimate" = "E_HH",
                                       "Age 17 & Younger" = "EP_AGE17",
                                       "Age 65 & Older" = "EP_AGE65",
                                       "More People than Rooms" = "EP_CROWD",
                                       "Population with a Disability" = "EP_DISABL",
                                       "Persons in Group Quarters" = "EP_GROUPQ",
                                       "Housing Cost Burden" = "EP_HBURD",
                                       "Limited English Speaking" = "EP_LIMENG",
                                       "Minority" = "EP_MINRTY",
                                       "Mobile Homes" = "EP_MOBILE",
                                       "Housing with Multiple Units" = "EP_MUNIT",
                                       "No High School Diploma" = "EP_NOHSDP",
                                       "Households with No Vehicles" = "EP_NOVEH",
                                       "Below 150% Poverty" = "EP_POV150",
                                       "Single Parent Households" = "EP_SNGPNT",
                                       "Unemployment Rate" = "EP_UNEMP",
                                       "No Health Insurance" = "EP_UNINSUR"),
                           selected = "Overall SVI Ranking"),
               # For Base Map
               selectInput("basemap", 
                           label = "Basemap:",
                           choices = c(
                             "All" = "All",
                             "Open Street Map" = "OpenStreetMap",
                             "CARTO Positron" = "CartoDB.Positron",
                             "CARTO Voyager" = "CartoDB.Voyager",
                             "Stadia Toner" = "Stadia.StamenToner"),
                           selected = "CartoDB.Voyager"),
               
               # For City
               selectInput("city",
                           label = "City:",
                           choices = c(
                             "All" = "All",
                             "Portland City" = "Portland",
                             "South Portland City" = "South Portland",
                             "Westbrook City" = "Westbrook"),
                           selected = "All"
                          ),
                           
               # For Census Tract Selected
               textOutput("tract_text"),
               width = 3),
             mainPanel(
               leafletOutput("map", width = "100%", height = "700px"), 
               # Data source caption below the map
               tags$div(style = "text-align: right; font-size: 10px; margin-top: 10px;", 
                        tags$b("Data source:"),
                        "2018-2022 ACS, US Census Bureau"),
               width = 9
             ))),
  ####################################################################################
  ## PLOT TAB
  ####################################################################################
  tabPanel("Plot", icon = icon("chart-line"),
           fluidRow(
             column(width = 3,
                    tags$div(class = "info-box", style = "background-color: #00c0ef; padding: 15px; border-radius: 3px; text-align: center;",
                             tags$p(style = "font-size: 15px;", "Total Census Tracts:"),
                             tags$h5(style = "font-weight: bold;", textOutput("tracts"))
                    )
             ),
             column(width = 3,
                    tags$div(class = "info-box", style = "background-color: #E0FFA0; padding: 15px; border-radius: 3px; text-align: center;",
                             tags$p(style = "font-size: 15px;", "Total Population:"),
                             tags$h5(style = "font-weight: bold;", textOutput("population"))
                    )
             ),
             column(width = 3,
                    tags$div(class = "info-box", style = "background-color: #ACFF85; padding: 15px; border-radius: 3px; text-align: center;",
                             tags$p(style = "font-size: 15px;", "Total Housing Units:"),
                             tags$h5(style = "font-weight: bold;", textOutput("housing"))
                    )
             ),
             column(width = 3,
                    tags$div(class = "info-box", style = "background-color: #FCB597; padding: 15px; border-radius: 3px; text-align: center;",
                             tags$p(style = "font-size: 15px;", "Total Households:"),
                             tags$h5(style = "font-weight: bold;", textOutput("households"))
                    )
             )
             
           ),
           fluidRow(
             column(width = 4,
                    tags$style(HTML("
                        .selectize-input, .selectize-dropdown {
                        font-size: 12px;
                        }"
                    )),
                    selectInput(inputId = "plot_variable",
                                label = "Select Variable to Plot:",
                                choices = c("Overall SVI Ranking" = "RPL_THEMES",
                                            "Socioeconomic Status Theme Ranking" = "RPL_THEME1",
                                            "Household Characteristics Theme Ranking" = "RPL_THEME2",
                                            "Racial & Ethnic Minority Status Theme Ranking" = "RPL_THEME3",
                                            "Housing Type & Transportation Theme Ranking" = "RPL_THEME4",
                                            "Population Estimate" = "E_TOTPOP",
                                            "Housing Units Estimate" = "E_HU",
                                            "Households Estimate" = "E_HH",
                                            "Age 17 & Younger" = "EP_AGE17",
                                            "Age 65 & Older" = "EP_AGE65",
                                            "More People than Rooms" = "EP_CROWD",
                                            "Population with a Disability" = "EP_DISABL",
                                            "Persons in Group Quarters" = "EP_GROUPQ",
                                            "Housing Cost Burden" = "EP_HBURD",
                                            "Limited English Speaking" = "EP_LIMENG",
                                            "Minority" = "EP_MINRTY",
                                            "Mobile Homes" = "EP_MOBILE",
                                            "Housing with Multiple Units" = "EP_MUNIT",
                                            "No High School Diploma" = "EP_NOHSDP",
                                            "Households with No Vehicles" = "EP_NOVEH",
                                            "Below 150% Poverty" = "EP_POV150",
                                            "Single Parent Households" = "EP_SNGPNT",
                                            "Unemployment Rate" = "EP_UNEMP",
                                            "No Health Insurance" = "EP_UNINSUR"),
                                selected = "RPL_THEMES")),
             column(width = 4,
                    tags$style(HTML("
                        .selectize-input, .selectize-dropdown {
                        font-size: 12px;
                        }"
                        )),
                    selectInput(inputId = "city",
                                label = "City:",
                                choices = c(
                                  "All" = "All",
                                  "Portland City" = "Portland",
                                  "South Portland City" = "South Portland",
                                  "Westbrook City" = "Westbrook"),
                                selected = "All"
                                ))
           ),
           fluidRow(
             column(width = 12,
                    girafeOutput("my_plot", height = "750px", width = "100%"))
           )
  ),
  
  ####################################################################################
  ## DATA TABLE
  ####################################################################################
  tabPanel("Data Table", icon = icon("table"),
           fluidRow(
             column(width = 6, align = "left",
                    actionButton("data_dictionary","View Data Dictionary"),
                    class = "small-button"
             ),
             
             column(width = 6, align = "right",
                    downloadButton("download_data","Download Data"),
                    class = "small-button"
             )),
           
           mainPanel(
             DT::dataTableOutput("data_table"),
             width = 12
           )),
  ####################################################################################
  ## ABOUT SECTION
  ####################################################################################
  tabPanel("About", icon = icon("info-circle"),
           mainPanel(width = 12,
                     tags$h3(tags$b("INTRODUCTION")),
                     tags$p("The Social Vulnerability Index (SVI), developed by the Centers for Disease Control and Prevention’s Agency for Toxic Substances and Disease Registry (CDC/ATSDR), is a tool designed to identify communities at increased risk during public health emergencies. 
                      Understanding social vulnerability allows policymakers, emergency response planners, and public health officials to allocate resources and interventions more effectively, ensuring that those who are most in need receive appropriate support."),
                     tags$h4(tags$b("WHAT IS SOCIAL VULNERABILITY?")),
                     tags$p("Every community must prepare for and respond to hazardous events, whether a natural disaster like a tornado or a disease outbreak, 
                      or an anthropogenic event such as a harmful chemical spill. The degree to which a community exhibits certain social conditions, 
                      including high poverty, low percentage of vehicle access, or crowded households, among others, may affect that community’s ability to prevent human suffering and financial loss in the event of a disaster. 
                      These factors describe a community’s social vulnerability."),
                     tags$h4(tags$b("THEMES AND CALCULATION")),
                     tags$p("The SVI ranks each census tract on 16 social factors, grouped into four related themes, 
                      each representing a different aspect of social vulnerability."),
                     tags$h5(tags$b("Socioeconomic Status:")),
                     tags$div(style = "padding-left: 5px;", 
                              tags$ul(#Indenting
                                tags$li("This theme includes indicators such as poverty, unemployment, housing cost burden, no health insurance, and no high school diploma.")
                              )
                     ),
                     tags$h5(tags$b("Household Characteristics:")),
                     tags$div(style = "padding-left: 5px;",
                              tags$ul(
                                tags$li("This includes metrics related to age (over 65 and under 17 years), disability, single-parent households, and English language proficiency."))),
                     tags$h5(tags$b("Racial and Ethnic Minority Status:")),
                     tags$div(style = "padding-left: 5px;",
                              tags$ul(
                                tags$li("This theme assesses the proportion of all racial/ethnic minorities other than Non-Hispanic Whites."))),
                     tags$h5(tags$b("Housing Type and Transportation:")),
                     tags$div(style = "padding-left: 5px;",
                              tags$ul(
                                tags$li("Indicators like multi-unit structures, mobile homes, crowding, no vehicle access, and group quarters are included, highlighting potential barriers in evacuation or shelter-in-place scenarios.")
                              )),
                     tags$p("For each theme, the SVI uses U.S. Census data to score and rank each tract relative to others, considering these factors collectively to identify areas with potentially greater challenges during emergencies.
                      Percentile ranking values range from 0 to 1, with higher values indicating greater social vulnerability."),
                     tags$p("Detail variables and calculations can be found",
                            tags$a(href="https://www.atsdr.cdc.gov/placeandhealth/svi/documentation/SVI_documentation_2022.html","here.", style = "text-decoration: underline;")),
                     tags$h4(tags$b("WHAT IS A CENSUS TRACT?")),
                     tags$p("A census tract is a geographic area defined for the purpose of collecting and analyzing demographic data in the United States. 
                            Census tracts typically contain a population of around 1,200 to 8,000 people and are designed to be relatively homogeneous in terms of population characteristics. 
                            They are used by the U.S. Census Bureau to gather detailed information about the population, such as socioeconomic status, housing, and health statistics. 
                            Census tracts help in understanding social, economic, and demographic trends within specific areas and are often used in urban planning and policy-making."),
                     tags$p("Detail explanation about census tract can be found",
                            tags$a(href="https://www.census.gov/programs-surveys/geography/about/glossary.html#par_textimage_13","here.", style = "text-decoration:underline;")),
                     tags$h4(tags$b("DATA SOURCES")),
                     tags$p("The Centers for Disease Control and Prevention (CDC) utilizes 5-year estimates from the American Community Survey (ACS) conducted by the U.S. Census Bureau to develop the Social Vulnerability Index (SVI). 
                      The dataset is available for download in CSV format by selecting the desired year and state",
                            tags$a(href = "https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html","here.",style = "text-decoration: underline;"),
                            "Additionally, the data can be downloaded in ESRI geodatabase format, either at the Census Tract or County level."),
                     tags$h4(tags$b("ADDITIONAL RESOURCES")),
                     tags$p("Visit these links to learn more about the CDC/ATSDR Social Vulnerability Index:"),
                     tags$div(style = "padding-left: 5px;",
                              tags$ul(
                                tags$li(tags$a(href="https://www.atsdr.cdc.gov/placeandhealth/svi/index.html","SVI Home Page", style = "text-decoration: underline;")),
                                tags$li(tags$a(href="https://www.atsdr.cdc.gov/placeandhealth/svi/fact_sheet/fact_sheet.html","SVI Fact Sheet", style = "text-decoration: underline;")),
                                tags$li(tags$a(href="https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html","SVI Data and Documentation", style = "text-decoration: underline;")),
                                tags$li(tags$a(href="https://www.atsdr.cdc.gov/placeandhealth/svi/faq_svi.html","Frequently Asked Questions", style = "text-decoration: underline;"))
                              ))
                     
           )))

server <- function(input, output, session) {
  
  #################################################################################
  ## DATA PREPARATION
  #################################################################################
  
  # Erase Water
  plsvi <- erase_water(svi, year = 2020) %>% st_make_valid()
  
  # Transform spatial data to WGS84
  sviwgs84 <- reactive({
    plsvi %>% st_transform(crs = 4326)
  })
  
  # Filter data based on city selection
  filtered_data <- reactive({
    if("All" %in% input$city){
      sviwgs84()
    } else {
    sviwgs84() %>% filter(CITY == input$city) 
    } 
  })
  
  
  # Map basemap names to their corresponding provider objects
  basemap_providers <- list(
    "OpenStreetMap" = providers$OpenStreetMap,
    "CartoDB.Positron" = providers$CartoDB.Positron,
    "CartoDB.Voyager" = providers$CartoDB.Voyager,
    "Stadia.StamenToner" = providers$Stadia.StamenToner
  )
  
  # Define the variable-label mapping using the choices vector
  variable_labels <- list(
    "RPL_THEMES" = "Overall SVI Ranking",
    "RPL_THEME1" = "Socioeconomic Status Theme Ranking",
    "RPL_THEME2" = "Household Characteristics Theme Ranking",
    "RPL_THEME3" = "Racial & Ethnic Minority Status Theme Ranking",
    "RPL_THEME4" = "Housing Type & Transportation Theme Ranking",
    "E_TOTPOP" = "Population Estimate",
    "E_HU" = "Housing Units Estimate",
    "E_HH" = "Households Estimate",
    "EP_AGE17" = "Age 17 & Younger",
    "EP_AGE65" = "Age 65 & Older",
    "EP_CROWD" = "More People than Rooms",
    "EP_DISABL" = "Population with a Disability",
    "EP_GROUPQ" = "Persons in Group Quarters",
    "EP_HBURD" = "Housing Cost Burden",
    "EP_LIMENG" = "Limited English Speaking",
    "EP_MINRTY" = "Minority",
    "EP_MOBILE" = "Mobile Homes",
    "EP_MUNIT" = "Housing with Multiple Units",
    "EP_NOHSDP" = "No High School Diploma",
    "EP_NOVEH" = "Households with No Vehicles",
    "EP_POV150" = "Below 150% Poverty",
    "EP_SNGPNT" = "Single Parent Households",
    "EP_UNEMP" = "Unemployment Rate",
    "EP_UNINSUR" = "No Health Insurance"
  )
  
  # Compute totals for info boxes
  total_tracts <- reactive({nrow(filtered_data())})
  total_population <- reactive({sum(st_drop_geometry(filtered_data())$E_TOTPOP, na.rm=TRUE)})
  total_housing <- reactive({sum(st_drop_geometry(filtered_data())$E_HU, na.rm=TRUE)})
  total_households <- reactive({sum(st_drop_geometry(filtered_data())$E_HH, na.rm=TRUE)})
  
  # Create a reactive dataframe
  portlandsvi <- reactive({
    filtered_data() %>% st_drop_geometry() %>% select(input$variable) %>% 
      pull() %>% as.numeric() # Need to drop geometry first otherwise give error
    }) 
  
  # Set colors
  pal <- reactive({
    colorNumeric("viridis", domain = portlandsvi(), alpha = 0.3, reverse = TRUE)
  })
  
  ####################################################################################
  ## INTERACTIVE MAP
  ####################################################################################
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(provider = basemap_providers[[input$basemap]]) %>% 
      addPolygons(
        data = filtered_data(),
        fillColor = ~ pal()(portlandsvi()),
        weight = 0.5,
        opacity = 0.5,
        fillOpacity = 0.33,
        smoothFactor = 0.2,
        color = "black",
        label = ~ if (input$variable %in% c("RPL_THEMES","RPL_THEME1","RPL_THEME2","RPL_THEME3",
                                            "RPL_THEME4","E_TOTPOP","E_HU","E_HH")) {
          paste0(variable_labels[[input$variable]], ": ", round(portlandsvi(), 2))
        } else {
          paste0(variable_labels[[input$variable]], ": ", round(portlandsvi(), 2),"%")
        },
        labelOptions = labelOptions(
          style = NULL,
          textsize = "10px",
          direction = "auto",
          opacity = 1,
          textOnly = FALSE
        ),
        # Tooltip for Census Tract
        highlight = highlightOptions(
          color = "red",
          weight = 1.5,
          bringToFront = TRUE
        ),
        # Tooltip for census tract
        popup = ~ paste(
          "<div style='font-size: 10px;'>",
          "<strong>Census Tract: </strong>", str_remove(NAMELSAD, "Census Tract"), "<br/>",
          "<strong>Population: </strong>", formatC(E_TOTPOP, format = "d", big.mark = ","), "<br/>",
          "<strong>Housing Units: </strong>", formatC(E_HU, format = "d", big.mark = ","), "<br/>",
          "<strong>Households: </strong>", formatC(E_HH, format = "d", big.mark = ","), "<br/>",
          "<strong>City: </strong>", CITY
        ),
        popupOptions = popupOptions(
          autoPan = TRUE,
          closeButton = TRUE
        )
      ) %>% 
      addLegend(
        position = "topright",
        pal = pal(),
        values = filtered_data()[[input$variable]],
        title = variable_labels[[input$variable]],
        opacity = 0.8,
        labFormat = labelFormat(suffix = "", digits = 2)
      ) %>% 
      setView(lng = -70.2110, lat = 43.6370, zoom = 11) 
  })
  
 
  
  ####################################################################################
  ## DATA TABLE
  ####################################################################################
  
  # Reactive expression to filter data based on selected census tracts
  selected_data <- reactive({
    svi %>% st_drop_geometry()
  })
  
  # Data Dictionary Button
  observeEvent(input$data_dictionary, {
    browseURL("https://www.atsdr.cdc.gov/placeandhealth/svi/map/data/docs/SVI2020Documentation_08.05.22.pdf#page=7")
  })
  
  # Render Data Table based on selected variables
  output$data_table <- DT::renderDataTable({
    DT::datatable(selected_data(),
                  options = list(pageLength = 8,
                                 autoWidth = TRUE,
                                 rownames = FALSE,
                                 scrollX = TRUE,
                                 scrollY = "340px",
                                 fixedHeader = TRUE),
                  class = "display nowrap")
  })
  
  # Download Files
  output$download_data <- downloadHandler(
    filename = function() {
      paste("portlandsvi", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(selected_data(), file, row.names = FALSE)
    }
  )
  
  ####################################################################################
  ## FOR PLOT
  ####################################################################################
  # Render the text output for the info boxes
  output$tracts <- renderText({
    total_tracts()
  })
  
  output$population <- renderText({
    formatC(total_population(), format = "d", big.mark = ",")
  })
  
  output$housing <- renderText({
    formatC(total_housing(), format = "d", big.mark = ",")
  })
  
  output$households <- renderText({
    formatC(total_households(), format = "d", big.mark = ",")
  })
  
  # Render ggplot
  output$my_plot <- renderGirafe({
    svi_geom <- filtered_data()
    svi_data <- st_drop_geometry(filtered_data())
    variable <- input$plot_variable
    variable_label <- variable_labels[[variable]]
    
    # Create the interactive map
    svi_map <- ggplot(svi_geom, aes(fill = .data[[variable]], data_id = NAMELSAD)) +
      geom_sf_interactive(aes(tooltip = paste0(NAMELSAD, "<br>",
                              variable_labels[[variable]], ": ", round(.data[[variable]], 2)),
                              data_id = GEOID)) +
      scale_fill_distiller(palette = "Greens", direction = 1, guide = "none") +
      theme_void()
    
    # Check if the variable is a THEME variable
    if (variable %in% c("RPL_THEMES", "RPL_THEME1", "RPL_THEME2", "RPL_THEME3", "RPL_THEME4")) {
      plot <- ggplot(svi_data, aes(x = reorder(NAMELSAD, .data[[variable]]), y = .data[[variable]])) +
        geom_bar_interactive(stat = "identity", fill = "forestgreen", 
                             aes(tooltip = paste0(NAMELSAD, "<br>",
                                                  variable_labels[[variable]], ": ", round(.data[[variable]], 2)), 
                                 data_id = GEOID)) +
        labs(title = paste(variable_label, "By Census Tract"),
             caption = "Data source: 2018-2022 ACS, US Census Bureau",
             x = " ",
             y = variable_label) +
        coord_flip() +
        theme_minimal() +
        theme(text = element_text(family = "Lato"),
              plot.title = element_text(face = "bold", hjust = 0.5, size = 9),
              plot.caption = element_text(size = 5),
              axis.title.y = element_text(face = "bold", vjust = 0.5, size = 7),
              axis.title.x = element_text(face = "bold", hjust = 0.5, size = 7),
              axis.text.y = element_text(size = 6),
              axis.text.x = element_text(size = 6))
      
    } else {
      # Determine the corresponding MOE variable
      if (startsWith(variable, "EP_")) {
        moe_variable <- paste0("MP_", substr(variable, 4, nchar(variable)))
      } else if (startsWith(variable, "E_")) {
        moe_variable <- paste0("M_", substr(variable, 3, nchar(variable)))
      } 
      
      # Adjust the label to include (%) if variable starts with "EP_"
      if (startsWith(variable, "EP_")) {
        y_label <- paste(variable_label, "(%)")
      } else {
        y_label <- variable_label
      }
      
      # Check if MOE variable exists in the data
      if (moe_variable %in% names(svi_data)) {
        plot <- ggplot(svi_data, aes(x = reorder(NAMELSAD, .data[[variable]]), y = .data[[variable]])) + 
          geom_bar_interactive(stat = "identity", fill = "forestgreen", 
                               aes(tooltip = paste0(NAMELSAD, "<br>",
                                                    variable_labels[[variable]], ": ", round(.data[[variable]], 2)), 
                                   data_id = GEOID)) +
          # geom_point_interactive(size = 2, shape = 21, color = "black", fill = "green", 
          #                        aes(tooltip = NAMELSAD, data_id = GEOID)) +
          # geom_errorbar(aes(ymin = .data[[variable]] - .data[[moe_variable]], 
          #                   ymax = .data[[variable]] + .data[[moe_variable]]), 
          #               width = 0.2, size = 0.2) +
          labs(title = paste(variable_label, "By Census Tract"),
               caption = "Data source: 2018-2022 ACS, US Census Bureau",
               y = y_label,
               x = " ") +
          coord_flip() +
          theme_minimal() +
          theme(text = element_text(family = "Lato"),
                plot.title = element_text(face = "bold", hjust = 0.5, size = 9),
                axis.title.y = element_text(face = "bold", vjust = 0.5, size = 7),
                axis.title.x = element_text(face = "bold", hjust = 0.5, size = 7),
                plot.caption = element_text(size = 5),
                axis.text.y = element_text(size = 6),
                axis.text.x = element_text(size = 6))
      }
    }
    
    combined_plot <- svi_map + plot + plot_layout(ncol = 2)
    x <- girafe(ggobj = combined_plot, width_svg = 10, height_svg = 5)  
    girafe_options(x, opts_hover(css = "fill:cyan;"), 
                     opts_selection(type = "multiple", only_shiny = TRUE),
                     opts_tooltip(css = "font-size: 10px; color: black; font-weight: bold;background-color: white;border-color: black;",
                                 opacity = 0.9))
    
  })
}



# Run the application 
shinyApp(ui = ui , server = server)

