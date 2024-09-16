library(shiny)
library(leaflet)
library(viridis)
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


# Load the data
svi <- st_read("portlandsvi.shp")
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
                      label = "Basemap",
                      choices = c(
                        "Open Street Map" = "OpenStreetMap",
                        "CARTO Positron" = "CartoDB.Positron",
                        "CARTO Voyager" = "CartoDB.Voyager",
                        "Stadia Toner" = "Stadia.StamenToner"),
                      selected = "CartoDB.Positron"),
          # For Census Tract Selected
          textOutput("tract_text"),
          width = 3),
    mainPanel(
      leafletOutput("map", width = "100%", height = "700px"), width = 9
    ))),
    
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
              column(width = 8)
            ),
            fluidRow(
              column(width = 12,
                     plotOutput("my_plot", height = "750px"))
            )
            ),
    
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
  
        # Erase Water
        plsvi <- erase_water(svi,
                             year = 2020) %>% st_make_valid()
        # Transform spatial data to WGS84
        sviwgs84 <- reactive({
          plsvi %>% 
            st_transform(crs = 4326)
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
        total_tracts <- nrow(svi)
        total_population <- sum(st_drop_geometry(svi)$E_TOTPOP, na.rm=TRUE)
        total_housing <- sum(st_drop_geometry(svi)$E_HU, na.rm=TRUE)
        total_households <- sum(st_drop_geometry(svi)$E_HH, na.rm=TRUE)
  
        # Create a reactive dataframe
        portlandsvi <- reactive({svi %>% st_drop_geometry() %>% select(input$variable) %>% 
            pull() %>% as.numeric() }) # Need to drop geometry first otherwise give error
        
        # Set colors
        pal <- reactive({
          colorNumeric("viridis", domain = portlandsvi(), alpha = 0.5, reverse = TRUE)})
        
        output$map <- renderLeaflet({
          leaflet() %>% 
          addProviderTiles(provider = basemap_providers[[input$basemap]]) %>% 
          addPolygons(
            data = sviwgs84(),
            fillColor = ~ pal()(portlandsvi()),
            weight = 0.5,
            fillOpacity = 0.5,
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
              color = "white",
              weight = 2.5,
              bringToFront = TRUE
            ),
            # Tooltip for census tract
            popup = ~ paste(
              "<div style='font-size: 10px;'>",
              "<strong>Census Tract: </strong>", str_remove(NAMELSAD, "Census Tract"), "<br/>",
              "<strong>Population: </strong>", formatC(E_TOTPOP, format = "d", big.mark = ","), "<br/>",
              "<strong>Housing Units: </strong>", formatC(E_HU, format = "d", big.mark = ","), "<br/>",
              "<strong>Households: </strong>", formatC(E_HH, format = "d", big.mark = ",")
            ),
            popupOptions = popupOptions(
             autoPan = TRUE,
             closeButton = TRUE
            )
          ) %>% 
          addLegend(
            position = "topright",
            pal = pal(),
            values = portlandsvi(),
            title = variable_labels[[input$variable]],
            opacity = 0.8,
            labFormat = labelFormat(suffix = "", digits = 2)
          )%>% 
            setView(lng = -70.2010, lat = 43.6190, zoom = 11.2) 
        })
        
        # # Detect the clicked tract by matching coordinates to the census tract
        # clicked_tract <- reactive({
        #   click <- input$map_shape_click
        #   if (!is.null(click)) {
        #     # Create an sf point from the clicked coordinates
        #     clicked_point <- st_as_sf(data.frame(lon = click$lng, lat = click$lat), 
        #                               coords = c("lon", "lat"), crs = 4326)
        #     
        #     # Find the tract that contains the clicked point
        #     matching_tracts <- st_contains(sviwgs84(), clicked_point, sparse = FALSE)
        #     
        #     # Get the corresponding census tract name
        #     clicked_tract <- sviwgs84()[which(matching_tracts), ]$NAMELSA
        #     return(clicked_tract)
        #   }
        #   return(NULL)
        # })
        # 
        # # Output the clicked tract
        # output$tract_text <- renderPrint({
        #   tract <- clicked_tract()
        #   if (!is.null(tract)) {
        #     glue::glue("You clicked on : {tract}")
        #   } 
        #   })
        
        # Reactive expression to filter data based on selected census tracts
        selected_data <- reactive({
                        svi %>% st_drop_geometry()
        })
        
        # Data Dictionary Button
        observeEvent(input$data_dictionary,{
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
                        class = "display nowrap"
                       )
        })
        
        # Download Files
        output$download_data <- downloadHandler(
          filename = function() {
            paste("portlandsvi", ".csv", sep = "")
          },
          content = function(file){
            write.csv(selected_data(),file, row.names = FALSE)
          }
        )
        
       # Render the text output for the info boxes
        output$tracts <- renderText({
          total_tracts
        })
        
        output$population <- renderText({
          formatC(total_population, format = "d", big.mark = ",")
        })
        
        output$housing <- renderText({
          formatC(total_housing, format = "d", big.mark = ",")
        })
        
        output$households <- renderText({
          formatC(total_households, format = "d", big.mark = ",")
        })
        
        
        # Render ggplot
        output$my_plot <- renderPlot({
          svi_data <- st_drop_geometry(svi)
          variable <- input$plot_variable
          variable_label <- variable_labels[[variable]]
          
          # Check if the variable is a THEME variable
          if (variable %in% c("RPL_THEMES", "RPL_THEME1", "RPL_THEME2", "RPL_THEME3", "RPL_THEME4")) {
            # For THEME variables, plot as before
            ggplot(svi_data, aes(x = reorder(NAMELSAD, .data[[variable]]), y = .data[[variable]])) +
              geom_bar(stat = "identity", fill = "forestgreen") +
              labs(title = paste(variable_label, "By Census Tract"),
                   caption = "Data source: 2018-2022 ACS, US Census Bureau",
                   x = " ",
                   y = variable_label) +
              coord_flip() +
              theme_minimal() +
              theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
                    axis.title.y = element_text(face = "bold",vjust = 0.5, size = 14),
                    axis.title.x = element_text(face = "bold", hjust = 0.5, size = 14),
                    axis.text.y = element_text(size = 11),
                    axis.text.x = element_text(size = 11))
                
          } else {
            # Determine the corresponding MOE variable
            if (startsWith(variable, "EP_")) {
              moe_variable <- paste0("MP_", substr(variable, 4, nchar(variable))) # start from the 4th character to the rest of the variable and replace prefix with MP
            } else if (startsWith(variable, "E_")) {
              moe_variable <- paste0("M_", substr(variable, 3, nchar(variable))) # start from the 3rd character to the rest of the variable and replace prefix with M
            } else {
              moe_variable <- NA
            }
            
            # Check if MOE variable exists in the data
            if (!is.na(moe_variable) && moe_variable %in% names(svi_data)) {
              # Plot with error bars
              ggplot(svi_data, aes(x = reorder(NAMELSAD, .data[[variable]]), y = .data[[variable]])) + 
              geom_bar(stat = "identity", fill = "forestgreen") +
                # geom_errorbar(aes(ymin = .data[[variable]] - .data[[moe_variable]], 
                #                   ymax = .data[[variable]] + .data[[moe_variable]]), width = 0.2) +
                labs(title = paste(variable_label, "By Census Tract"),
                     caption = "Data source: 2018-2022 ACS, US Census Bureau",
                     y = variable_label,
                     x = " ") +
                coord_flip()+
                theme_minimal() +
                theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
                      axis.title.y = element_text(face = "bold",vjust = 0.5, size = 14),
                      axis.title.x = element_text(face = "bold", hjust = 0.5, size = 14),
                      axis.text.y = element_text(size = 11),
                      axis.text.x = element_text(size = 11))
            } 
          }
        })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
