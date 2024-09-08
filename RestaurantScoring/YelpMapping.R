library(shiny)
library(shinydashboard)
library(shinymanager)
library(readr)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(sf)
library(DT)
library(leafpop)


scoring <- read_csv("./ranking_for_leaflet.csv")
scoring$score_cate <- NA

scoring[scoring$Overall >= 8, "score_cate"] <- "High"
scoring[scoring$Overall < 8 & scoring$Overall >= 7, "score_cate"] <- "Mid-High"
scoring[scoring$Overall < 7 & scoring$Overall >= 4, "score_cate"] <- "Mid"
scoring[scoring$Overall < 4 & scoring$Overall >= 3, "score_cate"] <- "Mid-Low"
scoring[scoring$Overall < 3, "score_cate"] <- "Low"

scoring$score_cate <- factor(scoring$score_cate, levels = c("Low", "Mid-Low", "Mid", "Mid-High", "High"))

header <- 
    dashboardHeader(
        title =  "Restaurant Scoring",
        tags$li(a(href = 'https://www.shinyapps.io/',
                  title = "Shiny Apps"),
                class = "dropdown")
        
    )


sidebar <- 
    dashboardSidebar(
        sidebarMenu(
            menuItem("Map", tabName = "rest_mapping_tab"),
            menuItem("Table", tabName = "rest_list_tab")
            )
        )


body <- 
    dashboardBody(
        tabItems(
            tabItem(tabName = "rest_mapping_tab",
                    fluidRow(
                        box(title = strong("Restaurant Score Mapping  (source: CHRIBA, Purdue University)"),
                            solidHeader = FALSE,
                            color = "black",
                            status = "primary",
                            width = 12,
                            height = "750",
                            leafletOutput("rest_map_output", height = 650))
                        )
                    ),
            tabItem(tabName = "rest_list_tab",
                    fluidRow(
                      box(title = strong("Restaurant List (source: CHRIBA, Purdue University"),
                          solidHeader = FALSE,
                          color = "black",
                          status = "primary",
                          width = 12,
                          DTOutput("rest_list_output")
                          )
                    )
                    
                    
                    
                    )

        ) 
    ) 


ui <- 
    dashboardPage(
        title = "Restaurant Scoring (Source: CHRIBA, Purdue University)",
        header,
        sidebar,
        body,
        skin = "black")

server <- function(input, output, session) {
    
  rest_map <- reactive({
    
    logos <- awesomeIconList(
      "Low" = makeAwesomeIcon(
        markerColor = "red",
        library = "fa"
      ),
      "Mid-Low" = makeAwesomeIcon(
        markerColor = "pink",
        library = "fa"
      ),
      "Mid" = makeAwesomeIcon(
        markerColor = "green",
        library = "fa"
      ),
      "Mid-High" = makeAwesomeIcon(
        markerColor = "lightblue",
        library = "fa"
      ),
      "High" = makeAwesomeIcon(
        markerColor = "blue",
        library = "fa"
      )
    )
    
    
    category_color_list <- c("red", "pink", "green", "lightblue", "blue")
    
    address.df <- split(scoring, scoring$category)
    color_df <- split(scoring, scoring$score_cate)
    
    
    color_df2 <- c()
    color_df2[["Low"]] <- color_df[[2]]
    color_df2[["Mid-Low"]] <- color_df[[5]]
    color_df2[["Mid"]] <- color_df[[3]]
    color_df2[["Mid-High"]] <- color_df[[4]]
    color_df2[["High"]] <- color_df[[1]]
    
    pal_gg <- colorFactor(category_color_list, scoring$score_cate)

    l <- 
      leaflet() %>% 
      addTiles() %>% 
      addProviderTiles('CartoDB.Positron')

    names(address.df) %>%
        purrr::walk(function(df) {
            l <<- l %>%
              addAwesomeMarkers(data = address.df[[df]],
                                icon = ~logos[score_cate],
                                lng = ~lon, 
                                lat = ~lat,
                                label = ~paste0(as.character(revised_name), " (", as.character(category), "/", as.character(address), ")"),
                                popup = ~popupTable(address.df[[df]] %>% select(revised_name, category, Ambience:`Social Value`), feature.id = FALSE, row.numbers = FALSE),
                                group = df,
                                labelOptions = labelOptions(noHide = F,
                                                             direction = 'auto'))
        })


    l %>%
        addLayersControl(
            overlayGroups = names(address.df),
            options = layersControlOptions(collapsed = FALSE)
        ) %>% 
      addLegend(position = "bottomright", 
                pal = pal_gg, 
                values = names(color_df), 
                opacity = 1, 
                title = "Score category")

  })

  output$rest_map_output <- leaflet::renderLeaflet({ rest_map() })
  
  output$rest_list_output <- DT::renderDataTable({ 
    
    reduced_df <- 
      scoring %>% 
      select(revised_name,
             category,
             address, 
             Ambience:Overall)
    
    DT::datatable(reduced_df, options = list(pageLength = 5,
                                             lengthMenu = c(5, 50, 100, 250)))
    
    })
  
 
}

shinyApp(ui, server)
