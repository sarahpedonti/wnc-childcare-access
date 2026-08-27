library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(sf)
library(scales)
library(RColorBrewer)
library(viridisLite)

source("R/helpers.R")


# =========================================================================
# DATA
# =========================================================================

hex <- st_read(
  "data/child_access_hex.geojson",
  quiet = TRUE
)

summary_df <- read_csv(
  "data/county_summary_public.csv",
  show_col_types = FALSE
)

county_choices <- c(
  "All WNC counties",
  sort(
    unique(
      summary_df$county[
        summary_df$county != "All WNC counties"
      ]
    )
  )
)

metric_choices <- c(
  "Capacity-weighted access, 2023" = "cap23",
  "Capacity-weighted access, 2026" = "cap26",
  "Change in capacity-weighted access, 2023–2026" = "capchange",
  "Quality-weighted access, 2023" = "qual23",
  "Quality-weighted access, 2026" = "qual26",
  "Change in quality-weighted access, 2023–2026" = "qualchange",
  "Persistent low-access communities" = "persistence"
)


# =========================================================================
# UI
# =========================================================================

ui <- navbarPage(
  
  title = "WNC Child Care Access",
  
  # -----------------------------------------------------------------------
  # MAP TAB
  # -----------------------------------------------------------------------
  
  tabPanel(
    
    "Interactive Map",
    
    fluidPage(
      
      tags$head(
        tags$style(
          HTML("
            body {
              font-family: Arial, sans-serif;
            }

            .page-title {
              font-size: 30px;
              font-weight: 700;
              margin-top: 20px;
              margin-bottom: 5px;
            }

            .subtitle {
              font-size: 16px;
              color: #555;
              margin-bottom: 20px;
            }

            .sidebar-box {
              background: #f7f7f7;
              border: 1px solid #ddd;
              border-radius: 6px;
              padding: 18px;
            }

            .metric-box {
              border: 1px solid #ddd;
              border-radius: 6px;
              padding: 12px;
              margin-bottom: 10px;
              background: white;
            }

            .metric-label {
              color: #666;
              font-size: 13px;
            }

            .metric-value {
              font-size: 22px;
              font-weight: 700;
            }

            .map-container {
              border: 1px solid #ddd;
              border-radius: 6px;
              overflow: hidden;
            }

            .note {
              color: #666;
              font-size: 13px;
              margin-top: 15px;
            }
          ")
        )
      ),
      
      div(
        class = "page-title",
        "Mapping Child Care Access and Resilience in Western North Carolina"
      ),
      
      div(
        class = "subtitle",
        paste(
          "A descriptive geospatial analysis of licensed center-based",
          "child care accessibility across 18 Western North Carolina",
          "counties, comparing 2023 and 2026."
        )
      ),
      
      fluidRow(
        
        column(
          
          width = 3,
          
          div(
            
            class = "sidebar-box",
            
            selectInput(
              "metric",
              "Measure",
              choices = metric_choices,
              selected = "capchange"
            ),
            
            selectInput(
              "county",
              "County",
              choices = county_choices,
              selected = "All WNC counties"
            ),
            
            hr(),
            
            uiOutput("metric_note"),
            
            div(
              class = "note",
              strong("Interpretation: "),
              paste(
                "2023–2026 differences are descriptive longitudinal",
                "changes, not causal estimates of Hurricane Helene."
              )
            )
          )
        ),
        
        column(
          
          width = 9,
          
          fluidRow(
            
            column(
              width = 3,
              div(
                class = "metric-box",
                div(
                  class = "metric-label",
                  "Mean access, 2023"
                ),
                div(
                  class = "metric-value",
                  textOutput("mean23")
                )
              )
            ),
            
            column(
              width = 3,
              div(
                class = "metric-box",
                div(
                  class = "metric-label",
                  "Mean access, 2026"
                ),
                div(
                  class = "metric-value",
                  textOutput("mean26")
                )
              )
            ),
            
            column(
              width = 3,
              div(
                class = "metric-box",
                div(
                  class = "metric-label",
                  "Locations improved"
                ),
                div(
                  class = "metric-value",
                  textOutput("pct_improved")
                )
              )
            ),
            
            column(
              width = 3,
              div(
                class = "metric-box",
                div(
                  class = "metric-label",
                  "No center within 20 min."
                ),
                div(
                  class = "metric-value",
                  textOutput("pct_zero")
                )
              )
            )
          ),
          
          h4(
            textOutput("map_title")
          ),
          
          div(
            class = "map-container",
            
            leafletOutput(
              "map",
              height = "650px"
            )
          )
        )
      )
    )
  ),
  
  
  # -----------------------------------------------------------------------
  # FINDINGS TAB
  # -----------------------------------------------------------------------
  
  tabPanel(
    
    "Key Findings",
    
    fluidPage(
      
      h2(
        "What changed from 2023 to 2026?"
      ),
      
      p(
        paste(
          "The regional child care system showed substantial recovery",
          "and expansion, but aggregate improvement did not eliminate",
          "geographically persistent low-access pockets."
        )
      ),
      
      hr(),
      
      h3(
        "Recovery without equalization"
      ),
      
      p(
        paste(
          "Regional gains in licensed supply and accessibility were",
          "broad, yet the lowest-access communities did not experience",
          "comparable improvement."
        )
      )
    )
  ),
  
  
  # -----------------------------------------------------------------------
  # METHODS TAB
  # -----------------------------------------------------------------------
  
  tabPanel(
    
    "Methods",
    
    fluidPage(
      
      h2(
        "Enhanced two-step floating catchment area analysis"
      ),
      
      p(
        paste(
          "The primary accessibility measure is a capacity-weighted",
          "enhanced two-step floating catchment area (E2SFCA) score.",
          "Provider supply is related to geographically weighted",
          "competing demand and summed across providers reachable",
          "from each child-demand location."
        )
      ),
      
      h3(
        "Travel-time structure"
      ),
      
      tags$ul(
        tags$li(
          "0–10 minute travel-time band: weight 1.0"
        ),
        tags$li(
          "10–20 minute travel-time band: weight 0.5"
        )
      ),
      
      h3(
        "Public visualization and privacy"
      ),
      
      p(
        paste(
          "The public app does not display raw residential locations.",
          "Child-level analytic results are aggregated to a regular",
          "hexagonal grid before publication."
        )
      ),
      
      h3(
        "Causal interpretation"
      ),
      
      p(
        paste(
          "The analysis describes pre/post system change around the",
          "Hurricane Helene period. It does not estimate a causal effect",
          "of the hurricane."
        )
      )
    )
  ),
  
  
  # -----------------------------------------------------------------------
  # ABOUT TAB
  # -----------------------------------------------------------------------
  
  tabPanel(
    
    "About",
    
    fluidPage(
      
      h2(
        "About this project"
      ),
      
      p(
        paste(
          "This project translates research on child care as social",
          "and economic infrastructure in rural and disaster-affected",
          "communities into an interactive research product."
        )
      ),
      
      p(
        strong("Researcher: "),
        "Sarah F. Pedonti, Ph.D."
      ),
      
      tags$a(
        href =
          "https://github.com/sarahpedonti/wnc-childcare-access",
        target = "_blank",
        "View reproducible code on GitHub"
      )
    )
  )
)


# =========================================================================
# SERVER
# =========================================================================

server <- function(input, output, session) {
  
  
  # -----------------------------------------------------------------------
  # FILTERED SPATIAL DATA
  # -----------------------------------------------------------------------
  
  filtered_hex <- reactive({
    
    req(input$county)
    
    if (
      input$county ==
      "All WNC counties"
    ) {
      
      hex
      
    } else {
      
      hex |>
        filter(
          county ==
            input$county
        )
    }
  })
  
  
  # -----------------------------------------------------------------------
  # SUMMARY DATA
  # -----------------------------------------------------------------------
  
  selected_summary <- reactive({
    
    req(input$county)
    
    summary_df |>
      filter(
        county ==
          input$county
      ) |>
      slice(1)
  })
  
  
  output$mean23 <- renderText({
    
    s <- selected_summary()
    
    fmt_access(
      s$mean_capacity_2023
    )
  })
  
  
  output$mean26 <- renderText({
    
    s <- selected_summary()
    
    fmt_access(
      s$mean_capacity_2026
    )
  })
  
  
  output$pct_improved <- renderText({
    
    s <- selected_summary()
    
    percent(
      s$pct_improved_capacity,
      accuracy = 0.1
    )
  })
  
  
  output$pct_zero <- renderText({
    
    s <- selected_summary()
    
    percent(
      s$pct_no_center_2026,
      accuracy = 0.1
    )
  })
  
  
  # -----------------------------------------------------------------------
  # MAP TITLE
  # -----------------------------------------------------------------------
  
  output$map_title <- renderText({
    
    switch(
      input$metric,
      
      cap23 =
        "Capacity-weighted access, 2023",
      
      cap26 =
        "Capacity-weighted access, 2026",
      
      capchange =
        "Change in capacity-weighted access, 2023–2026",
      
      qual23 =
        "Quality-weighted access, 2023",
      
      qual26 =
        "Quality-weighted access, 2026",
      
      qualchange =
        "Change in quality-weighted access, 2023–2026",
      
      persistence =
        "Persistent low-access communities"
    )
  })
  
  
  # -----------------------------------------------------------------------
  # NOTES
  # -----------------------------------------------------------------------
  
  output$metric_note <- renderUI({
    
    note <- switch(
      
      input$metric,
      
      cap23 =
        paste(
          "Higher values indicate greater licensed center capacity",
          "relative to geographically weighted competing demand."
        ),
      
      cap26 =
        paste(
          "The same fixed child-demand surface is used",
          "in 2023 and 2026."
        ),
      
      capchange =
        paste(
          "Positive values indicate improved accessibility;",
          "negative values indicate declining accessibility."
        ),
      
      qual23 =
        paste(
          "This secondary measure incorporates licensed capacity,",
          "competing demand, and provider star rating."
        ),
      
      qual26 =
        paste(
          "Quality-weighted comparisons are descriptive because",
          "North Carolina's rating framework changed after baseline."
        ),
      
      qualchange =
        paste(
          "Interpret longitudinal quality change descriptively rather",
          "than as a like-for-like causal comparison."
        ),
      
      persistence =
        paste(
          "Lowest access is defined independently within each year",
          "as the bottom decile of capacity-weighted accessibility."
        )
    )
    
    p(
      strong("Measure note: "),
      note
    )
  })
  
  
  # =========================================================================
  # MAP
  #
  # IMPORTANT:
  # This intentionally follows the exact construction pattern from
  # map_test.R that successfully rendered the colored polygons.
  # =========================================================================
  
  output$map <- renderLeaflet({
    
    d <- filtered_hex()
    
    req(
      nrow(d) > 0
    )
    
    
    # ---------------------------------------------------------------------
    # CAPACITY 2023
    # ---------------------------------------------------------------------
    
    if (input$metric == "cap23") {
      
      pal <- colorNumeric(
        palette =
          viridis(256),
        domain =
          d$access_capacity_2023,
        na.color =
          "#d9d9d9"
      )
      
      leaflet(d) |>
        addTiles() |>
        addPolygons(
          fillColor =
            ~pal(access_capacity_2023),
          fillOpacity =
            0.8,
          color =
            "white",
          weight =
            0.5
        ) |>
        addLegend(
          position =
            "bottomright",
          pal =
            pal,
          values =
            ~access_capacity_2023,
          title =
            "Capacity-weighted access",
          opacity =
            1
        )
    }
    
    
    # ---------------------------------------------------------------------
    # CAPACITY 2026
    # ---------------------------------------------------------------------
    
    else if (input$metric == "cap26") {
      
      pal <- colorNumeric(
        palette =
          viridis(256),
        domain =
          d$access_capacity_2026,
        na.color =
          "#d9d9d9"
      )
      
      leaflet(d) |>
        addTiles() |>
        addPolygons(
          fillColor =
            ~pal(access_capacity_2026),
          fillOpacity =
            0.8,
          color =
            "white",
          weight =
            0.5
        ) |>
        addLegend(
          position =
            "bottomright",
          pal =
            pal,
          values =
            ~access_capacity_2026,
          title =
            "Capacity-weighted access",
          opacity =
            1
        )
    }
    
    
    # ---------------------------------------------------------------------
    # CAPACITY CHANGE
    # ---------------------------------------------------------------------
    
    else if (input$metric == "capchange") {
      
      x <- d$capacity_access_change
      
      cap <- as.numeric(
        quantile(
          abs(x),
          0.98,
          na.rm = TRUE
        )
      )
      
      pal <- colorNumeric(
        palette =
          rev(
            brewer.pal(
              11,
              "RdBu"
            )
          ),
        domain =
          c(-cap, cap),
        na.color =
          "#d9d9d9"
      )
      
      leaflet(d) |>
        addTiles() |>
        addPolygons(
          fillColor =
            ~pal(capacity_access_change),
          fillOpacity =
            0.8,
          color =
            "white",
          weight =
            0.5
        ) |>
        addLegend(
          position =
            "bottomright",
          pal =
            pal,
          values =
            ~capacity_access_change,
          title =
            "Change in access",
          opacity =
            1
        )
    }
    
    
    # ---------------------------------------------------------------------
    # QUALITY 2023
    # ---------------------------------------------------------------------
    
    else if (input$metric == "qual23") {
      
      pal <- colorNumeric(
        palette =
          viridis(256),
        domain =
          d$quality_access_2023,
        na.color =
          "#d9d9d9"
      )
      
      leaflet(d) |>
        addTiles() |>
        addPolygons(
          fillColor =
            ~pal(quality_access_2023),
          fillOpacity =
            0.8,
          color =
            "white",
          weight =
            0.5
        ) |>
        addLegend(
          position =
            "bottomright",
          pal =
            pal,
          values =
            ~quality_access_2023,
          title =
            "Quality-weighted access",
          opacity =
            1
        )
    }
    
    
    # ---------------------------------------------------------------------
    # QUALITY 2026
    # ---------------------------------------------------------------------
    
    else if (input$metric == "qual26") {
      
      pal <- colorNumeric(
        palette =
          viridis(256),
        domain =
          d$quality_access_2026,
        na.color =
          "#d9d9d9"
      )
      
      leaflet(d) |>
        addTiles() |>
        addPolygons(
          fillColor =
            ~pal(quality_access_2026),
          fillOpacity =
            0.8,
          color =
            "white",
          weight =
            0.5
        ) |>
        addLegend(
          position =
            "bottomright",
          pal =
            pal,
          values =
            ~quality_access_2026,
          title =
            "Quality-weighted access",
          opacity =
            1
        )
    }
    
    
    # ---------------------------------------------------------------------
    # QUALITY CHANGE
    # ---------------------------------------------------------------------
    
    else if (input$metric == "qualchange") {
      
      x <- d$quality_access_change
      
      cap <- as.numeric(
        quantile(
          abs(x),
          0.98,
          na.rm = TRUE
        )
      )
      
      pal <- colorNumeric(
        palette =
          rev(
            brewer.pal(
              11,
              "RdBu"
            )
          ),
        domain =
          c(-cap, cap),
        na.color =
          "#d9d9d9"
      )
      
      leaflet(d) |>
        addTiles() |>
        addPolygons(
          fillColor =
            ~pal(quality_access_change),
          fillOpacity =
            0.8,
          color =
            "white",
          weight =
            0.5
        ) |>
        addLegend(
          position =
            "bottomright",
          pal =
            pal,
          values =
            ~quality_access_change,
          title =
            "Change in quality-weighted access",
          opacity =
            1
        )
    }
    
    
    # ---------------------------------------------------------------------
    # PERSISTENCE
    # ---------------------------------------------------------------------
    
    else {
      
      pal <- colorFactor(
        palette =
          c(
            "#d9d9d9",
            "#66c2a5",
            "#fc8d62",
            "#8c2d04"
          ),
        domain =
          c(
            "Not in lowest-access decile",
            "Moved out of lowest-access decile",
            "Moved into lowest-access decile",
            "Persistently lowest-access"
          ),
        na.color =
          "#d9d9d9"
      )
      
      leaflet(d) |>
        addTiles() |>
        addPolygons(
          fillColor =
            ~pal(persistence),
          fillOpacity =
            0.8,
          color =
            "white",
          weight =
            0.5
        ) |>
        addLegend(
          position =
            "bottomright",
          pal =
            pal,
          values =
            ~persistence,
          title =
            "Lowest-access status",
          opacity =
            1
        )
    }
  })
}


# =========================================================================
# RUN
# =========================================================================

shinyApp(
  ui,
  server
)