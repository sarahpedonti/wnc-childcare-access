library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(sf)
library(scales)

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


# =========================================================================
# VALIDATE PUBLIC DATA
# =========================================================================

required_hex_fields <- c(
  "county",
  "n_locations",
  "access_capacity_2023",
  "access_capacity_2026",
  "capacity_access_change",
  "quality_access_2023",
  "quality_access_2026",
  "quality_access_change",
  "persistence"
)

missing_hex_fields <- setdiff(
  required_hex_fields,
  names(hex)
)

if (length(missing_hex_fields) > 0) {
  
  stop(
    "The GeoJSON is missing these expected variables: ",
    paste(
      missing_hex_fields,
      collapse = ", "
    )
  )
}


required_summary_fields <- c(
  "county",
  "n_locations",
  "mean_capacity_2023",
  "mean_capacity_2026",
  "mean_capacity_change",
  "pct_improved_capacity",
  "pct_no_center_2023",
  "pct_no_center_2026",
  "mean_quality_2023",
  "mean_quality_2026",
  "mean_quality_change",
  "pct_improved_quality",
  "pct_persistent_low"
)

missing_summary_fields <- setdiff(
  required_summary_fields,
  names(summary_df)
)

if (length(missing_summary_fields) > 0) {
  
  stop(
    "The summary CSV is missing these expected variables: ",
    paste(
      missing_summary_fields,
      collapse = ", "
    )
  )
}


# =========================================================================
# CHOICES
# =========================================================================

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
# COMMON MAP SCALE LIMITS
# =========================================================================

capacity_all <- c(
  hex$access_capacity_2023,
  hex$access_capacity_2026
)

capacity_limits <- as.numeric(
  quantile(
    capacity_all,
    probs = c(
      0.02,
      0.98
    ),
    na.rm = TRUE
  )
)


quality_all <- c(
  hex$quality_access_2023,
  hex$quality_access_2026
)

quality_limits <- as.numeric(
  quantile(
    quality_all,
    probs = c(
      0.02,
      0.98
    ),
    na.rm = TRUE
  )
)


capacity_change_limit <- as.numeric(
  quantile(
    abs(
      hex$capacity_access_change
    ),
    0.98,
    na.rm = TRUE
  )
)


quality_change_limit <- as.numeric(
  quantile(
    abs(
      hex$quality_access_change
    ),
    0.98,
    na.rm = TRUE
  )
)


# =========================================================================
# BLUE-GREEN PALETTE
# =========================================================================

bluegreen_pal <- colorRampPalette(
  c(
    "#edf8fb",
    "#b2e2e2",
    "#66c2a4",
    "#2ca25f",
    "#006d2c"
  )
)(256)


# =========================================================================
# RUTHERFORDTON REFERENCE
# =========================================================================

rutherfordton_lng <- -81.9568
rutherfordton_lat <- 35.3693


# =========================================================================
# UI
# =========================================================================

ui <- navbarPage(
  
  title = "WNC Child Care Access",
  
  
  # -----------------------------------------------------------------------
  # INTERACTIVE MAP
  # -----------------------------------------------------------------------
  
  tabPanel(
    
    "Interactive Map",
    
    fluidPage(
      
      tags$head(
        
        tags$style(
          
          HTML(
            "
            body {
              font-family: Arial, sans-serif;
              background-color: #ffffff;
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
              max-width: 1000px;
            }

            .sidebar-box {
              background: #f7f9f8;
              border: 1px solid #dddddd;
              border-radius: 6px;
              padding: 18px;
            }

            .metric-box {
              border: 1px solid #dddddd;
              border-radius: 6px;
              padding: 12px;
              margin-bottom: 10px;
              background: white;
              min-height: 90px;
            }

            .metric-label {
              color: #666666;
              font-size: 13px;
              line-height: 1.2;
            }

            .metric-value {
              font-size: 22px;
              font-weight: 700;
              margin-top: 5px;
            }

            .map-container {
              border: 1px solid #dddddd;
              border-radius: 6px;
              overflow: hidden;
              background: white;
            }

            .note {
              color: #666666;
              font-size: 13px;
              margin-top: 15px;
              line-height: 1.4;
            }

            .methods-note {
              max-width: 900px;
              line-height: 1.6;
            }

            .leaflet-control {
              font-size: 12px;
            }
            "
          )
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
        
        # -----------------------------------------------------------------
        # SIDEBAR
        # -----------------------------------------------------------------
        
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
            
            uiOutput(
              "metric_note"
            ),
            
            div(
              class = "note",
              strong("Interpretation: "),
              paste(
                "2023–2026 differences are descriptive longitudinal",
                "changes and should not be interpreted as causal effects",
                "of Hurricane Helene."
              )
            )
          )
        ),
        
        
        # -----------------------------------------------------------------
        # MAIN PANEL
        # -----------------------------------------------------------------
        
        column(
          
          width = 9,
          
          fluidRow(
            
            column(
              width = 3,
              
              div(
                class = "metric-box",
                
                div(
                  class = "metric-label",
                  textOutput(
                    "card1_label"
                  )
                ),
                
                div(
                  class = "metric-value",
                  textOutput(
                    "card1_value"
                  )
                )
              )
            ),
            
            
            column(
              width = 3,
              
              div(
                class = "metric-box",
                
                div(
                  class = "metric-label",
                  textOutput(
                    "card2_label"
                  )
                ),
                
                div(
                  class = "metric-value",
                  textOutput(
                    "card2_value"
                  )
                )
              )
            ),
            
            
            column(
              width = 3,
              
              div(
                class = "metric-box",
                
                div(
                  class = "metric-label",
                  textOutput(
                    "card3_label"
                  )
                ),
                
                div(
                  class = "metric-value",
                  textOutput(
                    "card3_value"
                  )
                )
              )
            ),
            
            
            column(
              width = 3,
              
              div(
                class = "metric-box",
                
                div(
                  class = "metric-label",
                  textOutput(
                    "card4_label"
                  )
                ),
                
                div(
                  class = "metric-value",
                  textOutput(
                    "card4_value"
                  )
                )
              )
            )
          ),
          
          
          h4(
            textOutput(
              "map_title"
            )
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
  # KEY FINDINGS
  # -----------------------------------------------------------------------
  
  tabPanel(
    
    "Key Findings",
    
    fluidPage(
      
      h2(
        "What changed from 2023 to 2026?"
      ),
      
      p(
        class = "methods-note",
        paste(
          "The regional child care system showed broad improvement",
          "in both capacity-weighted and quality-weighted accessibility",
          "between 2023 and 2026, while geographically persistent",
          "low-access communities remained."
        )
      ),
      
      hr(),
      
      h3(
        "Recovery without equalization"
      ),
      
      p(
        class = "methods-note",
        paste(
          "Regional improvement does not imply equal geographic recovery.",
          "The maps show substantial spatial heterogeneity in access",
          "levels and longitudinal change across Western North Carolina."
        )
      ),
      
      h3(
        "Capacity and quality are related but distinct"
      ),
      
      p(
        class = "methods-note",
        paste(
          "Capacity-weighted accessibility reflects licensed provider",
          "capacity relative to competing geographic demand.",
          "Quality-weighted accessibility further scales provider",
          "capacity using available star ratings."
        )
      ),
      
      h3(
        "Rutherford County repair"
      ),
      
      p(
        class = "methods-note",
        paste(
          "The final public visualization uses the repaired 18-county",
          "demand surface, including reconstructed Rutherford County",
          "residential demand locations. The final analytic demand",
          "surface contains 41,078 modeled child-demand locations."
        )
      )
    )
  ),
  
  
  # -----------------------------------------------------------------------
  # METHODS
  # -----------------------------------------------------------------------
  
  tabPanel(
    
    "Methods",
    
    fluidPage(
      
      h2(
        "Enhanced two-step floating catchment area analysis"
      ),
      
      p(
        class = "methods-note",
        paste(
          "The primary accessibility measure is a capacity-weighted",
          "enhanced two-step floating catchment area (E2SFCA) score.",
          "For each provider, licensed capacity is evaluated relative",
          "to geographically weighted competing child demand.",
          "Provider accessibility ratios are then summed across",
          "providers reachable from each modeled child-demand location."
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
        "Quality-weighted accessibility"
      ),
      
      p(
        class = "methods-note",
        paste(
          "Quality-weighted accessibility uses the same E2SFCA",
          "capacity ratio but multiplies each provider contribution",
          "by its star rating divided by five.",
          "Unrated providers therefore contribute zero to the",
          "quality-weighted measure."
        )
      ),
      
      
      h3(
        "Final demand surface"
      ),
      
      p(
        class = "methods-note",
        paste(
          "The final repaired analysis includes 41,078 modeled",
          "child-demand locations across 18 Western North Carolina",
          "counties. This consists of the original 37,982-location",
          "fixed demand surface plus 3,096 reconstructed Rutherford",
          "County locations."
        )
      ),
      
      
      h3(
        "Longitudinal comparison"
      ),
      
      p(
        class = "methods-note",
        paste(
          "Within each metric family, the 2023 and 2026 maps use",
          "the same display scale so identical colors represent",
          "identical levels of accessibility across years."
        )
      ),
      
      
      h3(
        "Map scaling"
      ),
      
      p(
        class = "methods-note",
        paste(
          "For visualization, access scales are trimmed at the",
          "2nd and 98th percentiles. This prevents a small number",
          "of extreme values from compressing most regional",
          "variation into a narrow portion of the color scale.",
          "Original untrimmed analytic values remain in map popups."
        )
      ),
      
      
      h3(
        "Public visualization and privacy"
      ),
      
      p(
        class = "methods-note",
        paste(
          "Exact residential demand locations are not displayed.",
          "Results are aggregated to a regular 5-kilometer",
          "hexagonal grid. Hexagons represented by fewer than",
          "10 modeled demand locations are suppressed from the",
          "public visualization."
        )
      ),
      
      
      h3(
        "Persistent low access"
      ),
      
      p(
        class = "methods-note",
        paste(
          "Lowest access is defined independently within each year",
          "as the bottom decile of capacity-weighted accessibility.",
          "Persistent low-access locations fall within this lowest",
          "decile in both 2023 and 2026."
        )
      ),
      
      
      h3(
        "Causal interpretation"
      ),
      
      p(
        class = "methods-note",
        paste(
          "The analysis describes longitudinal system change across",
          "the period surrounding Hurricane Helene. It does not",
          "estimate a causal effect of the hurricane."
        )
      )
    )
  ),
  
  
  # -----------------------------------------------------------------------
  # ABOUT
  # -----------------------------------------------------------------------
  
  tabPanel(
    
    "About",
    
    fluidPage(
      
      h2(
        "About this project"
      ),
      
      p(
        class = "methods-note",
        paste(
          "This project translates research on child care as social,",
          "economic, and recovery infrastructure in rural communities",
          "into an interactive public research product."
        )
      ),
      
      p(
        strong(
          "Researcher: "
        ),
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

server <- function(
    input,
    output,
    session
) {
  
  
  # -----------------------------------------------------------------------
  # FILTERED HEXES
  # -----------------------------------------------------------------------
  
  filtered_hex <- reactive({
    
    req(
      input$county
    )
    
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
  # SELECTED SUMMARY ROW
  # -----------------------------------------------------------------------
  
  selected_summary <- reactive({
    
    req(
      input$county
    )
    
    summary_df |>
      filter(
        county ==
          input$county
      ) |>
      slice(
        1
      )
  })
  
  
  # =========================================================================
  # RESPONSIVE SUMMARY CARDS
  # =========================================================================
  
  output$card1_label <- renderText({
    
    if (
      input$metric %in%
      c(
        "qual23",
        "qual26",
        "qualchange"
      )
    ) {
      
      "Mean quality-weighted access, 2023"
      
    } else if (
      input$metric ==
      "persistence"
    ) {
      
      "Modeled demand locations"
      
    } else {
      
      "Mean capacity access, 2023"
    }
  })
  
  
  output$card1_value <- renderText({
    
    s <- selected_summary()
    
    if (
      input$metric %in%
      c(
        "qual23",
        "qual26",
        "qualchange"
      )
    ) {
      
      fmt_access(
        s$mean_quality_2023
      )
      
    } else if (
      input$metric ==
      "persistence"
    ) {
      
      comma(
        s$n_locations
      )
      
    } else {
      
      fmt_access(
        s$mean_capacity_2023
      )
    }
  })
  
  
  output$card2_label <- renderText({
    
    if (
      input$metric %in%
      c(
        "qual23",
        "qual26",
        "qualchange"
      )
    ) {
      
      "Mean quality-weighted access, 2026"
      
    } else if (
      input$metric ==
      "persistence"
    ) {
      
      "Persistent low-access share"
      
    } else {
      
      "Mean capacity access, 2026"
    }
  })
  
  
  output$card2_value <- renderText({
    
    s <- selected_summary()
    
    if (
      input$metric %in%
      c(
        "qual23",
        "qual26",
        "qualchange"
      )
    ) {
      
      fmt_access(
        s$mean_quality_2026
      )
      
    } else if (
      input$metric ==
      "persistence"
    ) {
      
      percent(
        s$pct_persistent_low,
        accuracy = 0.1
      )
      
    } else {
      
      fmt_access(
        s$mean_capacity_2026
      )
    }
  })
  
  
  output$card3_label <- renderText({
    
    if (
      input$metric %in%
      c(
        "qual23",
        "qual26",
        "qualchange"
      )
    ) {
      
      "Locations with improved quality-weighted access"
      
    } else if (
      input$metric ==
      "persistence"
    ) {
      
      "No center within 20 min., 2023"
      
    } else {
      
      "Locations with improved capacity access"
    }
  })
  
  
  output$card3_value <- renderText({
    
    s <- selected_summary()
    
    if (
      input$metric %in%
      c(
        "qual23",
        "qual26",
        "qualchange"
      )
    ) {
      
      percent(
        s$pct_improved_quality,
        accuracy = 0.1
      )
      
    } else if (
      input$metric ==
      "persistence"
    ) {
      
      percent(
        s$pct_no_center_2023,
        accuracy = 0.1
      )
      
    } else {
      
      percent(
        s$pct_improved_capacity,
        accuracy = 0.1
      )
    }
  })
  
  
  output$card4_label <- renderText({
    
    if (
      input$metric %in%
      c(
        "qual23",
        "qual26",
        "qualchange"
      )
    ) {
      
      "Mean quality-weighted change"
      
    } else if (
      input$metric ==
      "persistence"
    ) {
      
      "No center within 20 min., 2026"
      
    } else {
      
      "No center within 20 min., 2026"
    }
  })
  
  
  output$card4_value <- renderText({
    
    s <- selected_summary()
    
    if (
      input$metric %in%
      c(
        "qual23",
        "qual26",
        "qualchange"
      )
    ) {
      
      fmt_access(
        s$mean_quality_change
      )
      
    } else {
      
      percent(
        s$pct_no_center_2026,
        accuracy = 0.1
      )
    }
  })
  
  
  # =========================================================================
  # MAP TITLE
  # =========================================================================
  
  output$map_title <- renderText({
    
    switch(
      
      input$metric,
      
      cap23 =
        "Capacity-weighted child care access, 2023",
      
      cap26 =
        "Capacity-weighted child care access, 2026",
      
      capchange =
        "Change in capacity-weighted child care access, 2023–2026",
      
      qual23 =
        "Quality-weighted child care access, 2023",
      
      qual26 =
        "Quality-weighted child care access, 2026",
      
      qualchange =
        "Change in quality-weighted child care access, 2023–2026",
      
      persistence =
        "Persistent low-access communities"
    )
  })
  
  
  # =========================================================================
  # METRIC NOTE
  # =========================================================================
  
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
          "Higher values indicate greater licensed center capacity",
          "relative to geographically weighted competing demand.",
          "The same scale is used as in the 2023 map."
        ),
      
      capchange =
        paste(
          "Lighter blue-green values indicate greater decline or",
          "less improvement. Darker green values indicate greater",
          "improvement in capacity-weighted accessibility."
        ),
      
      qual23 =
        paste(
          "Quality-weighted access scales each provider's capacity",
          "contribution by its star rating divided by five."
        ),
      
      qual26 =
        paste(
          "The same quality-weighted display scale is used as in",
          "2023 to support direct visual comparison."
        ),
      
      qualchange =
        paste(
          "Lighter blue-green values indicate greater decline or",
          "less improvement. Darker green values indicate greater",
          "improvement in quality-weighted accessibility."
        ),
      
      persistence =
        paste(
          "Persistent low-access communities were in the bottom",
          "decile of capacity-weighted accessibility in both",
          "2023 and 2026."
        )
    )
    
    
    p(
      strong(
        "Measure note: "
      ),
      note
    )
  })
  
  
  # =========================================================================
  # MAP
  # =========================================================================
  
  output$map <- renderLeaflet({
    
    d <- filtered_hex()
    
    req(
      nrow(d) > 0
    )
    
    
    # ---------------------------------------------------------------------
    # KEY-FREE BASEMAP
    # ---------------------------------------------------------------------
    
    base_map <- function(data) {
      
      leaflet(
        data
      ) |>
        
        addTiles(
          
          urlTemplate =
            "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
          
          attribution =
            '&copy; OpenStreetMap contributors',
          
          options =
            tileOptions(
              opacity = 0.50,
              updateWhenIdle = TRUE
            )
        )
    }
    
    
    # ---------------------------------------------------------------------
    # CAPACITY 2023
    # ---------------------------------------------------------------------
    
    if (
      input$metric ==
      "cap23"
    ) {
      
      d$map_value <- pmax(
        pmin(
          d$access_capacity_2023,
          capacity_limits[2]
        ),
        capacity_limits[1]
      )
      
      
      pal <- colorNumeric(
        
        palette =
          bluegreen_pal,
        
        domain =
          capacity_limits,
        
        na.color =
          "#eeeeee"
      )
      
      
      m <- base_map(
        d
      ) |>
        
        addPolygons(
          
          fillColor =
            ~pal(
              map_value
            ),
          
          fillOpacity =
            0.86,
          
          color =
            "white",
          
          weight =
            0.6,
          
          popup =
            ~paste0(
              
              "<strong>",
              county,
              " County</strong><br>",
              
              "Capacity-weighted access, 2023: ",
              fmt_access(
                access_capacity_2023
              ),
              "<br>",
              
              "Demand locations in hex: ",
              comma(
                n_locations
              )
            )
        ) |>
        
        addLegend(
          
          position =
            "bottomright",
          
          pal =
            pal,
          
          values =
            capacity_limits,
          
          title =
            "Capacity-weighted access",
          
          opacity =
            1
        )
    }
    
    
    # ---------------------------------------------------------------------
    # CAPACITY 2026
    # ---------------------------------------------------------------------
    
    else if (
      input$metric ==
      "cap26"
    ) {
      
      d$map_value <- pmax(
        pmin(
          d$access_capacity_2026,
          capacity_limits[2]
        ),
        capacity_limits[1]
      )
      
      
      pal <- colorNumeric(
        palette =
          bluegreen_pal,
        domain =
          capacity_limits,
        na.color =
          "#eeeeee"
      )
      
      
      m <- base_map(
        d
      ) |>
        
        addPolygons(
          
          fillColor =
            ~pal(
              map_value
            ),
          
          fillOpacity =
            0.86,
          
          color =
            "white",
          
          weight =
            0.6,
          
          popup =
            ~paste0(
              
              "<strong>",
              county,
              " County</strong><br>",
              
              "Capacity-weighted access, 2026: ",
              fmt_access(
                access_capacity_2026
              ),
              "<br>",
              
              "Demand locations in hex: ",
              comma(
                n_locations
              )
            )
        ) |>
        
        addLegend(
          position =
            "bottomright",
          pal =
            pal,
          values =
            capacity_limits,
          title =
            "Capacity-weighted access",
          opacity =
            1
        )
    }
    
    
    # ---------------------------------------------------------------------
    # CAPACITY CHANGE
    # ---------------------------------------------------------------------
    
    else if (
      input$metric ==
      "capchange"
    ) {
      
      cap <-
        capacity_change_limit
      
      
      d$map_value <- pmax(
        pmin(
          d$capacity_access_change,
          cap
        ),
        -cap
      )
      
      
      pal <- colorNumeric(
        palette =
          bluegreen_pal,
        domain =
          c(
            -cap,
            cap
          ),
        na.color =
          "#eeeeee"
      )
      
      
      m <- base_map(
        d
      ) |>
        
        addPolygons(
          
          fillColor =
            ~pal(
              map_value
            ),
          
          fillOpacity =
            0.86,
          
          color =
            "white",
          
          weight =
            0.6,
          
          popup =
            ~paste0(
              
              "<strong>",
              county,
              " County</strong><br>",
              
              "Change in capacity-weighted access: ",
              fmt_access(
                capacity_access_change
              ),
              "<br>",
              
              "2023: ",
              fmt_access(
                access_capacity_2023
              ),
              "<br>",
              
              "2026: ",
              fmt_access(
                access_capacity_2026
              ),
              "<br>",
              
              "Demand locations in hex: ",
              comma(
                n_locations
              )
            )
        ) |>
        
        addLegend(
          position =
            "bottomright",
          pal =
            pal,
          values =
            c(
              -cap,
              cap
            ),
          title =
            "Change in capacity access",
          opacity =
            1
        )
    }
    
    
    # ---------------------------------------------------------------------
    # QUALITY 2023
    # ---------------------------------------------------------------------
    
    else if (
      input$metric ==
      "qual23"
    ) {
      
      d$map_value <- pmax(
        pmin(
          d$quality_access_2023,
          quality_limits[2]
        ),
        quality_limits[1]
      )
      
      
      pal <- colorNumeric(
        palette =
          bluegreen_pal,
        domain =
          quality_limits,
        na.color =
          "#eeeeee"
      )
      
      
      m <- base_map(
        d
      ) |>
        
        addPolygons(
          
          fillColor =
            ~pal(
              map_value
            ),
          
          fillOpacity =
            0.86,
          
          color =
            "white",
          
          weight =
            0.6,
          
          popup =
            ~paste0(
              
              "<strong>",
              county,
              " County</strong><br>",
              
              "Quality-weighted access, 2023: ",
              fmt_access(
                quality_access_2023
              ),
              "<br>",
              
              "Demand locations in hex: ",
              comma(
                n_locations
              )
            )
        ) |>
        
        addLegend(
          position =
            "bottomright",
          pal =
            pal,
          values =
            quality_limits,
          title =
            "Quality-weighted access",
          opacity =
            1
        )
    }
    
    
    # ---------------------------------------------------------------------
    # QUALITY 2026
    # ---------------------------------------------------------------------
    
    else if (
      input$metric ==
      "qual26"
    ) {
      
      d$map_value <- pmax(
        pmin(
          d$quality_access_2026,
          quality_limits[2]
        ),
        quality_limits[1]
      )
      
      
      pal <- colorNumeric(
        palette =
          bluegreen_pal,
        domain =
          quality_limits,
        na.color =
          "#eeeeee"
      )
      
      
      m <- base_map(
        d
      ) |>
        
        addPolygons(
          
          fillColor =
            ~pal(
              map_value
            ),
          
          fillOpacity =
            0.86,
          
          color =
            "white",
          
          weight =
            0.6,
          
          popup =
            ~paste0(
              
              "<strong>",
              county,
              " County</strong><br>",
              
              "Quality-weighted access, 2026: ",
              fmt_access(
                quality_access_2026
              ),
              "<br>",
              
              "Demand locations in hex: ",
              comma(
                n_locations
              )
            )
        ) |>
        
        addLegend(
          position =
            "bottomright",
          pal =
            pal,
          values =
            quality_limits,
          title =
            "Quality-weighted access",
          opacity =
            1
        )
    }
    
    
    # ---------------------------------------------------------------------
    # QUALITY CHANGE
    # ---------------------------------------------------------------------
    
    else if (
      input$metric ==
      "qualchange"
    ) {
      
      cap <-
        quality_change_limit
      
      
      d$map_value <- pmax(
        pmin(
          d$quality_access_change,
          cap
        ),
        -cap
      )
      
      
      pal <- colorNumeric(
        palette =
          bluegreen_pal,
        domain =
          c(
            -cap,
            cap
          ),
        na.color =
          "#eeeeee"
      )
      
      
      m <- base_map(
        d
      ) |>
        
        addPolygons(
          
          fillColor =
            ~pal(
              map_value
            ),
          
          fillOpacity =
            0.86,
          
          color =
            "white",
          
          weight =
            0.6,
          
          popup =
            ~paste0(
              
              "<strong>",
              county,
              " County</strong><br>",
              
              "Change in quality-weighted access: ",
              fmt_access(
                quality_access_change
              ),
              "<br>",
              
              "2023: ",
              fmt_access(
                quality_access_2023
              ),
              "<br>",
              
              "2026: ",
              fmt_access(
                quality_access_2026
              ),
              "<br>",
              
              "Demand locations in hex: ",
              comma(
                n_locations
              )
            )
        ) |>
        
        addLegend(
          position =
            "bottomright",
          pal =
            pal,
          values =
            c(
              -cap,
              cap
            ),
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
      
      persistence_levels <- c(
        "Not in lowest-access decile",
        "Moved out of lowest-access decile",
        "Moved into lowest-access decile",
        "Persistently lowest-access"
      )
      
      
      persistence_colors <- c(
        "#eeeeee",
        "#b2e2e2",
        "#66c2a4",
        "#006d2c"
      )
      
      
      pal <- colorFactor(
        palette =
          persistence_colors,
        domain =
          persistence_levels,
        na.color =
          "#eeeeee"
      )
      
      
      m <- base_map(
        d
      ) |>
        
        addPolygons(
          
          fillColor =
            ~pal(
              persistence
            ),
          
          fillOpacity =
            0.86,
          
          color =
            "white",
          
          weight =
            0.6,
          
          popup =
            ~paste0(
              
              "<strong>",
              county,
              " County</strong><br>",
              
              persistence,
              "<br>",
              
              "Capacity access, 2023: ",
              fmt_access(
                access_capacity_2023
              ),
              "<br>",
              
              "Capacity access, 2026: ",
              fmt_access(
                access_capacity_2026
              ),
              "<br>",
              
              "Demand locations in hex: ",
              comma(
                n_locations
              )
            )
        ) |>
        
        addLegend(
          position =
            "bottomright",
          pal =
            pal,
          values =
            persistence_levels,
          title =
            "Lowest-access status",
          opacity =
            1
        )
    }
    
    
    # ---------------------------------------------------------------------
    # RUTHERFORDTON REFERENCE
    # ---------------------------------------------------------------------
    
    show_rutherfordton <-
      input$county %in%
      c(
        "All WNC counties",
        "Rutherford"
      )
    
    
    if (
      show_rutherfordton
    ) {
      
      m <- m |>
        
        addCircleMarkers(
          
          lng =
            rutherfordton_lng,
          
          lat =
            rutherfordton_lat,
          
          radius =
            4,
          
          color =
            "#222222",
          
          weight =
            1.5,
          
          fillColor =
            "#ffffff",
          
          fillOpacity =
            1,
          
          label =
            "Rutherfordton",
          
          popup =
            paste0(
              "<strong>Rutherfordton, North Carolina</strong><br>",
              "Rutherford County reference location"
            )
        )
    }
    
    
    # ---------------------------------------------------------------------
    # RETURN MAP
    # ---------------------------------------------------------------------
    
    m
  })
}


# =========================================================================
# RUN APP
# =========================================================================

shinyApp(
  ui,
  server
)