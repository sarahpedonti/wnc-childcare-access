library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(sf)
library(scales)
library(htmltools)

# ==============================================================================
# DATA
# ==============================================================================

hex <- st_read(
  "data/child_access_hex.geojson",
  quiet = TRUE
) |>
  st_transform(4326)

summary_df <- read_csv(
  "data/county_summary_public.csv",
  show_col_types = FALSE
)

county_choices <- c(
  "All WNC counties",
  sort(unique(summary_df$county[summary_df$county != "All WNC counties"]))
)

metric_choices <- c(
  "EC-serving licensed-capacity access, 2023" = "cap23",
  "EC-serving licensed-capacity access, 2026" = "cap26",
  "Change in EC-serving licensed-capacity access, 2023–2026" = "capchange",
  "Quality-adjusted EC-serving capacity access, 2023" = "qual23",
  "Quality-adjusted EC-serving capacity access, 2026" = "qual26",
  "Change in quality-adjusted EC-serving capacity access, 2023–2026" = "qualchange",
  "Persistent low-access communities" = "persistence"
)

bluegreen_pal <- c(
  "#edf8fb",
  "#b2e2e2",
  "#66c2a4",
  "#2ca25f",
  "#006d2c"
)

fmt_access <- function(x) {
  scales::number(x, accuracy = 0.001)
}

# ------------------------------------------------------------------------------
# COMMON DISPLAY LIMITS
# ------------------------------------------------------------------------------

capacity_all <- c(
  hex$access_capacity_2023,
  hex$access_capacity_2026
)

capacity_limits <- as.numeric(
  quantile(
    capacity_all,
    probs = c(0.02, 0.98),
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
    probs = c(0.02, 0.98),
    na.rm = TRUE
  )
)

capacity_change_limit <- as.numeric(
  quantile(
    abs(hex$capacity_access_change),
    0.98,
    na.rm = TRUE
  )
)

quality_change_limit <- as.numeric(
  quantile(
    abs(hex$quality_access_change),
    0.98,
    na.rm = TRUE
  )
)

# ==============================================================================
# UI — deliberately uses classic Shiny layout for maximum stability
# ==============================================================================

ui <- navbarPage(
  title = "WNC Child Care Access",

  tabPanel(
    "Interactive map",

    fluidPage(
      tags$style(
        HTML("
          .hero-title {
            font-size: 30px;
            font-weight: 700;
            margin-top: 18px;
            margin-bottom: 6px;
          }
          .hero-sub {
            font-size: 16px;
            color: #5b6570;
            margin-bottom: 18px;
          }
          .small-note {
            font-size: 13px;
            color: #65717d;
          }
          .metric-box {
            border: 1px solid #ddd;
            border-radius: 6px;
            padding: 12px 14px;
            margin-bottom: 14px;
            background: #fff;
          }
          .metric-big {
            font-size: 23px;
            font-weight: 700;
          }
        ")
      ),

      div(
        class = "hero-title",
        "Mapping Child Care Access and Resilience in Western North Carolina"
      ),

      div(
        class = "hero-sub",
        paste(
          "A descriptive geospatial analysis of licensed center-based",
          "early-childhood accessibility across 18 Western North Carolina",
          "counties, comparing 2023 and 2026."
        )
      ),

      sidebarLayout(
        sidebarPanel(
          width = 3,

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
            class = "small-note",
            strong("Interpretation: "),
            paste(
              "2023–2026 differences are descriptive longitudinal changes,",
              "not causal estimates of Hurricane Helene."
            )
          ),

          br(),

          div(
            class = "small-note",
            strong("Why hexagons? "),
            paste(
              "Each colored hexagon summarizes nearby modeled child-demand",
              "locations. Hexagons with fewer than 10 modeled locations are",
              "not displayed."
            )
          )
        ),

        mainPanel(
          width = 9,

          fluidRow(
            column(
              4,
              div(
                class = "metric-box",
                strong("Mean access, 2023"),
                div(class = "metric-big", textOutput("mean23"))
              )
            ),
            column(
              4,
              div(
                class = "metric-box",
                strong("Mean access, 2026"),
                div(class = "metric-big", textOutput("mean26"))
              )
            ),
            column(
              4,
              div(
                class = "metric-box",
                strong("Locations improved"),
                div(class = "metric-big", textOutput("pct_improved"))
              )
            )
          ),

          h4(textOutput("map_title")),

          leafletOutput(
            "map",
            width = "100%",
            height = "650px"
          )
        )
      )
    )
  ),

  tabPanel(
    "Key findings",

    fluidPage(
      h2("What changed from 2023 to 2026?"),

      p(
        paste(
          "Across the matched 18-county region, the number of licensed",
          "providers serving children under age 5 declined slightly while",
          "total licensed capacity at those EC-serving providers increased.",
          "Geographic gains were uneven rather than universal."
        )
      ),

      fluidRow(
        column(
          4,
          div(
            class = "metric-box",
            strong("EC-serving providers"),
            div(class = "metric-big", "332 → 327"),
            div(class = "small-note", "−1.5%")
          )
        ),
        column(
          4,
          div(
            class = "metric-box",
            strong("Licensed capacity at EC-serving providers"),
            div(class = "metric-big", "20,000 → 21,883"),
            div(class = "small-note", "+9.4%")
          )
        ),
        column(
          4,
          div(
            class = "metric-box",
            strong("Capacity-access improvement"),
            div(class = "metric-big", "59.6%"),
            div(class = "small-note", "38.1% declined")
          )
        )
      ),

      h3("Capacity growth without uniform recovery"),

      p(
        paste(
          "Mean EC-serving licensed-capacity accessibility increased 9.4%,",
          "but 38.1% of modeled child-demand locations experienced declines.",
          "Jackson County illustrates this divergence: its EC-serving provider",
          "network fell from 18 to 12 providers and from 702 to 511 licensed",
          "slots at EC-serving facilities (−27.2%)."
        )
      ),

      h3("Quality-adjusted access"),

      p(
        paste(
          "Quality-adjusted EC-serving capacity access increased by 6.3%.",
          "About 57.8% of modeled locations improved and 39.8% declined.",
          "The mean number of reachable 4- or 5-star EC-serving providers",
          "was essentially flat (23.63 in 2023 vs. 23.51 in 2026)."
        )
      ),

      p(
        class = "small-note",
        paste(
          "Quality-rating coverage declined from 97.6% in 2023 to 90.2%",
          "in 2026. North Carolina also changed its quality-rating framework",
          "after the 2023 baseline, so longitudinal quality comparisons are",
          "descriptive."
        )
      )
    )
  ),

  tabPanel(
    "Methods",

    fluidPage(
      h2("Enhanced two-step floating catchment area analysis"),

      p(
        paste(
          "The primary accessibility measure is an enhanced two-step floating",
          "catchment area (E2SFCA) score. Provider supply is related to",
          "geographically weighted competing demand and then summed across",
          "providers reachable from each modeled child-demand location."
        )
      ),

      h3("Provider age eligibility"),

      p(
        paste(
          "The analytic provider universe was aligned with the under-5 demand",
          "population. School-age-only licenses were excluded. The routed",
          "analytic universe includes 332 EC-serving providers in 2023 and",
          "327 in 2026."
        )
      ),

      h3("Capacity measure"),

      p(
        paste(
          "For mixed-age programs, the broad capacity measure retains full",
          "licensed facility capacity because age-specific licensed slot counts",
          "are not consistently available. Values therefore represent licensed",
          "capacity at EC-serving providers, not preschool or under-5 slots."
        )
      ),

      h3("Travel-time structure"),

      tags$ul(
        tags$li("0–10 minute travel-time band: weight 1.0"),
        tags$li("10–20 minute travel-time band: weight 0.5")
      ),

      h3("Longitudinal design"),

      p(
        paste(
          "The same repaired 41,078-location residential child-demand surface",
          "is used in both years."
        )
      ),

      h3("Quality-adjusted analysis"),

      p(
        paste(
          "The secondary quality measure weights each provider's capacity",
          "E2SFCA ratio by star rating divided by five. Quality-rating coverage",
          "was 97.6% in 2023 and 90.2% in 2026. Longitudinal quality",
          "comparisons are descriptive because North Carolina changed its",
          "rating framework after the 2023 baseline."
        )
      ),

      h3("Public visualization and privacy"),

      p(
        paste(
          "The public app does not display raw residential locations.",
          "Child-level analytic results are aggregated to a regular hexagonal",
          "grid, and cells with fewer than 10 modeled demand locations are",
          "suppressed."
        )
      ),

      h3("Causal interpretation"),

      p(
        paste(
          "The analysis describes pre/post system change around the Hurricane",
          "Helene period. It does not estimate a causal effect of the hurricane."
        )
      )
    )
  ),

  tabPanel(
    "About",

    fluidPage(
      h2("About this project"),

      p(
        paste(
          "This project translates research on child care as social and",
          "economic infrastructure in rural and disaster-affected communities",
          "into an interactive research product."
        )
      ),

      p(
        strong("Researcher: "),
        "Sarah F. Pedonti, Ph.D."
      ),

      tags$a(
        href = "https://github.com/sarahpedonti/wnc-childcare-access",
        target = "_blank",
        "View reproducible code on GitHub"
      )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {

  filtered_hex <- reactive({

    req(input$county)

    if (input$county == "All WNC counties") {
      hex
    } else {
      hex |>
        filter(county == input$county)
    }
  })

  selected_summary <- reactive({

    req(input$county)

    summary_df |>
      filter(county == input$county) |>
      slice(1)
  })

  output$mean23 <- renderText({
    fmt_access(selected_summary()$mean_capacity_2023)
  })

  output$mean26 <- renderText({
    fmt_access(selected_summary()$mean_capacity_2026)
  })

  output$pct_improved <- renderText({
    percent(
      selected_summary()$pct_improved_capacity,
      accuracy = 0.1
    )
  })

  output$map_title <- renderText({

    switch(
      input$metric,
      cap23 = "EC-serving licensed-capacity access, 2023",
      cap26 = "EC-serving licensed-capacity access, 2026",
      capchange = "Change in EC-serving licensed-capacity access, 2023–2026",
      qual23 = "Quality-adjusted EC-serving capacity access, 2023",
      qual26 = "Quality-adjusted EC-serving capacity access, 2026",
      qualchange = "Change in quality-adjusted EC-serving capacity access, 2023–2026",
      persistence = "Persistent low-access communities"
    )
  })

  output$metric_note <- renderUI({

    note <- switch(
      input$metric,

      cap23 = paste(
        "Higher values indicate greater licensed capacity at EC-serving",
        "providers relative to geographically weighted competing demand."
      ),

      cap26 = paste(
        "The same fixed 41,078-location demand surface and display scale",
        "are used for longitudinal comparison."
      ),

      capchange = paste(
        "Lighter blue-green values indicate relative decline or little",
        "improvement; darker green values indicate greater improvement."
      ),

      qual23 = paste(
        "This secondary measure weights EC-serving licensed capacity by",
        "provider star rating."
      ),

      qual26 = paste(
        "Quality comparisons are descriptive; the state rating framework",
        "changed after baseline and rating coverage declined in 2026."
      ),

      qualchange = paste(
        "Lighter values indicate relative decline or less improvement;",
        "darker green values indicate greater improvement."
      ),

      persistence = paste(
        "Lowest access is defined independently within each year as the",
        "bottom decile of EC-serving licensed-capacity accessibility."
      )
    )

    p(
      strong("Measure note: "),
      note
    )
  })

  output$map <- renderLeaflet({

    d <- filtered_hex()

    req(nrow(d) > 0)

    # Always rebuild a clean Leaflet object.
    m <- leaflet(d) |>
      addTiles(
        urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution = '&copy; OpenStreetMap contributors',
        options = tileOptions(
          opacity = 0.55,
          updateWhenIdle = TRUE
        )
      )

    if (input$metric == "persistence") {

      persistence_levels <- c(
        "Not in lowest-access decile",
        "Moved out of lowest-access decile",
        "Moved into lowest-access decile",
        "Persistently lowest-access"
      )

      pal <- colorFactor(
        palette = c(
          "#eeeeee",
          "#b2e2e2",
          "#66c2a4",
          "#006d2c"
        ),
        domain = persistence_levels,
        na.color = "#eeeeee"
      )

      d$popup_html <- paste0(
        "<strong>",
        htmlEscape(d$county),
        " County</strong><br>",
        htmlEscape(d$persistence),
        "<br>Modeled demand locations: ",
        comma(d$n_locations)
      )

      m <- leaflet(d) |>
        addTiles(
          urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
          attribution = '&copy; OpenStreetMap contributors',
          options = tileOptions(opacity = 0.55)
        ) |>
        addPolygons(
          fillColor = ~pal(persistence),
          fillOpacity = 0.82,
          color = "white",
          weight = 0.5,
          popup = ~popup_html
        ) |>
        addLegend(
          position = "bottomright",
          pal = pal,
          values = persistence_levels,
          title = "Lowest-access status",
          opacity = 1
        )

    } else {

      # Select the metric explicitly. This avoids dynamic formula evaluation
      # inside leaflet/htmlwidgets.
      if (input$metric == "cap23") {
        raw_value <- d$access_capacity_2023
        limits <- capacity_limits
        legend_title <- "EC-serving capacity access"
      } else if (input$metric == "cap26") {
        raw_value <- d$access_capacity_2026
        limits <- capacity_limits
        legend_title <- "EC-serving capacity access"
      } else if (input$metric == "capchange") {
        raw_value <- d$capacity_access_change
        limits <- c(-capacity_change_limit, capacity_change_limit)
        legend_title <- "Change in capacity access"
      } else if (input$metric == "qual23") {
        raw_value <- d$quality_access_2023
        limits <- quality_limits
        legend_title <- "Quality-adjusted capacity access"
      } else if (input$metric == "qual26") {
        raw_value <- d$quality_access_2026
        limits <- quality_limits
        legend_title <- "Quality-adjusted capacity access"
      } else {
        raw_value <- d$quality_access_change
        limits <- c(-quality_change_limit, quality_change_limit)
        legend_title <- "Change in quality-adjusted access"
      }

      map_value <- pmax(
        pmin(raw_value, limits[2]),
        limits[1]
      )

      pal <- colorNumeric(
        palette = bluegreen_pal,
        domain = limits,
        na.color = "#eeeeee"
      )

      d$map_value <- as.numeric(map_value)
      d$popup_value <- as.numeric(raw_value)
      # Construct popup text without reactive/dynamic formula objects.
      d$popup_html <- paste0(
        "<strong>",
        htmlEscape(d$county),
        " County</strong><br>",
        htmlEscape(
          switch(
            input$metric,
            cap23 = "EC-serving licensed-capacity access, 2023",
            cap26 = "EC-serving licensed-capacity access, 2026",
            capchange = "Change in EC-serving licensed-capacity access",
            qual23 = "Quality-adjusted EC-serving capacity access, 2023",
            qual26 = "Quality-adjusted EC-serving capacity access, 2026",
            qualchange = "Change in quality-adjusted EC-serving capacity access"
          )
        ),
        ": ",
        fmt_access(d$popup_value),
        "<br>Modeled demand locations: ",
        comma(d$n_locations)
      )

      m <- leaflet(d) |>
        addTiles(
          urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
          attribution = '&copy; OpenStreetMap contributors',
          options = tileOptions(
            opacity = 0.55,
            updateWhenIdle = TRUE
          )
        ) |>
        addPolygons(
          fillColor = ~pal(map_value),
          fillOpacity = 0.82,
          color = "white",
          weight = 0.5,
          popup = ~popup_html
        ) |>
        addLegend(
          position = "bottomright",
          pal = pal,
          values = d$map_value,
          title = legend_title,
          opacity = 1
        )
    }

    # Add place-name labels above the hex layer.
    m <- m |>
      addProviderTiles(
        providers$CartoDB.PositronOnlyLabels,
        group = "Place labels"
      )

    # Reference marker.
    m <- m |>
      addCircleMarkers(
        lng = -81.9568,
        lat = 35.3693,
        radius = 4,
        color = "#222222",
        weight = 1.5,
        fillColor = "white",
        fillOpacity = 1,
        label = "Rutherfordton",
        popup = "<strong>Rutherfordton, NC</strong>"
      )

    bounds <- st_bbox(d)

    m |>
      fitBounds(
        as.numeric(bounds["xmin"]),
        as.numeric(bounds["ymin"]),
        as.numeric(bounds["xmax"]),
        as.numeric(bounds["ymax"])
      )
  })
}

shinyApp(ui, server)
