library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(readr)
library(scales)
library(htmltools)

source("R/helpers.R")

access <- read_csv("data/child_access_public.csv", show_col_types = FALSE)

ui <- page_sidebar(
  title = "Mapping Child Care Access and Resilience in Western North Carolina",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    selectInput("metric", "Map",
      choices = c(
        "Capacity-weighted access, 2023" = "cap23",
        "Capacity-weighted access, 2026" = "cap26",
        "Change in capacity-weighted access" = "capchange",
        "Quality-weighted access, 2023" = "qual23",
        "Quality-weighted access, 2026" = "qual26",
        "Change in quality-weighted access" = "qualchange",
        "Persistence in lowest-access communities" = "persistence"
      )
    ),
    selectInput("county", "County",
      choices = c("All WNC counties", sort(unique(access$county)))
    ),
    p(strong("Interpretation note: "),
      "2023–2026 changes are descriptive, not causal estimates of Hurricane Helene.")
  ),
  layout_column_wrap(
    width = 1/3,
    value_box("Demand locations", textOutput("n_locations")),
    value_box("Mean 2026 capacity access", textOutput("mean_access")),
    value_box("Locations improved", textOutput("pct_improved"))
  ),
  card(
    full_screen = TRUE,
    card_header(textOutput("map_title")),
    leafletOutput("map", height = 650)
  ),
  card(
    card_header("About the analysis"),
    p("The analysis uses a capacity-weighted enhanced two-step floating catchment area (E2SFCA) approach with 0–10 and 10–20 minute travel-time bands weighted 1.0 and 0.5."),
    p("The same fixed residential demand surface is used in 2023 and 2026. Quality-weighted comparisons are secondary and descriptive because the state quality-rating framework changed after the baseline.")
  )
)

server <- function(input, output, session) {
  filtered <- reactive({
    if (input$county == "All WNC counties") access
    else filter(access, county == input$county)
  })

  output$n_locations <- renderText(comma(nrow(filtered())))
  output$mean_access <- renderText(fmt_access(mean(filtered()$access_capacity_2026, na.rm = TRUE)))
  output$pct_improved <- renderText(percent(mean(filtered()$capacity_access_change > 0, na.rm = TRUE), accuracy = 0.1))

  output$map_title <- renderText({
    switch(input$metric,
      cap23 = "Capacity-Weighted Child Care Access, 2023",
      cap26 = "Capacity-Weighted Child Care Access, 2026",
      capchange = "Change in Capacity-Weighted Child Care Access, 2023–2026",
      qual23 = "Quality-Weighted Child Care Access, 2023",
      qual26 = "Quality-Weighted Child Care Access, 2026",
      qualchange = "Change in Quality-Weighted Child Care Access, 2023–2026",
      persistence = "Persistence and Change in the Lowest-Access Communities"
    )
  })

  output$map <- renderLeaflet({
    d <- filtered()
    m <- leaflet(d) |> addProviderTiles(providers$CartoDB.Positron)

    if (input$metric == "persistence") {
      pal <- colorFactor("Set2", domain = persistence_levels)
      return(m |>
        addCircleMarkers(~longitude_public, ~latitude_public,
          radius = 2.5, stroke = FALSE, fillOpacity = .72,
          color = ~pal(persistence),
          popup = ~paste0("<strong>", htmlEscape(county), " County</strong><br>",
                          htmlEscape(persistence))) |>
        addLegend("bottomright", pal = pal, values = ~persistence,
                  title = "Lowest-access status", opacity = .9))
    }

    metric_col <- switch(input$metric,
      cap23 = "access_capacity_2023",
      cap26 = "access_capacity_2026",
      capchange = "capacity_access_change",
      qual23 = "quality_access_2023",
      qual26 = "quality_access_2026",
      qualchange = "quality_access_change"
    )

    x <- d[[metric_col]]
    if (input$metric %in% c("capchange", "qualchange")) {
      lim <- as.numeric(quantile(abs(x), .98, na.rm = TRUE))
      x_map <- pmax(pmin(x, lim), -lim)
      pal <- colorNumeric("RdBu", domain = c(-lim, lim), reverse = TRUE)
      ttl <- "Change in access"
    } else {
      x_map <- x
      pal <- colorNumeric("viridis", domain = x)
      ttl <- "Accessibility"
    }

    m |>
      addCircleMarkers(~longitude_public, ~latitude_public,
        radius = 2.4, stroke = FALSE, fillOpacity = .72,
        color = pal(x_map),
        popup = paste0("<strong>", htmlEscape(d$county), " County</strong><br>",
                       ttl, ": ", fmt_access(x))) |>
      addLegend("bottomright", pal = pal, values = x_map, title = ttl, opacity = .9)
  })
}

shinyApp(ui, server)
