# ---- Load packages ----
library(tidyverse)
library(readxl)
library(janitor)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(viridis)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(bslib)
library(leaflet)
library(plotly)
library(scales)
library(showtext)
library(rsconnect)

# ---- Load fonts ----
font_add_google(name = "Open Sans", family = "Open Sans")
font_add_google(name = "Roboto Mono", family = "Roboto Mono")
font_add_google(name = "Fira Sans", family = "Fira Sans")
showtext_auto()

tl_data <- read_rds("data/dashboard_tl_data.rds")

regions_global_list <- c("Asia", "Central Africa", "East Africa", "Latin America", "Middle East & North Africa", "Ukraine Response", "West Africa", "Global")
regions_list <- c("Asia", "Central Africa", "East Africa", "Latin America", "Middle East & North Africa", "Ukraine Response", "West Africa")
countries_list <- c("Afghanistan", "Bangladesh", "Burkina Faso", "Burundi", "Cameroon", "CAR", "Chad", "Colombia", "Côte d'Ivoire", "DRC", "Ecuador", "El Salvador", "Ethiopia", "Guatemala", "Honduras", "India", "Iraq", "Jordan", "Kenya", "Lebanon", "Liberia", "Libya", "Malaysia", "Mali", "Mexico", "Myanmar", "Niger", "Nigeria", "Pakistan", "Peru", "Philippines", "Poland", "Sierra Leone", "Somalia", "South Sudan", "Sudan", "Syria", "Tanzania", "Thailand", "Uganda", "Ukraine", "Venezuela", "Yemen", "Zimbabwe")

sector_map <- c(
  "All sectors" = "clients",
  "Education" = "total_edu",
  "Health" = "total_health",
  "ERD" = "total_erd",
  "VPRU" = "total_safety",
  "Governance" = "total_gov"
)

# ---- Create Shiny App ----
# Define UI
ui <- fluidPage(
  # Custom CSS for fonts
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Fira+Sans:wght@400;700&family=Roboto:wght@400;700&family=Roboto+Mono:wght@400;700&display=swap"),
    tags$style(HTML("
      body { font-family: 'Fira Sans', sans-serif; }
      h1 { font-family: 'Fira Sans', sans-serif; font-size: 32px; font-weight: bold; text-align: center; }
      h3 { font-family: 'Roboto Mono', monospace; font-size: 20px; font-weight: bold; }
      .section-header { font-family: 'Fira Sans', sans-serif; font-size: 22px; font-weight: bold; text-align: center; margin-top: 15px; margin-bottom: 15px; }
      .sub-header { 
        font-family: 'Open Sans', sans-serif;
        font-size: 18px;
        font-weight: normal;
        text-align: center;
        margin-bottom: 30px;
      }
      .container { max-width: 1000px; margin: auto; }
      .model-title { font-size: 18px; font-weight: bold; margin-bottom: 10px; }
      .full-width { 
        text-align: center;
        font-family: 'Roboto Mono', monospace;
        font-size: 20px;
        font-weight: bold;
        width: 100%;
        display: block;
        margin-top: 15px;
        margin-bottom: 30px;
      }
    "))
  ),
  
  div(class = "container",
      # Dashboard Title
      h1("NGO Client Dashboard"),
      
      div("Visualising client numbers and service reach of NGOX's international programs division", class = "sub-header"),
      
      # Explanation Sections
      fluidRow(
        column(6,
               h3("What am I looking at?"),
               p("This dashboard allows you to view estimates of NGOX's client reach. Use the filters to create custom views by region, country, and sector."),
               p("These estimates are based on the Annual Statistics, a comprehensive effort by our international programmes division to accurately count those we serve. Not all services are the same, so these numbers don’t reflect the positive effect that NGOX has had on the lives of clients. A vaccine appointment, a know-your-rights session and entrepreneurship training are very different, have different costs, and different benefits. But all are important contributions to our clients’ wellbeing."),
               p("Note that the numbers for country totals are estimates of the unique number of clients.  If you sum the sectors, the total won't match the country totals. This is because NGOX often supports the same individuals across multiple sectors. A person may be counted in each relevant sector but only once in the country totals."),
               p("Enjoy!")
        ),
        column(6,
               h3("What are models?"),
               p("Our client reach models give reliable estimates of the number of clients reached by NGOX."),
               p("NGOX doesn't count individual clients because we don't want to create barriers to services. We provide services to people regardless of whether they register, and we don't want to put people off. Our Annual Statistics give validated and accurate numbers for service delivery. We use these numbers along with assumptions and prior datapoints to model the overall number of clients reached. The models are explained in more detail below the charts."),
               p("In brief, Model 1 is the original approach. Model 2 corrects for data reporting inconsistencies between countries. Model 3 makes more conservative assumptions about the reach of NGOX's health programmes. Model 4 models the overlap between sectors from the bottom up, using prior data to inform overlap percentages.")
        )
      ),
      
      # Filters
      box(
        title = "Filters",
        width = 12,
        solidHeader = TRUE,
        status = NULL,  # No header color
        # First row
        fluidRow(
          column(4, selectInput("model", "Select model", choices = c("m1", "m2", "m3", "m4"), selected = "m1")),
          column(4, selectInput("region", "Select region", choices = c("All", unique(regions_list)), selected = "All")),
          column(4, selectInput("country", "Select country", choices = c("All", countries_list), selected = "All"))
        ),
        # Second row
        fluidRow(
          column(4, selectInput("sector", "Select sector", choices = names(sector_map), selected = "All sectors")),
          column(4, selectInput("year", "Financial year", choices = c(sort(unique(tl_data$year))), selected = "2023")),
          column(4, actionButton("reset", "Reset filters"))
        )
      ),
      
      # Display Total Clients
      fluidRow(column(12, h3(uiOutput("total_clients")))),
      
      # Map & Sector Titles
      fluidRow(
        column(7, div("Where around the world does NGOX serve clients", class = "section-header")),
        column(5, div("Number of direct clients by sector", class = "section-header"))
      ),
      
      # Map & Bar Chart
      fluidRow(
        column(7, leafletOutput("map", height = "400px")),
        column(5, plotOutput("bar_chart", height = "400px"))
      ),
      
      # Time series chart title
      div("Change in client numbers over time", class = "section-header"),
      
      # Toggle Facet - countries
      fluidRow(column(12, checkboxInput("line_toggle", "Stacked lines?", value = TRUE))),
      
      # Time series chart
      #      fluidRow(column(12, plotOutput("time_series_chart", height = "400px", width = "1000px"))),
      fluidRow(
        # Filters Panel (Occupies 3 columns)
        column(3,
               box(title = "Select options", status = "primary", solidHeader = TRUE, width = 12,
                   div(style = "height: 300px; overflow-y: scroll;",  # Scrollable box
                       checkboxGroupInput("selected_global", "Global",
                                          choices = "Global", selected = "Global"),
                       checkboxGroupInput("selected_regions", "Regions",
                                          choices = regions_list, selected = NULL)
                       #                       checkboxGroupInput("selected_countries", "Countries",
                       #                                          choices = countries_list, selected = NULL)
                   ),
                   actionButton("reset_filters", "Reset")
               )
        ),
        # Chart Panel (Occupies 9 columns)
        column(9, box(status = "primary", solidHeader = TRUE, width = 12, plotOutput("time_series_chart")))
      ),
      
      # Country Totals Title
      div("Total number of clients by country", class = "section-header"),
      
      # Toggle Facet - countries
      fluidRow(column(12, checkboxInput("facet_toggle", "Facet by region?", value = TRUE))),
      
      # Country Totals Chart
      fluidRow(column(12, plotOutput("country_totals_chart", height = "800px", width = "1000px"))),
      
      # Model Explanation Section Header
      div("Can you tell me more about the different models?", class = "full-width"),
      
      # Model Explanations in Two Columns
      fluidRow(
        column(6,
               div(class = "model-section",
                   div("All models are based on Annual Statistics", class = "model-title"),
                   p("To estimate the number of unique clients reached, we developed four models that take different assumptions into account. Each model calculates the total number of individuals receiving services. The models progressively refine these estimates by incorporating adjustments and differing assumptions for data inconsistencies, overlapping service provision and so on."),
                   div("Model 1—the Original Estimate", class = "model-title"),
                   p("This model takes a mid-point estimate between two extremes (a minimum possible value and a maximum possible value) for each country."),
                   p("The lower bound is the total health population served within the country programme's catchment area. The upper bound is the sum of all services."),
                   p("Since the total reach must lie between these extremes, the model takes the mid-point (ie, the simple arithmetic mean)."),
                   
                   div("Model 2—correction for outliers in the data", class = "model-title"),
                   p("The raw data behind Model 1's estimates contain outliers that skew the country totals. These primarily arise from inconsistencies in reporting."),
                   p("Model 2 follows the same calculation as Model 1 but corrects outliers and imputes values that align more consistently with data across all countries.")
               )
        ),
        column(6,
               div(class = "model-section",
                   div("Model 3—revised assumptions about the reach of health programmes", class = "model-title"),
                   p("NGOX collects data on health services accessed directly but also operates health clinics and drop-in centres where it is challenging to measure the full number of people reached."),
                   p("Originally, NGOX’s client models used the health catchment area population as a plausible minimum estimate of client reach."),
                   p("However, the health catchment area may overestimate the number of people reached, while direct service counts almost certainly underestimate it."),
                   p("Model 3 applies a logarithmic function to estimate a midpoint between health catchment and direct health reach."),
                   
                   div("Model 4—accounting for overlap more deliberately", class = "model-title"),
                   p("Models 1–3 establish minimum and maximum values and take the mid-point (arithmetic mean), assuming health as the baseline. This effectively assigns a fixed 50% overlap."),
                   p("But what if health is not the baseline? What if other programme areas play a more central role in a given country?"),
                   p("Model 4 takes a bottom-up approach: it identifies the programme area with the greatest number of clients and sets it as the base. It then calculates the overlap of other programme areas relative to that baseline, maintaining the same 50% overlap assumption.")
               )
        )
      )
  )
)

#### Define Server ####
server <- function(input, output, session) {
  observe({
    # Exclude regions/global from country selection
    country_choices <- unique(tl_data$country[!(tl_data$country %in% regions_global_list)])
    
    if (input$region == "All") {
      updateSelectInput(session, "country", choices = c("All", country_choices))
    } else {
      updateSelectInput(session, "country", choices = c("All",unique(filter(tl_data, region == input$region & !(country %in% regions_global_list))$country)
      ))
    }
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "model", selected = "m1")
    updateSelectInput(session, "region", selected = "All")
    updateSelectInput(session, "country", selected = "All")
  })
  
  # Time series chart data entry
  observeEvent(input$selected_countries, {
    selected <- input$selected_countries
    global_selected <- "Global" %in% selected
    regions_selected <- selected[selected %in% unique(tl_data$region)]
    countries_selected <- selected[selected %in% unique(tl_data$country)]
    
    # If "Global" is selected, remove countries
    if (global_selected) {
      updateCheckboxGroupInput(session, "selected_countries", 
                               selected = c("Global", unique(tl_data$region)))
    } 
    # If a region is selected, remove global & countries
    else if (length(regions_selected) > 0) {
      updateCheckboxGroupInput(session, "selected_countries",
                               selected = regions_selected)
    }
    # If countries are selected, remove global & regions
    else if (length(countries_selected) > 0) {
      updateCheckboxGroupInput(session, "selected_countries",
                               selected = countries_selected)
    }
  })
  
  observeEvent(input$selected_global, {
    if (!is.null(input$selected_global) && length(input$selected_global) > 0) {
      updateCheckboxGroupInput(session, "selected_regions", selected = NULL)
      updateCheckboxGroupInput(session, "selected_countries", selected = NULL)
    }
  })
  
  observeEvent(input$selected_regions, {
    if (!is.null(input$selected_regions) && length(input$selected_regions) > 0) {
      updateCheckboxGroupInput(session, "selected_global", selected = NULL)
      updateCheckboxGroupInput(session, "selected_countries", selected = NULL)
    }
  })
  
  observeEvent(input$selected_countries, {
    if (!is.null(input$selected_countries) && length(input$selected_countries) > 0) {
      updateCheckboxGroupInput(session, "selected_global", selected = NULL)
      updateCheckboxGroupInput(session, "selected_regions", selected = NULL)
    }
  })
  
  observeEvent(input$reset_filters, {
    updateCheckboxGroupInput(session, "selected_global", selected = NULL)
    updateCheckboxGroupInput(session, "selected_regions", selected = NULL)
    updateCheckboxGroupInput(session, "selected_countries", selected = NULL)
  })
  
  filtered_data <- reactive({
    data <- tl_data %>%
      filter(
        model == input$model,
        year == input$year,
        !country %in% regions_global_list,
        variable == sector_map[input$sector]
      )
    if (input$region != "All") {
      data <- data %>% filter(region == input$region)
    }
    if (input$country != "All") {
      data <- data %>% filter(country == input$country)
    }
    data
  })
  
  observe({
    print(filtered_data(), n = 100)
  })
  
  dash_data <- reactive ({
    data <- tl_data %>%
      filter(
        model == input$model,
        year == input$year
      )
    
    # Region & Country filtering logic
    if (input$country != "All") {
      data <- data %>% filter(country == input$country)
    } else if (input$region != "All") {
      data <- data %>% filter(country == input$region)
    } else {
      data <- data %>% filter(country == "Global")
    }
    
    return(data)
  })
  
  observe({
    print(dash_data(), n = 100)
  })
  
  facet_totals_data <- reactive ({
    data <- tl_data %>%
      filter(
        model == input$model,
        year == input$year,
        !country %in% regions_global_list
      )
    
    # Sector filtering based on lookup table
    selected_variable <- sector_map[input$sector]
    data <- data %>% filter(variable == selected_variable)
    
    return(data)
  })
  
  timeline_data <- reactive({
    data <- tl_data %>%
      filter(model == input$model)
    
    selected_variable <- sector_map[input$sector]
    data <- data %>% filter(variable == selected_variable)
    
    # Apply filters based on selected category
    if (!is.null(input$selected_global) && length(input$selected_global) > 0) {
      data <- data %>% filter(country == "Global")
    } else if (!is.null(input$selected_regions) && length(input$selected_regions) > 0) {
      data <- data %>% filter(country %in% input$selected_regions)
    } else if (!is.null(input$selected_countries) && length(input$selected_countries) > 0) {
      data <- data %>% filter(country %in% input$selected_countries)
    }

    return(data)
  })
  
  output$total_clients <- renderUI({
    selected_fy <- input$year
    total_dash <- tl_data %>%
      filter(
        model == input$model,
        year == input$year,
        country == "Global",
        variable == "clients"
      ) %>%
      pull(value) %>%
      round(-3)
    total <- dash_data() %>%
      filter(
        variable == sector_map[input$sector]
      ) %>%
      pull(value) %>%
      round(-3)
    HTML(paste0(
      "<div style='text-align: center; font-size: 18px; margin-bottom: 20px; font-family: \"Open Sans\"; font-weight: normal;'>",
      "In FY", selected_fy,", NGOX reached around ",
      "<span style='font-family: \"Roboto Mono\"; font-weight: bold;'>",
      format(total_dash, big.mark = ",", scientific = FALSE),
      "</span>",
      " clients around the world. This estimate is based on data entered through the Annual Statistics process. In the areas currently filtered in this dashboard, the estimated reach is around ",
      "<span style='font-family: \"Roboto Mono\"; font-weight: bold;'>",
      format(total, big.mark = ",", scientific = FALSE),
      "</span>",
      " clients.</div>"
    ))
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 0, lat = 20, zoom = 2)
  })
  
  observe({
    req(filtered_data())  # Ensure data exists before updating map
    
    # Create a color palette based on the 'value' column
    pal <- colorNumeric(palette = "viridis", domain = filtered_data()$value, na.color = "transparent", reverse = TRUE)
    
    leafletProxy("map", data = filtered_data()) %>%
      clearShapes() %>%     # Remove previous country polygons
      clearControls() %>%   # Remove previous legend
      addPolygons(
        fillColor = ~pal(value),  # Apply the Viridis color scale
        color = "grey50",  # Border color
        weight = 1, 
        opacity = 1, 
        fillOpacity = 0.4, 
        highlight = highlightOptions(weight = 2, color = "grey50", fillOpacity = 0.9),
        label = ~paste0(country, ": ", label_text),  # Tooltip showing country & value
        labelOptions = labelOptions(style = list("font-weight" = "bold", "font-size" = "12px"))
      ) %>%
      addLegend(
        pal = pal, 
        values = ~value, 
        title = "Number of Clients",
        position = "bottomright",
        opacity = 1
      )
  })
  
  output$bar_chart <- renderPlot({
    dash_data() %>%
      filter(variable != "clients") %>%  # Exclude "clients"
      mutate(
        variable = case_when(
          variable == "total_edu" ~ "Education",
          variable == "total_erd" ~ "ERD",
          variable == "total_gov" ~ "Governance",
          variable == "total_health" ~ "Health",
          variable == "total_safety" ~ "VPRU",
          TRUE ~ NA_character_
        )
      ) %>%
      ggplot(aes(x = variable, y = value)) +
      geom_col(alpha = 0.5) +
      geom_text(
        aes(label = label_text),  # Use precomputed label_text
        hjust = -0.3, size = 7, family = "Roboto Mono"
      ) +
      coord_flip() +
      theme_minimal() +
      labs(x = NULL, y = NULL) +
      scale_y_continuous(
        labels = scales::label_number(scale_cut = scales::cut_short_scale()),  # Auto-scale Y-axis
        expand = expansion(mult = c(0, 0.3))
      ) +
      theme(
        text = element_text(size = 18, family = "Roboto Mono"),
        legend.position = "none",
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title.position = "plot",
        axis.text.x = element_text(size = 18, family = "Roboto Mono"),
        axis.text.y = element_text(size = 18, family = "Fira Sans"),
        plot.margin = margin(15, 30, 15, 15),  # Add margins to prevent clipping
        aspect.ratio = 1  # Fix the aspect ratio
      )
  })
  
  # Render the Time Series chart
  output$time_series_chart  <- renderPlot({
    
    #    req(input$selected_global, input$selected_regions, input$selected_countries)
    
    p <- timeline_data() %>%
      ggplot(aes(x = year, y = value, group = country))
    
    if (input$line_toggle) {
      p <- p +
        geom_area(aes(fill = country), alpha = .3, position = "stack") +
        geom_area(
          aes(color = country),  # Use same color as fill for outlines
          fill = NA,  # Only keep the borders
          position = "stack",
          size = 1,  # Adjust thickness of the borders
          show.legend = FALSE # Hide redundant legend entry
        ) +
        guides(color = "none")
    } else {
      p <- p +
        geom_line(
          aes(color = country),
          size = 1.2
        ) +
        guides(fill = "none")
    }
    
    p <- p +
      labs(
        x = NULL,
        y = NULL,
        fill = "Mapped areas",
        colour = "Mapped lines"
      ) +
      scale_x_continuous(
        breaks = 2021:2023
      ) +
      scale_y_continuous(
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      scale_colour_viridis(discrete = TRUE) +
      scale_fill_viridis(discrete = TRUE) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 21, family = "Roboto Mono"),
        axis.text.y = element_text(size = 21, family = "Fira Sans"),
        legend.title = element_text(size = 21, family = "Roboto Mono"),
        legend.text = element_text(size = 16, family = "Fira Sans")
      )
    p
  })
  
  # Render the Country Totals Chart
  output$country_totals_chart <- renderPlot({
  
    p <- facet_totals_data() %>%
      ggplot(aes(
        x = fct_reorder(country, value),
        y = value,
        fill = value
        #        text = paste0("Country: ", country, "<br>Clients: ", scales::label_number(scale_cut = scales::cut_short_scale())(value))
      )) +
      geom_col(alpha = .5) +
      geom_text(
        aes(label = label_text),
        hjust = -.3, size = 6, family = "Roboto Mono"
      ) +
      coord_flip() +
      theme_minimal() +
      scale_color_viridis(discrete = FALSE, direction = -1) +
      scale_fill_viridis(discrete = FALSE, direction = -1) +
      scale_y_continuous(
        labels = scales::label_number(scale_cut = scales::cut_short_scale()),
        expand = expansion(mult = c(0, 0.3))
      ) +
      labs(
        x = NULL,
        y = NULL
      ) +
      theme(
        legend.position = "none",
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(
          family = "Fira Sans",
          hjust = .5,
          margin = margin(b = 20)
        ),
        plot.title.position = "plot",
        axis.title.x = element_text(size = 21, margin = margin(t = 15)),
        axis.title.y = element_text(
          size = 21,
          family = "Fira Sans",
          margin = margin(r = 15)
        ),
        axis.text.x = element_text(size = 21, family = "Roboto Mono"),
        axis.text.y = element_text(size = 21, family = "Fira Sans"),
        strip.text = element_text(size = 21, face = "bold", family = "Fira Sans")
      )
    
    # Apply faceting only if "Yes" is selected
    if (input$facet_toggle) {
      p <- p + facet_wrap(~ region, drop = TRUE, scales = "free_y")
    }
    p
  })
  
}

# Run the app
shinyApp(ui, server)