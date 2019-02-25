#' Launch Shiny app for nesting data exploration
#'
#' \code{explore_nests} launches a Shiny app for dynamic exploration and
#' visualization of nests from movement data
#'
#' @details The function takes as input \code{gps_data} (see
#' \code{\link{find_nests}} or vignette for details on data format) and
#' launches a Shiny app that allows the user to dynamically explore
#' repeatedly visited locations for one burst at a time while interactively
#' tuning input parameters (see \code{\link{find_nests}} for the complete
#' list). Under the hood, \code{explore_nests} runs \code{find_nests}, and
#' then displays the results on a map.
#'
#' @param gps_data \code{data.frame} of movement data. Needs to include burst,
#' date, long, lat
#'
#' @return nothing
#'
#' @export
explore_nests <- function(gps_data) {

  # User interface

  ui <- shinydashboard::dashboardPage(

    skin = "blue",

    title = "nestR",

    shinydashboard::dashboardHeader(title = "nestR", titleWidth = 300),

    shinydashboard::dashboardSidebar(

      width = 300,

      shiny::tags$h3("Input Parameters"),

      shiny::tags$hr(),

      shiny::tags$h4("Species-specific Parameters"),

      shiny::textInput(inputId = "sea_start",
                       label = "Start of nesting season",
                       placeholder = "mm-dd"),
      shiny::textInput(inputId = "sea_end",
                       label = "End of nesting season",
                       placeholder = "mm-dd"),
      shiny::numericInput(inputId = "nest_cycle",
                          label = "Duration of complete nesting cycle (days)",
                          value = 0),

      shiny::tags$hr(),

      shiny::tags$h4("Data-related Parameters"),

      shiny::numericInput(inputId = "buffer",
                          label="Buffer (m)",
                          value = 0),
      shiny::numericInput(inputId = "min_pts",
                          label = "Minimum points within a buffer",
                          value = 0),
      shiny::numericInput(inputId = "min_d_fix",
                          label = "Minimum daily fixes",
                          value = 0),

      shiny::tags$hr(),

      shiny::tags$h4("Filtering Parameters"),

      shiny::numericInput(inputId = "min_consec",
                          label = "Minimum consecutive days",
                          value = 0),
      shiny::numericInput(inputId="min_top_att",
                          label = "Minimum % attendance on top day",
                          value = 0),
      shiny::numericInput(inputId = "min_days_att",
                          label = "Minimum % days visited",
                          value = 0),

      shiny::checkboxInput(inputId = "discard_overlapping",
                           label = "Discard temporally overlapping attempts",
                           value = FALSE),

      shiny::tags$hr(),

      shiny::htmlOutput("nests"),

      shiny::tags$hr(),

      shiny::actionButton("button", "Find nests"),

      shiny::tags$hr()

    ),

    shinydashboard::dashboardBody(

      shiny::tags$head(shiny::tags$style("#map{height:90vh !important;}")),

      shiny::dataTableOutput("nest_results"),

      leaflet::leafletOutput("map")

    )
  )

  # Server

  server <- function(input, output) {

    options(shiny.maxRequestSize = 200*1024^2)

    output$nests <- shiny::renderUI({

      bursts <- unique(gps_data$burst)

      shiny::selectInput(inputId = "burst",
                         label = "Burst",
                         choices = unique(bursts),
                         selected = unique(bursts)[1])
    })

    nests <- shiny::eventReactive(input$button, {

      sea_start <- input$sea_start
      sea_end <- input$sea_end
      nest_cycle <- input$nest_cycle
      buffer <- input$buffer
      min_pts <- input$min_pts
      min_d_fix <- input$min_d_fix
      min_consec <- input$min_consec
      min_top_att <- input$min_top_att
      min_days_att <- input$min_days_att
      discard_overlapping <- input$discard_overlapping

      gps_data <- gps_data %>%
        filter(burst == input$burst)

      nests <- find_nests(gps_data = gps_data,
                          buffer = buffer,
                          min_pts = min_pts,
                          sea_start = sea_start,
                          sea_end = sea_end,
                          nest_cycle = nest_cycle,
                          min_d_fix = min_d_fix,
                          min_consec = min_consec,
                          min_top_att = min_top_att,
                          min_days_att = min_days_att,
                          discard_overlapping = discard_overlapping)

      return(nests$nests)

    })


    shiny::observeEvent(input$button, {

      shiny::showModal(shiny::modalDialog("Looking for nests...",
                                   footer = "(This may take a while)"))

      output$nest_results <- shiny::renderDataTable({nests()},
                                                    options = list(autoWidth = TRUE,
                                                                   scrollX=TRUE))

      output$map <- leaflet::renderLeaflet({

        nest_results <- nests()

        icons <- leaflet::awesomeIcons(
          icon = "ios-close",
          iconColor = "black",
          library = "ion",
          markerColor = "blue")

        shiny::removeModal()

        leaflet::leaflet() %>%
          leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery,
                                    group = "Satellite Maptile") %>%
          leaflet::addAwesomeMarkers(data = nest_results,
                                     lng = ~long,
                                     lat = ~lat,
                                     icon = icons,
                                     label = as.character(nest_results$loc_id),
                                     group = "Points")

      })

    })

  }

  shiny::shinyApp(ui, server)

}
