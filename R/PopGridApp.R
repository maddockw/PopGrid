#' Shiny app to aggregate block-level population data from the decennial Census to a chosen grid definition
#'
#' @description
#' This function runs PopGrid via a shiny app UI, offering the same functionality as run_aggregation but with a simple way to provide inputs.
#'
#' @return [PopGrid::PopGridApp()] does not return a value, but instead saves three output files: a shapefile of the selected grid with population data, a CSV with the number of people in each grid cell for each of the race-ethnicity-gender-age demographic groups included in BenMAP, and a CSV with the fraction of the total population in each of the eight race-ethnic groups that comes from each U.S. county.
#' @export
#'
#' @examples
#' PopGridApp()
#'

PopGridApp <- function(){
  ui <- fluidPage(
    titlePanel("PopGrid"),
    sidebarLayout(
      sidebarPanel(
        HTML("<h3>Inputs</h3>"),
        selectInput("mode", "Select a mode", choices = c("Shapefile", "County", "Tract"), width = "100%"),
        selectInput("year", "Select a year", choices = c(2010, 2020), selected = 2020, width = "100%"),
        #selectInput("method", "Select an aggregation method", choices = c("Area weighting", "Centroids"), selected = "Area weighting", width = "100%"),
        selectInput("states", "States to include", choices = c("All CONUS states", "Select a subset of states"), selected = "All CONUS states", width = "100%"),
        ###
        HTML("<p><strong>Provide the file path where you would like to save outputs</strong></p>"),
        shinyDirButton("output_path", label = "Browse", title = "Select output path", multiple = FALSE),
        textOutput("selected_output_path"),
        HTML("<br>"),
        ###
        #textInput("out_path", "Provide the file path where you would like to save outputs", placeholder = "Output folder path", width = "100%"),
        textInput("outfile_name", "Provide a name for the output files (without file extension)", placeholder = "Output file name", width = "100%"),
        checkboxInput("overwrite", "Overwrite existing output files of the same name in the output folder?", value = FALSE, width = "100%"),
        actionButton("run", "Run PopGrid")
      ),
      mainPanel(
        uiOutput("conditionalTabs")
      )
    )
    )

  server <- function(input, output, session){
    output$conditionalTabs <- renderUI({
      tabsetPanel(
        tabPanel(
          "Glossary",
          HTML("<h4>Mode</h4>
                <p><strong>Shapefile</strong> mode allocates population to a user-provided shapefile. If selected, choose an input shapefile on the <em>Shapefile Selection</em> tab.</p>
                <p><strong>County</strong> and <strong>Tract</strong> modes instead use U.S. Census-defined county and tract shapes, respectively</p>
                <br>",
               "<h4>Year</h4>
                <p>Indicates the U.S. decennial Census year to use for population allocation.</p>
                <br>",
               "<h4>Aggregation Method</h4>
                <p>The <strong>Area Weighting</strong> approach allocates block population data to grid cells based on area proportion. This is the recommended approach.</p>
                <p>The <strong>Centroid</strong> approach allocates block population data to grid cells based on block centroids.</p>
                <br>",
               "<h4>States</h4>
                <p>Allocation can be done for <strong>All CONUS states</strong>, which includes all of the contiguous U.S. If you only need population data for a subset of states, you can <strong>Select a subset of states</strong> on the <em>State Selection</em> tab instead.</p>")
        ),
        if (input$mode == "Shapefile") {
          tabPanel(
            "Shapefile Selection",
            #textInput("grid_path", "Provide the file path for the folder containing your grid shapefile", placeholder = "Path to grid", width = "100%"),
            #textInput("grid_name", "Provide the name of your grid shapefile (without file extension)", placeholder = "Grid shapefile name", width = "100%"),
            "Select input grid shapefile:",
            shinyFilesButton("input_file", label = "Browse", title = "Select input grid shapefile", multiple = FALSE),
            textOutput("selected_input_file")
            )
          },
        if (input$states == "Select a subset of states") {
          tabPanel(
            "State Selection",
            selectInput("state_select", "Select all states to include",
                        choices = c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware",
                                    "Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky",
                                    "Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi",
                                    "Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico",
                                    "New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania",
                                    "Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont",
                                    "Virginia","Washington","West Virginia","Wisconsin","Wyoming"),
                        multiple = TRUE)
            )
          }
      )
    })

    shinyDirChoose(input, "output_path", roots= c(wd = ".", home = "~"))
    observeEvent(input$output_path, {
      outPath <- parseDirPath(roots = c(wd = getwd(), home = "~"), input$output_path)

      if (exists("outPath")){
        output$selected_output_path <- renderText({
          paste0("Selected folder: ", outPath)
        })
      }
    })

    cond_tab_values <- reactiveValues(input_file = NULL, input_states = NULL)
    observeEvent(input$input_file, {
      cond_tab_values$input_file <- parseFilePaths(roots = c(wd = getwd(), home = "~"), input$input_file)$datapath
    })
    observeEvent(input$state_select, {
      cond_tab_values$input_states <- input$state_select
    })

    shinyFileChoose(input, "input_file", roots= c(wd = ".", home = "~"), filetypes = c("", "shp"))
    observeEvent(input$input_file, {
      shinyFile <- parseFilePaths(roots = c(wd = getwd(), home = "~"), input$input_file)$datapath

      if (exists("shinyFile")){
        output$selected_input_file <- renderText({
          paste0("Selected file: ", shinyFile)
        })
        }
    })

    observeEvent(input$run, {
      if (input$states == "Select a subset of states" & is.null(cond_tab_values$input_states)) {
        showModal(
          modalDialog(
            title = "Error",
            HTML("Please choose state(s) on the <em>Select States</em> tab or change States to include input to <strong>All CONUS states</strong>."),
            easyClose = TRUE,
            scrollable = FALSE
          )
        )
      }
    })

    observeEvent(input$run, {
      if (input$mode == "Shapefile" & (((is.character(cond_tab_values$input_file)) & length(cond_tab_values$input_file) == 0) | is.null(cond_tab_values$input_file))) {
        message(typeof(cond_tab_values$input_file))
        message(length(cond_tab_values$input_file))
        showModal(
          modalDialog(
            title = "Error",
            HTML("Please select input grid on the <em>Shapefile Selection</em> tab or change Mode input to <strong>County</strong> or <strong>Tract</strong>."),
            easyClose = TRUE,
            scrollable = FALSE
          )
        )
      }
    })

    observeEvent(input$run, {
      message("inputs are:")
      message("mode = ", input$mode)
      message("in_path = ", cond_tab_values$input_file)
      message("year = ", input$year)
      message("method = ",  input$method)
      message("state = ",  cond_tab_values$input_states)
      message("out_path = ", input$out_path)
      message("out_name = ", input$outfile_name)
      message("overwrite = ",  input$overwrite)
      # run_aggregation(
      #   mode = input$mode,
      #   grid_path = input$grid_path,
      #   grid_name = input$grid_name,
      #   year = input$year,
      #   #variables = input$grid_path,
      #   area_weight = input$method,
      #   states = input$states,
      #   output_path = input$out_path,
      #   output_name = input$outfile_name,
      #   overwrite = input$overwrite
      # )
    })
  }

  shinyApp(ui, server)
}
