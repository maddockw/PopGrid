#' Aggregate block-level population data from the decennial Census to a chosen grid definition
#'
#' @description
#' This function allows users to aggregate block-level population data from the decennial U.S. Census to a chosen grid definition. Users may provide a grid in the form of a shapefile or use the Census's county or tract grids. Other inputs allow the user control over allocation method, Census year and variables to use, and included U.S. states.
#'
#' @param mode A character value indicating the type of grid to use for aggregation, one of `c("shapefile", "county", "tract")`. Defaults to using user-specified shapefile (`mode = "shapefile"`). `"county"` amd `"tract"` options instead use U.S. Census-defined county and tract shapes, respectively.
#' @param grid_path A character value representing the file path to the shapefile to use. Only used when `mode = "shapefile"`.
#' @param grid_name A character value representing the name of the shapefile to use. Do not include file extension. Only used when `mode = "shapefile"`.
#' @param year A numeric value indicating the year of the desired Census data. Defaults to `2010`.
#' @param variables A character vector indicating the Census variables to include. Use [tidycensus::load_variables()] to get information on Census variables.
#' @param area_weight A `TRUE/FALSE` value indicating whether to use an area weighting approach (block population data are allocated to grid cells based on area proportion, `area_weight = TRUE`) or a centroid approach (block population data are allocated to grid cells based on block centroids, `area_weight = FALSE`) to allocate block data to the chosen grid definition. Defaults to the centroid approach (`area_weight = FALSE`). Only used when `mode = "county"` or `"tract"`.
#' @param states A character value or character vector of state postal abbreviations indicating which states to include. If no input is provided, all CONUS states are included.
#' @param output_path A character value representing the file path to the output folder. Defaults to the current working directory (`output_path = getwd()`)
#' @param output_name A character value representing the name to use for output files.
#'
#' @return [PopGrid::run_aggregation()] does not return a value, but instead saves three output files: a shapefile of the selected grid with population data, a CSV with the number of people in each grid cell for each of the race-ethnicity-gender-age demographic groups included in BenMAP, and a CSV with the fraction of the total population in each of the eight race-ethnic groups that comes from each U.S. county.
#' @export
#'
#' @examples
#' run_aggregation(mode = "county", year = 2010, states = c("MA", "RI", "CT", "ME", "NH", "VT"), output_name = "New England")

run_aggregation <- function(
    mode = "shapefile",
    grid_path = NULL,
    grid_name = NULL,
    year = 2010,
    variables,
    area_weight = FALSE,
    states = NULL,
    output_path = getwd(),
    output_name
){
  t1 <- Sys.time()

  # message user about which states are being included
  if (is.null(states)){
    states <- state.abb[!state.abb %in% c("AK", "HI")] %>% append("DC")
    message("Using all CONUS states")
  } else {
    message("Using provided list of states")
  }

  # set CRS to BenMAP default input
  NA_eq <- st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

  # load Census variable information and filter to just input variables
  var_info <- load_variables(year = year, dataset = "sf1") %>% filter(name %in% variables)
  var_info <- var_info %>% filter(!substr(name, nchar(name) - 1, nchar(name)) %in% c("01", "02", "26")) %>%
    separate(label, into = c(NA, "Gender", "AgeRange"), sep = "!!") %>%
    mutate(Gender = toupper(Gender),
           AgeRange = gsub(" to ", "TO", AgeRange),
           Race = str_extract(concept, "(?<=\\()[^)]+(?= ALONE\\))")) %>%
    mutate(AgeRange = gsub(" years", "", AgeRange)) %>%
    mutate(AgeRange = gsub(" and over", "+", AgeRange)) %>%
    mutate(AgeRange = gsub(" and ", "TO", AgeRange)) %>%
    mutate(AgeRange = gsub("Under ", "0TO", AgeRange)) %>%
    mutate(Ethnicity = "ALL") %>%
    select(-concept)

  # if user has selected either county or tract mode, run helper functions specific to those grid definitions
  if (mode == "county"){
    message("Aggregating population data to U.S. Census County level")
    output <- county_aggregation(states = states, year = year, variables = variables, var_info = var_info, output_path = output_path, crs = NA_eq, output_name = output_name)
    return()
  } else if (mode == "tract"){
    message("Aggregating population data to U.S. Census Tract level")
    output <- tract_aggregation(states = states, year = year, variables = variables, var_info = var_info, output_path = output_path, crs = NA_eq, output_name = output_name)
    return()
  }

  # if user has selected shapefile mode, continue
  # read in county shapes
  county_state <- counties(state = states,
                           cb = FALSE,
                           year = year,
                           class = "sf"
  ) %>% st_transform(NA_eq)
  state_FIPS <- county_state$STATEFP10 %>% unique

  # read in user grid and convert to correct CRS
  message("Aggregating population data to user-provided grid definition")
  user_grid <- st_read(dsn = grid_path, layer = grid_name) %>% st_transform(NA_eq)

  # set up output files
  edge_outfile <- file.path(output_path, paste0("edge_merge", ".shp"))
  interior_outfile <- file.path(output_path, paste0(output_name, ".shp"))
  csv_outfile <- file.path(output_path, paste0(output_name, ".csv"))
  edge_weights_outfile <- file.path(output_path, paste0("edge_weights", ".csv"))
  weights_outfile <- file.path(output_path, paste0(output_name, "_weights", ".csv"))

  # iterate across states and counties
  state_FIPS %>% lapply(function(state){
    state_counties <- county_state %>% filter(STATEFP10 == state)
    county_names <- state_counties$COUNTYFP10 %>% unique

    county_names %>% lapply(function(county){
      # read in block-level data for the county
      raw_block_data <- suppressMessages(get_decennial(geography = "block",
                                                       variables = variables,
                                                       state = state,
                                                       county = county,
                                                       year = year,
                                                       output = "wide",
                                                       cb = FALSE,
                                                       geometry = TRUE
      )) %>% st_transform(NA_eq)

      # grab county shape and subset user grid to cells that overlap county
      county_shape <- county_state %>% filter(STATEFP10 == state & COUNTYFP10 == county)
      county_grid <- user_grid[county_shape,]

      # run spatial analysis using helper functions based on user's selected allocation method
      if (area_weight){
        dissolved <- allocate_area_weight(county_grid = county_grid, pop_data = raw_block_data, variables = variables, year = year)
        weights = dissolved$weight
        dissolved = dissolved$dissolved
      } else {
        dissolved <- allocate_centroids(county_grid = county_grid, pop_data = raw_block_data, variables = variables, year = year)
        weights = dissolved$weight
        dissolved = dissolved$dissolved
      }

      # grab the grid cells that border the edge of the county, these will be dissolved again later to remove county boundaries
      edge_buffer <- county_shape %>% st_boundary %>% st_buffer(dist = 0.01)
      diss_edge <- dissolved[edge_buffer,]
      weights_edge <- weights[edge_buffer,] %>% st_drop_geometry() %>% as.data.frame()

      # store gridIDs for edge cells
      edge_codes <- diss_edge$gridID %>% unique
      weights_edge_codes <- weights_edge$gridID %>% unique

      # grab the grid cells that don't border the edge of the county, these are ready to go and can be saved
      diss_interior <- dissolved %>% filter(!(gridID %in% edge_codes))
      interior_csv <- diss_interior %>%
        pivot_longer(cols = all_of(variables), names_to = "variable", values_to = "Population") %>%
        st_drop_geometry() %>%
        as.data.frame() %>%
        left_join(var_info, by = c("variable" = "name")) %>%
        select(-gridID, -variable) %>%
        mutate(year = year)
      weights_interior <- weights %>%
        filter(!(gridID %in% edge_codes)) %>%
        st_drop_geometry() %>%
        as.data.frame() %>%
        mutate(A_sum = rowSums(select(., matches("^P012A\\d{3}")), na.rm = TRUE),
               B_sum = rowSums(select(., matches("^P012B\\d{3}")), na.rm = TRUE),
               C_sum = rowSums(select(., matches("^P012C\\d{3}")), na.rm = TRUE),
               D_sum = rowSums(select(., matches("^P012D\\d{3}")), na.rm = TRUE)) %>%
        select(-all_of(variables)) %>%
        county_pop_weight(variables = c("A_sum", "B_sum", "C_sum", "D_sum"), year = year)

      diss_edge <- diss_edge %>% select(-gridID)
      diss_interior <- diss_interior %>% select(-gridID)

      # write output files
      if (which(state_FIPS == state) == 1 & which(county_names == county) == 1){
        if (file.exists(edge_outfile) || file.exists(interior_outfile) || file.exists(csv_outfile) || file.exists(weights_outfile)){
          overwrite <- readline("One or more output files already exist. Type 1 to overwrite existing file or type 0 to cancel output: ") %>%
            as.integer() %>%
            as.logical
          if (!overwrite){
            stop("Canceling...")
          } else{
            st_write(diss_edge, edge_outfile, delete_layer = overwrite, quiet = TRUE)
            st_write(diss_interior, interior_outfile, delete_layer = overwrite, quiet = TRUE)
            write.table(interior_csv, file = csv_outfile, row.names = FALSE, sep = ",")
            write.table(weights_interior, file = weights_outfile, row.names = FALSE, sep = ",")
            write.table(weights_edge, file = edge_weights_outfile, row.names = FALSE, sep = ",")
          }
        } else{
          st_write(diss_edge, edge_outfile)
          st_write(diss_interior, interior_outfile)
          write.table(interior_csv, file = csv_outfile, row.names = FALSE, sep = ",")
          write.table(weights_interior, file = weights_outfile, row.names = FALSE, sep = ",")
          write.table(weights_edge, file = edge_weights_outfile, row.names = FALSE, sep = ",")
        }
      } else{
        st_write(diss_edge, edge_outfile, append = TRUE, quiet = TRUE)
        st_write(diss_interior, interior_outfile, append = TRUE, quiet = TRUE)
        write.table(interior_csv, file = csv_outfile, append = TRUE, col.names = FALSE, row.names = FALSE, sep = ",")
        write.table(weights_interior, file = weights_outfile, append = TRUE, col.names = FALSE, row.names = FALSE, sep = ",")
        write.table(weights_edge, file = edge_weights_outfile, append = TRUE, col.names = FALSE, row.names = FALSE, sep = ",")
      }
    })
  })

  # read in edges
  full_edge <- st_read(dsn = output_path, layer = "edge_merge")
  edge_weights <- read_csv(edge_weights_outfile)

  # dissolve edges
  dissolved_edge <- full_edge %>%
    group_by(COL, ROW) %>%
    summarize(across(all_of(variables), ~sum(., na.rm = TRUE)))#summarize(pop = sum(pop, na.rm = T))
  rm(full_edge)
  edge_csv <- dissolved_edge %>%
    pivot_longer(cols = all_of(variables), names_to = "variable", values_to = "Population") %>%
    st_drop_geometry() %>%
    as.data.frame() %>%
    left_join(var_info, by = c("variable" = "name")) %>%
    mutate(year = year) %>%
    select(-variable)
  edge_weights <- edge_weights %>%
    mutate(A_sum = rowSums(select(., matches("^P012A\\d{3}")), na.rm = TRUE),
           B_sum = rowSums(select(., matches("^P012B\\d{3}")), na.rm = TRUE),
           C_sum = rowSums(select(., matches("^P012C\\d{3}")), na.rm = TRUE),
           D_sum = rowSums(select(., matches("^P012D\\d{3}")), na.rm = TRUE)) %>%
    select(-all_of(variables)) %>%
    county_pop_weight(variables = c("A_sum", "B_sum", "C_sum", "D_sum"), year = year)

  # write dissolved edges to final output files
  st_write(dissolved_edge, interior_outfile, append = TRUE)
  write.table(edge_csv, file = csv_outfile, append = TRUE, col.names = FALSE, row.names = FALSE, sep = ",")
  write.table(edge_weights, file = weights_outfile, append = TRUE, col.names = FALSE, row.names = FALSE, sep = ",")

  # delete intermediate output files
  edge_delete <- file.path(output_path, paste0("edge_merge"))
  file.remove(paste0(edge_delete, c(".shp", ".shx", ".dbf", ".prj")))
  file.remove(edge_weights_outfile)

  t2 <- Sys.time()-t1
  print(t2)
}
