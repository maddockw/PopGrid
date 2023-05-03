tract_aggregation <- function(
    states = NULL,
    year = 2010,
    variables,
    var_info,
    output_path = getwd(),
    crs = NULL,
    output_name
){
  # read in Census data at the county level
  raw_data <- suppressMessages(get_decennial(geography = "tract",
                                             variables = variables,
                                             state = states,
                                             year = year,
                                             cb = FALSE,
                                             geometry = TRUE,
                                             output = "wide"
  )) %>% st_transform(crs)

  # add COL (state + county FIPS) and ROW (tract FIPS) columns
  out_data <- raw_data %>% mutate(COL = substr(GEOID, 1, 5), ROW = substr(GEOID, 6, 11))

  # reshape to long table and drop geometry
  out_csv <- out_data %>%
    pivot_longer(cols = all_of(variables), names_to = "variable", values_to = "Population") %>%
    st_drop_geometry() %>%
    as.data.frame() %>%
    left_join(var_info, by = c("variable" = "name")) %>%
    select(-GEOID, -NAME, -variable) %>%
    mutate(year = year)

  # generate weights
  out_weights <- out_data %>%
    st_drop_geometry() %>%
    as.data.frame() %>%
    mutate(P012A = rowSums(select(., matches("^P012A\\d{3}")), na.rm = TRUE),
           P012B = rowSums(select(., matches("^P012B\\d{3}")), na.rm = TRUE),
           P012C = rowSums(select(., matches("^P012C\\d{3}")), na.rm = TRUE),
           P012D = rowSums(select(., matches("^P012D\\d{3}")), na.rm = TRUE),
           P012E = rowSums(select(., matches("^P012E\\d{3}")), na.rm = TRUE),
           P012E = rowSums(select(., matches("^P012F\\d{3}")), na.rm = TRUE),
           P012G = rowSums(select(., matches("^P012G\\d{3}")), na.rm = TRUE)) %>%
    select(-all_of(variables), -NAME) %>%
    county_pop_weight(variables = c("P012A", "P012B", "P012C", "P012D", "P012E", "P012F", "P012G"), year = year)

  # set up output files
  outfile <- file.path(output_path, paste0(output_name, ".shp"))
  csv_outfile <- file.path(output_path, paste0(output_name, ".csv"))
  weights_outfile <- file.path(output_path, paste0(output_name, "_weights", ".csv"))

  # write output files
  if (file.exists(outfile)){
    overwrite <- readline("One or more output files already exist. Type 1 to overwrite existing file or type 0 to cancel output: ") %>%
      as.integer() %>%
      as.logical
    if (!overwrite){
      return()
    } else{
      st_write(out_data, outfile, delete_layer = overwrite, quiet = TRUE)
      write.table(out_csv, file = csv_outfile, row.names = FALSE, sep = ",")
      write.table(out_weights, file = weights_outfile, row.names = FALSE, sep = ",")
      return()
    }
  } else{
    st_write(out_data, outfile, quiet = TRUE)
    write.table(out_csv, file = csv_outfile, row.names = FALSE, sep = ",")
    write.table(out_weights, file = weights_outfile, row.names = FALSE, sep = ",")
  }

}
