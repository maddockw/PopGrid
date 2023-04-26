county_aggregation <- function(
    states = NULL,
    year = 2010,
    variables,
    var_info,
    output_path = getwd(),
    crs = NULL,
    output_name
){
  # read in Census data at the county level
  raw_data <- suppressMessages(get_decennial(geography = "county",
                                             variables = variables,
                                             state = states,
                                             year = year,
                                             cb = FALSE,
                                             geometry = TRUE,
                                             output = "wide"
  )) %>% st_transform(crs)

  # add COL (state FIPS) and ROW (county FIPS) columns
  out_data <- raw_data %>% mutate(COL = substr(GEOID, 1, 2), ROW = substr(GEOID, 3, 5))

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
    mutate(A_sum = rowSums(select(., matches("^P012A\\d{3}")), na.rm = TRUE),
           B_sum = rowSums(select(., matches("^P012B\\d{3}")), na.rm = TRUE),
           C_sum = rowSums(select(., matches("^P012C\\d{3}")), na.rm = TRUE),
           D_sum = rowSums(select(., matches("^P012D\\d{3}")), na.rm = TRUE)) %>%
    select(-all_of(variables), -NAME) %>%
    county_pop_weight(variables = c("A_sum", "B_sum", "C_sum", "D_sum"), year = year)

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
