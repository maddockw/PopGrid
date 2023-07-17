tract_aggregation <- function(
    states = NULL,
    year = 2020,
    census_file = "dhc",
    variables,
    var_info,
    var_info_abb,
    output_path = getwd(),
    crs = NULL,
    output_name,
    overwrite = FALSE
){
  # read in Census data at the county level
  raw_data <- suppressMessages(get_decennial(geography = "tract",
                                             variables = variables,
                                             state = states,
                                             sumfile = census_file,
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
  if (year == 2020){
    out_weights <- out_data %>%
      st_drop_geometry() %>%
      as.data.frame() %>%
      mutate(P12I = rowSums(select(., matches("^P12I")), na.rm = TRUE),
             P12J = rowSums(select(., matches("^P12J")), na.rm = TRUE),
             P12K = rowSums(select(., matches("^P12K")), na.rm = TRUE),
             P12L = rowSums(select(., matches("^P12L")), na.rm = TRUE),
             P12M = rowSums(select(., matches("^P12M")), na.rm = TRUE),
             P12N = rowSums(select(., matches("^P12N")), na.rm = TRUE),
             P12O = rowSums(select(., matches("^P12O")), na.rm = TRUE),
             P12P = rowSums(select(., matches("^P12P")), na.rm = TRUE),
             P12Q = rowSums(select(., matches("^P12Q")), na.rm = TRUE),
             P12R = rowSums(select(., matches("^P12R")), na.rm = TRUE),
             P12S = rowSums(select(., matches("^P12S")), na.rm = TRUE),
             P12T = rowSums(select(., matches("^P12T")), na.rm = TRUE),
             P12U = rowSums(select(., matches("^P12U")), na.rm = TRUE),
             P12V = rowSums(select(., matches("^P12V")), na.rm = TRUE)) %>%
      select(-all_of(variables), -NAME) %>%
      county_pop_weight(variables = c("P12I", "P12J", "P12K", "P12L", "P12M", "P12N", "P12O", "P12P", "P12Q", "P12R", "P12S", "P12T", "P12U", "P12V"), year = year) %>%
      pivot_longer(cols = all_of(c("P12I", "P12J", "P12K", "P12L", "P12M", "P12N", "P12O", "P12P", "P12Q", "P12R", "P12S", "P12T", "P12U", "P12V")), names_to = "variable", values_to = "Value") %>%
      left_join(var_info_abb, by = c("variable" = "var"))
  } else {
    out_weights <- out_data %>%
      st_drop_geometry() %>%
      as.data.frame() %>%
      mutate(P012A = rowSums(select(., matches("^P012A\\d{3}")), na.rm = TRUE),
             P012B = rowSums(select(., matches("^P012B\\d{3}")), na.rm = TRUE),
             P012C = rowSums(select(., matches("^P012C\\d{3}")), na.rm = TRUE),
             P012D = rowSums(select(., matches("^P012D\\d{3}")), na.rm = TRUE),
             P012E = rowSums(select(., matches("^P012E\\d{3}")), na.rm = TRUE),
             P012F = rowSums(select(., matches("^P012F\\d{3}")), na.rm = TRUE),
             P012G = rowSums(select(., matches("^P012G\\d{3}")), na.rm = TRUE)) %>%
      select(-all_of(variables), -NAME) %>%
      county_pop_weight(variables = c("P012A", "P012B", "P012C", "P012D", "P012E", "P012F", "P012G"), year = year) %>%
      pivot_longer(cols = all_of(c("P012A", "P012B", "P012C", "P012D", "P012E", "P012F", "P012G")), names_to = "variable", values_to = "Value") %>%
      left_join(var_info_abb, by = c("variable" = "var"))
  }

  # set up output files
  outfile <- file.path(output_path, paste0(output_name, ".shp"))
  csv_outfile <- file.path(output_path, paste0(output_name, ".csv"))
  weights_outfile <- file.path(output_path, paste0(output_name, "_weights", ".csv"))

  # write output files
  if (file.exists(outfile)|| file.exists(csv_outfile) || file.exists(weights_outfile)){
    if (!overwrite){
      stop("One or more output files already exist and overwrite is set to FALSE. Canceling...")
    } else{
      message("One or more output files already exist and overwrite is set to TRUE. Overwriting...")
      st_write(out_data, outfile, delete_layer = overwrite, quiet = TRUE)
      write.table(out_csv, file = csv_outfile, row.names = FALSE, sep = ",")
      write.table(out_weights, file = weights_outfile, row.names = FALSE, sep = ",")
    }
  } else{
    st_write(out_data, outfile, quiet = TRUE)
    write.table(out_csv, file = csv_outfile, row.names = FALSE, sep = ",")
    write.table(out_weights, file = weights_outfile, row.names = FALSE, sep = ",")
  }

}
