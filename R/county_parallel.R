#' @export

county_parallel <- function(
    #state = NULL,
    county = NULL,
    #year = 2020,
    #census_file = "dhc",
    #variables,
    #var_info,
    #var_info_abb,
    #tract_vars,
    #final_vars,
    #state_shape,
    #user_grid,
    #output_path = getwd(),
    #crs = NULL,
    #output_name,
    #overwrite = FALSE,
    retries
    #edge_outfile,
    #interior_outfile,
    #csv_outfile,
    #edge_weights_outfile,
    #weights_outfile,
    #overwrite_check,
    #error_track
){
  tryCatch({
    # read in block-level data for the county
    raw_block_data <- suppressMessages(get_decennial(geography = "block",
                                                     variables = variables,
                                                     state = state,
                                                     county = county,
                                                     year = year,
                                                     sumfile = census_file,
                                                     output = "wide",
                                                     cb = FALSE,
                                                     geometry = TRUE
    )) %>% st_transform(NA_eq)

    raw_block_data <- raw_block_data %>% adjust_bins(year = year, state = state, county = county, block_variables = variables, tract_variables = tract_vars)

    # grab county shape and subset user grid to cells that overlap county
    county_shape <- county_state %>% filter(STATEFP == state & COUNTYFP == county)
    county_grid <- user_grid[county_shape,]

    # run spatial analysis using helper functions based on user's selected allocation method
    if (area_weight){
      dissolved <- allocate_area_weight(county_grid = county_grid, pop_data = raw_block_data, variables = final_vars, year = year)
      weights <- dissolved$weight
      dissolved <- dissolved$dissolved
    } else {
      dissolved <- allocate_centroids(county_grid = county_grid, pop_data = raw_block_data, variables = final_vars, year = year)
      weights <- dissolved$weight
      dissolved <- dissolved$dissolved
    }

    # grab the grid cells that overlap the edge of the county, these will be dissolved again later to remove county boundaries
    edge_buffer <- county_shape %>% st_boundary %>% st_buffer(dist = 0.01)
    diss_edge <- dissolved[edge_buffer,]
    weights_edge <- weights[edge_buffer,] %>% st_drop_geometry() %>% as.data.frame()

    # store gridIDs for edge cells
    edge_codes <- diss_edge$gridID %>% unique
    weights_edge_codes <- weights_edge$gridID %>% unique

    # grab the grid cells that don't border the edge of the county, these are ready to go and can be saved
    diss_interior <- dissolved %>% filter(!(gridID %in% edge_codes))
    interior_csv <- diss_interior %>%
      pivot_longer(cols = all_of(final_vars), names_to = "variable", values_to = "Population") %>%
      st_drop_geometry() %>%
      as.data.frame() %>%
      left_join(var_info, by = c("variable" = "name")) %>%
      select(-gridID, -variable) %>%
      mutate(Year = year)
    if (year == 2020){
      weights_interior <- weights %>%
        filter(!(gridID %in% edge_codes)) %>%
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
        select(-all_of(final_vars)) %>%
        county_pop_weight(variables = c("P12I", "P12J", "P12K", "P12L", "P12M", "P12N", "P12O", "P12P", "P12Q", "P12R", "P12S", "P12T", "P12U", "P12V"), year = year) %>%
        pivot_longer(cols = all_of(c("P12I", "P12J", "P12K", "P12L", "P12M", "P12N", "P12O", "P12P", "P12Q", "P12R", "P12S", "P12T", "P12U", "P12V")), names_to = "variable", values_to = "Value") %>%
        left_join(var_info_abb, by = c("variable" = "var")) %>%
        rename(TargetCol = Column, TargetRow = Row) %>%
        mutate(SourceCol = substr(countyID, 1, 2), SourceRow = substr(countyID, 3, 5)) %>%
        select(-c(countyID, gridID, variable))
    } else{
      weights_interior <- weights %>%
        filter(!(gridID %in% edge_codes)) %>%
        st_drop_geometry() %>%
        as.data.frame() %>%
        mutate(P012A = rowSums(select(., matches("^P012A\\d{3}")), na.rm = TRUE),
               P012B = rowSums(select(., matches("^P012B\\d{3}")), na.rm = TRUE),
               P012C = rowSums(select(., matches("^P012C\\d{3}")), na.rm = TRUE),
               P012D = rowSums(select(., matches("^P012D\\d{3}")), na.rm = TRUE),
               P012E = rowSums(select(., matches("^P012E\\d{3}")), na.rm = TRUE),
               P012F = rowSums(select(., matches("^P012F\\d{3}")), na.rm = TRUE),
               P012G = rowSums(select(., matches("^P012G\\d{3}")), na.rm = TRUE)) %>%
        select(-all_of(variables)) %>%
        county_pop_weight(variables = c("P012A", "P012B", "P012C", "P012D", "P012E", "P012F", "P012G"), year = year) %>%
        pivot_longer(cols = all_of(c("P012A", "P012B", "P012C", "P012D", "P012E", "P012F", "P012G")), names_to = "variable", values_to = "Value") %>%
        left_join(var_info_abb, by = c("variable" = "var")) %>%
        rename(TargetCol = Column, TargetRow = Row) %>%
        mutate(SourceCol = substr(countyID, 1, 2), SourceRow = substr(countyID, 3, 5)) %>%
        select(-c(countyID, gridID, variable))
    }

    diss_edge <- diss_edge %>% select(-gridID)
    diss_interior <- diss_interior %>% select(-gridID)

    # write output files
    lock_file <- file.path(output_path, "lock.txt")
    l1 <- lock(lock_file, exclusive = TRUE)
    if (which(state_FIPS == state) == 1 & !file.exists(overwrite_check)){ #which(county_names == county) == 1){
      if (file.exists(edge_outfile) || file.exists(interior_outfile) || file.exists(csv_outfile) || file.exists(weights_outfile)){
        if (!overwrite){
          stop("One or more output files already exist and overwrite is set to FALSE. Canceling...", call. = FALSE)
        } else{
          line <- paste0("Processed element in 1: ", state, ", ", county, " on node: ", Sys.getpid())
          cat(line, file = "track.txt", sep = "\n", append = TRUE)
          message("One or more output files already exist and overwrite is set to TRUE. Overwriting...")
          file.create(overwrite_check)
          st_write(diss_edge, edge_outfile, delete_layer = overwrite, quiet = TRUE)
          st_write(diss_interior, interior_outfile, delete_layer = overwrite, quiet = TRUE)
          write.table(interior_csv, file = csv_outfile, row.names = FALSE, sep = ",")
          write.table(weights_interior, file = weights_outfile, row.names = FALSE, sep = ",")
          write.table(weights_edge, file = edge_weights_outfile, row.names = FALSE, sep = ",")
        }
      } else{
        line <- paste0("Processed element in 2: ", state, ", ", county, " on node: ", Sys.getpid())
        cat(line, file = "track.txt", sep = "\n", append = TRUE)
        file.create(overwrite_check)
        st_write(diss_edge, edge_outfile)
        st_write(diss_interior, interior_outfile)
        write.table(interior_csv, file = csv_outfile, row.names = FALSE, sep = ",")
        write.table(weights_interior, file = weights_outfile, row.names = FALSE, sep = ",")
        write.table(weights_edge, file = edge_weights_outfile, row.names = FALSE, sep = ",")
      }
    } else{
      line <- paste0("Processed element in 3: ", state, ", ", county, " on node: ", Sys.getpid())
      cat(line, file = "track.txt", sep = "\n", append = TRUE)
      st_write(diss_edge, edge_outfile, append = TRUE, quiet = TRUE)
      st_write(diss_interior, interior_outfile, append = TRUE, quiet = TRUE)
      write.table(interior_csv, file = csv_outfile, append = TRUE, col.names = FALSE, row.names = FALSE, sep = ",")
      write.table(weights_interior, file = weights_outfile, append = TRUE, col.names = FALSE, row.names = FALSE, sep = ",")
      write.table(weights_edge, file = edge_weights_outfile, append = TRUE, col.names = FALSE, row.names = FALSE, sep = ",")
    }
    unlock(l1)
  },
  error = function(err){
    err_lock_file <- file.path(output_path, "err_lock.txt")
    l2 <- lock(err_lock_file, exclusive = TRUE)
    cat(as.character(county), sep = "\n", file = error_track, append = TRUE)
    cat(paste0(as.character(state), " ", as.character(county), ": ", conditionMessage(err)), sep = "\n", file = "county_errors.txt", append = TRUE)
    unlock(l2)

    if (retries <= 4){
      county_parallel(county, retries + 1)
    }
  }
  )
}
