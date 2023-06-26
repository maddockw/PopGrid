#' @export

zero_bin <- function(
    block_data = NULL,
    year = 2020,
    state = NULL,
    county = NULL,
    block_variables = NULL,
    tract_variables = NULL
){
  # add tract ID column for joining later
  block_data <- block_data %>% mutate(tractID = substr(GEOID, 1, 11))

  # pull tract data
  tract <- get_decennial(
    geography = "tract",
    variables = tract_variables,
    state = state,
    county = county,
    year = year,
    sumfile = "dhc",
    cb = FALSE,
    output = "wide"
  )

  # calculate proportion in the 0-1 age bin for each race/sex combination
  patterns <- c("^PCT12A_0\\d{2}N", "^PCT12B_0\\d{2}N", "^PCT12C_0\\d{2}N", "^PCT12D_0\\d{2}N", "^PCT12E_0\\d{2}N", "^PCT12F_0\\d{2}N", "^PCT12G_0\\d{2}N",
                "^PCT12A_1\\d{2}N", "^PCT12B_1\\d{2}N", "^PCT12C_1\\d{2}N", "^PCT12D_1\\d{2}N", "^PCT12E_1\\d{2}N", "^PCT12F_1\\d{2}N", "^PCT12G_1\\d{2}N")

  for (pattern in patterns) {
    column_name <- substr(pattern, 2, 9)
    if (substr(column_name, nchar(column_name), nchar(column_name)) == "0") {
      col_to_divide <- paste0(column_name, "03N")
    } else {
      col_to_divide <- paste0(column_name, "07N")
    }

    tract <- tract %>%
      mutate({{ column_name }} := .data[[col_to_divide]] / rowSums(select(., matches(pattern)), na.rm = TRUE)) #
  }

  tract <- replace(tract, is.na(tract), 0)
  #tract <- tract %>% select(GEOID, matches(c("PCT12[A-G]_0$", "PCT12[A-G]_1$", "PCT12[A-G]_003N", "PCT12[A-G]_107N")))
  tract <- tract %>% select(GEOID, matches(c("PCT12[A-G]_0$", "PCT12[A-G]_1$")))

  block_data <- block_data %>% left_join(tract, by = c("tractID" = "GEOID"))

  block_patterns <- c("P12I_003N", "P12J_003N", "P12K_003N", "P12L_003N", "P12M_003N", "P12N_003N", "P12O_003N",
                      "P12P_003N", "P12Q_003N", "P12R_003N", "P12S_003N", "P12T_003N", "P12U_003N", "P12V_003N",
                      "P12I_027N", "P12J_027N", "P12K_027N", "P12L_027N", "P12M_027N", "P12N_027N", "P12O_027N",
                      "P12P_027N", "P12Q_027N", "P12R_027N", "P12S_027N", "P12T_027N", "P12U_027N", "P12V_027N")

  tract_patterns <- list("P12I_003N" = "PCT12A_0",
                         "P12P_003N" = "PCT12A_0",
                         "P12J_003N" = "PCT12B_0",
                         "P12Q_003N" = "PCT12B_0",
                         "P12K_003N" = "PCT12C_0",
                         "P12R_003N" = "PCT12C_0",
                         "P12L_003N" = "PCT12D_0",
                         "P12S_003N" = "PCT12D_0",
                         "P12M_003N" = "PCT12E_0",
                         "P12T_003N" = "PCT12E_0",
                         "P12N_003N" = "PCT12F_0",
                         "P12U_003N" = "PCT12F_0",
                         "P12O_003N" = "PCT12G_0",
                         "P12V_003N" = "PCT12G_0",
                         "P12I_027N" = "PCT12A_1",
                         "P12P_027N" = "PCT12A_1",
                         "P12J_027N" = "PCT12B_1",
                         "P12Q_027N" = "PCT12B_1",
                         "P12K_027N" = "PCT12C_1",
                         "P12R_027N" = "PCT12C_1",
                         "P12L_027N" = "PCT12D_1",
                         "P12S_027N" = "PCT12D_1",
                         "P12M_027N" = "PCT12E_1",
                         "P12T_027N" = "PCT12E_1",
                         "P12N_027N" = "PCT12F_1",
                         "P12U_027N" = "PCT12F_1",
                         "P12O_027N" = "PCT12G_1",
                         "P12V_027N" = "PCT12G_1")

  # subset 0-5 bin into 0-1 and 1-5 bins using tract proportion
  for (key in names(tract_patterns)) {
    proportion <- tract_patterns[[key]]
    if (substr(key, 7, 7) == "0") {
      new_col_name <- paste0(substr(key, 1, 6),"02N")
    } else {
      new_col_name <- paste0(substr(key, 1, 6),"26N")
    }

    block_data[[new_col_name]] <- block_data[[key]] * block_data[[proportion]]
    block_data[[key]] <- block_data[[key]] - block_data[[new_col_name]]
  }

  # remove tract columns and return
  block_data <- block_data %>% select(-c(setdiff(names(tract), "GEOID"), "tractID"))
  return(block_data)
}
