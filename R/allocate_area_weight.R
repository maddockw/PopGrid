allocate_area_weight <- function(
    county_grid,
    pop_data,
    variables,
    year = year
){
  # calculate area of each block
  pop_data$block_area <- pop_data %>% st_area

  # intersect blocks with user grid and calculate area of resulting shapes
  intersection <- st_intersection(county_grid, pop_data)
  intersection$int_area <- intersection %>% st_area

  # add a new column of spatially weighted data for each segment for each population variable
  intersection_pop <- intersection %>%
    mutate(across(all_of(variables), ~. * (int_area/block_area)), .keep = "unused")

  # dissolve to the user grid
  dissolved <- intersection_pop %>%
    group_by(COL, ROW) %>%
    summarize(across(all_of(variables), ~sum(.x, na.rm = TRUE))) %>%
    mutate(gridID = paste0(COL, ROW))

  # dissolve to user grid plus county ID for generating weights file later
  dissolved_wt <- intersection_pop %>%
    mutate(countyID = substr(GEOID,1,5), keep = "unused") %>%
    group_by(COL, ROW, countyID) %>%
    summarize(across(all_of(variables), ~sum(.x, na.rm = TRUE))) %>%
    mutate(gridID = paste0(COL, ROW))

  return(list(dissolved = dissolved, weight = dissolved_wt))
}
