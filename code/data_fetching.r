library(rnassqs)
library(dplyr)

# STEP 1: Set My API key
nassqs_auth(key = "7D552412-6005-33D8-A66D-B3A4CCB6D45A")

# STEP 2: Define function to retrieve data for multiple crops, states, and years
get_nass_data <- function(stats, units, crops, years, states) {
  all_data <- list()
  
  for (crop in crops) {
    for (state in states) {
      for (i in seq_along(stats)) {
        stat <- stats[i]
        unit <- units[i]
        
        yearly_data <- lapply(years, function(y) {
          params <- list(
            commodity_desc = crop,
            statisticcat_desc = stat,
            unit_desc = unit,
            year = y,
            agg_level_desc = "STATE",
            state_alpha = state
          )
          tryCatch({
            data <- nassqs(params)
            data$crop <- crop
            data$state <- state
            data$statistic <- stat
            data
          }, error = function(e) {
            NULL
          })
        })
        
        # Combine and store
        combined <- do.call(rbind, yearly_data)
        all_data[[paste(crop, state, stat, sep = "_")]] <- combined
      }
    }
  }
  
  # Return one big data frame
  bind_rows(all_data)
}

# STEP 3: Customize parameters
crops <- c("CORN", "SOYBEANS")
states <- c("IL", "IA", "NE", "MN")  # Illinois, Iowa, Nebraska, Minnesota
years <- 2010:2020
stats <- c("YIELD", "AREA PLANTED")
units <- c("BU / ACRE", "ACRES")    # Match order with stats

# STEP 4: Download the data
usda_data <- get_nass_data(stats, units, crops, years, states)

# STEP 5: Clean the data
usda_clean <- usda_data %>%
  select(year = year, state, crop, statistic, unit_desc, value = Value) %>%
  mutate(
    year = as.numeric(year),
    value = as.numeric(gsub(",", "", value))
  ) %>%
  filter(!is.na(value))

# STEP 6: View and save the result
View(usda_clean)
write.csv(usda_clean, "usda_multicrop_data.csv", row.names = FALSE)
