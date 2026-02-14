# ============================================================
# WTA match data ingestion from Jeff Sackmann GitHub
# ============================================================

library(tidyverse)
library(httr)
library(jsonlite)

# ------------------------------------------------------------
# 1. Helper: get list of WTA match CSV files from GitHub
# ------------------------------------------------------------

get_wta_match_files <- function() {
  
  api_url <- "https://api.github.com/repos/JeffSackmann/tennis_wta/contents"
  
  response <- GET(api_url)
  stop_for_status(response)
  
  content <- content(response, as = "text", encoding = "UTF-8")
  files <- fromJSON(content)
  
  files %>%
    as_tibble() %>%
    filter(
      type == "file",
      str_detect(name, "^wta_matches_[0-9]{4}\\.csv$")
    ) %>%
    arrange(name)
}

# ------------------------------------------------------------
# 2. Helper: read single season file
# ------------------------------------------------------------

read_wta_season <- function(download_url) {
  
  read_csv(
    file = download_url,
    col_types = cols(.default = col_character()),
    progress = FALSE
  )
}

# ------------------------------------------------------------
# 3. Main function: load and combine all seasons
# ------------------------------------------------------------

load_wta_matches <- function() {
  
  files <- get_wta_match_files()
  message("Found ", nrow(files), " WTA season files")
  
  wta_matches <- files %>%
    mutate(
      data = map(download_url, read_wta_season)
    ) %>%
    select(name, data) %>%
    unnest(data) %>%
    
    # --- Type normalization AFTER merge ---
    mutate(
      tourney_date = as.integer(tourney_date),
      match_date   = as.Date(tourney_date, format = "%Y%m%d"),
      season       = lubridate::year(match_date),
      
      winner_seed  = as.character(winner_seed),
      loser_seed   = as.character(loser_seed),
      
      best_of      = as.integer(best_of),
      draw_size    = as.integer(draw_size)
    )
  
  wta_matches
}


# ------------------------------------------------------------
# 4. Load data
# ------------------------------------------------------------

wta_matches_raw <- load_wta_matches()

# Quick sanity check
glimpse(wta_matches_raw)

write.csv2(wta_matches_raw, paste0(getwd(),"/WTA tennis/Data/wta_matches_raw.csv"), row.names = F)
