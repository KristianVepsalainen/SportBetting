# ============================================================
# Parse WTA score string into set- and game-level features
# ============================================================

library(tidyverse)

wta_matches_raw <- read_csv2(paste0(getwd(),"/Tennis/WTA/wta_matches_raw.csv"))

# ------------------------------------------------------------
# 1. Parse single set score (e.g. "7-6(5)" -> c(7, 6))
# ------------------------------------------------------------

parse_single_set <- function(set_string) {
  
  # Remove tie-break info "(x)"
  clean_set <- str_remove(set_string, "\\(.*\\)")
  
  games <- str_split(clean_set, "-", simplify = TRUE)
  
  if (ncol(games) != 2) return(c(NA_integer_, NA_integer_))
  
  as.integer(games)
}

# ------------------------------------------------------------
# 2. Parse full match score
# ------------------------------------------------------------

parse_wta_score <- function(score) {
  
  # Exclude invalid matches
  if (is.na(score)) return(NULL)
  if (str_detect(score, "RET|W/O|DEF|ABN")) return(NULL)
  
  sets <- str_split(score, "\\s+")[[1]]
  
  set_games <- map(sets, parse_single_set)
  set_games <- keep(set_games, ~ !any(is.na(.x)))
  
  if (length(set_games) < 2) return(NULL)
  
  # Extract per-set totals
  set_totals <- map_int(set_games, sum)
  
  tibble(
    n_sets = length(set_games),
    set1_games = set_totals[1],
    set2_games = set_totals[2],
    set3_games = ifelse(length(set_totals) >= 3, set_totals[3], NA_integer_),
    total_games = sum(set_totals),
    played_third_set = as.integer(length(set_totals) == 3)
  )
}

# ============================================================
# Apply score parsing to full WTA dataset
# ============================================================

wta_matches_parsed <- wta_matches_raw %>%
  
  # Parse score safely
  mutate(
    parsed = map(score, parse_wta_score)
  ) %>%
  
  # Drop invalid / incomplete matches
  filter(map_lgl(parsed, ~ !is.null(.x))) %>%
  
  unnest(parsed)

write.csv2(wta_matches_parsed,file = paste0(getwd(),"/Tennis/WTA/wta_matches_parsed.csv"),row.names = F)



