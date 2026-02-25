setwd("~/R-koodi/SportBetting/WTA tennis/WTA_full_generative_model")
# Lataa raakadata
wta_raw <- read.csv("wta_matches_raw.csv")

# 1. Valmistele hold-data
hold_df <- prepare_hold_data(wta_raw)

# 2. Fit hold-malli
hold_fit <- fit_hold_model(hold_df)

# 3. Ennusta ottelu
predict_ou_match(
  hold_fit,
  player_A = "Iga Swiatek",
  player_B = "Aryna Sabalenka",
  surface = "Hard",
  line = 22.5,
  odds_over = 1.90,
  odds_under = 1.90
)
