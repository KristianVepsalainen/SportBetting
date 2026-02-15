# =========================================================
# WTA TOTALS – AJOTIEDOSTO (LAJENNUS: GAME-LEVEL SIMULAATIO)
# =========================================================

library(tidyverse)
library(slider)
library(DBI)
library(RSQLite)
library(brms)

options(brms.backend = "cmdstanr")
options(pillar.width = Inf)

setwd("~/R-koodi/SportBetting/WTA tennis/")

# ---------------------------------------------------------
# 🟢 UUSI: Game-level simulaatiomoduuli
# ---------------------------------------------------------
source("WTA_game_level_simulation.R")

# ---------------------------------------------------------
# Tietokantayhteys
# ---------------------------------------------------------
con <- dbConnect(RSQLite::SQLite(), "Vedonlyonti.sqlite")

# ---------------------------------------------------------
# Ladataan mallit (EI MUUTOKSIA)
# ---------------------------------------------------------
setwd("~/R-koodi/SportBetting/WTA tennis/Models")

third_set_fit  <- readRDS("third_set_fit.rds")
total_games_fit <- readRDS("total_games_fit.rds")
tiebreak_fit   <- readRDS("tiebreak_fit.rds")

# ---------------------------------------------------------
# Ladataan data (EI MUUTOKSIA)
# ---------------------------------------------------------
wta_model_data <- readRDS("wta_model_data.rds")
wta_model_data_long <- readRDS("wta_model_data_long.rds")
wta_player_features <- readRDS("wta_player_features.rds")
wta_player_set_features <- readRDS("wta_player_set_features.rds")

# ---------------------------------------------------------
# Kelly-apufunktio (EI MUUTOKSIA)
# ---------------------------------------------------------
kelly_fraction <- function(p, odds, fraction = 0.25) {
  b <- odds - 1
  f_star <- (p * b - (1 - p)) / b
  fraction * max(f_star, 0)
}

# =========================================================
# PÄÄFUNKTIO
# =========================================================
evaluate_wta_totals_bet <- function(
    player_a,
    player_b,
    surface,
    ou_line,
    odds_over,
    odds_under,
    bankroll,
    wta_player_features,
    wta_player_set_features,
    third_set_fit,
    total_games_fit,
    seed = 2026
) {
  
  set.seed(seed)
  
  # --------------------------------------------------------
  # 1. Pelaajafeaturit (EI MUUTOKSIA)
  # --------------------------------------------------------
  feats <- wta_player_features %>%
    filter(player %in% c(player_a, player_b)) %>%
    group_by(player) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  if (nrow(feats) < 2) stop("Puuttuvat player_features.")
  
  set_feats <- wta_player_set_features %>%
    filter(player %in% c(player_a, player_b)) %>%
    group_by(player) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  # --------------------------------------------------------
  # 2. Mean-malli total games (EI MUUTOKSIA)
  # --------------------------------------------------------
  newdata_total <- tibble(
    surface = surface,
    mean_games_per_set = mean(set_feats$games_per_set),
    sd_games_per_set = mean(set_feats$sd_games_per_set),
    abs_diff_games_per_set = abs(diff(set_feats$games_per_set)),
    expected_tiebreaks = sum(set_feats$p_tiebreak)
  )
  
  total_draw <- posterior_predict(
    total_games_fit,
    newdata = newdata_total,
    draws = 4000
  )
  
  total_mean <- mean(total_draw)
  
  # --------------------------------------------------------
  # 3. Over/Under -todennäköisyys (mean-pohjainen)
  # --------------------------------------------------------
  p_over <- mean(total_draw > ou_line)
  p_under <- 1 - p_over
  
  ev_over <- p_over * odds_over - 1
  ev_under <- p_under * odds_under - 1
  
  # --------------------------------------------------------
  # 🟢 4. GAME-LEVEL SIMULAATIO (UUSI KOKONAISUUS)
  # --------------------------------------------------------
  
  # YKSINKERTAINEN HOLD-ESTIMAATTI (ALKUUN)
  p_hold_A <- mean(set_feats$hold_pct[set_feats$player == player_a], na.rm = TRUE)
  p_hold_B <- mean(set_feats$hold_pct[set_feats$player == player_b], na.rm = TRUE)
  
  # fallback jos puuttuu
  if (is.na(p_hold_A)) p_hold_A <- 0.75
  if (is.na(p_hold_B)) p_hold_B <- 0.75
  
  sim_games <- simulate_match_distribution(
    pA = p_hold_A,
    pB = p_hold_B,
    n_sim = 30000,
    best_of = 3,
    tiebreak = TRUE
  )
  
  tail_metrics <- extract_tail_metrics(
    sim_games,
    line = ou_line
  )
  
  # --------------------------------------------------------
  # 🟢 5. PÄÄTÖSLOGIIKKA (mean + tail)
  # --------------------------------------------------------
  if (ev_over > 0 && tail_metrics$p_far_over > 0.12) {
    bet_type <- "AGGRESSIVE_OVER"
    stake <- bankroll * kelly_fraction(p_over, odds_over, 0.5)
    
  } else if (ev_over > 0) {
    bet_type <- "NORMAL_OVER"
    stake <- bankroll * kelly_fraction(p_over, odds_over, 0.25)
    
  } else {
    bet_type <- "SKIP"
    stake <- 0
  }
  
  # --------------------------------------------------------
  # 6. Palautus
  # --------------------------------------------------------
  tibble(
    player_a = player_a,
    player_b = player_b,
    surface = surface,
    line = ou_line,
    odds_over = odds_over,
    ev_over = ev_over,
    bet_type = bet_type,
    stake = stake,
    mean_total_games = total_mean,
    tail_q95 = tail_metrics$q95,
    tail_p_far_over = tail_metrics$p_far_over
  )
}
