# =========================================================
# WTA FULL BAYESIAN GENERATIVE O/U MODEL
# =========================================================

library(tidyverse)
library(brms)
library(cmdstanr)

options(brms.backend = "cmdstanr")

# =========================================================
# 1. PREPARE HOLD DATA (SACKMANN RAW)
# =========================================================

prepare_hold_data <- function(wta_matches_raw, lambda = 0.0025) {
  
  df_winner <- wta_matches_raw %>%
    transmute(
      date = as.Date(tourney_date),
      surface = surface,
      server = winner_name,
      returner = loser_name,
      svpt = w_svpt,
      bp_faced = w_bpFaced,
      bp_saved = w_bpSaved
    )
  
  df_loser <- wta_matches_raw %>%
    transmute(
      date = as.Date(tourney_date),
      surface = surface,
      server = loser_name,
      returner = winner_name,
      svpt = l_svpt,
      bp_faced = l_bpFaced,
      bp_saved = l_bpSaved
    )
  
  df <- bind_rows(df_winner, df_loser) %>%
    filter(!is.na(svpt), svpt > 0)
  
  df <- df %>%
    mutate(
      service_games = svpt / 4,
      breaks = bp_faced - bp_saved,
      holds = service_games - breaks,
      hold_rate = holds / service_games
    ) %>%
    filter(hold_rate > 0.01, hold_rate < 0.99)
  
  max_date <- max(df$date, na.rm = TRUE)
  
  df <- df %>%
    mutate(
      days_ago = as.numeric(max_date - date),
      time_weight = exp(-lambda * days_ago),
      final_weight = service_games * time_weight
    )
  
  df
}

# =========================================================
# 2. FIT HOLD MODEL (SURFACE-SPECIFIC + TIME DECAY)
# =========================================================
fit_hold_model <- function(hold_df) {
  
  brm(
    hold_rate | weights(final_weight) ~
      1 +
      surface +
      (1 | server) +
      (1 | returner) +
      (0 + surface | server),
    
    data = hold_df,
    family = Beta(),
    
    prior = c(
      prior(normal(0, 1), class = "b"),
      prior(normal(0, 0.5), class = "sd")
    ),
    
    chains = 2,
    cores = 2,
    iter = 2000,
    control = list(adapt_delta = 0.9)
  )
}


# =========================================================
# 3. HOLD DRAWS FOR MATCH
# =========================================================

get_hold_draws <- function(hold_fit, player_A, player_B, surface) {
  
  newdata_A <- tibble(
    server = player_A,
    returner = player_B,
    surface = surface,
    final_weight = 10
  )
  
  newdata_B <- tibble(
    server = player_B,
    returner = player_A,
    surface = surface,
    final_weight = 10
  )
  
  pA <- posterior_epred(
    hold_fit,
    newdata = newdata_A,
    allow_new_levels = TRUE
  )
  
  pB <- posterior_epred(
    hold_fit,
    newdata = newdata_B,
    allow_new_levels = TRUE
  )
  
  list(
    pA = as.vector(pA),
    pB = as.vector(pB)
  )
}

# =========================================================
# 4. MATCH SIMULATION
# =========================================================

simulate_set <- function(pA, pB, tiebreak = TRUE) {
  
  games_A <- 0
  games_B <- 0
  server <- "A"
  
  while (TRUE) {
    
    if (server == "A") {
      if (runif(1) < pA) games_A <- games_A + 1 else games_B <- games_B + 1
      server <- "B"
    } else {
      if (runif(1) < pB) games_B <- games_B + 1 else games_A <- games_A + 1
      server <- "A"
    }
    
    if ((games_A >= 6 | games_B >= 6) &&
        abs(games_A - games_B) >= 2) break
    
    if (tiebreak && games_A == 6 && games_B == 6) {
      
      p_tb_A <- pA / (pA + pB)
      
      if (runif(1) < p_tb_A) games_A <- games_A + 1
      else games_B <- games_B + 1
      
      break
    }
  }
  
  list(
    total_games = games_A + games_B,
    winner = ifelse(games_A > games_B, "A", "B")
  )
}

simulate_match <- function(pA, pB) {
  
  sets_A <- 0
  sets_B <- 0
  total_games <- 0
  
  while (sets_A < 2 && sets_B < 2) {
    
    s <- simulate_set(pA, pB)
    
    total_games <- total_games + s$total_games
    
    if (s$winner == "A") sets_A <- sets_A + 1
    else sets_B <- sets_B + 1
  }
  
  total_games
}

simulate_posterior_distribution <- function(
    hold_fit,
    player_A,
    player_B,
    surface,
    n_sim_per_draw = 20
) {
  
  hold_draws <- get_hold_draws(
    hold_fit,
    player_A,
    player_B,
    surface
  )
  
  pA_vec <- hold_draws$pA
  pB_vec <- hold_draws$pB
  
  all_games <- numeric()
  
  for (i in seq_along(pA_vec)) {
    
    sims <- replicate(
      n_sim_per_draw,
      simulate_match(pA_vec[i], pB_vec[i])
    )
    
    all_games <- c(all_games, sims)
  }
  
  all_games
}

# =========================================================
# 5. MAIN PREDICTION FUNCTION
# =========================================================

predict_ou_match <- function(
    hold_fit,
    player_A,
    player_B,
    surface,
    line,
    odds_over,
    odds_under,
    n_sim_per_draw = 20
) {
  
  sim_games <- simulate_posterior_distribution(
    hold_fit,
    player_A,
    player_B,
    surface,
    n_sim_per_draw
  )
  
  p_over <- mean(sim_games > line)
  p_under <- 1 - p_over
  
  ev_over <- p_over * odds_over - 1
  ev_under <- p_under * odds_under - 1
  
  tibble(
    player_A = player_A,
    player_B = player_B,
    surface = surface,
    line = line,
    mean_games = mean(sim_games),
    sd_games = sd(sim_games),
    q90 = quantile(sim_games, 0.9),
    q95 = quantile(sim_games, 0.95),
    p_over = p_over,
    ev_over = ev_over,
    p_under = p_under,
    ev_under = ev_under
  )
}
