# =========================================================
# Game-level Monte Carlo simulation for WTA totals
# Purpose: Long-tail risk detection (e.g. 16-15 sets)
# =========================================================

#yhden pelin simulaatio
simulate_game <- function(p_hold) {
  runif(1) < p_hold
}

#set (erä) simulaatio
simulate_set <- function(pA, pB, tiebreak = TRUE) {
  
  games_A <- 0
  games_B <- 0
  server <- sample(c("A", "B"), 1)
  
  while (TRUE) {
    
    if (server == "A") {
      if (simulate_game(pA)) {
        games_A <- games_A + 1
      } else {
        games_B <- games_B + 1
      }
      server <- "B"
    } else {
      if (simulate_game(pB)) {
        games_B <- games_B + 1
      } else {
        games_A <- games_A + 1
      }
      server <- "A"
    }
    
    # normaali setti
    if ((games_A >= 6 | games_B >= 6) &&
        abs(games_A - games_B) >= 2) {
      break
    }
    
    # tiebreak
    if (tiebreak && games_A == 6 && games_B == 6) {
      if (runif(1) < 0.5) {
        games_A <- games_A + 1
      } else {
        games_B <- games_B + 1
      }
      break
    }
  }
  
  games_A + games_B
}

#ottelun simulaatio
simulate_match <- function(pA, pB, best_of = 3, tiebreak = TRUE) {
  
  sets_A <- 0
  sets_B <- 0
  total_games <- 0
  need_sets <- ceiling(best_of / 2)
  
  while (sets_A < need_sets && sets_B < need_sets) {
    
    g <- simulate_set(pA, pB, tiebreak)
    total_games <- total_games + g
    
    if (runif(1) < 0.5) {
      sets_A <- sets_A + 1
    } else {
      sets_B <- sets_B + 1
    }
  }
  
#Monte Carlo-jakauma:
  
  simulate_match_distribution <- function(pA, pB,
                                          n_sim = 50000,
                                          best_of = 3,
                                          tiebreak = TRUE) {
    
    replicate(
      n_sim,
      simulate_match(pA, pB, best_of, tiebreak)
    )
  }

#Tail-metriikat    
  extract_tail_metrics <- function(sim_games, line) {
    
    list(
      mean_sim = mean(sim_games),
      q90 = quantile(sim_games, 0.90),
      q95 = quantile(sim_games, 0.95),
      q99 = quantile(sim_games, 0.99),
      p_over = mean(sim_games > line),
      p_far_over = mean(sim_games > (line + 4))
    )
  }
  
  total_games
}
