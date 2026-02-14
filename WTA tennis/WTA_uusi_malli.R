
  #Long-muoto otteludatasta
  
  library(tidyverse)
library(slider)
  library(brms)

options(mc.cores = 4)          # älä käytä kaikkia 8
options(brms.backend = "cmdstanr")
Sys.setenv(STAN_NUM_THREADS = 4)


options(brms.backend = "cmdstanr")
setwd("~/R-koodi/SportBetting")
#nämä siirretty tiedostosta score_parseri_kolmas_set_prob. 
#Siellä voi kirjoittaa wta_matches_parsed_valmiiksi ja lukea sitten täällä
wta_matches_parsed <- read_csv2(paste0(getwd(),"/Tennis/WTA/wta_matches_parsed.csv"))

wta_matches_parsed <- wta_matches_parsed %>% 
  filter(tourney_level %in% c("G", "P", "M", "A", "I", "F"))

p_third_set_overall <- wta_matches_parsed %>%
  summarise(p = mean(played_third_set)) %>%
  pull(p)

p_third_set_by_surface <- wta_matches_parsed %>%
  group_by(surface) %>%
  summarise(
    p_third_set = mean(played_third_set),
    n_matches = n(),
    .groups = "drop"
  )

#tehdään otteluid.
wta_matches_parsed <- wta_matches_parsed %>%
  mutate(
    match_id = paste(tourney_id, match_num, season, sep = "_"),
    season = substr(tourney_date,1,4),
    surface = factor(surface),
    tourney_id = factor(tourney_id)
  ) %>% 
  filter(
    !is.na(winner_name),!is.na(loser_name),!is.na(score), n_sets >= 2,total_games > 0, season >= 2013
  )

#lisätään tie-brake-dynamiikkaa:

wta_matches_parsed <- wta_matches_parsed %>%
  mutate(
    set1_tiebreak = as.integer(set1_games == 13),
    set2_tiebreak = as.integer(set2_games == 13),
    set3_tiebreak = if_else(
      n_sets == 3,
      as.integer(set3_games == 13),
      0
    ),
    tiebreak_any =
      set1_tiebreak == 1 |
      set2_tiebreak == 1 |
      set3_tiebreak == 1
  )


wta_long <- wta_matches_parsed %>%
  select(
    match_id,
    match_date,
    surface,
    winner_name,
    loser_name,
    total_games,
    played_third_set,
    tourney_id
  ) %>%
  mutate(surface = factor(surface)) %>%
  pivot_longer(
    cols = c(winner_name, loser_name),
    names_to = "role",
    values_to = "player"
  ) %>%
  group_by(match_id) %>%
  mutate(
    opponent = player[rev(row_number())],
    is_winner = as.integer(role == "winner_name"),
    player = factor(player)
  ) %>%
  ungroup() %>%
  arrange(player, match_date)


stopifnot(
  all(wta_long %>%
        count(match_id) %>%
        pull(n) == 2)
)

#yleisiä keskiarvoja ja todennäköisyyksiä vertailukohdaksi.
global_avg_total_games <- mean(
  wta_matches_parsed$total_games,
  na.rm = TRUE
)

global_p_third_set <- mean(
  wta_matches_parsed$played_third_set,
  na.rm = TRUE
)


global_p_tiebreak <- mean(
  wta_matches_parsed$tiebreak_any,
  na.rm = TRUE
)

#tehdään yksi välivaihe, jossa rakennetaan set-kohtaiset ominaisuuudet

wta_long_sets <- wta_matches_parsed %>%
  filter(
    n_sets >= 2,
    total_games > 0
  ) %>%
  mutate(
    games_per_set = total_games / n_sets
  ) %>%
  select(
    match_id,
    match_date,
    surface,
    winner_name,
    loser_name,
    games_per_set,
    n_sets,
    total_games,
    played_third_set,
    tiebreak_any #tiebreakit mukaan
  ) %>%
  pivot_longer(
    cols = c(winner_name, loser_name),
    names_to = "role",
    values_to = "player"
  ) %>%
  mutate(player = factor(player)) %>%
  arrange(player, match_date)

#Pelaajan ominaisuudet
wta_player_features <- wta_long_sets %>%
  group_by(player) %>%
  mutate(
    matches_played = row_number() - 1,
    
    # Raaka rolling-mean
    avg_total_games_20 = slide_dbl(
      total_games,
      mean,
      .before = 20,
      .complete = FALSE
    ),
    
    p_third_set_20 = slide_dbl(
      played_third_set,
      mean,
      .before = 20,
      .complete = FALSE
    ),
    
    # Shrinkage-paino
    w = pmin(matches_played / 20, 1),
    
    # Shrinkatut featuret (nyt globaalit löytyvät)
    avg_total_games_20_shrunk =
      w * avg_total_games_20 + (1 - w) * global_avg_total_games,
    
    p_third_set_20_shrunk =
      w * p_third_set_20 + (1 - w) * global_p_third_set
  ) %>%
  ungroup()

#Laitetaan mukaan tiebreakit:


#pelaajakohtaiset set-ominaisuudet:

library(slider)

wta_player_set_features <- wta_long_sets %>%
  group_by(player) %>%
  mutate(
    matches_played = row_number() - 1,
    
    avg_games_per_set_20 = slide_dbl(
      games_per_set,
      mean,
      .before = 20,
      .complete = FALSE
    ),
    
    sd_games_per_set_20 = slide_dbl(
      games_per_set,
      sd,
      .before = 20,
      .complete = FALSE
    ),
    
    p_tiebreak_20 = slide_dbl(
      tiebreak_any,
      mean,
      .before = 20,
      .complete = FALSE
    ),
    
    w = pmin(matches_played / 20, 1),
    
    p_tiebreak_20_shrunk =
      w * p_tiebreak_20 + (1 - w) * global_p_tiebreak
  ) %>%
  ungroup()



wta_model_data <- wta_matches_parsed %>%
  select(
    match_id,
    surface,
    total_games,
    played_third_set,
    winner_name,
    loser_name,
    tourney_id
  ) %>%
  left_join(
    wta_player_set_features %>%
      select(match_id, player, avg_games_per_set_20, sd_games_per_set_20),
    by = c("match_id", "winner_name" = "player")
  ) %>%
  rename_with(~ paste0(.x, "_w"),
              c(avg_games_per_set_20, sd_games_per_set_20)) %>%
  left_join(
    wta_player_set_features %>%
      select(match_id, player, avg_games_per_set_20, sd_games_per_set_20),
    by = c("match_id", "loser_name" = "player")
  ) %>%
  rename_with(~ paste0(.x, "_l"),
              c(avg_games_per_set_20, sd_games_per_set_20)) %>%
  left_join(
    wta_player_set_features %>%
      select(match_id, player, p_tiebreak_20_shrunk),
    by = c("match_id", "winner_name" = "player")
  ) %>%
  rename(p_tiebreak_w = p_tiebreak_20_shrunk) %>%
  left_join(
    wta_player_set_features %>%
      select(match_id, player, p_tiebreak_20_shrunk),
    by = c("match_id", "loser_name" = "player")
  ) %>%
  rename(p_tiebreak_l = p_tiebreak_20_shrunk) %>%
  mutate(
    mean_games_per_set =
      (avg_games_per_set_20_w + avg_games_per_set_20_l) / 2,
    
    sd_games_per_set =
      (sd_games_per_set_20_w + sd_games_per_set_20_l) / 2,
    
    avg_p_tiebreak =
      (p_tiebreak_w + p_tiebreak_l) / 2
  ) %>%
  replace_na(list(0))




#tehdään long-versio:

wta_model_data_long <- wta_model_data %>%
  select(
    match_id,
    surface,
    total_games,
    played_third_set,
    tourney_id,
    winner_name,
    loser_name
  ) %>%
  pivot_longer(
    cols = c(winner_name, loser_name),
    names_to = "role",
    values_to = "player"
  ) %>%
  mutate(
    player = factor(player),
    surface = factor(surface)
  )


##### 

##Rakennetaan malli tiebreakille:

wta_tiebreak_data <- wta_matches_parsed %>%
  mutate(
    tiebreak_any = as.integer(grepl("7-", score) | grepl("6-7", score))
  ) %>%
  select(
    match_id,
    surface,
    winner_name,
    loser_name,
    tiebreak_any
  )

tiebreak_training_data <- wta_model_data %>%
  left_join(
    wta_tiebreak_data %>% select(match_id, tiebreak_any),
    by = "match_id"
  ) %>%
  mutate(
    abs_diff_games_per_set = abs(
      avg_games_per_set_20_w - avg_games_per_set_20_l
    ),
    
    player_pair = map2_chr(
      winner_name,
      loser_name,
      ~ paste(sort(c(.x, .y)), collapse = "__")
    ),
    player_pair = factor(player_pair, levels = unique(player_pair)),
    surface = factor(surface)
  ) %>%
  drop_na(tiebreak_any)

tiebreak_formula <- bf(
  tiebreak_any ~
    abs_diff_games_per_set +
    surface +
    (1 | player_pair),
  family = bernoulli(link = "logit")
)

tiebreak_fit <- brm(
  formula = tiebreak_formula,
  data    = tiebreak_training_data,
  prior = c(
    set_prior("normal(0, 1)", class = "b"),
    set_prior("normal(0, 1.5)", class = "Intercept"),
    set_prior("exponential(1)", class = "sd", group = "player_pair")
  ),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  cores = 4,
  backend = "cmdstanr",
  seed = 2026,
  control = list(adapt_delta = 0.95)
)


#Kolmennen erän malli (logistinen)

library(brms)

#muutettu tässä trainin_data <- wta_model_data. Tehty sama muutos third_set_fitiin, jo
#training_data <- wta_model_data
wta_model_data <- wta_model_data %>%
  mutate(
    # Tasaisuus (kuinka lähellä pelaajat ovat toisiaan)
    abs_diff_games_per_set =
      abs(avg_games_per_set_20_w - avg_games_per_set_20_l),
    
    # DOMINANSSI (suunta mukana)
    dom_games_per_set =
      avg_games_per_set_20_w - avg_games_per_set_20_l,
    
    # Tiebreak-profiilin samankaltaisuus
    abs_diff_p_tiebreak =
      abs(p_tiebreak_w - p_tiebreak_l),
    
    #Odotetut tiebreakit:
    expected_tiebreaks =
      p_tiebreak_w + p_tiebreak_l,
    
    # Setti-varianssi (kaoottisuus)
    sd_games_per_set = sd_games_per_set,
    
    #kolmas erä lähes varma
    very_close =
      abs_diff_games_per_set < quantile(abs_diff_games_per_set, 0.15),
    
    # Pelaajapari (jos joskus halutaan takaisin)
    player_pair = map2_chr(
      winner_name,
      loser_name,
      ~ paste(sort(c(.x, .y)), collapse = "__")
    ),
    
    surface = factor(surface)
  ) %>%
  drop_na()

# third_set_formula <- bf(
#   played_third_set ~
#     abs_diff_games_per_set +          # ottelun tasaisuus
#     s(dom_games_per_set, k = 5) +      # dominanssi (EI-lineaarinen)
#     sd_games_per_set +                # ottelun kaoottisuus
#     abs_diff_p_tiebreak +              # TB-alttius
#     very_close +                      # kolmas erä lähes varma
#     t2(dom_games_per_set, sd_games_per_set, k = c(5, 5)) +
#     (1 | surface),
#   family = bernoulli(link = "logit")
# )

third_set_formula <- bf(
  played_third_set ~
    abs_diff_games_per_set +      # ottelun tasaisuus
    s(dom_games_per_set, k = 5, by = very_close) + # dominanssi (EI-lineaarinen) very_closelle truen ja falsen perusteella eri splinit
    sd_games_per_set +            # ottelun kaoottisuus
    abs_diff_p_tiebreak +         # TB-alttius
    very_close +                  # kolmas erä lähes varma
    very_close:dom_games_per_set +
#    t2(dom_games_per_set, sd_games_per_set, k = c(5, 5)) +
    (1 | surface),
  family = bernoulli(link = "logit")
)


third_set_fit <- brm(
  formula = third_set_formula,
  data    = wta_model_data, #tässä oli training_data
  prior = c(
    set_prior("normal(0, 0.7)", class = "b"),
    set_prior("normal(0, 1.2)", class = "Intercept"),
    set_prior("exponential(1)", class = "sd", group = "surface"),
    set_prior("exponential(2)", class = "sds")  # spline-regularisointi
  ),
  chains  = 4,
  iter    = 4000,
  warmup = 1000,
  cores   = 4,
  backend = "cmdstanr",
  seed    = 2026,
  control = list(
    adapt_delta = 0.95,
    max_treedepth = 12
  )
)

#saveRDS(tiebreak_fit, "~/R-koodi/SportBetting/WTA tennis/Models/tiebreak_fit.rds")
saveRDS(third_set_fit, "~/R-koodi/SportBetting/WTA tennis/Models/hird_set_fit.rds")

rm(third_set_fit)
rm(tiebreak_fit)

library(brms)
#lisää päämalliin:
#+ very_close +
#+ played_third_set:very_close
#lisää tänne sigmaan:
#  played_third_set:very_close
total_games_formula <- bf(
  total_games ~
    surface +
    played_third_set +
    mean_games_per_set +
    sd_games_per_set +
    abs_diff_games_per_set +
    played_third_set:mean_games_per_set +
    played_third_set:abs_diff_games_per_set +
    expected_tiebreaks +
    played_third_set:expected_tiebreaks +
    (1 | tourney_id),
  
  sigma ~
    sd_games_per_set +
    played_third_set +
    played_third_set +
    played_third_set:abs_diff_games_per_set
)


total_games_priors <- c(
  
  # Mean-modelin priors
  prior(normal(20, 5), class = "Intercept"),
  prior(normal(0, 1), class = "b"),
  
  # Random effect (tourney)
  prior(exponential(1), class = "sd", group = "tourney_id"),
  
  # Sigma-modelin priors
  prior(normal(-1, 1), class = "Intercept", dpar = "sigma"),
  prior(normal(0, 0.5), class = "b", dpar = "sigma")
  
)


total_games_fit <- brm(
  formula = total_games_formula,
  data    = wta_model_data,
  prior  = total_games_priors,
  chains = 4,
  iter   = 4000,
  warmup = 1000,
  cores  = 4,
  backend = "cmdstanr",
  seed = 2026,
  control = list(
    adapt_delta = 0.95,
    max_treedepth = 12
  )
)
saveRDS(total_games_fit, "~/R-koodi/SportBetting/WTA tennis/Models/total_games_fit.rds")
saveRDS(wta_model_data, "~/R-koodi/SportBetting/WTA tennis/Models/wta_model_data.rds")
saveRDS(wta_model_data_long, "~/R-koodi/SportBetting/WTA tennis/Models/wta_model_data_long.rds")

#Kirjoitetaan talteen mallit, jotta ei tarvitse kääntää, jos R kaatuu
setwd("~/R-koodi/SportBetting/WTA tennis/Models/Uusi malli")

# saveRDS(tiebreak_fit, "~/R-koodi/SportBetting/WTA tennis/Models/wta_tiebreak_fit.rds")
# saveRDS(third_set_fit, "~/R-koodi/SportBetting/WTA tennis/Models/wta_third_set_fit.rds")


saveRDS(wta_player_features, "~/R-koodi/SportBetting/WTA tennis/Models/wta_player_features.rds")
saveRDS(wta_player_set_features, "~/R-koodi/SportBetting/WTA tennis/Models/wta_player_set_features.rds")


#Listaus turnausten tasoista:
ta_tourney_lookup <- wta_matches_parsed %>%
  filter(season >= 2018) %>%
  distinct(
    tourney_name,
    tourney_level,
    surface,
    season,
    tourney_date
  ) %>%
  mutate(
    week = as.integer(substr(tourney_date, 5, 6))  # kk-pohjainen proxy
  )

