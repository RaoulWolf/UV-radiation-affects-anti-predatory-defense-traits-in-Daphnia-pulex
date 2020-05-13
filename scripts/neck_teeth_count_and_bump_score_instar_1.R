library(tidyverse)
library(brms)
options(mc.cores = parallel::detectCores() - 1L)

read_rds("data/experiment_data.rds") ->
  data

data %>% 
  filter(Instar == "1") %>% 
  select(Neck_teeth_count, Bump_score, Clone, Kairomone, UVR, Treatment, Mother_ID) %>% 
  drop_na() %>% 
  mutate(Mother_ID = fct_drop(Mother_ID)) ->
  i1_full_data

mvbrmsformula(brmsformula(formula = Bump_score | thres(2) ~ Clone * Kairomone * UVR + (1 | i | Clone:Treatment:Mother_ID),
                          flist = list(disc ~ Clone),
                          family = cumulative),
              brmsformula(formula = Neck_teeth_count | trials(5) ~ Clone * Kairomone * UVR + (1 | i | Clone:Treatment:Mother_ID),
                          family = binomial),
              rescor = FALSE) ->
  i1_full_formula

i1_full_formula %>% 
  get_prior(data = i1_full_data)

c(## Correlation structure
  prior(prior = lkj(1), class = "cor"),
  
  ## Average (mu) effect priors
  prior(prior = normal(0, 10), class = "Intercept", resp = "Bumpscore"),
  prior(prior = normal(0, 10), class = "Intercept", resp = "Neckteethcount"),
  prior(prior = normal(0, 10), class = "b", resp = "Bumpscore"),
  prior(prior = normal(0, 10), class = "b", resp = "Neckteethcount"),
  
  # Group-level (random) effect priors
  prior(prior = normal(0, 10), class = "sd", resp = "Bumpscore"),
  prior(prior = normal(0, 10), class = "sd", resp = "Neckteethcount"),
  
  # Distributional parameter priors
  prior(prior = normal(0, 10), class = "Intercept", resp = "Bumpscore", dpar = "disc"),
  prior(prior = normal(0, 10), class = "b", resp = "Bumpscore", dpar = "disc")) ->
  i1_full_priors

brm(formula = i1_full_formula,
    data = i1_full_data,
    prior = i1_full_priors,
    file = "models/ntc_bs_i1_full") ->
  ntc_bs_i1_full

ntc_bs_i1_full %>% 
  summary()

ntc_bs_i1_full %>% 
  plot()

ntc_bs_i1_full %>% 
  pp_check(type = "dens_overlay", resp = "Bumpscore")

ntc_bs_i1_full %>% 
  pp_check(type = "dens_overlay", resp = "Neckteethcount")

ntc_bs_i1_full %>% 
  pp_check(type = "ecdf_overlay", resp = "Bumpscore")

ntc_bs_i1_full %>% 
  pp_check(type = "ecdf_overlay", resp = "Neckteethcount")

ntc_bs_i1_full %>% 
  fixef()

ntc_bs_i1_full %>% 
  ranef()
