library(tidyverse)
library(brms)
options(mc.cores = parallel::detectCores() - 1L)

read_rds("data/experiment_data.rds") ->
  data

data %>%
  select(Body_length, Body_width, Spina_length, Clone, Kairomone, UVR, Treatment, Instar, Mother_ID) %>%
  drop_na() ->
  full_data

mvbrmsformula(brmsformula(formula = Body_length ~ Clone * Kairomone * UVR * Instar + (1 | i | Clone:Treatment:Mother_ID),
                          flist = list(sigma ~ Clone),
                          family = gaussian),
              brmsformula(formula = Body_width ~ Clone * Kairomone * UVR * Instar + (1 | i | Clone:Treatment:Mother_ID),
                          flist = list(sigma ~ Clone),
                          family = gaussian),
              brmsformula(formula = Spina_length ~ Clone * Kairomone * UVR * Instar + (1 | i | Clone:Treatment:Mother_ID),
                          flist = list(sigma ~ Clone),
                          family = gaussian),
              rescor = TRUE) ->
  full_formula

full_formula %>% 
  get_prior(data = full_data)

c(## Correlation structure
  prior(prior = lkj(1), class = "cor"),
  prior(prior = lkj(1), class = "rescor"),
  
  ## Average (mu) effect priors
  prior(prior = normal(1, 5), class = "Intercept", resp = "Bodylength"),
  prior(prior = normal(1, 5), class = "Intercept", resp = "Bodywidth"),
  prior(prior = normal(1, 5), class = "Intercept", resp = "Spinalength"),
  prior(prior = normal(0, 5), class = "b", resp = "Bodylength"),
  prior(prior = normal(0, 5), class = "b", resp = "Bodywidth"),
  prior(prior = normal(0, 5), class = "b", resp = "Spinalength"),

  ## Variance (sigma) effect priors
  prior(prior = normal(0, 5), class = "Intercept", resp = "Bodylength", dpar = "sigma"),
  prior(prior = normal(0, 5), class = "Intercept", resp = "Bodywidth", dpar = "sigma"),
  prior(prior = normal(0, 5), class = "Intercept", resp = "Spinalength", dpar = "sigma"),
  prior(prior = normal(0, 5), class = "b", resp = "Bodylength", dpar = "sigma"),
  prior(prior = normal(0, 5), class = "b", resp = "Bodywidth", dpar = "sigma"),
  prior(prior = normal(0, 5), class = "b", resp = "Spinalength", dpar = "sigma"),
  
  ## Group-level ("random") effect priors
  prior(prior = normal(0, 5), class = "sd", resp = "Bodylength"),
  prior(prior = normal(0, 5), class = "sd", resp = "Bodywidth"),
  prior(prior = normal(0, 5), class = "sd", resp = "Spinalength")) ->
  full_priors

brm(formula = full_formula,
    data = full_data,
    prior = full_priors,
    file = "models/bl_bw_sl_full") ->
  bl_bw_sl_full

bl_bw_sl_full %>% 
  summary()

bl_bw_sl_full %>% 
  plot()

bl_bw_sl_full %>% 
  pp_check(type = "dens_overlay")

bl_bw_sl_full %>% 
  pp_check(type = "ecdf_overlay")

bl_bw_sl_full %>% 
  fixef()

bl_bw_sl_full %>% 
  ranef()

## Reduced model

mvbrmsformula(brmsformula(formula = Body_length ~ Clone * Kairomone * UVR * Instar - Clone:Kairomone:UVR:Instar + (1 | i | Clone:Treatment:Mother_ID),
                          flist = list(sigma ~ Clone),
                          family = gaussian),
              brmsformula(formula = Body_width ~ Clone * Kairomone * UVR * Instar - Clone:Kairomone:UVR:Instar + (1 | i | Clone:Treatment:Mother_ID),
                          flist = list(sigma ~ Clone),
                          family = gaussian),
              brmsformula(formula = Spina_length ~ Clone * Kairomone * UVR * Instar - Clone:Kairomone:UVR:Instar + (1 | i | Clone:Treatment:Mother_ID),
                          flist = list(sigma ~ Clone),
                          family = gaussian),
              rescor = TRUE) ->
  red_formula

brm(formula = red_formula,
    data = full_data,
    prior = full_priors,
    file = "models/bl_bw_sl_red") ->
  bl_bw_sl_red

bl_bw_sl_red %>% 
  summary()

bl_bw_sl_red %>% 
  plot()

bl_bw_sl_red %>% 
  pp_check(type = "dens_overlay")

bl_bw_sl_red %>% 
  pp_check(type = "ecdf_overlay")

bl_bw_sl_red %>% 
  fixef()

bl_bw_sl_red %>% 
  ranef()
