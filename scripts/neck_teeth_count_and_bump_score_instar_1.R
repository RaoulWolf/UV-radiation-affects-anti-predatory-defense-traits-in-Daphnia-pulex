library(brms)
options(mc.cores = parallel::detectCores() - 1L)

data <- readRDS("data/experiment_data.rds")

i1_full_data <- na.omit(data[data$Instar == "1", c("Neck_teeth_count", "Bump_score", "Clone", "Kairomone", "UVR", "Treatment", "Mother_ID")])

## Full model

i1_full_formula <- mvbrmsformula(brmsformula(formula = Bump_score | thres(2) ~ Clone * Kairomone * UVR + (1 | i | Clone:Treatment:Mother_ID),
                                             flist = list(disc ~ Clone),
                                             family = cumulative),
                                 brmsformula(formula = Neck_teeth_count | trials(5) ~ Clone * Kairomone * UVR + (1 | i | Clone:Treatment:Mother_ID),
                                             family = binomial),
                                 rescor = FALSE)

get_prior(formula = i1_full_formula,
          data = i1_full_data)

i1_full_priors <- c(## Correlation structure
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
                    prior(prior = normal(0, 10), class = "b", resp = "Bumpscore", dpar = "disc"))

ntc_bs_i1_full <- brm(formula = i1_full_formula,
                      data = i1_full_data,
                      prior = i1_full_priors,
                      file = "models/ntc_bs_i1_full")

summary(ntc_bs_i1_full)

plot(ntc_bs_i1_full)

pp_check(ntc_bs_i1_full, type = "dens_overlay", resp = "Bumpscore")

pp_check(ntc_bs_i1_full, type = "dens_overlay", resp = "Neckteethcount")

pp_check(ntc_bs_i1_full, type = "ecdf_overlay", resp = "Bumpscore")

pp_check(ntc_bs_i1_full, type = "ecdf_overlay", resp = "Neckteethcount")

fixef(ntc_bs_i1_full)

ranef(ntc_bs_i1_full)

## Reduced model

i1_red_formula <- mvbrmsformula(brmsformula(formula = Bump_score | thres(2) ~ Clone * Kairomone * UVR - Clone:Kairomone:UVR + (1 | i | Clone:Treatment:Mother_ID), 
                                            flist = list(disc ~ Clone),
                                            family = cumulative),
                                brmsformula(formula = Neck_teeth_count | trials(5) ~ Clone * Kairomone * UVR - Clone:Kairomone:UVR + (1 | i | Clone:Treatment:Mother_ID),
                                            family = binomial),
                                rescor = FALSE)

ntc_bs_i1_red <- brm(formula = i1_red_formula,
                     data = i1_full_data,
                     prior = i1_full_priors,
                     file = "models/ntc_bs_i1_red")

summary(ntc_bs_i1_red)

plot(ntc_bs_i1_red)

pp_check(ntc_bs_i1_red, type = "dens_overlay", resp = "Bumpscore")

pp_check(ntc_bs_i1_red, type = "dens_overlay", resp = "Neckteethcount")

pp_check(ntc_bs_i1_red, type = "ecdf_overlay", resp = "Bumpscore")

pp_check(ntc_bs_i1_red, type = "ecdf_overlay", resp = "Neckteethcount")

fixef(ntc_bs_i1_red)

ranef(ntc_bs_i1_red)
