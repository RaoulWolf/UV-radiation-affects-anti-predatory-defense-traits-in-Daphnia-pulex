library(brms)
options(mc.cores = parallel::detectCores() - 1L)

data <- readRDS("data/experiment_data.rds")

full_data <- na.omit(data[, c("Body_length", "Body_width", "Spina_length", "Clone", "Kairomone", "UVR", "Treatment", "Instar", "Mother_ID")])

## Full model

full_formula <- mvbrmsformula(brmsformula(formula = Body_length | trunc(lb = 0) ~ Clone * Kairomone * UVR * Instar + (1 | i | Clone:Treatment:Mother_ID),
                                          flist = list(sigma ~ Clone),
                                          family = gaussian),
                              brmsformula(formula = Body_width | trunc(lb = 0) ~ Clone * Kairomone * UVR * Instar + (1 | i | Clone:Treatment:Mother_ID),
                                          flist = list(sigma ~ Clone),
                                          family = gaussian),
                              brmsformula(formula = Spina_length | trunc(lb = 0) ~ Clone * Kairomone * UVR * Instar + (1 | i | Clone:Treatment:Mother_ID),
                                          flist = list(sigma ~ Clone),
                                          family = gaussian),
                              rescor = FALSE)

get_prior(formula = full_formula, 
          data = full_data)

full_priors <- c(## Correlation structure
                 prior(prior = lkj(1), class = "cor"),
                 
                 ## Average (mu) effect priors
                 prior(prior = normal(0, 10), class = "Intercept", resp = "Bodylength"),
                 prior(prior = normal(0, 10), class = "Intercept", resp = "Bodywidth"),
                 prior(prior = normal(0, 10), class = "Intercept", resp = "Spinalength"),
                 prior(prior = normal(0, 10), class = "b", resp = "Bodylength"),
                 prior(prior = normal(0, 10), class = "b", resp = "Bodywidth"),
                 prior(prior = normal(0, 10), class = "b", resp = "Spinalength"),
               
                 ## Variance (sigma) effect priors
                 prior(prior = normal(0, 10), class = "Intercept", resp = "Bodylength", dpar = "sigma"),
                 prior(prior = normal(0, 10), class = "Intercept", resp = "Bodywidth", dpar = "sigma"),
                 prior(prior = normal(0, 10), class = "Intercept", resp = "Spinalength", dpar = "sigma"),
                 prior(prior = normal(0, 10), class = "b", resp = "Bodylength", dpar = "sigma"),
                 prior(prior = normal(0, 10), class = "b", resp = "Bodywidth", dpar = "sigma"),
                 prior(prior = normal(0, 10), class = "b", resp = "Spinalength", dpar = "sigma"),
                 
                 ## Group-level ("random") effect priors
                 prior(prior = normal(0, 10), class = "sd", resp = "Bodylength"),
                 prior(prior = normal(0, 10), class = "sd", resp = "Bodywidth"),
                 prior(prior = normal(0, 10), class = "sd", resp = "Spinalength"))

bl_bw_sl_full <- brm(formula = full_formula,
                     data = full_data,
                     prior = full_priors,
                     file = "models/bl_bw_sl_full")

summary(bl_bw_sl_full)

plot(bl_bw_sl_full)

pp_check(bl_bw_sl_full, type = "dens_overlay", resp = "Bodylength")

pp_check(bl_bw_sl_full, type = "dens_overlay", resp = "Bodywidth")

pp_check(bl_bw_sl_full, type = "dens_overlay", resp = "Spinalength")

pp_check(bl_bw_sl_full, type = "ecdf_overlay", resp = "Bodylength")

pp_check(bl_bw_sl_full, type = "ecdf_overlay", resp = "Bodywidth")

pp_check(bl_bw_sl_full, type = "ecdf_overlay", resp = "Spinalength")

fixef(bl_bw_sl_full)

ranef(bl_bw_sl_full)

## Reduced model

red_formula <- mvbrmsformula(brmsformula(formula = Body_length | trunc(lb = 0) ~ Clone * Kairomone * UVR * Instar - Clone:Kairomone:UVR:Instar + (1 | i | Clone:Treatment:Mother_ID),
                                         flist = list(sigma ~ Clone),
                                         family = gaussian),
                             brmsformula(formula = Body_width | trunc(lb = 0) ~ Clone * Kairomone * UVR * Instar - Clone:Kairomone:UVR:Instar + (1 | i | Clone:Treatment:Mother_ID),
                                         flist = list(sigma ~ Clone),
                                         family = gaussian),
                             brmsformula(formula = Spina_length | trunc(lb = 0) ~ Clone * Kairomone * UVR * Instar - Clone:Kairomone:UVR:Instar + (1 | i | Clone:Treatment:Mother_ID),
                                         flist = list(sigma ~ Clone),
                                         family = gaussian),
                             rescor = FALSE)

bl_bw_sl_red <- brm(formula = red_formula,
                    data = full_data,
                    prior = full_priors,
                    file = "models/bl_bw_sl_red")

summary(bl_bw_sl_red)

plot(bl_bw_sl_red)

pp_check(bl_bw_sl_red, type = "dens_overlay", resp = "Bodylength")

pp_check(bl_bw_sl_red, type = "dens_overlay", resp = "Bodywidth")

pp_check(bl_bw_sl_red, type = "dens_overlay", resp = "Spinalength")

pp_check(bl_bw_sl_red, type = "ecdf_overlay", resp = "Bodylength")

pp_check(bl_bw_sl_red, type = "ecdf_overlay", resp = "Bodywidth")

pp_check(bl_bw_sl_red, type = "ecdf_overlay", resp = "Spinalength")

fixef(bl_bw_sl_red)

ranef(bl_bw_sl_red)
