# Loading the brms package and setup parallel drawing
library(brms)
options(mc.cores = min(parallel::detectCores() - 1, 5))

# Reading in data and preparing data for modelling
data <- read.csv("data/Data_Experiment_Direct effect on Daphnia.csv")
data$Clone <- factor(data$Clone, levels = c("UNI", "P5")) 
data$Kairomone <- factor(data$Kairomone, levels = c("-Kairomone", "+Kairomone"))
data$UVR <- factor(data$UVR, levels = c("-UVR", "+UVR"))

size_data <- na.omit(data[, c("Body_length", "Body_width", "Spina_length", "Clone", "Kairomone", "UVR", "Treatment", "Instar", "Mother_ID")])

# Full model
# Defining the trivariate model formula
size_form_full <- mvbrmsformula(brmsformula(formula = Body_length | trunc(lb = 0) ~ Clone * Kairomone * UVR * Instar + (1 | gr(Clone:Treatment:Mother_ID, id = "i", cor = FALSE)),
                                            flist = list(sigma ~ Clone),
                                            family = gaussian),
                                brmsformula(formula = Body_width | trunc(lb = 0) ~ Clone * Kairomone * UVR * Instar + (1 | gr(Clone:Treatment:Mother_ID, id = "i", cor = FALSE)),
                                            flist = list(sigma ~ Clone),
                                            family = gaussian),
                                brmsformula(formula = Spina_length | trunc(lb = 0) ~ Clone * Kairomone * UVR * Instar + (1 | gr(Clone:Treatment:Mother_ID, id = "i", cor = FALSE)),
                                            flist = list(sigma ~ Clone),
                                            family = gaussian),
                                rescor = FALSE)

# Defining the model priors
get_prior(formula = size_form_full, data = size_data)

size_prior <- c(prior(prior = normal(0, 10), class = "b", resp = "Bodylength"),
                prior(prior = normal(0, 10), class = "Intercept", resp = "Bodylength"),
                prior(prior = normal(0, 10), class = "sd", resp = "Bodylength"),
                prior(prior = normal(0, 10), class = "b", resp = "Bodylength", dpar = "sigma"),
                prior(prior = normal(0, 10), class = "b", resp = "Bodywidth"),
                prior(prior = normal(0, 10), class = "Intercept", resp = "Bodywidth"),
                prior(prior = normal(0, 10), class = "sd", resp = "Bodywidth"),
                prior(prior = normal(0, 10), class = "b", resp = "Bodywidth", dpar = "sigma"),
                prior(prior = normal(0, 10), class = "b", resp = "Spinalength"),
                prior(prior = normal(0, 10), class = "Intercept", resp = "Spinalength"),
                prior(prior = normal(0, 10), class = "sd", resp = "Spinalength"),
                prior(prior = normal(0, 10), class = "b", resp = "Spinalength", dpar = "sigma"))

# Fitting the model
size_brm_full <- brm(formula = size_form_full,
                     data = size_data,
                     prior = size_prior,
                     chains = 5,
                     iter = 4000,
                     file = "models/size_full")

# Model exploration
summary(size_brm_full)

fixef(size_brm_full)

ranef(size_brm_full)

plot(size_brm_full)

pp_check(size_brm_full, type = "dens_overlay", resp = "Bodylength")

pp_check(size_brm_full, type = "dens_overlay", resp = "Bodywidth")

pp_check(size_brm_full, type = "dens_overlay", resp = "Spinalength")

pp_check(size_brm_full, type = "ecdf_overlay", resp = "Bodylength")

pp_check(size_brm_full, type = "ecdf_overlay", resp = "Bodywidth")

pp_check(size_brm_full, type = "ecdf_overlay", resp = "Spinalength")

conditional_effects(size_brm_full, resp = "Bodylength")

conditional_effects(size_brm_full, resp = "Bodywidth")

conditional_effects(size_brm_full, resp = "Spinalength")

# Reduced model
# Defining the trivariate model formula
size_form_red <- mvbrmsformula(brmsformula(formula = Body_length | trunc(lb = 0) ~ Clone * Kairomone * UVR * Instar - Clone:Kairomone:UVR:Instar + (1 | gr(Clone:Treatment:Mother_ID, id = "i", cor = FALSE)),
                                           flist = list(sigma ~ Clone),
                                           family = gaussian),
                               brmsformula(formula = Body_width | trunc(lb = 0) ~ Clone * Kairomone * UVR * Instar - Clone:Kairomone:UVR:Instar + (1 | gr(Clone:Treatment:Mother_ID, id = "i", cor = FALSE)),
                                           flist = list(sigma ~ Clone),
                                           family = gaussian),
                               brmsformula(formula = Spina_length | trunc(lb = 0) ~ Clone * Kairomone * UVR * Instar - Clone:Kairomone:UVR:Instar + (1 | gr(Clone:Treatment:Mother_ID, id = "i", cor = FALSE)),
                                           flist = list(sigma ~ Clone),
                                           family = gaussian),
                               rescor = FALSE)

size_brm_red <- brm(formula = size_form_red,
                    data = size_data,
                    prior = size_prior,
                    chains = 5,
                    iter = 4000,
                    file = "models/size_red")

# Model exploration
summary(size_brm_red)

fixef(size_brm_red)

ranef(size_brm_red)

plot(size_brm_red)

pp_check(size_brm_red, type = "dens_overlay", resp = "Bodylength")

pp_check(size_brm_red, type = "dens_overlay", resp = "Bodywidth")

pp_check(size_brm_red, type = "dens_overlay", resp = "Spinalength")

pp_check(size_brm_red, type = "ecdf_overlay", resp = "Bodylength")

pp_check(size_brm_red, type = "ecdf_overlay", resp = "Bodywidth")

pp_check(size_brm_red, type = "ecdf_overlay", resp = "Spinalength")

conditional_effects(size_brm_red, resp = "Bodylength")

conditional_effects(size_brm_red, resp = "Bodywidth")

conditional_effects(size_brm_red, resp = "Spinalength")
