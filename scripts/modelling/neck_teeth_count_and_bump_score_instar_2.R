# Loading the brms package and setup parallel drawing
library(brms)
options(mc.cores = min(parallel::detectCores() - 1, 5))

# Reading in data and preparing data for modelling
data <- read.csv("data/Data_Experiment_Direct effect on Daphnia.csv")
data$Clone <- factor(data$Clone, levels = c("UNI", "P5")) 
data$Kairomone <- factor(data$Kairomone, levels = c("-Kairomone", "+Kairomone"))
data$UVR <- factor(data$UVR, levels = c("-UVR", "+UVR"))
data$Bump_score <- factor(data$Bump_score, levels = c("A", "B", "C"), ordered = TRUE)

inst2_data <- na.omit(data[data$Instar == "Instar 2", c("Neck_teeth_count", "Bump_score", "Clone", "Kairomone", "UVR", "Treatment", "Mother_ID")])

# Full model
# Defining the bivariate model formula
inst2_form_full <- mvbrmsformula(brmsformula(formula = Bump_score | thres(2) ~ Clone * Kairomone * UVR + (1 | gr(Clone:Treatment:Mother_ID, id = "i", cor = FALSE)),
                                             flist = list(disc ~ Clone),
                                             family = cumulative),
                                 brmsformula(formula = Neck_teeth_count | trials(5) ~ Clone * Kairomone * UVR + (1 | gr(Clone:Treatment:Mother_ID, id = "i", cor = FALSE)),
                                             family = binomial))

# Defining the model priors
get_prior(formula = inst2_form_full, data = inst2_data)

inst2_prior <- c(prior(prior = normal(0, 10), class = "b", resp = "Bumpscore"),
                 prior(prior = normal(0, 10), class = "Intercept", resp = "Bumpscore"),
                 prior(prior = normal(0, 10), class = "sd", resp = "Bumpscore"),
                 prior(prior = normal(0, 10), class = "b", resp = "Bumpscore", dpar = "disc"),
                 prior(prior = normal(0, 10), class = "Intercept", resp = "Bumpscore", dpar = "disc"),
                 prior(prior = normal(0, 10), class = "b", resp = "Neckteethcount"),
                 prior(prior = normal(0, 10), class = "Intercept", resp = "Neckteethcount"),
                 prior(prior = normal(0, 10), class = "sd", resp = "Neckteethcount"))

# Fitting the model
inst2_brm_full <- brm(formula = inst2_form_full,
                      data = inst2_data,
                      prior = inst2_prior,
                      chains = 5,
                      iter = 4000,
                      file = "models/induction_instar2_full")

# Model exploration
summary(inst2_brm_full)

fixef(inst2_brm_full)

ranef(inst2_brm_full)

plot(inst2_brm_full)

pp_check(inst2_brm_full, type = "dens_overlay", resp = "Bumpscore")

pp_check(inst2_brm_full, type = "dens_overlay", resp = "Neckteethcount")

pp_check(inst2_brm_full, type = "ecdf_overlay", resp = "Bumpscore")

pp_check(inst2_brm_full, type = "ecdf_overlay", resp = "Neckteethcount")

conditional_effects(inst2_brm_full, resp = "Bumpscore", categorical = TRUE)

conditional_effects(inst2_brm_full, resp = "Neckteethcount")

# Reduced model
# Defining the bivariate model formula
inst2_form_red <- mvbrmsformula(brmsformula(formula = Bump_score | thres(2) ~ Clone * Kairomone * UVR - Clone:Kairomone:UVR + (1 | gr(Clone:Treatment:Mother_ID, id = "i", cor = FALSE)), 
                                            flist = list(disc ~ Clone),
                                            family = cumulative),
                                brmsformula(formula = Neck_teeth_count | trials(5) ~ Clone * Kairomone * UVR - Clone:Kairomone:UVR + (1 | gr(Clone:Treatment:Mother_ID, id = "i", cor = FALSE)),
                                            family = binomial))

# Model fitting
inst2_brm_red <- brm(formula = inst2_form_red,
                     data = inst2_data,
                     prior = inst2_prior,
                     chains = 5,
                     iter = 4000,
                     file = "models/induction_instar2_red")

# Model exploration
summary(inst2_brm_red)

fixef(inst2_brm_red)

ranef(inst2_brm_red)

plot(inst2_brm_red)

pp_check(inst2_brm_red, type = "dens_overlay", resp = "Bumpscore")

pp_check(inst2_brm_red, type = "dens_overlay", resp = "Neckteethcount")

pp_check(inst2_brm_red, type = "ecdf_overlay", resp = "Bumpscore")

pp_check(inst2_brm_red, type = "ecdf_overlay", resp = "Neckteethcount")

conditional_effects(inst2_brm_red, resp = "Bumpscore", categorical = TRUE)

conditional_effects(inst2_brm_red, resp = "Neckteethcount")
