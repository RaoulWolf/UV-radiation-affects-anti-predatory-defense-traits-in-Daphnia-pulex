# Loading the brms package and setup parallel drawing
library(brms)
options(mc.cores = min(parallel::detectCores() - 1, 5))

# Reading in data and preparing data for modelling
data <- read.csv("data/Data_Experiment_Direct effect on Daphnia.csv")
data$Clone <- factor(data$Clone, levels = c("UNI", "P5")) 
data$Kairomone <- factor(data$Kairomone, levels = c("-Kairomone", "+Kairomone"))
data$UVR <- factor(data$UVR, levels = c("-UVR", "+UVR"))
data$Bump_score <- factor(data$Bump_score, levels = c("A", "B", "C"), ordered = TRUE)

inst1_data <- na.omit(data[data$Instar == "Instar 1", c("Neck_teeth_count", "Bump_score", "Clone", "Kairomone", "UVR", "Treatment", "Mother_ID")])

# Full model
# Defining the bivariate model formula
inst1_form_full <- mvbrmsformula(brmsformula(formula = Bump_score | thres(2) ~ Clone * Kairomone * UVR + (1 | gr(Clone:Treatment:Mother_ID, id = "i", cor = FALSE)),
                                             flist = list(disc ~ Clone),
                                             family = cumulative),
                                 brmsformula(formula = Neck_teeth_count | trials(5) ~ Clone * Kairomone * UVR + (1 | gr(Clone:Treatment:Mother_ID, id = "i", cor = FALSE)),
                                             family = binomial))

# Defining the model priors
get_prior(formula = inst1_form_full, data = inst1_data)

inst1_prior <- c(prior(prior = normal(0, 10), class = "b", resp = "Bumpscore"),
                 prior(prior = normal(0, 10), class = "Intercept", resp = "Bumpscore"),
                 prior(prior = normal(0, 10), class = "sd", resp = "Bumpscore"),
                 prior(prior = normal(0, 10), class = "b", resp = "Bumpscore", dpar = "disc"),
                 prior(prior = normal(0, 10), class = "Intercept", resp = "Bumpscore", dpar = "disc"),
                 prior(prior = normal(0, 10), class = "b", resp = "Neckteethcount"),
                 prior(prior = normal(0, 10), class = "Intercept", resp = "Neckteethcount"),
                 prior(prior = normal(0, 10), class = "sd", resp = "Neckteethcount"))

# Fitting the model
inst1_brm_full <- brm(formula = inst1_form_full,
                      data = inst1_data,
                      prior = inst1_prior,
                      chains = 5,
                      iter = 4000,
                      file = "models/induction_instar1_full")

# Model exploration
summary(inst1_brm_full)

fixef(inst1_brm_full)

ranef(inst1_brm_full)

plot(inst1_brm_full)

pp_check(inst1_brm_full, type = "dens_overlay", resp = "Bumpscore")

pp_check(inst1_brm_full, type = "dens_overlay", resp = "Neckteethcount")

pp_check(inst1_brm_full, type = "ecdf_overlay", resp = "Bumpscore")

pp_check(inst1_brm_full, type = "ecdf_overlay", resp = "Neckteethcount")

conditional_effects(inst1_brm_full, resp = "Bumpscore", categorical = TRUE)

conditional_effects(inst1_brm_full, resp = "Neckteethcount")

# Reduced model
# Defining the bivariate model formula
inst1_form_red <- mvbrmsformula(brmsformula(formula = Bump_score | thres(2) ~ Clone * Kairomone * UVR - Clone:Kairomone:UVR + (1 | gr(Clone:Treatment:Mother_ID, id = "i", cor = FALSE)), 
                                            flist = list(disc ~ Clone),
                                            family = cumulative),
                                brmsformula(formula = Neck_teeth_count | trials(5) ~ Clone * Kairomone * UVR - Clone:Kairomone:UVR + (1 | gr(Clone:Treatment:Mother_ID, id = "i", cor = FALSE)),
                                            family = binomial))

# Model fitting
inst1_brm_red <- brm(formula = inst1_form_red,
                     data = inst1_data,
                     prior = inst1_prior,
                     chains = 5,
                     iter = 4000,
                     file = "models/induction_instar1_red")

# Model exploration
summary(inst1_brm_red)

fixef(inst1_brm_red)

ranef(inst1_brm_red)

plot(inst1_brm_red)

pp_check(inst1_brm_red, type = "dens_overlay", resp = "Bumpscore")

pp_check(inst1_brm_red, type = "dens_overlay", resp = "Neckteethcount")

pp_check(inst1_brm_red, type = "ecdf_overlay", resp = "Bumpscore")

pp_check(inst1_brm_red, type = "ecdf_overlay", resp = "Neckteethcount")

conditional_effects(inst1_brm_red, resp = "Bumpscore", categorical = TRUE)

conditional_effects(inst1_brm_red, resp = "Neckteethcount")
