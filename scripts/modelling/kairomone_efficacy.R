# Loading the brms package and setup parallel drawing
library(brms)
options(mc.cores = min(parallel::detectCores() - 1, 5))

# Reading in data and preparing data for modelling
efficacy_data <- read.csv("data/Data_Experiment_Effectivity of kairomones.csv")
efficacy_data$Treatment <- factor(efficacy_data$Treatment, levels = c("CONTROL", "PAR", "UVR"))
efficacy_data$Pedestal_score <- factor(efficacy_data$Pedestal_score, levels = c("A", "B", "C"), ordered = TRUE)

# Defining the bivariate model formula
efficacy_form <- mvbrmsformula(brmsformula(formula = Pedestal_score | thres(2) ~ Treatment,
                                           family = cumulative),
                               brmsformula(formula = Neckteeth_count | trials(5) ~ Treatment,
                                           family = binomial))

# Defining the model priors
get_prior(efficacy_form, efficacy_data)

efficacy_prior <- c(prior(prior = normal(0, 10), class = "b", resp = "Neckteethcount"),
                    prior(prior = normal(0, 10), class = "Intercept", resp = "Neckteethcount"),
                    prior(prior = normal(0, 10), class = "b", resp = "Pedestalscore"),
                    prior(prior = normal(0, 10), class = "Intercept", resp = "Pedestalscore"))

# Fitting the model
efficacy_brm <- brm(formula = efficacy_form,
                    data = efficacy_data,
                    prior = efficacy_prior,
                    chains = 5,
                    iter = 4000,
                    file = "models/efficacy")

# Model exploration
summary(efficacy_brm)

fixef(efficacy_brm)

plot(efficacy_brm)

pp_check(efficacy_brm, type = "dens_overlay", resp = "Neckteethcount")

pp_check(efficacy_brm, type = "dens_overlay", resp = "Pedestalscore")

pp_check(efficacy_brm, type = "ecdf_overlay", resp = "Neckteethcount")

pp_check(efficacy_brm, type = "ecdf_overlay", resp = "Pedestalscore")

conditional_effects(efficacy_brm, resp = "Neckteethcount")

conditional_effects(efficacy_brm, resp = "Pedestalscore", categorical = TRUE)

# Hypothesis testing
# Are the pedestal scores of the two kairomone treatments the same as for the control?
hypothesis(efficacy_brm, 
           hypothesis = c("Pedestalscore_TreatmentPAR = 0",
                          "Pedestalscore_TreatmentUVR = 0"))    

# Are the neckteeth counts of the two kairomone treatments the same as for the control?
hypothesis(efficacy_brm, 
           hypothesis = c("Neckteethcount_TreatmentPAR = 0",
                          "Neckteethcount_TreatmentUVR = 0"))

# Is the kairomone efficacy different between PAR and UVR treatments?
hypothesis(efficacy_brm, 
           hypothesis = c("Pedestalscore_TreatmentPAR = Pedestalscore_TreatmentUVR",
                          "Neckteethcount_TreatmentPAR = Neckteethcount_TreatmentUVR"))
