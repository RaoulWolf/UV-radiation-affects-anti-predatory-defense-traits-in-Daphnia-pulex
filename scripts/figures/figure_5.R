library(tidyverse)
library(brms)

induction_instar1 <- read_rds("models/induction_instar1_full.rds")
induction_instar2 <- read_rds("models/induction_instar2_full.rds")

induction_instar1_sum <- induction_instar1 %>% 
  fixef(probs = c(0.025, 0.25, 0.75, 0.975)) %>% 
  as_tibble(rownames = "Parameter") %>%
  filter(!str_detect(Parameter, "disc"),
         !str_detect(Parameter, "Intercept"),
         !str_detect(Parameter, "phi")) %>% 
  mutate(eff = if_else(Q2.5 <= 0 & Q97.5 >= 0, "Weak", "Strong"),
         Endpoint = case_when(str_detect(Parameter, "Neckteethcount") ~ "Neckteeth count",
                              str_detect(Parameter, "Bumpscore") ~ "Pedestal score"),
         Parameter = str_remove(Parameter, "Neckteethcount_"),
         Parameter = str_remove(Parameter, "Bumpscore_"),
         Parameter = str_remove(Parameter, "PKairomone"),
         Parameter = str_remove(Parameter, "P5"),
         Parameter = str_remove(Parameter, "PUVR"),
         Instar = "Instar 1")

induction_instar2_sum <- induction_instar2 %>% 
  fixef(probs = c(0.025, 0.25, 0.75, 0.975)) %>% 
  as_tibble(rownames = "Parameter") %>%
  filter(!str_detect(Parameter, "disc"),
         !str_detect(Parameter, "Intercept"),
         !str_detect(Parameter, "phi")) %>% 
  mutate(eff = if_else(Q2.5 <= 0 & Q97.5 >= 0, "Weak", "Strong"),
         Endpoint = case_when(str_detect(Parameter, "Neckteethcount") ~ "Neckteeth count",
                              str_detect(Parameter, "Bumpscore") ~ "Pedestal score"),
         Parameter = str_remove(Parameter, "Neckteethcount_"),
         Parameter = str_remove(Parameter, "Bumpscore_"),
         Parameter = str_remove(Parameter, "PKairomone"),
         Parameter = str_remove(Parameter, "P5"),
         Parameter = str_remove(Parameter, "PUVR"),
         Instar = "Instar 2")

fig_5 <- induction_instar1_sum %>% 
  bind_rows(induction_instar2_sum) %>% 
  mutate(Instar = fct_relevel(Instar, "Instar 1", "Instar 2")) %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_segment(mapping = aes(x = Q2.5, xend = Q97.5, y = reorder(Parameter, -str_length(Parameter)), yend = reorder(Parameter, -str_length(Parameter)), colour = eff),
               size = 0.5, lineend = "round", show.legend = FALSE) +
  geom_segment(mapping = aes(x = Q25, xend = Q75, y = reorder(Parameter, -str_length(Parameter)), yend = reorder(Parameter, -str_length(Parameter)), colour = eff),
               size = 1, lineend = "round", show.legend = FALSE) +
  geom_point(mapping = aes(x = Estimate, y = reorder(Parameter, -str_length(Parameter)), col = eff), size = 2, show.legend = FALSE) +
  scale_colour_brewer(palette = "Set1") +
  facet_grid(Instar ~ Endpoint) +
  labs(x = "Estimates (logit-scale)",
       y = NULL) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 4),
        plot.margin = margin(b = 5.5, l = 11, t = 5.5, r = 5.5),
        panel.grid.minor = element_blank())

ggsave("figures/figure_5.png", fig_5, width = 5.25, height = 7, units = "in", dpi = 600, type = "cairo-png")
