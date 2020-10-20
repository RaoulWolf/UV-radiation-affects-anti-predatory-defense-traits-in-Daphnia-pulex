library(tidyverse)
library(brms)

size_brm_red <- read_rds("models/size_red.rds")

fig_3 <- size_brm_red %>% 
  fixef(probs = c(0.025, 0.25, 0.75, 0.975)) %>% 
  as_tibble(rownames = "Parameter") %>% 
  filter(!str_detect(Parameter, "sigma"),
         !str_detect(Parameter, "Intercept")) %>% 
  mutate(eff = if_else(Q2.5 <= 0 & Q97.5 >= 0, "Weak", "Strong"),
         Endpoint = case_when(str_detect(Parameter, "Bodylength") ~ "Body length",
                              str_detect(Parameter, "Bodywidth") ~ "Body width",
                              str_detect(Parameter, "Spinalength") ~ "Spina length"),
         Parameter = str_remove(Parameter, "Bodylength_"),
         Parameter = str_remove(Parameter, "Instar"),
         Parameter = str_remove(Parameter, "Bodywidth_"),
         Parameter = str_remove(Parameter, "Spinalength_"),
         Parameter = str_remove(Parameter, "P5"),
         Parameter = str_remove(Parameter, "PUVR"),
         Parameter = str_remove(Parameter, "PKairomone"),
         Parameter = str_remove(Parameter, "2")) %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_segment(mapping = aes(x = Q2.5, xend = Q97.5, y = reorder(Parameter, -str_length(Parameter)), yend = reorder(Parameter, -str_length(Parameter)), colour = eff),
               size = 0.5, lineend = "round", show.legend = FALSE) +
  geom_segment(mapping = aes(x = Q25, xend = Q75, y = reorder(Parameter, -str_length(Parameter)), yend = reorder(Parameter, -str_length(Parameter)), colour = eff),
               size = 1, lineend = "round", show.legend = FALSE) +
  geom_point(mapping = aes(x = Estimate, y = reorder(Parameter, -str_length(Parameter)), col = eff), size = 1, show.legend = FALSE) +
  scale_colour_brewer(palette = "Set1") +
  facet_wrap(~ Endpoint) +
  labs(x = "Estimates",
       y = NULL) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 4),
        plot.margin = margin(b = 5.5, l = 11, t = 5.5, r = 5.5),
        panel.grid.minor = element_blank())

ggsave("figures/figure_3.png", fig_3, width = 5.25, height = 3.5, units = "in", dpi = 600, type = "cairo-png")
