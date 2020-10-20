library(tidyverse)
library(patchwork)

data <- read.csv("data/Data_Experiment_Direct effect on Daphnia.csv")

fig_2_bl <- data %>% 
  select(Clone, Body_length, Kairomone, UVR, Instar) %>% 
  drop_na() %>% 
  ggplot() +
  geom_jitter(mapping = aes(x = Clone, y = Body_length, colour = UVR, shape = UVR), 
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5, jitter.height = 0), 
              alpha = 0.2) +
  scale_color_brewer(palette = "Dark2") +
  facet_grid(Instar ~ Kairomone) +
  labs(x = NULL, 
       y = "Body length (mm)") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        axis.text.x.bottom = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 4),
        plot.margin = margin(b = 5.5, l = 11, t = 5.5, r = 5.5))

fig_2_sl <- data %>% 
  select(Clone, Spina_length, Kairomone, UVR, Instar) %>% 
  drop_na() %>% 
  ggplot() +
  geom_jitter(mapping = aes(x = Clone, y = Spina_length, colour = UVR, shape = UVR), 
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5, jitter.height = 0), 
              alpha = 0.2) +
  scale_color_brewer(palette = "Dark2") +
  facet_grid(Instar ~ Kairomone) +
  labs(x = "Clone", 
       y = "Spina length (mm)") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        strip.background.x = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 4),
        plot.margin = margin(b = 5.5, l = 11, t = 5.5, r = 5.5))

fig_2 <- wrap_plots(fig_2_bl, fig_2_sl, ncol = 1, guides = "collect")

ggsave("figures/figure_2.png", fig_2, width = 5.25, height = 7, units = "in", dpi = 600, type = "cairo-png")
