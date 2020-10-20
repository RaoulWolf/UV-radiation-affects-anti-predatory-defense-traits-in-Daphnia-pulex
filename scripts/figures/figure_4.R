library(tidyverse)
library(patchwork)

data <- read.csv("data/Data_Experiment_Direct effect on Daphnia.csv") %>% 
  mutate(Bump_score = factor(Bump_score, levels = c("A", "B", "C"), ordered = TRUE))

fig_4_ntc <- data %>% 
  select(Clone, Neck_teeth_count, Kairomone, UVR, Instar) %>% 
  drop_na() %>% 
  ggplot() +
  geom_jitter(mapping = aes(x = Clone, y = Neck_teeth_count, colour = UVR, shape = UVR), 
              position = position_jitterdodge(jitter.width = 1/5, dodge.width = 1/2, jitter.height = 1/6), 
              alpha = 0.2) +
  scale_color_brewer(palette = "Dark2") +
  facet_grid(Instar ~ Kairomone) +
  labs(x = NULL,
       y = "Neckteeth count") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        axis.text.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 4),
        plot.margin = margin(b = 5.5, l = 11, t = 5.5, r = 5.5))

fig_4_ps <- data %>% 
  select(Clone, Bump_score, Kairomone, UVR, Instar) %>% 
  drop_na() %>% 
  ggplot() +
  geom_jitter(mapping = aes(x = Clone, y = as.integer(Bump_score), colour = UVR, shape = UVR), 
              position = position_jitterdodge(jitter.width = 1/5, dodge.width = 1/2, jitter.height = 1/12), 
              alpha = 0.2) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = 1:3, labels = c("A", "B", "C")) +
  facet_grid(Instar ~ Kairomone) +
  labs(y = "Pedestal score") +
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

fig_4 <- wrap_plots(fig_4_ntc, fig_4_ps, ncol = 1, guides = "collect")

ggsave("figures/figure_4.png", fig_4, width = 5.25, height = 7, units = "in", dpi = 600, type = "cairo-png")
