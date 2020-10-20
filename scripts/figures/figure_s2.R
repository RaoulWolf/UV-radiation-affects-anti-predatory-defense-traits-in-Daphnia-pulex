library(tidyverse)
library(patchwork)

data <- read_csv("data/Data_Experiment_Direct effect on Daphnia.csv")

fig_s2 <- data %>% 
  select(Clone, Body_width, Kairomone, UVR, Instar) %>% 
  drop_na() %>% 
  ggplot() +
  geom_jitter(mapping = aes(x = Clone, y = Body_width, colour = UVR, shape = UVR), 
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5, jitter.height = 0), 
              alpha = 0.2) +
  scale_color_brewer(palette = "Dark2") +
  facet_grid(Instar ~ Kairomone) +
  labs(x = "Clone", 
       y = "Body width (mm)") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 4),
        plot.margin = margin(b = 5.5, l = 11, t = 5.5, r = 5.5))

ggsave("figures/figure_s2.png", fig_s2, width = 5.25, height = 3.5, units = "in", dpi = 600, type = "cairo-png")
