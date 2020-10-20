library(tidyverse)
library(patchwork)

data <- read_csv("data/Data_Experiment_Direct effect on Daphnia.csv")

fig_s3 <- data %>% 
  select(Clone, Bump_score, Kairomone, UVR, Instar, Neck_teeth_count) %>% 
  drop_na() %>% 
  mutate(Bump_score = fct_relevel(Bump_score, "A", "B", "C"),
         Bump_score = as.integer(Bump_score) - 1L,
         Neckteeth_induction = case_when(Bump_score == 0L ~ (Neck_teeth_count * 0.1) + 0.0,
                                         Bump_score == 1L ~ (Neck_teeth_count * 0.1) + 0.3,
                                         Bump_score == 2L ~ (Neck_teeth_count * 0.1) + 0.5)) %>% 
  ggplot() +
  geom_jitter(mapping = aes(x = Clone, y = Neckteeth_induction, colour = UVR, shape = UVR), 
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5, jitter.height = 1/200), alpha = 0.2) +
  scale_color_brewer(palette = "Dark2") +
  facet_grid(Instar ~ Kairomone) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = scales::label_percent(), limits = c(NA, 1), 
                     minor_breaks = 0:10/10) +
  labs(x = "Clone",
       y = "Induction score") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 4),
        plot.margin = margin(b = 5.5, l = 11, t = 5.5, r = 5.5))

ggsave("figures/figure_s3.png", fig_s3, width = 5.25, height = 3.5, units = "in", dpi = 600, type = "cairo-png")
