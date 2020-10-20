library(tidyverse)
library(patchwork)

efficacy_data <- read_csv("data/Data_Experiment_Effectivity of kairomones.csv")

fig_1_ntc <- efficacy_data %>% 
  select(Treatment, Hour, Neckteeth_count) %>% 
  mutate(Treatment = case_when(Treatment == "CONTROL" ~ "\U2212Kairomone",
                               Treatment == "PAR" ~ "+PAR",
                               Treatment == "UVR" ~ "+UVR"),
         Treatment = fct_relevel(Treatment, "\U2212Kairomone", "+PAR", "+UVR"),
         Hour = as.character(Hour),
         Hour = fct_relevel(Hour, "2", "4", "6", "8"),
         Neckteeth_count = as.integer(Neckteeth_count)) %>% 
  ggplot() +
  geom_jitter(mapping = aes(x = Hour, y = Neckteeth_count, colour = Treatment, shape = Treatment), 
              position = position_jitterdodge(jitter.width = 1/4, dodge.width = 2/3, jitter.height = 1/6), 
              alpha = 1/3) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = 0:5, limits = c(NA, 5)) +
  labs(x = NULL, 
       y = "Neckteeth count") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(vjust = -1),
        axis.text.x.bottom = element_blank(),
        axis.title.y = element_text(vjust = 4),
        plot.margin = margin(b = 5.5, l = 11, t = 5.5, r = 5.5))

fig_1_ps <- efficacy_data %>% 
  select(Treatment, Hour, Pedestal_score) %>% 
  mutate(Treatment = case_when(Treatment == "CONTROL" ~ "\U2212Kairomone",
                               Treatment == "PAR" ~ "+PAR",
                               Treatment == "UVR" ~ "+UVR"),
         Treatment = fct_relevel(Treatment, "\U2212Kairomone", "+PAR", "+UVR"),
         Hour = as.character(Hour),
         Hour = fct_relevel(Hour, "2", "4", "6", "8"),
         Pedestal_score = fct_relevel(Pedestal_score, "A", "B", "C"),
         Pedestal_score = as.integer(Pedestal_score) - 1L) %>% 
  ggplot() +
  geom_jitter(mapping = aes(x = Hour, y = Pedestal_score, colour = Treatment, shape = Treatment), 
              position = position_jitterdodge(jitter.width = 1/4, dodge.width = 2/3, jitter.height = 1/12), 
              alpha = 1/3) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = 0:2, labels = c("A", "B", "C")) +
  labs(x = "Time (h)",
       y = "Pedestal score") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 4),
        plot.margin = margin(b = 5.5, l = 11, t = 5.5, r = 5.5))

fig_1 <- wrap_plots(fig_1_ntc, fig_1_ps, ncol = 1, guides = "collect")

ggsave("figures/figure_1.png", fig_1, width = 5.25, height = 7, units = "in", dpi = 600, type = "cairo-png")
