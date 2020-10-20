library(tidyverse)
library(patchwork)

efficacy_data <- read_csv("data/Data_Experiment_Effectivity of kairomones.csv")

fig_s1 <- efficacy_data %>% 
  select(Treatment, Hour, Pedestal_score, Neckteeth_count) %>% 
  mutate(Treatment = case_when(Treatment == "CONTROL" ~ "\U2212Kairomone",
                               Treatment == "PAR" ~ "+PAR",
                               Treatment == "UVR" ~ "+UVR"),
         Treatment = fct_relevel(Treatment, "\U2212Kairomone", "+PAR", "+UVR"),
         Hour = as.character(Hour),
         Hour = fct_relevel(Hour, "2", "4", "6", "8"),
         Pedestal_score = fct_relevel(Pedestal_score, "A", "B", "C"),
         Pedestal_score = as.integer(Pedestal_score) - 1L,
         Neckteeth_induction = case_when(Pedestal_score == 0L ~ (Neckteeth_count * 0.1) + 0.0,
                                         Pedestal_score == 1L ~ (Neckteeth_count * 0.1) + 0.3,
                                         Pedestal_score == 2L ~ (Neckteeth_count * 0.1) + 0.5)) %>% 
  ggplot() +
  geom_jitter(mapping = aes(x = Hour, y = Neckteeth_induction, colour = Treatment, shape = Treatment), 
              position = position_jitterdodge(jitter.width = 1/4, dodge.width = 2/3, jitter.height = 1/100), 
              alpha = 1/3) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = scales::label_percent(), limits = c(NA, 1), 
                     minor_breaks = 0:10/10) +
  labs(x = "Time (h)",
       y = "Induction score") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 4),
        plot.margin = margin(b = 5.5, l = 11, t = 5.5, r = 5.5))

ggsave("figures/figure_s1.png", fig_s1, width = 5.25, height = 3.5, units = "in", dpi = 600, type = "cairo-png")
