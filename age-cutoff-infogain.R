# find "optimal" age cutoff to split cohort into a "young" and an "old" subgroup
# the decision criterion is which of the unique binary age splits yields 
# maximum information gain with respect to the target variable `pathology`
library(tidyverse)
library(FSelector)

# customized discretization function
cutt <- function(x, cutpoint) {
  cut(x, c(-Inf, cutpoint, Inf), 
      labels = paste0(c("young (<=", "old (>"), cutpoint, ")"))
}

# each of the subgroups "young" and "old" must contain at least `min_group_size`
# persons
min_group_size <- 30

# -> prep-data.R
df <- read_rds("Data/data-prep.rds")
age <- df$age
patho <- df$pathology

# for each age value occurring in the data,
# obtain the number of persons in the subgroups "young"/"old",
# then keep age values as cutpoint candidates which lead to both subgroups 
# having at least `min_group_size` persons,
# then calculate information gain wrt. pathology for each cutpoint candidate,
# then sort by information gain in descending order
ig <- tibble(cutpoint = sort(unique(age))) %>%
  mutate(subgroups = map(cutpoint, ~ table(cutt(age, .x), patho))) %>%
  mutate(subgroup_sizes = map(subgroups, rowSums)) %>%
  mutate(valid = map_lgl(subgroup_sizes, ~ all(.x > min_group_size))) %>%
  filter(valid) %>%
  select(cutpoint) %>%
  mutate(infogain = map_dbl(cutpoint, function(x) {
    d <- tibble(
      x = cutt(age, x),
      y = patho
    )
    information.gain(y ~ x, d)[[1]]
  })) %>%
  arrange(desc(infogain))

# optimal age cutpoint
opt_cutpoint <- ig$cutpoint[1]
opt_cutpoint
table(cutt(age, opt_cutpoint), patho)

# Histogram of age distribution ----
source("paper-ggplot-settings.R", echo = FALSE)
library(ggtext)

ggplot(df, aes(age)) +
  scale_x_continuous(breaks = c(setdiff(seq(20,80,20), -1), opt_cutpoint),
                     labels = c(setdiff(seq(20,80,20), -1), paste0("**", opt_cutpoint, "**")),
                     expand = c(0,0.5,0,0.5)) +
  geom_histogram(data = df %>% select(-pathology),
                 fill = "grey95", color = "grey90", size = 0.2, alpha = 0.5,
                 binwidth = 5, boundary = 0) +
  geom_histogram(color = "grey60", size = 0.2, alpha = 0.9, fill = "grey70",
                 binwidth = 5, boundary = 0) +
  geom_vline(xintercept = opt_cutpoint, size = 0.5, linetype = "solid") +
  facet_wrap(~ pathology, nrow = 1) +
  # scale_fill_manual(values = .fig_opts$colors_target[["pathology"]]) +
  labs(x = "Age", y = "Count", fill = NULL) +
  theme(legend.position = "top") +
  theme(strip.text = element_text(size = 8, face = "plain", margin = margin(0.5,0,0.5,0,"mm"))) +
  theme(strip.background = element_rect(color = NA, fill = "grey90")) +
  theme(panel.grid.major.x = element_blank()) +
  # theme(panel.spacing.y = unit(1, "mm")) +
  theme(axis.text.x = element_markdown()) +
  theme(axis.ticks.x = element_line()) +
  theme(axis.text.y = element_text(angle = 0)) +
  # theme(legend.key.size = unit(0.35, "cm")) +
  # theme(legend.box.margin = margin(0,0,-3,0,"mm")) +
  theme(plot.margin = margin(0.5,1,0.5,0.5,"mm"))

ggsave(filename = "Figures/histogram-age-distribution.pdf",
       width = 8, height = 3.25, units = "cm")
