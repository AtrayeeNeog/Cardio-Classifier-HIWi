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
  scale_x_continuous(breaks = c(seq(20,80,10), opt_cutpoint),
                     labels = c(seq(20,80,10), paste0("**", opt_cutpoint, "**")),
                     expand = c(0,1,0,1)) +
  geom_histogram(data = df %>% select(-pathology),
                 fill = "grey90", color = "grey90", size = 0.2, alpha = 0.5,
                 binwidth = 5, boundary = opt_cutpoint) +
  geom_histogram(aes(fill = pathology), color = "grey90", size = 0.2, alpha = 0.9,
                 binwidth = 5, boundary = opt_cutpoint) +
  geom_vline(xintercept = opt_cutpoint, size = 0.5, linetype = "solid") +
  facet_wrap(~ pathology, ncol = 1) +
  scale_fill_manual(values = .fig_opts$colors_target[["pathology"]]) +
  labs(x = "Age", y = "Count", fill = NULL) +
  theme(legend.position = "top") +
  theme(strip.text = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.spacing.y = unit(0, "mm")) +
  theme(axis.text.x = element_markdown()) +
  theme(axis.ticks.x = element_line()) +
  theme(legend.key.size = unit(0.35, "cm"))

ggsave(filename = "Figures/histogram-age-distribution.pdf",
       width = 8, height = 4, units = "cm")
