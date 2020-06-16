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

df <- read_csv("data/200606_data.csv") %>% filter(pathology %in% c(1,2))
age <- df$age
patho <- factor(df$pathology, labels = c("healthy", "BAV"))

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
