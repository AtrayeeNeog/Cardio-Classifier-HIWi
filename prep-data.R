library(tidyverse)

df <- read_csv("Data/data.csv") %>%
  filter(pathology %in% c(1,2)) %>%
  mutate(pathology = factor(pathology, 
                            levels = c(1,2), 
                            labels = c("Heart-healthy volunteers", 
                                       "BAV patients"))) %>%
  mutate(gender = factor(gender, 
                         levels = c(2,1), 
                         labels = c("Female", "Male")))

write_rds(df, "Data/data-prep.rds")

