library(tidyverse)
# library(latex2exp)
# library(ggforce)
# library(ggrepel)
# library(patchwork)
# library(ggpubr)
df <- read_rds("Data/data-prep.rds")

df_cor <- df %>%
  select(-pathology) %>%
  mutate_if(is.factor, as.integer)

df_coef <- expand_grid(x = names(df_cor), y = names(df_cor)) %>%
  filter(x != y) %>%
  rowwise() %>%
  mutate(coef = cor(x = df_cor[[x]], y = df_cor[[y]], method = "spearman")) %>%
  ungroup()

clu <- df_coef %>%
  mutate(coef = 1 - coef) %>%
  complete(x, y, fill = list(coef = 0)) %>%
  pivot_wider(names_from = y, values_from = coef) %>%
  select(-1) %>%
  as.dist() %>%
  hclust(method = "complete") #method = "ward.D"

f_order <- levels(ggdendro::dendro_data(clu)$labels$label)

df_plot <- df_coef %>%
  mutate(across(c(x, y), ~ factor(.x, levels = f_order)))

ct <- cutree(clu, 5)
df_groups <- tibble(f_order = f_order) %>%
  inner_join(tibble(f = names(ct), cluster = ct), by = c("f_order" = "f")) %>%
  # add_count(cluster, name = "cluster_size") %>%
  # group_by(cluster) %>%
  # slice(c(1, n())) %>%
  # ungroup %>%
  # filter(cluster_size > 30) %>%
  mutate(f_order = factor(f_order, levels = f_order)) %>%
  group_by(cluster) %>%
  summarize(xmin = f_order[1], xmax = f_order[n()])

# manual
df_groups <- tribble(
  ~xmin, ~xmax,
  "maxOverallAxialVelocityQ99", "vortexCoverageRel",
  "diastolicMaxLeftRotationVolumeRel", "systolicMaxLeftRotationVolumeRel",
  "diastolicMaxMeanAxialVelocity", "systolicMaxOverallVelocityTime",
  "systolicMaxMeanPressureInVortexRegion", "minFlowJetHighVelocityAreaPercentVelocityWeighted",
  "age", "medianDiameter"
)

ggplot(df_plot, aes(x, y, fill = coef)) +
    coord_equal() +
    geom_raster() +
  geom_rect(data = df_groups, aes(xmin = xmin, ymin=xmin, xmax=xmax, ymax =xmax),
            inherit.aes = FALSE, fill = NA, color = "black",
            size = 0.4) +
    labs(fill = "Feature-feature\ncorrelation") +
    scale_fill_distiller(palette = "BrBG", 
                         limits = c(-1,1),
                         breaks = seq(-1, 1, 0.5)) +
  guides(fill = guide_colorbar(ticks.colour = "black", nbin = 100)) +
  theme(axis.title = element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.line = element_blank()) +
  theme(legend.position = "top") +
  theme(legend.key.width = unit(0.7, "cm")) +
  theme(legend.key.height = unit(0.3, "cm")) +
  # theme(legend.title =  = margin(0,0,0,1, "lines")) +
  theme(legend.title = element_text(size = 7, vjust = 0.75, margin = margin(0,0.5,0,0,"lines"))) +
  theme(legend.text = element_text(size = 6)) +
  # theme(plot.margin = margin(-1,0,-1,0,"lines")) +
  theme(legend.margin = margin(0,0,-2,0, "mm"))

# ggsave(filename = "Figures/corplot.pdf", width = 50, height = 50, units = "cm")
ggsave(filename = "Figures/corplot.pdf", width = 7.2, height = 8, units = "cm")
