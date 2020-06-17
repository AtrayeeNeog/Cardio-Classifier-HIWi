# display units of measurements??
library(tidyverse)
library(latex2exp)
library(ggforce)
library(ggrepel)
library(patchwork)
library(ggpubr)
source("paper-plom-histogram.R", echo = FALSE)
source("paper-plom-scatter.R", echo = FALSE)
source("paper-plom-hulls.R", echo = FALSE)
source("paper-plom-rowcol.R", echo = FALSE)
df <- read_rds("Data/data-prep.rds")

# select features and target ----
# enhancement: automatically read result files instead of 
# manually typing variable names 
features <- c("diastolicMaxVortexVolumeTime",
              "maxMeancircumferentialVelocity",
              "systolicMaxMeanAxialVelocityTime")
target <- "pathology"

# general figure settings ----
.fig_opts <- list(
  base_size = 8,
  colors_target = list(
    pathology = c("steelblue3", "indianred3"),
    gender = NULL # !fix me
  )
)

if(!exists(".old_theme")) .old_theme <- theme_get()
theme_set(
  theme_minimal(base_size = .fig_opts$base_size, base_family = "sans",
                base_line_size = .fig_opts$base_size / 30,
                base_rect_size = .fig_opts$base_size / 30) +
    theme(panel.grid.minor = element_blank()) +
    theme(strip.text = element_text(face = "bold"))
)

# make_plom() ----
# (i=0) or (j=0) -> feature names
# (i=j) Histogram
# (i>j) Scatterplot + Linear regression + Spearman correlation
# (i<j) Convex hulls
make_plom <- function(id, row, col, fr, fc) {
  if(row == 0 & col == 0) return(plot_spacer())
  if(row == 0 & !(col == 0)) return(plom_rowcol(features[col], "x"))
  if(!(row == 0) & col == 0) return(plom_rowcol(features[row], "y"))
  if(row > col) return(plom_scatter(df, fr, fc, target))
  if(row < col) return(plom_hulls(df, fr, fc, target))
  plom_histogram(df, fr, target)
}

# create and fill plot grid ----
f_grid <- expand_grid(fr = features, fc = features) %>% 
  rowid_to_column("id") %>%
  mutate(row = (id - 1) %/% length(features) + 1, .after = "id") %>%
  mutate(col = (id - 1) %% length(features) + 1, .after = "row") %>%
  complete(row = 0:n_distinct(features), col = 0:n_distinct(features)) %>%
  print()
plot_list <- pmap(f_grid, make_plom)

# make shared legend ----
idx <- f_grid %>% rowid_to_column("rn") %>% filter(row == 1, col == 1) %>% pull(rn) 
legend <- as_ggplot(
  get_legend(
    plot_list[[idx]] + 
      guides(fill = guide_legend(NULL, direction = "horizontal", nrow = 1)) +
      theme(legend.text = element_text(size = rel(1), margin = margin(r = 0.3, unit = "cm")))
  )
) #%>% print()

# patchwork ----
ww <- 0.06 # size of the feature name subplots
rr <- 0.04 # height of the legend
mm <- 0.1 # subplot margins (trbl) in cm
p <- wrap_plots(c(list(legend), plot_list), 
                design = paste0(paste0(rep("A", length(features)+1), collapse = ""), "\n",
                                str_replace_all(paste0(LETTERS[1+(1:nrow(f_grid))], collapse = ""),
                                                paste0("(.{", length(features) + 1, "})"), "\\1\n")),
                byrow = TRUE,
                widths = c(ww, rep((1-ww)/length(features), length(features))),
                heights = c(rr, ww, rep((1-ww-rr)/length(features), length(features)))
) & theme(plot.margin = margin(mm, mm, mm, mm, "cm")) 
# p

ggsave(filename = here::here("Figures", "plom", paste0("plom-", target, ".pdf")),
       plot = p, width = 14, height = 13, units = "cm")
