# display units of measurements??
library(tidyverse)
library(latex2exp)
library(ggforce)
library(ggrepel)
library(patchwork)
library(ggpubr)
source("paper-ggplot-settings.R", echo = FALSE)
source("paper-plom-histogram.R", echo = FALSE)
source("paper-plom-scatter.R", echo = FALSE)
source("paper-plom-hulls.R", echo = FALSE)
source("paper-plom-rowcol.R", echo = FALSE)
df <- read_rds("Data/data-prep.rds")

# select classification task ----
task <- 3

# task 1: HHV vs. BAV patients
if(task == 1) {
  features <- c(
    "Time-to-Peak-Vorticity" = "maxVortexVolumeTime",
    "Time-to-Peak-In-Plane-Velocity" = "maxOverallCircumferentialVelocityTime",
    "Peak-Systolic-In-Plane-Mean-Velocity" = "systolicMaxMeancircumferentialVelocity"
  )
  target <- "pathology"
  rel_width_feature_name_subplots <- 0.1
  dim <- 9.5 # width and height of PLOM in cm
  caption <- "(a)"
}

# task 2: task 2: old HHV vs. BAV patients
if(task == 2) {
  features <- c(
    "Peak-Systolic-Mean-Velocity" = "maxMeanAxialVelocity",
    "Time-to-Peak-Systolic-Through-Plane-Mean-Velocity" = "systolicMaxMeanAxialVelocityTime",
    "Time-to-Peak-Diastolic-In-Plane-Mean-Velocity" = "diastolicMaxMeanCircumferentialVelocityTime",
    "Diastolic-Median-Right-Rotation-Volume-Rel" = "diastolicMedianRightRotationVolumeRel",
    "Peak-Mean-Vorticity-Pressure" = "maxMeanPressureInVortexRegion"
  )
  target <- "ohhv_bav"
  df <-
    df %>%
    filter(pathology == "BAV patients" | age > 47) %>%
    mutate(!!target := fct_recode(pathology, "Older heart-healthy volunteers" = "Heart-healthy volunteers"))
  rel_width_feature_name_subplots <- 0.06
  dim <- 15 # width and height of PLOM in cm
  caption <- NULL
}

# task 3: male vs. female HHV
if(task == 3) {
  features <- c(
    "Peak-Velocity" = "maxOverallVelocity",
    "Peak-Systolic-VelocityQ99" = "systolicMaxOverallVelocityQ99",
    "Time-to-Peak-Diastolic-Through-Plane-Velocity" = "diastolicMaxOverallAxialVelocityTime"
  )
  target <- "gender"
  df <- df %>%
    filter(pathology == "Heart-healthy volunteers") %>%
    mutate(!!target := factor(!!sym(target), 
                              labels = paste(c("Female", "Male"),
                                             "heart-healthy volunteers")))
  rel_width_feature_name_subplots <- 0.1
  dim <- 9.5 # width and height of PLOM in cm
  caption <- "(b)"
}

nam_feat <- which(names(features) != "")
if(length(nam_feat) > 0) {
  pos <- which(names(df) %in% features[nam_feat])
  names(df)[pos] <- names(features)[nam_feat]
  features[nam_feat] <- names(features[nam_feat])
  features <- unname(features)
}


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
      theme(legend.text = element_text(size = 6, margin = margin(r = 0.3, unit = "cm"))) +
      theme(legend.key.size = unit(4, "mm"))
  )
) #%>% print()

# patchwork ----
ww <- rel_width_feature_name_subplots # size of the feature name subplots
rr <- 0.04 # height of the legend
mm <- 0.05 # subplot margins (trbl) in cm
LETTERSletters <- c(LETTERS, letters)
p <- wrap_plots(c(list(legend), plot_list) %>% set_names(LETTERSletters[1:length(.)]),
                design = paste0(paste0(rep("A", length(features)+1), collapse = ""), "\n",
                                str_replace_all(paste0(LETTERSletters[1+(1:nrow(f_grid))], collapse = ""),
                                                paste0("(.{", length(features) + 1, "})"), "\\1\n")),
                byrow = TRUE,
                widths = c(ww, rep((1-ww)/length(features), length(features))),
                heights = c(rr, ww, rep((1-ww-rr)/length(features), length(features))),
) &
  theme(plot.margin = margin(mm, mm, mm, mm, "cm"))
if(!is.null(caption)) {
  p <- p +
    plot_annotation(
      title = caption,
      theme = theme(plot.title = element_text(size = 8, face = "bold",
                                              margin = margin(1,0,-3.5,0, "mm")))
    )
}

# p

ggsave(filename = here::here("Figures", "plom", paste0("plom-", target, ".pdf")),
       plot = p, width = dim, height = dim, units = "cm") 

# library(rpart)
# library(rpart.plot)
# dft <- df %>% select(one_of(features), target = one_of(target))
# tree <- rpart(target ~ ., data = dft, control = rpart.control(minbucket = 5, maxdepth = 4))
# rpart.plot(tree, type = 5, extra = 1)
