# general figure settings ----
.fig_opts <- list(
  base_size = 7,
  colors_target = list(
    pathology = c("steelblue3", "indianred3"),
    gender = c("#B73377", "#2DA9D9"),
    ohhv_bav = c("#6D90AF", #"grey50"
                 "indianred3")
  )
)

if(!exists(".old_theme")) .old_theme <- theme_get()
theme_set(
  theme_minimal(base_size = .fig_opts$base_size, base_family = "sans",
                base_line_size = .fig_opts$base_size / 50,
                base_rect_size = .fig_opts$base_size / 50) +
    theme(panel.grid.minor = element_blank()) +
    theme(strip.text = element_text(face = "bold")) +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
    theme(axis.ticks = element_line())
)