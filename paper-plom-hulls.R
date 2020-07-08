# convex hulls for PLOM upper triangle entries
# meant to be called from make_plom()
plom_hulls <- function(df, fr, fc, target) {
  dfp <- tibble(
    fr = df[[fr]],
    fc = df[[fc]],
    target = df[[target]]
  )
  
  ggplot(dfp, aes(x = fc, y = fr)) +
    scale_x_continuous(expand = c(0.1,0)) +
    scale_y_continuous(expand = c(0.1,0)) +
    geom_mark_hull(aes(fill = target), expand = 0.04,
                   radius = unit(1, "mm"),
                   concavity = 5,
                   alpha = 0.2,
                   size = 0.2) +
    geom_point(aes(color = target), size = 0.75, pch = 16, alpha = 0.5) +
    # geom_jitter(aes(color = target), size = 1, pch = 16, alpha = 0.75,
    #             width = 0.01, height = 0.01) +
    scale_color_manual(values = .fig_opts$colors_target[[target]]) +
    scale_fill_manual(values = .fig_opts$colors_target[[target]]) +
    guides(color = FALSE, fill = FALSE) +
    # labs(x = fc, y = fr)
    labs(x = NULL, y = NULL)
}