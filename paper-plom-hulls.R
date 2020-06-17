# convex hulls for PLOM upper triangle entries
# meant to be called from make_plom()
plom_hulls <- function(df, fr, fc, target) {
  dfp <- tibble(
    fr = df[[fr]],
    fc = df[[fc]],
    target = df$pathology
  )
  
  ggplot(dfp, aes(x = fc, y = fr)) +
    geom_mark_hull(aes(fill = target), expand = 0.04,
                   concavity = 5, alpha = 0.2,
                   size = 0.3) +
    geom_point(aes(color = target), size = 0.7) +
    scale_color_manual(values = .fig_opts$colors_target[[target]]) +
    scale_fill_manual(values = .fig_opts$colors_target[[target]]) +
    guides(color = FALSE, fill = FALSE) +
    # labs(x = fc, y = fr)
    labs(x = NULL, y = NULL)
}