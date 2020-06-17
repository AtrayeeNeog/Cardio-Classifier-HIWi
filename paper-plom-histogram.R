# histogram for PLOM main diagonal entries
# meant to be called from make_plom()
plom_histogram <- function(df, f, target) {
  dfp <- df %>%
    select(any_of(f), any_of(target)) %>%
    rename(f = 1, target = 2)
  ggplot(dfp, aes(x = f)) +
    geom_histogram(aes(fill = target), bins = 12, color = "black", 
                   size = 0.3, alpha = 0.75) +
    facet_wrap(~ target, ncol = 1) +
    scale_fill_manual(values = .fig_opts$colors_target[[target]]) +
    guides(fill = FALSE) +
    labs(x = NULL, y = NULL) +
    # labs(x = f, y = "Count") +
    theme(strip.text = element_blank())
}