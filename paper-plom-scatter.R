# scatterplots for PLOM lower triangle entries
# meant to be called from make_plom()
plom_scatter <- function(df, fr, fc, target) {
  dfp <- tibble(
    fr = df[[fr]],
    fc = df[[fc]],
    target = df[[target]]
  ) %>%
    mutate(target = fct_expand(target, "all"))
  
  dfcor <-
    dfp %>%
    bind_rows(dfp %>% mutate(target = factor("all", levels = levels(dfp$target)))) %>%
    nest_by(target) %>%
    mutate(fit = list(lm(fr ~ fc, data = data))) %>%
    ungroup() %>%
    mutate(x_rng = map(data, ~ range(.x$fc))) %>%
    mutate(x_min = min(map_dbl(x_rng, 1))) %>%
    mutate(x_max = max(map_dbl(x_rng, 2))) %>%
    mutate(cor = map_dbl(data, ~cor(.x$fr, .x$fc, method = "spearman"))) %>%
    mutate(label_pos_x = x_max + 0.04 * (x_max - x_min)) %>%
    mutate(label_pos_y = map2_dbl(label_pos_x, fit, ~ predict(.y, newdata = tibble(fc = .x)))) %>%
    mutate(label_text = paste0("$\\rho = ", format(round(cor,2),2), "$"))
  
  ggplot(dfp, aes(x = fc, y = fr)) +
    scale_x_continuous(expand = c(0.02,0,0.02,0)) +
    geom_point(aes(color = target), size = 0.7) +
    geom_smooth(aes(color = target), method = "lm", formula = "y ~ x", se = FALSE,
                size = 0.5, fullrange = TRUE) +
    geom_smooth(method = "lm", formula = "y ~ x", color = "black", se = FALSE,
                size = 0.7, fullrange = TRUE) +
    geom_label_repel(data = dfcor, 
                    aes(x = label_pos_x, y = label_pos_y,
                        label = lapply(label_text, function(x){TeX(x, output = "character")}), 
                        color = target),
                    size = 8/.pt, hjust = 0, direction = "y", #xlim = max(dfcor$x_max, Inf),
                    fill = "white", alpha = 0.85,
                    label.size = 0, box.padding = 0, label.padding = 0.1,
                    force = 0.5, parse = TRUE) +
    scale_color_manual(values = c(.fig_opts$colors_target[[target]], "black"),
                       drop = FALSE) +
    guides(color = FALSE) +
    # labs(x = fc, y = fr)
    labs(x = NULL, y = NULL)
}

# plom_scatter(df, fr = f_grid$fr[[7]], fc = f_grid$fc[[7]], target = target)
