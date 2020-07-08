# plots that only contain feature names 
# shown left to or above the plom
# meant to be called from make_plom()
plom_rowcol <- function(f, what = c("x", "y")[1], max_length = 21) {
  angle <- c("x" = 0, "y" = 90)[[what]]
  label <- f %>%
    str_replace_all("([A-Z])", " \\1") %>%
    str_wrap(max_length) %>%
    str_replace_all("\n", "-\n") %>%
    str_remove_all(" ")
  # if(nchar(label) > max_length) label <- paste0(str_sub(label, 1, max_length), "...")
  ggplot(tibble(label = label)) +
    geom_text(aes(x = 0, y = 0, label = label),
              size = 6/.pt, lineheight = 0.8,
              fontface = "bold", angle = angle) +
    theme_void() +
    theme(panel.background = element_rect(color = NA, fill = "grey90"))
}