#' @title to interpolate missing values
#' @description \code{agri.fast_draw}
#' @export

agri.fast_draw <- function(df, variable, xlab = "x", ylab = "y", fill = "", fill_low = "cyan", fill_high = "tomato"){
  variable = ifelse(variable == "-", "0", variable)
  df %>%
    ggplot()+
    geom_sf(color = NA)+
    aes(fill = as.numeric(variable))+
    scale_fill_gradient(low = fill_low, high = fill_high)+
    labs(x = xlab, y = ylab, fill = fill)+
    theme_minimal()
}
