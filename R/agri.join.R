#' @title to interpolate missing values
#' @description \code{agri.join}
#' @export

agri.join <- function(shp, df){
  shp <- shp %>%
    mutate(KEY_CODE = as.numeric(KEY_CODE))
  df <- df %>%
    mutate(KEY_CODE = as.numeric(KEY_CODE))
  return(
    left_join(shp, df, by = "KEY_CODE")
  )
}
