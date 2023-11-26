#' @title to look at cute frogs!!!
#' @description \code{agri.frog_rain}
#' @export

agri.frog_rain <- function(n = 100){
  for (i in 1:n){
    r <- runif(1, 0, 1)
    if (r >= 0.5) {
      cat("🐸", "\n")
    }
    if (r < 0.5 & r >= 0.1){
      cat("🥒", "\n")
    }
    if (r < 0.1){
      cat("☔️", "\n")
    }
  }
}
