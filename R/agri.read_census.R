# 農林業センサスのデータを読み込んでそのままオブジェクト化
#' @title read census data as object
#' @description \code{agri.read_census}
#' @export

agri.read_census <- function(pref_code){
  agribbit::agri.collect_census(pref_code)
  if (pref_code <= 9){
    pref_code <- as.character(paste("0", pref_code, sep = ""))
  }
  else{
    pref_code <- pref_code
  }
  download_dir <- paste(as.character(pref_code), "農林業センサス2020", sep = "")
  df <- agribbit::agri.read_as_csv(download_dir)
  unlink(download_dir, recursive = T)
  return(df)
}

