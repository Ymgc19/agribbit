# shpのobjを出力する関数
#' @title to look at cute frogs!!!
#' @description \code{agri.read_census_shp}
#' @export
agri.read_census_shp <- function(pref_code){
  agribbit::agri.collect_shp(pref_code)
  if (pref_code <= 9){
    pref_code <- as.character(paste("0", pref_code, sep = ""))
  }
  else{
    pref_code <- pref_code
  }
  folder_name <- paste(pref_code, "農林業センサス2020_shp", sep = "")
  file_name <- paste("agri2020", pref_code, ".shp", sep = "")
  shp_place <- paste(folder_name, file_name, sep = "/")
  shp <- read_sf(shp_place)
  shp <- shp %>%
    mutate(KEY_CODE = as.numeric(KEY_CODE))
  return(shp)
}
