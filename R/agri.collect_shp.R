#' @title to collect shape files from internet
#' @description \code{agri.collect_shp}
#' @export

agri.collect_shp <- function(pref_code) {
  library(sf)
  url1 <- "https://www.e-stat.go.jp/gis/statmap-search/data?dlserveyId=A005002092007&code="
  if (pref_code <= 9){
    pref_code <- as.character(paste("0", pref_code, sep = ""))
  }
  url2 <- "&coordSys=2&format=shape&downloadType=5&datum=2011"
  url <- paste(url1, pref_code, url2, sep = "")
  # フォルダ名の作成
  folder_name <- paste(pref_code, "農林業センサス2020_shp", sep = "")
  dir.create(folder_name, showWarnings = FALSE)
  # ZIPファイルをダウンロードし、解凍
  zip_file <- file.path(folder_name, "shapefile.zip")
  download.file(url, destfile = zip_file, mode = "wb") # 'wb'モードでバイナリファイルをダウンロード
  unzip(zip_file, exdir = folder_name)
  # ZIPファイルを削除
  file.remove(zip_file)
}

