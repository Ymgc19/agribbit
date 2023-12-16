#' @title to interpolate missing values
#' @description \code{agri.collect_census}

agri.collect_census <- function(pref_code){
  library(utils)
  if (pref_code <= 9){
    pref_code <- as.character(paste("0", pref_code, sep = ""))
  }
  url1 <- "https://www.e-stat.go.jp/gis/statmap-search/data?statsId=T0010"
  url2 <- "&code="
  pref_code <- as.character(pref_code)  # pref_number を文字列に変換する必要があります
  url3 <- "&downloadType=2"

  # ディレクトリを作成
  download_dir <- paste(as.character(pref_code), "農林業センサス2020", sep = "")
  if (!file.exists(download_dir)) {
    dir.create(download_dir)
  }
  # 指定された都道府県のデータをfor文でdownload
  zip_url <- c()  # zip_url ベクトルを初期化
  for (i in 1:35){
    num <- i + 38
    url4 <- paste0(url1, as.character(num), url2, pref_code, url3)  # url を正しく生成
    zip_url <- c(zip_url, url4)  # zip_url ベクトルに追加
  }

  # for文でデータを全て読み込む
  for (url in zip_url) {
    filename <- basename(url)
    download.file(url, destfile = file.path(download_dir, filename), mode = "wb")
    unzip(file.path(download_dir, filename), exdir = download_dir)
    txt_files <- list.files(download_dir, pattern = ".txt", full.names = TRUE)
    file.remove(file.path(download_dir, filename))
  }
  return(download_dir)
}
