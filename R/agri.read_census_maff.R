#' @title to interpolate missing values
#' @description \code{agri.read_census_maff}
#' @export

# エクセルファイルをダウンロードして読み込む関数
agri.read_census_maff <- function(pref_code) {
  # codeの整形
  if (pref_code <= 9){
    pref_code <- as.character(paste("0", pref_code, sep = ""))
  }
  else{
    pref_code <- pref_code
  }

  # ダウンロードするディレクトリを作成
  download_dir <- paste(as.character(pref_code), "農林業センサス2020", sep = "")
  if (!file.exists(download_dir)) {
    dir.create(download_dir)
  }

  # maffからのcensusの読み込み
  library(readxl)
  url_list <- c(
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA0001_2020_2020_", #1
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA0002_2020_2020_", #2
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA1009_2020_2020_", #3
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA1011_2020_2020_", #4
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA1012_2020_2020_", #5
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA1014_2020_2020_", #6
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA1019_2020_2020_", #7
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA1024_2020_2020_", #8
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA1025_2020_2020_", #9
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA1026_2020_2020_", #10
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA1037_2020_2020_", #11
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA1040_2020_2020_", #12
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA1055_2020_2020_", #13
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA1059_2020_2020_", #14
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA1062_2020_2020_", #15
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA1065_2020_2020_", #16
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA1066_2020_2020_", #17
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA1090_2020_2020_", #18
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA1107_2020_2020_", #19
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA1111_2020_2020_", #20
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA1112_2020_2020_", #21
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA1113_2020_2020_", #22
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA1114_2020_2020_", #23
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA1115_2020_2020_", #24
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA4001_2020_2020_", #25
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA4002_2020_2020_", #26
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA4003_2020_2020_", #27
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA4004_2020_2020_", #28
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA5194_2020_2020_", #29
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA5200_2020_2020_", #30
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA7001_2020_2020_", #31
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA7002_2020_2020_", #32
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA7003_2020_2020_", #33
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA7007_2020_2020_", #34
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA7008_2020_2020_", #35
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA7004_2020_2020_", #36
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA7005_2020_2020_", #37
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA7009_2020_2020_", #38
    "https://www.machimura.maff.go.jp/shurakudata/2020/sa/SA7006_2020_2020_" #39
  )
  url_last <- ".xlsx"
  dfs <- list()
  # for文でまとめて読み込み
  for (i in 1:39){
    # urlの生成
    url <- paste0(url_list[i], as.character(pref_code), url_last)
    temp_file <- tempfile(fileext = ".xlsx")
    download.file(url, destfile = temp_file, mode = "wb")
    df <- read_excel(temp_file)
    unlink(temp_file)
    dfs[[i]] <- df
  }
  final_df <- left_join(dfs)
  return(final_df)
}

