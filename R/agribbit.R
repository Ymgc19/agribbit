#' @title to read files
#' @description \code{agri.read_as_csv}
#' @export

agri.read_as_csv <- function(dir_folder){
  library(tidyverse)
  fs::dir_ls(here::here(dir_folder),
             recurse = TRUE,
             regexp = ".txt$") %>%
    purrr::set_names(
      fs::dir_ls(here::here(dir_folder)) %>%
        basename() %>%
        stringr::str_remove("（.+）")
    ) %>%
    purrr::map(
      \(x) read_delim(x, delim = ",",
                      locale = locale(encoding = "cp932"))
    ) %>%
    reduce(left_join, by = "KEY_CODE") %>%
    dplyr::select(-contains("."))
}


#' @title to interpolate missing values
#' @description \code{agri.interpolate}
#' @export

agri.interpolate <- function(df, target, kernel = "rbfdot"){
  library(kernlab)
  library(tidyverse)

  # 説明変数の行列を作成
  indep <- df %>%
    dplyr::select(
      contains(c("KEY_CODE", "1039", "1065", "1067", "1068", "1069", "1070", "1071", "1072", "1073"))
    ) %>%
    mutate(KEY_CODE = as.numeric(KEY_CODE)) %>%
    filter(KEY_CODE%%1000 != 0) %>% # 農業集落になっているものだけを抽出
    mutate_all(~as.numeric(str_replace_all(., "-", "0"))) %>%
    mutate( # ちょっとした特徴量エンジニアリング
      hozen_sum = T001072001 + T001072004 + T001072007 + T001072010 + T001072013,
      yoriai_sum = T001070002 + T001070003 + T001070004 + T001070005 + T001070006 + T001070007 +
        T001070008 + T001070009 + T001070010 + T001070011 + T001070012,
      jissen_sum = T001073001 + T001073003 + T001073005 +  T001073007 + T001073009 + T001073011 + T001073013
    ) %>%
    dplyr::select( # 冗長な変数を落とす
      -T001072002, -T001072005, -T001072008, -T001072011, -T001072014, -T001071001, -T001071003,
      -T001070013, -T001067002
    )

  # 目的変数のベクトルを作成
  # データフレームからtargetとkeycodeを切り出す
  dep <- df[, target]
  key <- df[, "KEY_CODE"]
  dep <- dplyr::bind_cols(key, dep) # 目的変数とキーコードだけが入ったデータフレームが完成
  dep <- dep %>%
    mutate(KEY_CODE = as.numeric(KEY_CODE)) %>%
    filter(as.numeric(KEY_CODE)%%1000 != 0) %>%
    mutate_all(~as.numeric(str_replace_all(., "-", "0")))

  # ここまでに作成した2つのデータフレームを結合
  merged <- left_join(indep, dep, by = "KEY_CODE")

  # ここから学習用のデータフレームを作成
  indep_learn <- na.omit(merged) %>%
    dplyr::select(-contains("KEY"))
  # 正解データ
  dep_learn <- indep_learn[,target] %>%
    as.matrix()
  # 学習データ
  indep_learn <- indep_learn %>%
    dplyr::select(-target) %>%
    as.matrix()

  # ここから学習開始
  fit <- kernlab::gausspr(indep_learn, dep_learn, kernel = kernel, variance.model=T)


  # 真値と予測値のgeom_point
  true.vs.predicted <- ggplot()+
    geom_point()+
    aes(predict(fit, indep_learn), dep_learn)+
    geom_density_2d(size = .3)+
    geom_abline(intercept = 0)+
    labs(x = "predicted", y = "TRUE")+
    theme_minimal()

  # 欠損しているデータの説明変数行列を作成する
  indep <- df %>%
    dplyr::select(
      contains(c("KEY_CODE", "1039", "1065", "1067", "1068", "1069", "1070", "1071", "1072", "1073"))
    ) %>%
    mutate(KEY_CODE = as.numeric(KEY_CODE)) %>%
    filter(KEY_CODE%%1000 != 0) %>%
    # "-"を0に置換
    mutate_all(~as.numeric(str_replace_all(., "-", "0"))) %>%
    # hozen_sumとyoriai_sumとjissen_sumを作成
    mutate(
      hozen_sum = T001072001 + T001072004 + T001072007 + T001072010 + T001072013,
      yoriai_sum = T001070002 + T001070003 + T001070004 + T001070005 + T001070006 + T001070007 +
        T001070008 + T001070009 + T001070010 + T001070011 + T001070012,
      jissen_sum = T001073001 + T001073003 + T001073005 +  T001073007 + T001073009 + T001073011 + T001073013
    ) %>%
    dplyr::select(
      -T001072002, -T001072005, -T001072008, -T001072011, -T001072014, -T001071001, -T001071003,
      -T001070013, -T001067002
    )
  # 欠損のあるデータだけのデータフレームを作成
  key <- df[, "KEY_CODE"]
  dep <- df[, target]
  key_dep <- bind_cols(key, dep) %>%
    mutate(KEY_CODE = as.numeric(KEY_CODE)) %>%
    filter(as.numeric(KEY_CODE)%%1000 != 0) %>%
    mutate_all(~as.numeric(str_replace_all(., "-", "0")))

  # 説明変数indepとkey_depを結合させて，key_depがNAのものだけを抽出 → 説明変数行列に変換
  key_dep <- key_dep[is.na(key_dep[, 2]), ]
  indep_NA <- left_join(key_dep, indep, by = "KEY_CODE") %>%
    dplyr::select(-target, -KEY_CODE) %>%
    as.matrix()

  # indep_NAをモデルに適合
  # key_depの1列目，欠損データのキーコードを用いる
  predicted_vec <- predict(fit, indep_NA)
  key_predicted <- bind_cols(predicted_vec, key_dep[,1]) # ここでのメッセージを非表示にしたい
  # ここで生成されたkey_predictedの1行目が"...1"なので，名称を変更
  colnames(key_predicted)[1] <- paste("inputed", target, sep = "_")
  # 補完されたデータのsummaryをオブジェクトにして最後に出力
  predicted_summary <- summary(key_predicted[,1])

  #欠損していなかったデータのKEYと...1のデータフレームを作成
  not_miss <- df[, target]
  not_miss <- bind_cols(not_miss, key) %>%
    mutate_all(~as.numeric(str_replace_all(., "-", "0"))) %>%
    filter(KEY_CODE%%1000 != 0)
  not_miss <- not_miss[!is.na(not_miss[, 1]), ]
  colnames(not_miss)[1] <- paste("inputed", target, sep = "_")

  # データの結合
  ret_df <- bind_rows(not_miss, key_predicted) %>%
    # ここでソートするkeycodeで．
    arrange(KEY_CODE)
  ret_df <- ret_df %>%
    mutate(KEY_CODE = as.numeric(KEY_CODE))
  df <- df %>%
    mutate(KEY_CODE = as.numeric(KEY_CODE))
  ret_df <- left_join(df, ret_df, by = "KEY_CODE")
  return( list(inputed = ret_df, true.vs.predicted = true.vs.predicted,
               predicted_summary = predicted_summary, fit = fit) )
}



#' @title to interpolate missing values
#' @description \code{agri.join}
#' @export

agri.join <- function(shp, df){
  shp <- shp %>%
    mutate(KEY_CODE = as.numeric(KEY_CODE))
  return(
    left_join(shp, df, by = "KEY_CODE")
  )
}


#' @title to interpolate missing values
#' @description \code{agri.sf_plot_continuous}
#' @export

agri.sf_plot_continuous <- function(df, variable, xlab = "x", ylab = "y", fill = "", fill_low = "cyan", fill_high = "tomato"){
  df %>%
    ggplot()+
    geom_sf(color = NA)+
    aes(fill = variable)+
    scale_fill_gradient(low = fill_low, high = fill_high)+
    labs(x = xlab, y = ylab, fill = fill)+
    theme_minimal()
}




#' @title to interpolate missing values
#' @description \code{agri.collect_census}
#' @export
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
  shp <- read_sf(shp_place) %>%
    mutate(KEY_CODE = as.numeric(KEY_CODE))
  return(shp)
}

