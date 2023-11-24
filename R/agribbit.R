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
  df <- df %>% as.data.frame()
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

  dep <- df %>%
    dplyr::select(KEY_CODE, target) %>%
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
  key_dep <- df %>%
    dplyr::select(KEY_CODE, target) %>%
    mutate(KEY_CODE = as.numeric(KEY_CODE)) %>%
    filter(as.numeric(KEY_CODE)%%1000 != 0) %>%
    mutate_all(~as.numeric(str_replace_all(., "-", "0")))

  # 説明変数indepとkey_depを結合させて，key_depがNAのものだけを抽出 → 説明変数行列に変換
  key_dep <- key_dep[is.na(key_dep[, 2]), ]
  # 目的変数が欠損していた地域の説明変数行列
  indep_NA <- left_join(key_dep, indep, by = "KEY_CODE") %>%
    dplyr::select(-target, -KEY_CODE) %>%
    as.matrix()

  # ここから予測
  predicted_vec <- predict(fit, indep_NA)
  key_predicted <- bind_cols(predicted_vec, key_dep[,1]) # ここでのメッセージを非表示にしたい
  # ここで生成されたkey_predictedの1行目が"...1"なので，名称を変更
  colnames(key_predicted)[1] <- paste("inputed", target, sep = "_")
  colnames(key_predicted)[2] <- "KEY_CODE"
  # 補完されたデータのsummaryをオブジェクトにして最後に出力
  predicted_summary <- summary(key_predicted[,1])

  #欠損していなかったデータのKEYと...1のデータフレームを作成
  key <- df[, "KEY_CODE"]
  not_miss <- df[, target]
  not_miss <- bind_cols(not_miss, key) %>%
    mutate_all(~as.numeric(str_replace_all(., "-", "0")))
  not_miss <- not_miss[!is.na(not_miss[, 1]), ]
  colnames(not_miss)[1] <- paste("inputed", target, sep = "_")
  colnames(not_miss)[2] <- "KEY_CODE"

  # 欠損していたdfと欠損していなかったdfを結合．
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
  df <- df %>%
    mutate(KEY_CODE = as.numeric(KEY_CODE))
  return(
    left_join(shp, df, by = "KEY_CODE")
  )
}


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
  shp <- read_sf(shp_place)
  shp <- shp %>%
    mutate(KEY_CODE = as.numeric(KEY_CODE))
  return(shp)
}






# 特徴量エンジニアリング関数
agri.fe_houjin <- function(df){
  df <- df %>%
  mutate(
    # 法人化している農業経営体数
    fe_per_houjin = inputed_T001041002 / inputed_T001041001,
    # 農産物販売金額平均
    fe_mean_sell = (inputed_T001042002*0 + inputed_T001042003*25 + inputed_T001042004*75 + inputed_T001042005*200 +
                      inputed_T001042006*400 + inputed_T001042007*750 + inputed_T001042008*2000 + inputed_T001042009*4000 +
                      inputed_T001042010*7500 + inputed_T001042011*15000 + inputed_T001042012*25000 + inputed_T001042013*40000 +
                      inputed_T001042014*60000) / inputed_T001042001,
    # 農産物販売金額1位の部門別経営体数割合
    fe_per_rice_top = inputed_T001043002 / inputed_T001043001,
    # 農業関連生産事業をおこなっている経営体の割合
    fe_per_kanren_jigyo = inputed_T001046003 / inputed_T001046001,
    # 農産物の販売をおこなった経営体の割合
    fe_per_hanbai = inputed_T001047003 / inputed_T001047001,
    # 農産物の売上1位のものが農協である割合
    fe_per_noukyo = inputed_T001048002 / inputed_T001048001,
    # 経営耕地のうち，田が占める割合
    fe_per_keiei_paddy = inputed_T001049002 / inputed_T001049006,
    # 各経営体の経営耕地面積の平均値
    fe_mean_keiei_field = (inputed_T001050002*0 + inputed_T001050003*0.1 + inputed_T001050004*0.4 + inputed_T001050005*0.7 +
      inputed_T001050006*1.25 + inputed_T001050007*1.75 + inputed_T001050008*2 + inputed_T001050009*4 + inputed_T001050010*7.5 +
      inputed_T001050011*15 + inputed_T001050012*25 + inputed_T001050013*40 + inputed_T001050014*75 + inputed_T001050015*125 +
      inputed_T001050016*200) / inputed_T001050001,
    # 貸付耕地のある経営体割合
    fe_per_kahitsuke = inputed_T001052003 / inputed_T001052001,
    # 稲の作付経営体数
    fe_per_rice_keieitai = inputed_T001053003 / inputed_T001053001,
    # 稲の作付面積割合
    fe_per_rice_menseki = inputed_T001053004 / inputed_T001053002,
    # 耕地部門の作業を受託した経営体のうち，水稲作を受託した割合
    fe_per_rice_jitaku = inputed_T001055003 / inputed_T001055002,
    # 水稲受託作業種類別経営体すうと受託作業面積という変数について何かしたい．

    # 受託料金の平均
    fe_mean_jutaku = (inputed_T001057002*0 + inputed_T001057003*25 + inputed_T001057004*75 + inputed_T001057005*200 +
      inputed_T001057006*400 + inputed_T001057007*750 + inputed_T001057008*2000 + inputed_T001057009*4000 +
      inputed_T001057010*7500 + inputed_T001057011*15000 + inputed_T001057012*25000 + inputed_T001057013*40000 +
      inputed_T001057014*60000) / inputed_T001057001,
    # 60日以上農業に従事した人で作る平均値，男女合計
    fe_mean_work_days = inputed_T001058002*80 + inputed_T001056003*125 + inputed_T001056004*175 + inputed_T001056005*225 + inputed_T001056006*275,
    # 常雇いの割合，計のべ人日に占める農業の述べ人日の割合
    fe_per_nobe_agri = inputed_T001059004 / inputed_T001059003,
    #
  )
}

