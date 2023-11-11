# install.packages("devtools")
devtools::install_github("Ymgc19/agribbit")
library(agribbit)

# estatからtxtを取得して読み込み．そしてその後フォルダを削除．


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
#  file.remove(file.path(download_dir)) # このファイルがemptyではないのでerrorが出てくる．
  return(df)
}

hokkaido <- agri.read_census(1)




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


agri.read_as_csv("04農林業センサス2020")


toyama <- agribbit::agri.read_census(16)




# interpolationを改稿
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


