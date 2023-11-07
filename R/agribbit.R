#' @title to read files
#' @description \code{agri.read_csv}
#' @export

agri.read_csv <- function(dir_folder){
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
  dplyr::bind_cols() %>%
  mutate(KEY_CODE = KEY_CODE...1) %>%
  dplyr::select(-contains("."))
}


#' @title to interpolate missing values
#' @description \code{agri.interpolate}
#' @export

agri.interpolate <- function(df, obj, kernel = "rbfdot"){
  library(tidyverse)
  # まずは農林業センサスのデータを整形．
  # 説明変数のデータのmatrixを調整
  indep <- df %>%
    dplyr::select(
      contains(c("KEY_CODE", "1039", "1065", "1067", "1068", "1069", "1070", "1071", "1072", "1073"))
    ) %>%
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

  # ここから目的変数ベクトルの作成
  dep <- df[,obj]
  key <- df[, "KEY_CODE"]
  dep <- bind_cols(key, dep)
  dep <- dep %>%
    filter(
      KEY_CODE%%1000 != 0
    ) %>%
    mutate_all(~as.numeric(str_replace_all(., "-", "0")))

  # 被説明変数と説明変数のベクトルを結合させる
  merged <- bind_cols(dep, indep)

  # 学習用に欠損のない行だけを取り出す
  indep_learn <- na.omit(merged) %>%
    dplyr::select(-contains("KEY"))
  # 正解データ
  dep_learn <- indep_learn[,obj] %>%
    as.matrix()
  # 学習データ
  indep_learn <- indep_learn %>%
    dplyr::select(-obj) %>%
    as.matrix()
  # ここまでで学習用のデータセットができた．

  # ここから学習
  fit <- kernlab::gausspr(indep_learn, dep_learn, kernel = kernel, variance.model=T)
  fit

  # 真値と予測値のgeom_point
  true.vs.predicted <- ggplot()+
    geom_point()+
    aes(predict(fit, indep_learn), dep_learn)+
    geom_density_2d(bins = 30, size = .2)+
    geom_abline(intercept = 0)+
    labs(x = "predicted", y = "TRUE")+
    theme_minimal()

  # 欠損しているデータの説明変数行列を作成する
  indep <- df %>%
    dplyr::select(
      contains(c("KEY_CODE", "1039", "1065", "1067", "1068", "1069", "1070", "1071", "1072", "1073"))
    ) %>%
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
  dep <- df[, obj]
  key_dep <- bind_cols(key, dep) %>%
    mutate_all(~as.numeric(str_replace_all(., "-", "0"))) %>%
    filter(KEY_CODE%%1000 != 0)
  # データフレームの2列目を削除
  key_dep <- key_dep[is.na(key_dep[, 2]), ]
  # indepとkey_depを結合．欠損していたデータだけのDFを作成→matrix
  target <- left_join(key_dep, indep, by = "KEY_CODE") %>%
    dplyr::select(-1, -2) %>%
    as.matrix()
  # 予測値の出力
  predicted_vec <- predict(fit, target)
  key_predicted <- bind_cols(predicted_vec, key_dep)
  colnames(key_predicted)[1] <- paste("inputed", obj, sep = "_")
  key_predicted <- key_predicted %>%
    dplyr::select(1, 2)
  # 予測データのヒストを作成
  predicted_summary <- summary(key_predicted[,1])

  #欠損していなかったデータのKEYと...1のデータフレームを作成
  not_miss <- df[, obj]
  not_miss <- bind_cols(not_miss, key) %>%
    mutate_all(~as.numeric(str_replace_all(., "-", "0"))) %>%
    filter(KEY_CODE%%1000 != 0)
  not_miss <- not_miss[!is.na(not_miss[, 1]), ]
  colnames(not_miss)[1] <- paste("inputed", obj, sep = "_")

  # データの結合
  ret_df <- bind_rows(not_miss, key_predicted) %>%
    # ここでソートするkeycodeで
    arrange(KEY_CODE)

  return( list(inputed = ret_df, true.vs.predicted = true.vs.predicted,
               predicted_summary = predicted_summary) )
}

