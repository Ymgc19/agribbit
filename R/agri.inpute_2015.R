#' @title to interpolate missing values
#' @description \code{agri.interpolate}
#' @export

agri.inpute_2015 <- function(df, target, kernel = "rbfdot"){
  library(kernlab)
  library(tidyverse)
  df <- df %>% as.data.frame()

  # 説明変数の行列を作成
  indep <- df %>% # 欠損の少ないデータ
    dplyr::select(
      contains(c("KEY_CODE", "T000759001", "T000759007", "T000760001", "T000761001", "T000762001", "T000766001", "T000767001", "T000768001", "T000771001",
                 "T000788001", "T000807001", "T000807004", "T000808001", "T000808002", "T000813004", "T000813007", "T000814001", "T000815001", "T000818010",
                 "T000818012", "T000818014", "T000818016"))
    ) %>%
    mutate(KEY_CODE = as.numeric(KEY_CODE)) %>%
    filter(KEY_CODE%%1000 != 0) %>% # 農業集落になっているものだけを抽出
    mutate_all(~as.numeric(str_replace_all(., "-", "0"))) # - を 0 に置換

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
      contains(c("KEY_CODE", "T000759001", "T000759007", "T000760001", "T000761001", "T000762001", "T000766001", "T000767001", "T000768001", "T000771001",
                 "T000788001", "T000807001", "T000807004", "T000808001", "T000808002", "T000813004", "T000813007", "T000814001", "T000815001", "T000818010",
                 "T000818012", "T000818014", "T000818016"))
    ) %>%
    mutate(KEY_CODE = as.numeric(KEY_CODE)) %>%
    filter(KEY_CODE%%1000 != 0) %>%
    # "-"を0に置換
    mutate_all(~as.numeric(str_replace_all(., "-", "0")))

  key_dep <- df %>%
    dplyr::select(KEY_CODE, target) %>%
    mutate(KEY_CODE = as.numeric(KEY_CODE)) %>%
    filter(as.numeric(KEY_CODE)%%1000 != 0) %>%
    mutate_all(~as.numeric(str_replace_all(., "-", "0")))

  # 説明変数indepとkey_depを結合させて，key_depがNAのものだけを抽出 → 説明変数行列に変換
  key_dep <- key_dep[is.na(key_dep[, 2]), ] # 欠損しているデータのみを抜き出す
  # 目的変数が欠損していた地域の説明変数行列
  indep_NA <- left_join(key_dep, indep, by = "KEY_CODE") %>%
    dplyr::select(-target, -KEY_CODE) %>%
    as.matrix()

  # ここから予測
  predicted_vec <- predict(fit, indep_NA)
  key_predicted <- bind_cols(predicted_vec, key_dep[,1]) # 欠損していたデータのkeyと予測値のセット

  # ここで生成されたkey_predictedの1行目が"...1"なので，名称を変更
  colnames(key_predicted)[1] <- paste("inputed", target, sep = "_")
  colnames(key_predicted)[2] <- "KEY_CODE"
  # 補完されたデータのsummaryをオブジェクトにして最後に出力
  predicted_summary <- summary(key_predicted[,1])

  #欠損していなかったデータのKEYと...1のデータフレームを作成
  key <- df[, "KEY_CODE"]
  not_miss <- df[, target] # 欠損していないデータの目的変数
  not_miss <- bind_cols(not_miss, key) %>%
    mutate_all(~as.numeric(str_replace_all(., "-", "0")))
  not_miss <- not_miss[!is.na(not_miss[, 1]), ]# 欠損していない変数のkeyと目的変数のセット
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
