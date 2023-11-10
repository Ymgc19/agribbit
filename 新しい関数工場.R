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
  indep <- df %>%
    dplyr::select(
      contains(c("KEY_CODE", "1039", "1065", "1067", "1068", "1069", "1070", "1071", "1072", "1073"))
    ) %>%
    mutate(KEY_CODE = as.numeric(KEY_CODE)) %>%
    filter(KEY_CODE%%1000 != 0) %>%
    mutate_all(~as.numeric(str_replace_all(., "-", "0"))) %>%
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

  dep <- df[,obj]
  key <- as.numeric(df[, "KEY_CODE"])
  dep <- bind_cols(key, dep)
  dep <- dep %>%
    filter(
      KEY_CODE%%1000 != 0
    ) %>%
    mutate_all(~as.numeric(str_replace_all(., "-", "0")))
  return(indep)
}


agri.interpolate(toyama) %>%
  glimpse()




