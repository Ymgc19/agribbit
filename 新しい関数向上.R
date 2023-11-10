# install.packages("devtools")
devtools::install_github("Ymgc19/agribbit")
library(agribbit)

# estatからtxtを取得して読み込み．そしてその後フォルダを削除．

agri.read_census <- function(pref_code, place){
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


miyagi <- agri.read_census(18)
miyagi %>% glimpse()




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










