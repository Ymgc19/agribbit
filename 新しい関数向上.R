# install.packages("devtools")
devtools::install_github("Ymgc19/agribbit")
library(agribbit)

# estatからtxtを取得して読み込み．そしてその後フォルダを削除．

agri.read_census <- function(pref_code, place){
  agribbit::agri.collect_census(pref_code)
  download_dir <- paste(as.character(pref_code), "農林業センサス2020", sep = "")
  df <- agribbit::agri.read_as_csv(download_dir)
  file.remove(file.path(download_dir))
  return(df)
}


miyagi <- agri.read_census(4)
