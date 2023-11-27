# install.packages("devtools")
devtools::install_github("Ymgc19/agribbit", force = T)
library(agribbit)
library(summarytools)

# 先に欠損値を補完してからエンジニアリング
ishikawa <- agribbit::agri.read_census(17)
ishikawa_inputed <- ishikawa %>%
  agri.interpolate_importants()
# 小地域だけのデータを使う
ishikawa_inputed <- ishikawa_inputed %>%
  filter(KEY_CODE%%1000 != 0) %>%
  dplyr::select(contains("inputed"))

view(dfSummary(ishikawa_inputed))
