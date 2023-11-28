# install.packages("devtools")
devtools::install_github("Ymgc19/agribbit", force = T)
library(agribbit)
library(summarytools)

# 先に欠損値を補完してからエンジニアリング
ishikawa <- agribbit::agri.read_census(17)
ishikawa_inputed <- ishikawa %>%
  agri.interpolate_importants()

# 小地域だけのデータを使う
#ishikawa_inputed <- ishikawa_inputed %>%
#  filter(KEY_CODE%%1000 != 0) %>%
#  dplyr::select(contains("inputed"))
#view(dfSummary(ishikawa_inputed))

# 特徴量エンジニアリングまとめて
ishikawa_fe <- ishikawa_inputed %>%
  agribbit::agri.fe_inputed_census()

# feの結果をまとめて確認
a <- ishikawa_fe %>%
  filter(KEY_CODE%%1000 != 0) %>%
  dplyr::select(contains("fe_")) %>%
  rename_with(~gsub("fe_", "", .), starts_with("fe_")) %>%
  as.data.frame() %>%
  mutate_all(., scale)
view(dfSummary(a))

library(corrplot)
ishikawa_cor <- cor(a)
col <- colorRampPalette(rev(c("tomato", "cyan", "blue")))
corrplot(ishikawa_cor, method="shade", shade.col=NA, tl.col="black", tl.srt=30,tl.cex = .3,cl.cex = 0.05,
         col=col(1000), addCoef.col="black", addcolorlabel="no", order="AOE")





