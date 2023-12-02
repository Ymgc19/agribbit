# install.packages("devtools")
devtools::install_github("Ymgc19/agribbit", force = T)
library(agribbit)
library(summarytools)
library(tidyverse)

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
  agri.fe_inputed_census()

# feの結果をまとめて確認
a <- ishikawa_fe %>%
  filter(KEY_CODE%%1000 != 0) %>%
  dplyr::select(contains("fe_")) %>%
  rename_with(~gsub("fe_", "", .), starts_with("fe_")) %>%
  as.data.frame() %>%
  mutate_all(., scale)
view(dfSummary(a))

a$per_male %>% hist

library(corrplot)
ishikawa_cor <- cor(a)
col <- colorRampPalette(rev(c("tomato", "cyan", "blue")))
corrplot(ishikawa_cor, method="shade", shade.col=NA, tl.col="black", tl.srt=0,tl.cex = .3,cl.cex = 0.05,
         col=col(1000), addCoef.col="black", addcolorlabel="no", order="AOE",)



######################
##### 簡単な分析 #####
######################
library(spdep)
library(CARBayes)
ishikawa_fe <- ishikawa_fe %>%
  filter(KEY_CODE%%1000 != 0) %>%
  dplyr::select(contains(c("KEY_CODE", "fe_")))
ishikawa_fe %>% glimpse()

ishikawa_fe <- ishikawa_fe %>%
  agri.join(agri.read_census_shp(17), .)

ishikawa_fe %>% glimpse()
ishikawa_fe %>% agri.fast_draw(., ishikawa_fe$fe_nouka_sum)


# ndviデータを読み込む
ndvi <- read_csv("/Users/yamaguchiyuhei/Desktop/master-s_thesis_code/衛星統計データ/noto_ndvi_200609.csv")
ndvi %>% glimpse()

ishi_merged <- left_join(ndvi, ishikawa_fe,
                         by = "KEY_CODE") %>%
  st_as_sf()
ishi_merged %>% glimpse()

ishi_merged %>% agri.fast_draw(ishi_merged$fe_nouka_sum)

# 近接行列
ishi_for_model <- ishi_merged %>%
  dplyr::select(contains(c("fe_", "ndvi"))) %>%
  filter(fe_mean_age <= 100) %>%
  mutate_at(vars(-one_of(c("ndvi", "geometry"))), scale)

w_nb <- poly2nb(ishi_for_model)
W <- nb2mat(w_nb, style = "B")
formula <- ndvi ~ fe_yoriai + fe_hozen + fe_nousanson + fe_per_rice_top + fe_mean_work_days
formula <- ndvi ~ fe_yoriai + fe_hozen + fe_nousanson + fe_per_rice_top

#回帰式の実行
resLex <- S.CARleroux(formula, data = ishi_for_model, family = "gaussian",
                      W = W, burnin = 5000, n.sample = 20000)
resLex
ishi_for_model$fit <- resLex$fitted.values
ishi_for_model %>%
  ggplot()+
  geom_point()+
  geom_density2d()+
  geom_abline()+
  aes(fit, ndvi)+
  labs(x = "fitted ndvi", y = "true ndvi")+
  theme_minimal()



b <- resLex$samples$beta[,2] %>% as.data.frame()
c <- resLex$samples$beta[,3] %>% as.data.frame()
d <- resLex$samples$beta[,4] %>% as.data.frame()
e <- resLex$samples$beta[,5] %>% as.data.frame()

plot_df <- bind_rows(b, c, d, e)
plot_df$var <- NA
plot_df$var[1:15000] <- "community"
plot_df$var[15001:30000] <- "conservation"
plot_df$var[30001:45000] <- "rural"
plot_df$var[45001:60000] <- "rice sales"
plot_df %>% glimpse()

library(ggridges)
plot_df %>%
  ggplot()+
  geom_density_ridges()+
  aes(var1, var, fill = var)+
  scale_fill_manual(values = c("turquoise", "seagreen3", "royalblue", "steelblue2"))+
  theme_minimal()+
  labs(x = "", y = "")+
  theme(legend.position = "none")

n <- c(1:15000)

ggplot()+
  geom_line(linewidth = .3)+
  aes(n, resLex$samples$beta[,2], color = "")+
  labs(x = "", y = "")+
  scale_color_manual(values = "turquoise")+
  theme_minimal()+
  theme(legend.position = "none")
ggplot()+
  geom_line(linewidth = .3)+
  aes(n, resLex$samples$beta[,3], color = "")+
  labs(x = "", y = "")+
  scale_color_manual(values = "seagreen3")+
  theme_minimal()+
  theme(legend.position = "none")
ggplot()+
  geom_line(linewidth = .3)+
  aes(n, resLex$samples$beta[,4], color = "")+
  labs(x = "", y = "")+
  scale_color_manual(values = "steelblue2")+
  theme_minimal()+
  theme(legend.position = "none")
ggplot()+
  geom_line(linewidth = .3)+
  aes(n, resLex$samples$beta[,5], color = "")+
  labs(x = "", y = "")+
  scale_color_manual(values = "royalblue")+
  theme_minimal()+
  theme(legend.position = "none")





