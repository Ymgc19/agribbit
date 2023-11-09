
# agribbit

<!-- badges: start -->
<!-- badges: end -->

The goal of agribbit is to ...

## Installation

You can install the development version of agribbit like so:

``` r
# install.packages("devtools")
devtools::install_github("Ymgc19/agribbit")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(agribbit)
## basic example code
```

If you want to collet census data of Toyama Pref. and interpolate missing values, following code enable you to do it. I also show the example to draw maps with this package.
``` r
# 農林業センサスの富山県データをまとめて読み込む．
# 富山県の都道府県コードは16なので引数でそれを指定．
# データはestatから自動的にダウンロードされる．
# データは自動的に"16農林業センサス2020"というフォルダに保存される.
agribbit::agri.collect_census(16)

# 富山県データをまとめて読み込む.
# データが入っているフォルダを指定．
toyama <- agribbit::agri.read_as_csv("16農林業センサス2020")

# ガウス過程回帰による補完
# 一例として稲作が販売金額の8割以上を占める経営体の数を補完．
# warningがたくさん出るが無視してもOK（なはず）．
toyama_interpolated <- agribbit::agri.interpolate(
  toyama, "T001044002"
)
# 結果を簡易的に表示
toyama_interpolated$fit # ガウス過程回帰の結果
toyama_interpolated$inputed %>% glimpse() # 欠損を補完したデータフレームをチラ見
toyama_interpolated$predicted_summary # 埋め合わされた欠損値の要約統計量
toyama_interpolated$true.vs.predicted # 学習における実測値と予測値との対応図

# 補完したデータを新しくtoyamaとする．
toyama <- toyama_interpolated$inputed

# 富山県のshpを取得
# ここでも都道府県コードを引数にしたらestatからデータ取得できる．
agribbit::agri.collect_shp(16)
# shpの読み込み
toyama_shp <- read_sf("16農林業センサス2020_shp/agri202016.shp") %>% 
  mutate(KEY_CODE = as.numeric(KEY_CODE))

# 統計データとshpデータの結合
toyama <- agribbit::agri.join(toyama_shp, toyama)

# 簡単に地図描画
# 現在（2023/11/08）の仕様では，2個目の引数はdf$varという形にしないといけない．
agribbit::agri.sf_plot_continuous(toyama, toyama$inputed_T001044002)
```


