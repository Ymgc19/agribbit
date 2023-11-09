
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
# 農林業センサスの宮城県データをまとめて読み込む．
agribbit::agri.collect_census(16)

# 富山県データをまとめて読み込む
toyama <- agribbit::agri.read_as_csv("16農林業センサス2020")

# ガウス過程回帰による補完
# 稲作が販売金額の8割以上を占める経営体の数を補完
toyama_interpolated <- agribbit::agri.interpolate(
  toyama, "T001044002"
)
# 結果を簡易的に表示
toyama_interpolated$fit
toyama_interpolated$inputed %>% glimpse()
toyama_interpolated$predicted_summary
toyama_interpolated$true.vs.predicted

# 補完したデータをtoyamaとする．
toyama <- toyama_interpolated$inputed

# 富山県のshpを取得
agribbit::agri.collect_shp(16)
# shpの読み込み
toyama_shp <- read_sf("16農林業センサス2020_shp/agri202016.shp") %>% 
  mutate(KEY_CODE = as.numeric(KEY_CODE))

# 統計データとshpデータの結合
toyama <- agribbit::agri.join(toyama_shp, toyama)

# 簡単に地図描画
agribbit::agri.sf_plot_continuous(toyama, toyama$inputed_T001044002)
```


