# install.packages("devtools")
devtools::install_github("Ymgc19/agribbit")

library(agribbit)


ishikawa <- agribbit::agri.read_census(16)
ishikawa %>% glimpse()

# まとめてエンジニアリング．
ishikawa_inputed_fe <- ishikawa %>%
  agri.fe_census()
ishikawa_inputed_fe %>% glimpse()

# 補完
ishikawa_ip <- ishikawa_fe %>%
  agri.interpolate_fe_vars()

# 空間データ結合
shp <- agribbit::agri.read_census_shp(16)
df <- agri.join(
  shp, ishikawa_ip
)

df %>% agri.fast_draw(df$inputed_fe_mean_keiei_field)


# shpのobjを出力する関数
#' @title feature engineering
#' @description \code{agri.fe_census}
#' @export

# 特徴量エンジニアリング関数
agri.fe_census <- function(df){
  df <- df %>%
    mutate_all(~as.numeric(str_replace_all(., "-", "0"))) %>%
    mutate(
      # 法人化している農業経営体数
      fe_per_houjin = case_when(T001041001 != 0 ~ T001041002 / T001041001,
                                TRUE ~ 0
      ),

      # 農産物販売金額平均 分母が0の場合の処理．
      fe_mean_sell = case_when(T001042001 != 0 ~ (T001042002*0 + T001042003*25 + T001042004*75 + T001042005*200 +
                                                    T001042006*400 + T001042007*750 + T001042008*2000 + T001042009*4000 +
                                                    T001042010*7500 + T001042011*15000 + T001042012*25000 + T001042013*40000 +
                                                    T001042014*60000) / T001042001,
                               TRUE ~ 0
      ),

      # 農産物販売金額1位の部門別経営体数割合
      fe_per_rice_top = case_when(T001043001 != 0 ~ T001043002 / T001043001,
                                  TRUE ~ 0
      ),
      # 農業関連生産事業をおこなっている経営体の割合
      fe_per_kanren_jigyo = case_when(T001046001 != 0 ~ T001046003 / T001046001,
                                      TRUE ~ 0),
      # 農産物の販売をおこなった経営体の割合
      fe_per_hanbai = case_when(T001047001 != 0 ~ T001047003 / T001047001,
                                TRUE ~ 0),
      # 農産物の売上1位のものが農協である割合
      fe_per_noukyo = case_when(T001048001 != 0 ~ T001048002 / T001048001,
                                TRUE ~ 0),
      # 経営耕地のうち，田が占める割合
      fe_per_keiei_paddy = case_when(T001049002 != 0 ~ T001049006 / T001049002,
                                     TRUE ~ 0),
      # 各経営体の経営耕地面積の平均値
      fe_mean_keiei_field = case_when(T001050001 != 0 ~ (T001050002*0 + T001050003*0.1 + T001050004*0.4 + T001050005*0.7 +
                                                           T001050006*1.25 + T001050007*1.75 + T001050008*2 + T001050009*4 + T001050010*7.5 +
                                                           T001050011*15 + T001050012*25 + T001050013*40 + T001050014*75 + T001050015*125 +
                                                           T001050016*200) / T001050001,
                                      TRUE ~ 0),
      # 貸付耕地のある経営体割合
      fe_per_kahitsuke = case_when(T001052001 != 0 ~ T001052003 / T001052001,
                                   TRUE ~ 0),
      # 稲の作付経営体数
      fe_per_rice_keieitai = case_when(T001053001 != 0 ~ T001053003 / T001053001,
                                       TRUE ~ 0),
      # 稲の作付面積割合
      fe_per_rice_menseki = case_when(T001053002 != 0 ~ T001053004 / T001053002,
                                      TRUE ~ 0),
      # 耕地部門の作業を受託した経営体のうち，水稲作を受託した割合
      fe_per_rice_jitaku = case_when(T001055002 != 0 ~ T001055003 / T001055002,
                                     TRUE ~ 0),
      # 水稲受託作業種類別経営体すうと受託作業面積という変数について何かしたい．
      # 受託料金の平均
      fe_mean_jutaku = case_when(T001057001 != 0 ~ (T001057002*0 + T001057003*25 + T001057004*75 + T001057005*200 +
                                                      T001057006*400 + T001057007*750 + T001057008*2000 + T001057009*4000 +
                                                      T001057010*7500 + T001057011*15000 + T001057012*25000 + T001057013*40000 +
                                                      T001057014*60000) / T001057001,
                                 TRUE ~ 0),
      # 60日以上農業に従事した人で作る平均値，男女合計
      fe_mean_work_days = case_when(T001058001 != 0 ~ (T001058002*80 + T001058003*125 + T001058004*175 + T001058005*225 +
                                                         T001058006*275) / T001058001,
                                    TRUE ~ 0),
      # 常雇いの割合，計のべ人日に占める農業の述べ人日の割合
      fe_per_nobe_agri = case_when(T001059003 != 0 ~ T001059004 / T001059003,
                                   TRUE ~ 0),
      # 経営体の副業的の割合
      fe_per_fukugyo = case_when(T001061001 != 0 ~ T001061006 / T001061001,
                                 TRUE ~ 0),
      # 世帯員の男性割合 なぜか1を超えてくる
      fe_per_male = case_when(T001062001 != 0 ~ T001062002 / T001062001,
                              TRUE ~ 0),
      # 世帯員の平均年齢
      fe_mean_age = case_when(T001062010 != 0 ~ (T001062011*17 + T001062012*22 + T001062013*27 + T001062014*32 +
                                                   T001062015*37 + T001062016*42 + T001062017*47 + T001062018*52 +
                                                   T001062019*57 + T001062020*62 + T001062021*67 + T001062022*72 +
                                                   T001062023*77 + T001062024*82 + T001062025*87) / T001062010,
                              TRUE ~ 0),
      # 自営農業従事日数階層別の農業従事者数
      fe_mean_jiei_days = case_when(T001063001 != 0 ~ (T001063002*15 + T001063003*45 + T001063004*80 + T001063005*125 +
                                                         T001063006*175 + T001063007*225 + T001063008*275) / T001063001,
                                    TRUE ~ 0),
      # 総農家数のうち販売農家の割合
      fe_per_hanbai_nouka = case_when(T001065001 != 0 ~ T001065002 / T001065001,
                                      TRUE ~ 0),
      # 経営耕地のある農家数に占める販売農家の割合
      fe_per_hanbai_keieikouchi = case_when(T001066001 != 0 ~ T001066003 / T001066001,
                                            TRUE ~ 0),
      # 特定農山村地域
      fe_nousanson = T001068011,
      # 寄り合いスコア
      fe_yoriai = T001070002 + T001070003 + T001070004 + T001070005 + T001070006 + T001070007 +
        T001070008 + T001070009 + T001070010 + T001070011 + T001070012,
      # 実行組合バイナリ
      fe_jikkou = T001071002,
      # 保全スコア
      # まずは保全状況をそれぞれいい感じにする
      hozen_nouchi = case_when(
        T001072001 == 1 ~ 1,
        T001072002 == 1 ~ -1,
        T ~ 0
      ),
      hozen_shinrin = case_when(
        T001072004 == 1 ~ 1,
        T001072005 == 1 ~ -1,
        T ~ 0
      ),
      hozen_tameike = case_when(
        T001072007 == 1 ~ 1,
        T001072008 == 1 ~ -1,
        T ~ 0
      ),
      hozen_kasen = case_when(
        T001072010 == 1 ~ 1,
        T001072011 == 1 ~ -1,
        T ~ 0
      ),
      hozen_haisui = case_when(
        T001072013 == 1 ~ 1,
        T001072014 == 1 ~ -1,
        T ~ 0
      ),
      fe_hozen = hozen_nouchi + hozen_shinrin + hozen_tameike + hozen_kasen + hozen_haisui,
      # 取り組み状況
      fe_torikumi = T001073001 + T001073003 + T001073005 + T001073007 + T001073009 + T001073011 + T001073013
    )
}


