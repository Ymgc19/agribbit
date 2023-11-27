#' @title feature engineering
#' @description \code{agri.fe_inputed_census}
#' @export

# 特徴量エンジニアリング関数
# roundをかませた方がいい
# 便宜的に1を超えた値は1にする．マイナスも0にする．
agri.fe_inputed_census <- function(df){
  df <- df %>%
    mutate_all(~as.numeric(str_replace_all(., "-", "0"))) %>%
    mutate_all(., round) %>%
    mutate(
      # 法人化している農業経営体数
      fe_per_houjin = case_when(inputed_T001041001 != 0 ~ inputed_T001041002 / inputed_T001041001,
                                inputed_T001041001 == 0 ~ 0,
                                TRUE ~ NA
      ),
      fe_per_houjin = case_when(fe_per_houjin >= 1 ~ 1,
                                fe_per_houjin <= 0 ~ 0,
                                TRUE ~ fe_per_houjin),
      # 農産物販売金額平均 分母が0の場合の処理．
      fe_mean_sell = case_when(inputed_T001042001 != 0 ~ (inputed_T001042002*0 + inputed_T001042003*25 + inputed_T001042004*75 + inputed_T001042005*200 +
                                                            inputed_T001042006*400 + inputed_T001042007*750 + inputed_T001042008*2000 + inputed_T001042009*4000 +
                                                            inputed_T001042010*7500 + inputed_T001042011*15000 + inputed_T001042012*25000 + inputed_T001042013*40000 +
                                                            inputed_T001042014*60000) / inputed_T001042001,
                               inputed_T001042001 == 0 ~ 0,
                               TRUE ~ NA),
      # 農産物販売金額1位の部門別経営体数割合
      fe_per_rice_top = case_when(inputed_T001043001 != 0 ~ inputed_T001043002 / inputed_T001043001,
                                  inputed_T001043001 == 0 ~ 0,
                                  TRUE ~ NA
      ),
      fe_per_rice_top = case_when(fe_per_rice_top >= 1 ~ 1,
                                  fe_per_rice_top <= 0 ~ 0,
                                  TRUE ~ fe_per_rice_top),
      # 農業関連生産事業をおこなっている経営体の割合
      fe_per_kanren_jigyo = case_when(inputed_T001046001 != 0 ~ inputed_T001046003 / inputed_T001046001,
                                      inputed_T001046001 == 0 ~ 0,
                                      TRUE ~ NA
      ),
      fe_per_kanren_jigyo = case_when(fe_per_kanren_jigyo >= 1 ~ 1,
                                      fe_per_kanren_jigyo <= 0 ~ 0,
                                      TRUE ~ fe_per_kanren_jigyo),
      # 農産物の販売をおこなった経営体の割合
      fe_per_hanbai = case_when(inputed_T001047001 != 0 ~ inputed_T001047003 / inputed_T001047001,
                                inputed_T001047001 == 0 ~ 0,
                                TRUE ~ NA),
      fe_per_hanbai = case_when(fe_per_hanbai >= 1 ~ 1,
                                fe_per_hanbai <= 0 ~ 0,
                                TRUE ~ fe_per_hanbai),
      # 農産物の売上1位のものが農協である割合
      fe_per_noukyo = case_when(inputed_T001048001 != 0 ~ inputed_T001048002 / inputed_T001048001,
                                inputed_T001048001 == 0 ~ 0,
                                TRUE ~ NA),
      fe_per_noukyo = case_when(fe_per_noukyo >= 1 ~ 1,
                                fe_per_noukyo <= 0 ~ 0,
                                TRUE ~ fe_per_noukyo),
      # 経営耕地のうち，田が占める割合
      fe_per_keiei_paddy = case_when(inputed_T001049002 != 0 ~ inputed_T001049006 / inputed_T001049002,
                                     inputed_T001049002 == 0 ~ 0,
                                     TRUE ~ NA),
      fe_per_keiei_paddy = case_when(fe_per_keiei_paddy >= 1 ~ 1,
                                     fe_per_keiei_paddy <= 0 ~ 0,
                                     TRUE ~ fe_per_keiei_paddy),
      # 各経営体の経営耕地面積の平均値
      fe_mean_keiei_field = case_when(inputed_T001050001 != 0 ~ (inputed_T001050002*0 + inputed_T001050003*0.1 + inputed_T001050004*0.4 + inputed_T001050005*0.7 +
                                                                   inputed_T001050006*1.25 + inputed_T001050007*1.75 + inputed_T001050008*2 + inputed_T001050009*4 + inputed_T001050010*7.5 +
                                                                   inputed_T001050011*15 + inputed_T001050012*25 + inputed_T001050013*40 + inputed_T001050014*75 + inputed_T001050015*125 +
                                                                   inputed_T001050016*200) / inputed_T001050001,
                                      inputed_T001050001 == 0 ~ 0,
                                      TRUE ~ NA),
      # 貸付耕地のある経営体割合
      fe_per_kashitsuke = case_when(inputed_T001052001 != 0 ~ inputed_T001052003 / inputed_T001052001,
                                    inputed_T001052001 == 0 ~ 0,
                                    TRUE ~ NA),
      fe_per_kashitsuke = case_when(fe_per_kashitsuke >= 1 ~ 1,
                                    fe_per_kashitsuke <= 0 ~ 0,
                                    TRUE ~ fe_per_kashitsuke),
      # 稲の作付経営体数
      fe_per_rice_keieitai = case_when(inputed_T001053001 != 0 ~ inputed_T001053003 / inputed_T001053001,
                                       inputed_T001053001 == 0 ~ 0,
                                       TRUE ~ NA),
      fe_per_rice_keieitai = case_when(fe_per_rice_keieitai >= 1 ~ 1,
                                       fe_per_rice_keieitai <= 0 ~ 0,
                                       TRUE ~ fe_per_rice_keieitai),
      # 稲の作付面積割合
      fe_per_rice_menseki = case_when(inputed_T001053002 != 0 ~ inputed_T001053004 / inputed_T001053002,
                                      inputed_T001053002 == 0 ~ 0,
                                      TRUE ~ NA),
      fe_per_rice_menseki = case_when(fe_per_rice_menseki >= 1 ~ 1,
                                      fe_per_rice_menseki <= 0 ~ 0,
                                      TRUE ~ fe_per_rice_menseki),
      # 耕地部門の作業を受託した経営体のうち，水稲作を受託した割合
      fe_per_rice_jutaku = case_when(inputed_T001055002 != 0 ~ inputed_T001055003 / inputed_T001055002,
                                     inputed_T001055002 == 0 ~ 0,
                                     TRUE ~ NA),
      fe_per_rice_jutaku = case_when(fe_per_rice_jutaku >= 1 ~ 1,
                                     fe_per_rice_jutaku <= 0 ~ 0,
                                     TRUE ~ fe_per_rice_jutaku),
      # 水稲受託作業種類別経営体すうと受託作業面積という変数について何かしたい．
      # 受託料金の平均
      fe_mean_jutaku = case_when(inputed_T001057001 != 0 ~ (inputed_T001057002*0 + inputed_T001057003*25 + inputed_T001057004*75 + inputed_T001057005*200 +
                                                              inputed_T001057006*400 + inputed_T001057007*750 + inputed_T001057008*2000 + inputed_T001057009*4000 +
                                                              inputed_T001057010*7500 + inputed_T001057011*15000 + inputed_T001057012*25000 + inputed_T001057013*40000 +
                                                              inputed_T001057014*60000) / inputed_T001057001,
                                 inputed_T001057001 == 0 ~ 0,
                                 TRUE ~ NA),
      # 60日以上農業に従事した人で作る平均値，男女合計
      fe_mean_work_days = case_when(inputed_T001058001 != 0 ~ (inputed_T001058002*80 + inputed_T001058003*125 + inputed_T001058004*175 + inputed_T001058005*225 +
                                                                 inputed_T001058006*275) / inputed_T001058001,
                                    inputed_T001058001 == 0 ~ 30,
                                    TRUE ~ NA),
      # 常雇いの割合，計のべ人日に占める農業の述べ人日の割合
      fe_per_nobe_agri = case_when(inputed_T001059003 != 0 ~ inputed_T001059004 / inputed_T001059003,
                                   inputed_T001059003 == 0 ~ 0,
                                   TRUE ~ NA),
      fe_per_nobe_agri = case_when(fe_per_nobe_agri >= 1 ~ 1,
                                   fe_per_nobe_agri <= 0 ~ 0,
                                   TRUE ~ fe_per_nobe_agri),
      # 経営体の副業的の割合
      fe_per_fukugyo = case_when(inputed_T001061001 != 0 ~ inputed_T001061006 / inputed_T001061001,
                                 inputed_T001061001 == 0 ~ 0,
                                 TRUE ~ NA),
      fe_per_fukugyo = case_when(fe_per_fukugyo >= 1 ~ 1,
                                 fe_per_fukugyo <= 0 ~ 0,
                                 TRUE ~ fe_per_fukugyo),
      # 世帯員の男性割合
      fe_per_male = case_when(inputed_T001062001 != 0 ~ inputed_T001062002 / inputed_T001062001,
                              inputed_T001062001 == 0 ~ 0.5,
                              TRUE ~ NA),
      fe_per_male = case_when(fe_per_male >= 1 ~ 1,
                              fe_per_male <= 0 ~ 0,
                              TRUE ~ fe_per_male),
      # 世帯員の平均年齢
      fe_mean_age = case_when(inputed_T001062010 != 0 ~ (inputed_T001062011*17 + inputed_T001062012*22 + inputed_T001062013*27 + inputed_T001062014*32 +
                                                           inputed_T001062015*37 + inputed_T001062016*42 + inputed_T001062017*47 + inputed_T001062018*52 +
                                                           inputed_T001062019*57 + inputed_T001062020*62 + inputed_T001062021*67 + inputed_T001062022*72 +
                                                           inputed_T001062023*77 + inputed_T001062024*82 + inputed_T001062025*87) / inputed_T001062010,
                              inputed_T001062010 == 0 ~ 0,
                              TRUE ~ NA),
      # 自営農業従事日数階層別の農業従事者数
      fe_mean_jiei_days = case_when(inputed_T001063001 != 0 ~ (inputed_T001063002*15 + inputed_T001063003*45 + inputed_T001063004*80 + inputed_T001063005*125 +
                                                                 inputed_T001063006*175 + inputed_T001063007*225 + inputed_T001063008*275) / inputed_T001063001,
                                    inputed_T001063001 == 0 ~ 0,
                                    TRUE ~ NA),
      # 総農家数のうち販売農家の割合
      fe_per_hanbai_nouka = case_when(T001065001 != 0 ~ T001065002 / T001065001,
                                      T001065001 == 0 ~ 0,
                                      TRUE ~ NA),
      fe_per_hanbai_nouka = case_when(fe_per_hanbai_nouka >= 1 ~ 1,
                                      fe_per_hanbai_nouka <= 0 ~ 0,
                                      TRUE ~ fe_per_hanbai_nouka),
      # 総農家数
      fe_nouka_sum = T001065001,
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
        TRUE ~ 0
      ),
      hozen_shinrin = case_when(
        T001072004 == 1 ~ 1,
        T001072005 == 1 ~ -1,
        TRUE ~ 0
      ),
      hozen_tameike = case_when(
        T001072007 == 1 ~ 1,
        T001072008 == 1 ~ -1,
        TRUE ~ 0
      ),
      hozen_kasen = case_when(
        T001072010 == 1 ~ 1,
        T001072011 == 1 ~ -1,
        TRUE ~ 0
      ),
      hozen_haisui = case_when(
        T001072013 == 1 ~ 1,
        T001072014 == 1 ~ -1,
        TRUE ~ 0
      ),
      fe_hozen = hozen_nouchi + hozen_shinrin + hozen_tameike + hozen_kasen + hozen_haisui,
      # 取り組み状況
      fe_torikumi = T001073001 + T001073003 + T001073005 + T001073007 + T001073009 + T001073011 + T001073013
      )
}
