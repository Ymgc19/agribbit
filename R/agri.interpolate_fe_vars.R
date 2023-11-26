#' @title to interpolate important variables at once
#' @description \code{agri.interpolate_fe_vars}
#' @export

# まとめて欠損値の補完をする関数．
agri.interpolate_fe_vars <- function(df){
  df <- df %>% agri.fe_census()
  var_list <- c("fe_per_houjin", "fe_mean_sell", "fe_per_rice_top", "fe_per_kanren_jigyo",
                "fe_per_hanbai", "fe_per_noukyo", "fe_per_keiei_paddy", "fe_mean_keiei_field",
                "fe_per_kashitsuke", "fe_per_rice_keieitai", "fe_per_rice_menseki", "fe_per_rice_jutaku",
                "fe_mean_jutaku", "fe_mean_work_days", "fe_per_nobe_agri", "fe_per_fukugyo",
                "fe_per_male", "fe_mean_age", "fe_mean_jiei_days", "fe_per_hanbai_nouka",
                "fe_per_hanbai_keieikouchi")
  for (i in 1:length(var_list)){
    a <- agribbit::agri.interpolate(df, var_list[i])
    df <- a$inputed
    print(i / length(var_list))
  }
  return(df)
}
