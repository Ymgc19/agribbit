# install.packages("devtools")
devtools::install_github("Ymgc19/agribbit", force = T)
library(agribbit)


ishikawa <- agribbit::agri.read_census(17)
ishikawa_fe <- ishikawa %>% agribbit::agri.fe_census()

ishikawa_fe$fe_mean_age %>% table
sum(is.na(ishikawa_fe$fe_mean_age))

shp <- agri.read_census_shp(17)
ishikawa_fe <- agri.join(
  shp, ishikawa_fe
)

agri.fast_draw(ishikawa_fe, ishikawa_fe$fe_mean_work_days)

a <- agri.interpolate(ishikawa_fe, "fe_mean_work_days")
a$true.vs.predicted


agri.join(shp, a$inputed) %>%
  agri.fast_draw(., a$inputed$inputed_fe_mean_work_days)

