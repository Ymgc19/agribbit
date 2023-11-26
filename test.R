ishikawa <- agribbit::agri.read_census(17)
ishikawa_fe <- ishikawa %>% agribbit::agri.fe_census()

ishikawa_fe$fe_mean_age %>% table
