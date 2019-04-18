library(readr)

  #mapping table from key monthly reporting indicators to MER-ish names
key_ind <- read_csv("data-raw/key_ind.csv")

save(key_ind, file = "data/key_ind.rda")
