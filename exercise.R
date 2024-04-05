library(readxl)
library(tidyverse)

#data preparation --------
#data comes from: Bolland, A., Rey, F., Gobet, E., Tinner, W., & Heiri, O. (2020). Summer temperature development 18,000–14,000 cal. BP recorded by a new chironomid record from Burgäschisee, Swiss Plateau. Quaternary Science Reviews, 243, 106484.

bolland20_chiro <- read_xlsx("data/bolland20_data.xlsx",
                             sheet = "Chironomid Data", skip = 1)[,-1] #dane nalezy przeliczyc na procenty przed wykonaniem rekonstrukcji!

bolland20_age <- read_xlsx("data/bolland20_data.xlsx",
                           sheet = "Temperature reconstruction",
                           skip = 1)$`Age (cal. BP)`

bolland20_cit <- read_xlsx("data/bolland20_data.xlsx",
                           sheet = "Temperature reconstruction",
                           skip = 1)$`July air temperature (°C)`

ns_ts <- read.table("https://www.ncei.noaa.gov/pub/data/paleo/insecta/chironomidae/europe/heiri2011-swiss-norwegian-calibration.txt", header = TRUE)

spp <- ns_ts %>% 
  select("Abiskomy":"Zavrelim")

ns_ts_temp <- ns_ts$July.temp

bolland20_harmonized_names <- read_csv("data/bolland20_harmonized_names.csv")

names(bolland20_chiro) <- bolland20_harmonized_names$short_name

spp_names <- names(spp)
b20_names <- names(bolland20_chiro)
cols_to_add <- names(spp[!names(spp) %in% b20_names])

bolland20_chiro[,cols_to_add] <- 0

b20_rec <- bolland20_chiro %>% 
  select(sort(names(bolland20_chiro)))

names(spp) %in% names(b20_rec)

#reconstruction -------