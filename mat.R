library(analogue)
library(rioja)
library(vegan)
library(tidyverse)
library(palaeoSig)
library(patchwork)
library(tidypaleo)
library(svglite)
dat <- readRDS("data/chiro_dat_spore_kusowo.rds")
age <- dat$age
depth <- dat$depth

fos_names <- read_csv("data/fos_names.csv")

fos_prep <- dat$fossils
names(fos_prep) <- fos_names$name

fos <- fos_prep %>% 
  rename(`Corynoeura coronata-type` = `Corynoneura cornata-type`,
         `Nanocladius (Nanocladius)` = `Nanocladius rectinervis-type`,
         `Nanocladius (Plecopterocanthus)` = `Nanocladius branchicolus-type`) %>% 
  mutate(`Cryptochironomus/Demicryptochironomus` = `Cryptochironomus` + `Demicryptochironomus`,
         `Glyptotendipes` = `Glyptotendipes barbipes-type` + `Glyptotendipes pallens_severini`) %>% 
  select(!c(`Chironomini,1st,instar`, `chironomini,undiff`, `Corynoneura,undiff`, `Cricotopus bicinctus-type`,
            `Cricotopus/Orthocladius`, `Micropsectra,undiff`, `Orthocladiinae,undiff`, `Parakiefferiella triquetra-type`,
            `Paratanytarsus,undiff`, `Tanypodinae,undiff`, `Tanytarsini,undiff`, `Tribelos`,
            `Cryptochironomus`, `Demicryptochironomus`, `Glyptotendipes barbipes-type`, `Glyptotendipes pallens_severini`))

harmonized_names <- read_csv("data/harmonized_names.csv")

names(fos) <- harmonized_names$short_name

ns_ts <- read.table("https://www.ncei.noaa.gov/pub/data/paleo/insecta/chironomidae/europe/heiri2011-swiss-norwegian-calibration.txt", header = TRUE)

spp <- ns_ts %>% 
  select("Abiskomy":"Zavrelim")

ns_ts_temp <- ns_ts$July.temp

fit <- mat(spp/100, ns_ts_temp, method = "SQchord", kmax =15)

screeplot(fit)

spp_names_mat <- names(spp)
fos_names_mat <- names(fos)
cols_to_add <- names(spp[!names(spp) %in% fos_names_mat])

fos[,cols_to_add] <- 0

fos_mat <- fos %>% 
  select(sort(names(fos)))


names_to_marge <- spp %>% 
  all_of(fos_names_mat)

pred_mat <- predict(fit, fos_mat/100, k =3, bootstrap = TRUE, n.boot = 100)

reconPlot(pred_mat, depths = age, use.labels = TRUE, ylab = "CIT", xlab = "Age (yr BP)", 
          display.error = "bars", predictions = "bootstrap")