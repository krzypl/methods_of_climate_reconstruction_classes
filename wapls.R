library(rioja)
library(tidyverse)

#data preparation
dat <- readRDS("data/chiro_dat_spore_kusowo.rds") #load data
age <- dat$age #extract age
depth <- dat$depth #extract depth

fos_names <- read_csv("data/fos_names.csv") #load full names 

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

spp_names_wapls <- names(spp)
fos_names_wapls <- names(fos)
cols_to_add <- names(spp[!names(spp) %in% fos_names_wapls])

fos[,cols_to_add] <- 0

fos_wapls <- fos %>% 
  select(sort(names(fos)))

names(spp) %in% names(fos_wapls) #all names should match in both datasets

#reconstruction

fit <- WAPLS(spp, ns_ts_temp)
fit #this is a model performance; we need a prediction performance, and thus run a cross validation (cv)
fit.cv <- crossval(fit, cv.method="loo")
fit.cv

rand.t.test(fit.cv)
screeplot(fit.cv)

#predict the core
pred <- predict(fit, fos_wapls)

plot(age, pred$fit[, 2], type="b", ylab="C-IT", las=1)
lines(age, dat$CIT_original, col = "red")
