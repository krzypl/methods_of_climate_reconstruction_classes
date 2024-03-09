library(analogue)
library(rioja)
library(vegan)
library(tidyverse)
library(palaeoSig)
library(patchwork)
library(tidypaleo)
library(svglite)
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

fit <- mat(spp/100, ns_ts_temp, method = "SQchord", kmax =15) #modern analogue technique transfer function

screeplot(fit) #select optimal k

spp_names_mat <- names(spp)
fos_names_mat <- names(fos)
cols_to_add <- names(spp[!names(spp) %in% fos_names_mat])

fos[,cols_to_add] <- 0

fos_mat <- fos %>% 
  select(sort(names(fos)))

names(spp) %in% names(fos_mat) #all names should match in both datasets

pred_mat <- predict(fit, fos_mat/100, k =7, bootstrap = TRUE, n.boot = 100) #jaki wplyw ma zmiana parametru k?

reconPlot(pred_mat, depths = age, use.labels = TRUE, ylab = "CIT", xlab = "Age (yr BP)", 
          display.error = "bars", predictions = "bootstrap")

pred_mat_min_dis <- predict(fit, fos_mat/100)
min_dis <- minDC(pred_mat_min_dis) #extract minium dissimilarity between fos and spp

plot(min_dis, age, use.labels = TRUE, xlab = "Age (years BP)")


ca <- cca(spp~ns_ts_temp)
plot(ca, display = "sites")

rlen <- residLen(spp, ns_ts_temp, fos_mat, method="cca")

plot(age, rlen$passive, ylab="Squared residual length", xlab="age", type = "b")
abline(h=quantile(rlen$train, probs = c(0.9,0.95)), col=c("orange", "red"))
rlen.probs <- quantile(rlen$train, probs = c(0.9,0.95))

