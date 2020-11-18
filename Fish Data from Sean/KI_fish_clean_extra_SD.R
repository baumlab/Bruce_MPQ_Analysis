
## Remove data from 2018 surveys
#ki_full <- ki_full[!grepl("2018", ki_full$Year), ]

## Check sample sizes for each site
ki_full$site.date <- with(ki_full, paste(KI.Date, Site, sep = "."))
samples <- ki_full %>% group_by(site.date) %>% summarise(count = n_distinct(Transect))
low.samps <- samples[samples$count <3, ]
# Remove anything with fewer than 3 transects (removes 6 surveys)
ki_full <- ki_full[!(ki_full$site.date %in% low.samps$site.date),]

## Check that 2 divers were at all sites
obs <- ki_full %>% group_by(site.date) %>% summarise(count = n_distinct(Observer))
low.obs <- obs[obs$count <2, ]
# Remove anything with fewer than 2 observers (removes 2 surveys)
ki_full <- ki_full[!(ki_full$site.date %in% low.obs$site.date),]
unique(ki_full$Year)
## Add heat stress variable
ki_full$heat <- NA
ki_full$heat[ki_full$Year == "2011"] <- "Before"
ki_full$heat[ki_full$Year == "2013"] <- "Before"
ki_full$heat[ki_full$Year == "2015"] <- "During"
ki_full$heat[ki_full$Year == "2017"] <- "After"
ki_full$heat[ki_full$Year == "2018"] <- "Recovery"
ki_full$heat[ki_full$Year == "2019"] <- "Recovery"


ki_full$heat <- as.factor(ki_full$heat)
levels(ki_full$heat)
ki_full$heat <- factor(ki_full$heat, levels(factor(ki_full$heat))[c(2,3,1,4)])

## Remove sharks and jacks (due to potential survey biases)
ki_full <- ki_full[!grepl("Caranx", ki_full$Species), ] # Removes 63 observations
ki_full <- ki_full[!grepl("Carangoides", ki_full$Species), ] # Removes 3 observations
ki_full <- ki_full[!grepl("Scomberoides", ki_full$Species), ] # Removes 44 observations
ki_full <- ki_full[!grepl("Carcharhinus", ki_full$Species), ] # Removes 4 observations


## Consolidate to 10 functional groups (carnivores split into fine FGs)
ki_full$Trophic <- NULL
# Corallivores
ki_full$Trophic[ki_full$FineFG == "Corallivore"] <- "Corallivore"
ki_full$Trophic[ki_full$FineFG == "Obligate corallivore"] <- "Corallivore"
ki_full$Trophic[ki_full$FineFG == "Facultative corallivore"] <- "Corallivore"
ki_full$Trophic[ki_full$FineFG == " Facultative corallivore"] <- "Corallivore"
# Detritivores
ki_full$Trophic[ki_full$CoarseFG == "Detritivore"] <- "Detritivore"
# Generalist carnivores
ki_full$Trophic[ki_full$FineFG == "Generalist carnivore"] <- "Generalist carnivore"
# Herbivores
ki_full$Trophic[ki_full$CoarseFG == "Herbivore"] <- "Herbivore"
# Invertivores
ki_full$Trophic[ki_full$FineFG == "Ectoparasites"] <- "Invertivore"
ki_full$Trophic[ki_full$FineFG == "Invertivore"] <- "Invertivore"
ki_full$Trophic[ki_full$FineFG == "Benthic invertivore"] <- "Invertivore"
ki_full$Trophic[ki_full$FineFG == "Benthic intertivore"] <- "Invertivore"
# Onmivores
ki_full$Trophic[ki_full$FineFG == "Benthic omnivore"] <- "Omnivore"
ki_full$Trophic[ki_full$FineFG == "Pelagic omnivore"] <- "Omnivore"
ki_full$Trophic[ki_full$FineFG == "Herbivore/Corallivore"] <- "Omnivore"
ki_full$Trophic[ki_full$CoarseFG == "Herbivore/Planktivore"] <- "Omnivore"
ki_full$Trophic[ki_full$CoarseFG == "Planktivore/Carnivore"] <- "Omnivore"
# Piscivores
ki_full$Trophic[ki_full$FineFG == "Piscivore"] <- "Piscivore"
ki_full$Trophic[ki_full$FineFG == "Diurnal piscivore"] <- "Piscivore"
ki_full$Trophic[ki_full$FineFG == "Nocturnal piscivore"] <- "Piscivore"
# Planktivores
ki_full$Trophic[ki_full$CoarseFG == "Planktivore"] <- "Planktivore"

# Change trophic group assignments of some species
ki_full$Trophic[ki_full$Species == "Caracanthus maculatus"] <- "Generalist carnivore"
ki_full$Trophic[ki_full$Species == "Chaetodon lunula"] <- "Corallivore"
ki_full$Trophic[ki_full$Species == "Chaetodon ulietensis"] <- "Corallivore"
ki_full$Trophic[ki_full$Species == "Cirrhitichthys oxycephalus"] <- "Generalist carnivore"
ki_full$Trophic[ki_full$Species == "Eviota albolineata"] <- "Generalist carnivore"
ki_full$Trophic[ki_full$Species == "Paracirrhites xanthus"] <- "Generalist carnivore"
ki_full$Trophic[ki_full$Species == "Cirrhitops hubbardi"] <- "Generalist carnivore"

# Remove observations that could not be assigned to a trophic group (removes 51 rows)
ki.na <- ki_full[is.na(ki_full$Trophic), ]
ki_full <- ki_full[!(is.na(ki_full$Trophic)), ]

# Calculate biomass
ki_full$biomass <- ki_full$Number * ki_full$MASS

# Order fishing pressure levels
ki_full$f.pressure <- factor(ki_full$f.pressure, levels(factor(ki_full$f.pressure))[c(5,2,3,1,4)])
levels(ki_full$f.pressure)

## Order time of day levels
ki_full$time_of_day <- as.factor(ki_full$time_of_day)
levels(ki_full$time_of_day)
ki_full$time_of_day <- factor(ki_full$time_of_day, levels(factor(ki_full$time_of_day))[c(4,3,1,2)])

# Remove observation from RT
ki_full <- ki_full[!grepl("RT", ki_full$Observer), ]
ki_full$Observer <- droplevels(ki_full$Observer)

## Create time of day table
ki_time <- ki_full %>% group_by(Year, KI.Date, Site, Observer) %>% 
  summarise(tod = paste(unique(time_of_day), collapse = '-'))

# Clean rows where surveys cover multiple time periods
ki_time$dso <- paste(ki_time$KI.Date, ki_time$Site, ki_time$Observer, sep = ".")

ki_time$tod[ki_time$dso == "25_07_2011.26.SMW"] <- "MORN"
ki_time$tod[ki_time$dso == "25_07_2011.33.SMW"] <- "AFT"
ki_time$tod[ki_time$dso == "27_07_2011.27.SMW"] <- "MID"
ki_time$tod[ki_time$dso == "28_07_2011.24.SMW"] <- "MID"
ki_time$tod[ki_time$dso == "28_07_2011.38.SMW"] <- "MID"
ki_time$tod[ki_time$dso == "29_07_2011.19.SMW"] <- "AFT"
ki_time$tod[ki_time$dso == "30_07_2011.16.SMW"] <- "MID"
ki_time$tod[ki_time$dso == "31_07_2011.35.SMW"] <- "AFT"
ki_time$tod[ki_time$dso == "01_08_2011.2.SMW"] <- "MID"
ki_time$tod[ki_time$dso == "02_08_2013.19.SC"] <- "MID"
ki_time$tod[ki_time$dso == "25_07_2013.23.JLG"] <- "MORN"
ki_time$tod[ki_time$dso == "25_07_2013.23.SC"] <- "MORN"
ki_time$tod[ki_time$dso == "26_07_2013.3.JLG"] <- "AFT"
ki_time$tod[ki_time$dso == "26_07_2013.3.SC"] <- "AFT"
ki_time$tod[ki_time$dso == "29_07_2013.19.SC"] <- "MID"
ki_time$tod[ki_time$dso == "04_07_2015.27.SC"] <- "MORN"
ki_time$tod[ki_time$dso == "17_07_2015.35.SC"] <- "MORN"
ki_time$tod[ki_time$dso == "17_07_2015.35.SD"] <- "MORN"
ki_time$tod[ki_time$dso == "14_07_2017.19.SD"] <- "MORN"
ki_time$tod[ki_time$dso == "14_07_2017.19.TAP"] <- "MORN"
ki_time$tod[ki_time$dso == "21_07_2017.33.SD"] <- "MID"
ki_time$tod[ki_time$dso == "21_07_2017.33.TAP"] <- "MID"
ki_time$tod[ki_time$dso == "31_07_2017.10.SD"] <- "MORN"
ki_time$tod[ki_time$dso == "31_07_2017.10.TAP"] <- "MORN"

ki_time$tod <- as.factor(ki_time$tod)
levels(ki_time$tod)
ki_time$tod <- factor(ki_time$tod, levels(factor(ki_time$tod))[c(4,3,1,2)])

ki_n_time <- ki_time %>% group_by(Year, tod) %>% summarise(n_time = n_distinct(dso)/2)

### Transfer TOD changes to ki_full dataset

ki_full$dso <- paste(ki_full$KI.Date, ki_full$Site, ki_full$Observer, sep = ".")

ki_full$time_of_day[ki_full$dso == "25_07_2011.26.SMW"] <- "MORN"
ki_full$time_of_day[ki_full$dso == "25_07_2011.33.SMW"] <- "AFT"
ki_full$time_of_day[ki_full$dso == "27_07_2011.27.SMW"] <- "MID"
ki_full$time_of_day[ki_full$dso == "28_07_2011.24.SMW"] <- "MID"
ki_full$time_of_day[ki_full$dso == "28_07_2011.38.SMW"] <- "MID"
ki_full$time_of_day[ki_full$dso == "29_07_2011.19.SMW"] <- "AFT"
ki_full$time_of_day[ki_full$dso == "30_07_2011.16.SMW"] <- "MID"
ki_full$time_of_day[ki_full$dso == "31_07_2011.35.SMW"] <- "AFT"
ki_full$time_of_day[ki_full$dso == "01_08_2011.2.SMW"] <- "MID"
ki_full$time_of_day[ki_full$dso == "02_08_2013.19.SC"] <- "MID"
ki_full$time_of_day[ki_full$dso == "25_07_2013.23.JLG"] <- "MORN"
ki_full$time_of_day[ki_full$dso == "25_07_2013.23.SC"] <- "MORN"
ki_full$time_of_day[ki_full$dso == "26_07_2013.3.JLG"] <- "AFT"
ki_full$time_of_day[ki_full$dso == "26_07_2013.3.SC"] <- "AFT"
ki_full$time_of_day[ki_full$dso == "29_07_2013.19.SC"] <- "MID"
ki_full$time_of_day[ki_full$dso == "04_07_2015.27.SC"] <- "MORN"
ki_full$time_of_day[ki_full$dso == "17_07_2015.35.SC"] <- "MORN"
ki_full$time_of_day[ki_full$dso == "17_07_2015.35.SD"] <- "MORN"
ki_full$time_of_day[ki_full$dso == "14_07_2017.19.SD"] <- "MORN"
ki_full$time_of_day[ki_full$dso == "14_07_2017.19.TAP"] <- "MORN"
ki_full$time_of_day[ki_full$dso == "21_07_2017.33.SD"] <- "MID"
ki_full$time_of_day[ki_full$dso == "21_07_2017.33.TAP"] <- "MID"
ki_full$time_of_day[ki_full$dso == "31_07_2017.10.SD"] <- "MORN"
ki_full$time_of_day[ki_full$dso == "31_07_2017.10.TAP"] <- "MORN"

ki_full$time_of_day <- as.factor(ki_full$time_of_day)
levels(ki_full$time_of_day)
ki_full$time_of_day <- factor(ki_full$time_of_day, levels(factor(ki_full$time_of_day))[c(4,3,1,2)])
