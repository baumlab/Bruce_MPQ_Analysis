
library(stringr)
library(tidyr)
library(dplyr)


## Check sample sizes for each site
ki_full$site.date <- with(ki_full, paste(KI.Date, Site, sep = "."))
samples <- ki_full %>% group_by(site.date) %>% summarise(count = n_distinct(Transect))
low.samps <- samples[samples$count <3, ]
# Remove anything with fewer than 3 transects (removes 406 rows)
ki_full <- ki_full[!(ki_full$site.date %in% low.samps$site.date),]

## Check that 2 divers were at all sites
obs <- ki_full %>% group_by(site.date) %>% summarise(count = n_distinct(Observer))
low.obs <- obs[obs$count <2, ]
# Remove anything with fewer than 2 observers (removes 111)
ki_full <- ki_full[!(ki_full$site.date %in% low.obs$site.date),]

## Add heat stress variable
ki_full$heat <- NA
ki_full$heat[ki_full$Year == "2011"] <- "Before"
ki_full$heat[ki_full$Year == "2013"] <- "Before"
ki_full$heat[ki_full$Year == "2015"] <- "During"
ki_full$heat[ki_full$Year == "2017"] <- "After"
ki_full$heat[ki_full$Year == "2018"] <- "Recovery"
ki_full$heat[ki_full$Year == "2019"] <- "Recovery"

ki_full$heat <- as.factor(ki_full$heat)
#ki_full$heat <- factor(ki_full$heat, levels(factor(ki_full$heat))[c(2,3,1)])

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

# Remove observation from RT
ki_full <- ki_full[!grepl("RT", ki_full$Observer), ]
ki_full$Observer <- droplevels(ki_full$Observer)

### Create time of day table
ki_time <- ki_full %>% group_by(Year, KI.Date, Site, Observer) %>% 
  summarise(tod = paste(unique(Time), collapse = '-'))
# Isolate time of first survey
ki_time$time_first <- str_split_fixed(ki_time$tod, "-", 3)[,1]


# Clean rows where survey times are different between observers (change to earliest time)
ki_time$dso <- paste(ki_time$KI.Date, ki_time$Site, ki_time$Observer, sep = ".")

#introducing ":" into all times

ki_time2 <- ki_time

ki_time2$time2 <- ifelse(grepl(":", ki_time2$time_first), "outside", "inside")

ki_time3 <- ki_time2[(ki_time2$time2 == "inside"),]

ki_time4 <- ki_time2[(ki_time2$time2 == "outside"),]

ki_time3$time_first <- as.numeric(ki_time3$time_first)

ki_time3$time_first <- sprintf("%04d", ki_time3$time_first)

ki_time3 <- ki_time3 %>% separate(time_first, into = c("Hour", "Minute"), sep = 2)

ki_time3$Time <- paste(ki_time3$Hour, ki_time3$Minute, sep= ":")

#removing extra columns
ki_time3 <- subset(ki_time3, select= -c(Hour, Minute))

#renaming columns so they match between dataframes
colnames(ki_time3)

names(ki_time3) <- c("Year", "KI.Date", "Site", "Observer", "tod", "dso", "time2", "time_first")

ki_time <- rbind(ki_time3, ki_time4)


#now replacing times in time2 that didn't have ":"

ki_time$time_first[ki_time$dso == "01_08_2011.2.SMW"] <- "12:20"
ki_time$time_first[ki_time$dso == "23_07_2011.25.SMW"] <- "13:25"
ki_time$time_first[ki_time$dso == "23_07_2011.30.SMW"] <- "15:35"
ki_time$time_first[ki_time$dso == "24_07_2011.6.SC"] <- "11:20"
ki_time$time_first[ki_time$dso == "24_07_2011.8.SMW"] <- "15:00"
ki_time$time_first[ki_time$dso == "24_07_2011.14.SC"] <- "12:50"
ki_time$time_first[ki_time$dso == "25_07_2011.9.SMW"] <- "13:40"
ki_time$time_first[ki_time$dso == "25_07_2011.31.SMW"] <- "11:34"
ki_time$time_first[ki_time$dso == "27_07_2011.27.SC"]  <- "11:40"
ki_time$time_first[ki_time$dso == "25_07_2011.33.SC"] <- "16:05"
ki_time$time_first[ki_time$dso == "25_07_2011.33.SMW"] <- "16:05"
ki_time$time_first[ki_time$dso == "27_07_2011.32.SMW"] <- "14:25"
ki_time$time_first[ki_time$dso == "27_07_2011.40.SC"]  <- "16:30"
ki_time$time_first[ki_time$dso == "28_07_2011.24.SMW"] <- "12:15"
ki_time$time_first[ki_time$dso == "28_07_2011.38.SMW"] <- "10:35"
ki_time$time_first[ki_time$dso == "29_07_2011.15.SMW"] <- "10:35"
ki_time$time_first[ki_time$dso == "30_07_2011.18.SMW"] <- "12:40"
ki_time$time_first[ki_time$dso == "29_07_2011.19.SMW"] <- "12:35"
ki_time$time_first[ki_time$dso == "30_07_2011.18.SMW"] <- "12:40"
ki_time$time_first[ki_time$dso == "04_08_2013.30.JLG"] <- "08:30"
ki_time$time_first[ki_time$dso == "12_07_2015.14.SD"] <- "13:40"
ki_time$time_first[ki_time$dso == "12_07_2015.14.SC"] <- "13:40"
ki_time$time_first[ki_time$dso == "30_07_2011.16.SMW"] <- "10:35"
ki_time$time_first[ki_time$dso == "30_07_2011.17.SMW"] <- "14:54"
ki_time$time_first[ki_time$dso == "31_07_2011.34.SMW"] <- "13:20"
ki_time$time_first[ki_time$dso == "01_08_2013.14.JLG"] <- "10:40"
ki_time$time_first[ki_time$dso == "01_08_2013.27.JLG"] <- "13:24"
ki_time$time_first[ki_time$dso == "01_08_2013.35.SC"] <- "15:40"
ki_time$time_first[ki_time$dso == "02_08_2013.15.SC"] <- "09:50"
ki_time$time_first[ki_time$dso == "02_08_2013.15.JLG"] <- "09:50"
ki_time$time_first[ki_time$dso == "02_08_2013.19.SC"] <- "12:09"
ki_time$time_first[ki_time$dso == "03_08_2013.1.SC"] <- "11:34"
ki_time$time_first[ki_time$dso == "03_08_2013.22.SC"] <- "14:03"
ki_time$time_first[ki_time$dso == "03_08_2013.23.JLG"] <- "10:00"
ki_time$time_first[ki_time$dso == "04_08_2013.3.JLG"] <- "10:30"
ki_time$time_first[ki_time$dso == "04_08_2013.24.JLG"] <- "12:55"
ki_time$time_first[ki_time$dso == "04_08_2013.32.JLG"] <- "16:10"
ki_time$time_first[ki_time$dso == "20_07_2013.34.JLG"] <- "15:40"
ki_time$time_first[ki_time$dso == "21_07_2013.26.JLG"] <- "14:30"
ki_time$time_first[ki_time$dso == "22_07_2013.6.JLG"] <- "09:34"
ki_time$time_first[ki_time$dso == "22_07_2013.6.SC"] <- "09:34"
ki_time$time_first[ki_time$dso == "29_07_2013.19.SC"] <- "11:54"
ki_time$time_first[ki_time$dso == "22_07_2013.14.SC"] <- "13:56"
ki_time$time_first[ki_time$dso == "24_07_2013.1.JLG"] <- "13:20"
ki_time$time_first[ki_time$dso == "24_07_2013.24.SC"] <- "08:36"
ki_time$time_first[ki_time$dso == "25_07_2013.22.SC"] <- "11:28"
ki_time$time_first[ki_time$dso == "25_07_2013.23.SC"] <- "09:23"
ki_time$time_first[ki_time$dso == "26_07_2013.3.SC"] <- "15:22"
ki_time$time_first[ki_time$dso == "27_07_2013.27.SC"] <- "07:59"
ki_time$time_first[ki_time$dso == "27_07_2013.30.JLG"] <- "14:00"
ki_time$time_first[ki_time$dso == "28_07_2013.8.SC"] <- "08:17"
ki_time$time_first[ki_time$dso == "28_07_2013.34.SC"] <- "12:34"
ki_time$time_first[ki_time$dso == "28_07_2013.35.JLG"] <- "10:20"
ki_time$time_first[ki_time$dso == "29_07_2013.15.JLG"] <- "10:05"
ki_time$time_first[ki_time$dso == "29_07_2013.19.JLG"] <- "11:54"
ki_time$time_first[ki_time$dso == "31_07_2013.25.SC"] <- "08:08"
ki_time$time_first[ki_time$dso == "31_07_2013.26.SC"] <- "11:09"
ki_time$time_first[ki_time$dso == "31_07_2013.32.SC"] <- "15:54"
ki_time$time_first[ki_time$dso == "07_07_2015.25.SC"] <- "08:44"
ki_time$time_first[ki_time$dso == "11_07_2015.34.SD"] <- "13:05"
ki_time$time_first[ki_time$dso == "09_07_2017.12.TAP"] <- "16:26"
ki_time$time_first[ki_time$dso == "06_07_2015.3.SD"] <- "16:00"
ki_time$time_first[ki_time$dso == "09_07_2015.38.SD"] <- "09:40"
ki_time$time_first[ki_time$dso == "11_07_2015.8.SD"] <- "08:45"
ki_time$time_first[ki_time$dso == "12_07_2015.6.SD"] <- "08:45"
ki_time$time_first[ki_time$dso == "13_07_2015.5.SD"] <- "09:35"
ki_time$time_first[ki_time$dso == "14_07_2015.34.SC"] <- "10:05"
ki_time$time_first[ki_time$dso == "14_07_2015.35.SD"] <- "12:40"
ki_time$time_first[ki_time$dso == "16_07_2015.27.SD"] <- "13:15"
ki_time$time_first[ki_time$dso == "16_07_2015.30.SD"] <- "08:35"
ki_time$time_first[ki_time$dso == "17_07_2015.35.SD"] <- "09:05"
ki_time$time_first[ki_time$dso == "19_07_2015.1.SC"] <- "09:04"
ki_time$time_first[ki_time$dso == "19_07_2015.2.SC"] <- "10:48"
ki_time$time_first[ki_time$dso == "20_07_2015.32.SC"] <- "11:40"
ki_time$time_first[ki_time$dso == "07_07_2017.5.TAP"] <- "13:50"
ki_time$time_first[ki_time$dso == "08_07_2017.3.TAP"] <- "14:02"
ki_time$time_first[ki_time$dso == "08_07_2017.25.SD"] <- "11:07"
ki_time$time_first[ki_time$dso == "08_07_2017.38.SD"] <- "08:17"
ki_time$time_first[ki_time$dso == "09_07_2017.12.TAP"] <- "16:26"
ki_time$time_first[ki_time$dso == "09_07_2017.36.SD"] <- "14:00"
ki_time$time_first[ki_time$dso == "09_07_2017.37.SD"] <- "11:16"
ki_time$time_first[ki_time$dso == "10_07_2017.6.SD"] <- "08:12"
ki_time$time_first[ki_time$dso == "10_07_2017.8.SD"] <- "16:52"
ki_time$time_first[ki_time$dso == "10_07_2017.14.TAP"] <- "10:50"
ki_time$time_first[ki_time$dso == "11_07_2017.8.TAP"] <- "10:46"
ki_time$time_first[ki_time$dso == "11_07_2017.34.SD"] <- "16:30"
ki_time$time_first[ki_time$dso == "11_07_2017.35.TAP"] <- "13:37"
ki_time$time_first[ki_time$dso == "13_07_2017.1.SD"] <- "10:58"
ki_time$time_first[ki_time$dso == "13_07_2017.22.SD"] <- "13:48"
ki_time$time_first[ki_time$dso == "13_07_2017.23.SD"] <- "08:14"
ki_time$time_first[ki_time$dso == "14_07_2017.19.SD"] <- "09:00"
ki_time$time_first[ki_time$dso == "14_07_2017.32.SD"] <- "16:10"
ki_time$time_first[ki_time$dso == "15_07_2017.27.SD"] <- "13:43"
ki_time$time_first[ki_time$dso == "16_07_2017.30.SD"] <- "07:35"
ki_time$time_first[ki_time$dso == "16_07_2017.40.SD"] <- "13:14"
ki_time$time_first[ki_time$dso == "17_07_2017.31.SD"] <- "17:02"
ki_time$time_first[ki_time$dso == "21_07_2017.33.TAP"] <- "12:27"
ki_time$time_first[ki_time$dso == "22_07_2017.38.TAP"] <- "11:00"
ki_time$time_first[ki_time$dso == "24_07_2017.24.TAP"] <- "10:22"
ki_time$time_first[ki_time$dso == "25_07_2017.4.SD"] <- "08:22"
ki_time$time_first[ki_time$dso == "25_07_2017.7.SD"] <- "12:42"
ki_time$time_first[ki_time$dso == "31_07_2017.2.SD"] <- "16:46"
ki_time$time_first[ki_time$dso == "31_07_2017.11.SD"] <- "11:15"
ki_time$time_first[ki_time$dso == "31_07_2017.20.TAP"] <- "14:01"
ki_time$time_first[ki_time$dso == "27_07_2019.30.SD"] <- "07:50"
ki_time$time_first[ki_time$dso == "29_07_2019.40.KB"] <- "08:05"
ki_time$time_first[ki_time$dso == "29_07_2019.8.KB"] <- "10:52"
ki_time$time_first[ki_time$dso == "29_07_2019.8.SD"] <- "10:52"


ki_time$time_first[ki_time$dso == "08_07_2017.38.TAP"] <- "08:37"
ki_time$time_first[ki_time$dso == "09_07_2017.5.TAP"] <- "08:52"
ki_time$time_first[ki_time$dso == "15_07_2017.26.SD"] <- "08:16"
ki_time$time_first[ki_time$dso == "31_07_2017.10.SD"] <- "08:55"
ki_time$time_first[ki_time$dso == "20_07_2019.1.KB"] <- "08:33"
ki_time$time_first[ki_time$dso == "21_07_2019.37.KB"] <- "08:26"
ki_time$time_first[ki_time$dso == "22_07_2019.5.KB"] <- "08:23"
ki_time$time_first[ki_time$dso == "23_07_2019.34.KB"] <- "07:55"
ki_time$time_first[ki_time$dso == "27_07_2019.30.KB"] <- "07:50"


ki_time$time_first <- as.character(ki_time$time_first)

ki_timeshort <- ki_time[nchar(ki_time$time_first) == 4,]

### Add time of day values to ki_full dataset
ki_full$dso <- paste(ki_full$KI.Date, ki_full$Site, ki_full$Observer, sep = ".")
ki_full$time_first <- ki_time$time_first[match(ki_full$dso, ki_time$dso)]




unique(ki_full$time_first)
