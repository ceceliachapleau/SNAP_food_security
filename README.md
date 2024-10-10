# SNAP_food_security
#I used Current Population Survey Food Supplement to assess the trendings in food stamp use and food security in Michigan and Indiana from 2010 to 2021
ddi <- read_ipums_ddi("cps_00002.xml")
data <- read_ipums_micro(ddi)

library(ipumsr) 
library(tidyverse)
library(Hmisc)
library(dplyr)
library(janitor)

#Create a new dataset with those in the food supplement survey
dataFS <- filter(data,FSSUPINT==1)

#Remove missing values by changing to NA and dropping them in analysis
dataFS$FSSUPPWT[dataFS$FSSUPPWT == 0]<- NA
dataFS$SEX[dataFS$SEX == 9]<- NA
dataFS$FSSTATUS[dataFS$FSSTATUS == 98]<- NA
dataFS$FSSTATUS[dataFS$FSSTATUS == 99]<- NA
dataFS$FSSTATUSA[dataFS$FSSTATUSA == 98]<- NA
dataFS$FSSTATUSA[dataFS$FSSTATUSA == 99]<- NA
dataFS$FSSTATUSC[dataFS$FSSTATUSC == 98]<- NA
dataFS$FSSTATUSC[dataFS$FSSTATUSC == 99]<- NA
dataFS$EMPSTAT[dataFS$EMPSTAT == 00]<- NA
dataFS$FSSTMPVALC[dataFS$FSSTMPVALC == 996]<- NA
dataFS$FSSTMPVALC[dataFS$FSSTMPVALC == 997]<- NA
dataFS$FSSTMPVALC[dataFS$FSSTMPVALC == 998]<- NA
dataFS$FSSTMPVALC[dataFS$FSSTMPVALC == 999]<- NA
dataFS$FAMINC[dataFS$FAMINC == 999]<- NA

#Create new race and ethnicity variable with categories of White (non-hispanic), Black (non-hispanic), Hispanic, and Other
dataFS$raceethno <- dataFS %>% 
  mutate(raceethno = case_when((HISPAN>=100 & HISPAN<=612)~"Hispanic",
                      (RACE==100 & HISPAN==000)~"White",
                      (RACE==200 & HISPAN==000)~"Black",
                      (RACE>200 & RACE<900 & HISPAN==000)~"Other"
                      ))

#New income variable
dataFS$income <- dataFS %>% 
  mutate(income = case_when((FAMINC == 100|FAMINC == 200|FAMINC == 300)~"under10",
                      (FAMINC == 430|FAMINC == 470|FAMINC == 500|FAMINC == 600)~"10_25",
                      (FAMINC == 710|FAMINC == 720|FAMINC == 730|FAMINC == 740)~"25_50",
                      (FAMINC == 820|FAMINC == 830|FAMINC == 841|FAMINC == 842|FAMINC == 843)~"above50"
                      ))

#New employment status variable
dataFS$employment <- dataFS %>% 
  mutate(income = case_when((EMPSTAT == 1|EMPSTAT == 10|EMPSTAT == 12)~"employed",
                      (EMPSTAT == 20|EMPSTAT == 21|EMPSTAT == 22)~"unemployed",
                      (FAMINC == 710|FAMINC == 720|FAMINC == 730|FAMINC == 740)~"25_50",
                      (EMPSTAT == 32|EMPSTAT == 34|EMPSTAT == 36)~"NILF"))


#Create two new datasets separating Michigan and Indiana
dataFS_IN <- filter(dataFS, STATEFIP==18)
dataFS_MI <- filter(dataFS, STATEFIP==26)

#For Michigan
#Filtering food status to those that are food insecure (2 and 3)
#Filtering food stamps variable to contain those on food stamps, those not on food stamps and those not in universe
Food_insecure_mi <- dataFS_MI %>% 
  filter(FSSTATUS == 2|FSSTATUS ==3) %>% 
  filter(FSFDSTMP == 1|FSFDSTMP == 2|FSFDSTMP == 99)


#Table showing food insecure populations in Michigan from 2010-2021 with survey weights
prop.table(wtd.table(Food_insecure_mi$YEAR, Food_insecure_mi$FSFDSTMP, na.rm = TRUE, weights=Food_insecure_mi$FSSUPPWT), 1)

#Table showing sex of food insecure populations from 2010-2021 
prop.table(wtd.table(Food_insecure_mi$YEAR, Food_insecure_mi$SEX, na.rm = TRUE, weights=Food_insecure_mi$FSSUPPWT), 1)

#Table showing employment status of food insecure from 2010-2021
prop.table(wtd.table(Food_insecure_mi$YEAR, Food_insecure_mi$EMPSTAT, na.rm = TRUE, weights=Food_insecure_mi$FSSUPPWT), 1)

#Table showing race and ethnicity of food insecure from 2010 to 2021
prop.table(wtd.table(Food_insecure_mi$YEAR, Food_insecure_mi$racethno, na.rm = TRUE, Food_insecure_mi$FSSUPPWT), 1)

#Table showing percent of total population receiving food stamps
prop.table(wtd.table(dataFS_MI$YEAR, dataFS_MI$FSFDSTMP, na.rm = TRUE, weights=dataFS_MI$FSSUPPWT), 1)

#Table showing food status of households
prop.table(wtd.table(dataFS_MI$YEAR, dataFS_MI$FSSTATUS, na.rm = TRUE, weights=dataFS_MI$FSSUPPWTH), 1)

#Table showing food status of adults
prop.table(wtd.table(dataFS_MI$YEAR, dataFS_MI$FSSTATUSA, na.rm = TRUE, weights=dataFS_MI$FSSUPPWT), 1)

#Table showing food status of children
prop.table(wtd.table(dataFS_MI$YEAR, dataFS_MI$FSSTATUSC, na.rm = TRUE, weights=dataFS_MI$FSSUPPWT), 1)

#For Indiana
#Filtering food status to those that are food insecure (2 and 3)
#Filtering food stamps variable to contain those on food stamps, those not on food stamps and those not in universe
Food_insecure_in <- dataFS_IN %>% 
  filter(FSSTATUS == 2|FSSTATUS ==3|FSSTATUS == 99) %>% 
  filter(FSFDSTMP == 1|FSFDSTMP == 2|FSFDSTMP == 99)

#Create a table showing food insecure populations in Indiana from 2010-2021 with survey weights
prop.table(wtd.table(Food_insecure_in$YEAR, Food_insecure_in$FSFDSTMP, na.rm = TRUE, weights=Food_insecure_in$FSSUPPWT), 1)

#Table showing sex of food insecure populations from 2010-2021 
prop.table(wtd.table(Food_insecure_in$YEAR, Food_insecure_in$SEX, na.rm = TRUE, weights=Food_insecure_in$FSSUPPWT), 1)

#Table showing employment status of food insecure from 2010-2021
prop.table(wtd.table(Food_insecure_in$YEAR, Food_insecure_in$employment, na.rm = TRUE, weights=Food_insecure_in$FSSUPPWT), 1)

#Table showing race and ethnicity of food insecure from 2010 to 2021
prop.table(wtd.table(Food_insecure_in$YEAR, Food_insecure_in$racethno, na.rm = TRUE, Food_insecure_in$FSSUPPWT), 1)

#Table showing percent of total population receiving food stamps
prop.table(wtd.table(dataFS_IN$YEAR, dataFS_IN$FSFDSTMP, na.rm = TRUE, weights=dataFS_IN$FSSUPPWT), 1)

#Table showing food status of households
prop.table(wtd.table(dataFS_IN$YEAR, dataFS_MI$FSSTATUS, na.rm = TRUE, weights=dataFS_IN$FSSUPPWTH), 1)

#Table showing food status of adults
prop.table(wtd.table(dataFS_IN$YEAR, dataFS_IN$FSSTATUSA, na.rm = TRUE, weights=dataFS_IN$FSSUPPWT), 1)

#Table showing food status of children
prop.table(wtd.table(dataFS_IN$YEAR, dataFS_IN$FSSTATUSC, na.rm = TRUE, weights=dataFS_IN$FSSUPPWT), 1)

#Create a new dataset with only those who receive food stamps
SNAP <- dataFS %>% 
  filter(FSFDSTMP == 2)

#Separate Michigan and Indiana
SNAP_mi <- dataFS_MI %>% 
  filter(FSFDSTMP == 2)

SNAP_in <- dataFS_IN %>% 
  filter(FSFDSTMP == 2)
  
#For Michigan
#Table showing income of those on SNAP
prop.table(wtd.table(SNAP_mi$YEAR, SNAP_mi$FAMINC, na.rm = TRUE, weights=SNAP_mi$FSSUPPWT), 1)

#Table showing employment status of those on SNAP
prop.table(wtd.table(SNAP_mi$YEAR, SNAP_mi$employment, na.rm = TRUE, weights=SNAP_mi$FSSUPPWT), 1)

#Table showing gender by SNAP participation
prop.table(wtd.table(SNAP_mi$YEAR, SNAP_mi$SEX, na.rm = TRUE, weights=SNAP_mi$FSSUPPWT), 1)

#For Indiana
#Table showing income of those on SNAP
prop.table(wtd.table(SNAP_in$YEAR, SNAP_in$FAMINC, na.rm = TRUE, weights=SNAP_in$FSSUPPWT), 1)

#Table showing employment status of those on SNAP
prop.table(wtd.table(SNAP_in$YEAR, SNAP_in$employment, na.rm = TRUE, weights=SNAP_in$FSSUPPWT), 1)

#Table showing gender by SNAP participation
prop.table(wtd.table(SNAP_in$YEAR, SNAP_in$SEX, na.rm = TRUE, weights=SNAP_in$FSSUPPWT), 1)

#Create table showing food stamp values 
table8 <- SNAP %>% 
  drop_na(FSSTMPVALC) %>%
  drop_na(FSSTATUS) %>% 
  group_by(YEAR, STATEFIP = as_factor(STATEFIP)) %>% 
  summarise(weight_value = weighted.mean(FSSTMPVALC, FSSUPPWT, na.rm = TRUE))



