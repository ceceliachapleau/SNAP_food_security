# SNAP_food_security
#I used Current Population Survey Food Supplement to assess the trendings in food stamp use and food security in Michigan and Indiana from 2010 to 2021
ddi <- read_ipums_ddi("cps_00002.xml")
data <- read_ipums_micro(ddi)

library(ipumsr) 
library(tidyverse)
library(Hmisc)
library(dplyr)
library(janitor)
library(dplyr)

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
  mutate(raceethno = case_when(HISPAN>=100 & HISPAN<=612~"Hispanic",
                      RACE==100 & HISPAN==000~"White",
                      RACE==200 & HISPAN==000~"Black",
                      RACE>200 & RACE<900 & HISPAN==000~"Other"
                      ))

#New income variable
dataFS$income <- dataFS %>% 
  mutate(income = case_when(FAMINC == 100|FAMINC == 200|FAMINC == 300 ~"under10",
                      FAMINC == 430|FAMINC == 470|FAMINC == 500|FAMINC == 600~"10_25",
                      FAMINC == 710|FAMINC == 720|FAMINC == 730|FAMINC == 740~"25_50",
                      FAMINC == 820|FAMINC == 830|FAMINC == 841|FAMINC == 842|FAMINC == 843~"above50"
                      ))

#New employment status variable
dataFS$employment <- dataFS %>% 
  mutate(income = case_when(EMPSTAT == 1|EMPSTAT == 10|EMPSTAT == 12~"employed",
                      EMPSTAT == 20|EMPSTAT == 21|EMPSTAT == 22~"unemployed",
                      FAMINC == 710|FAMINC == 720|FAMINC == 730|FAMINC == 740~"25_50",
                      EMPSTAT == 32|EMPSTAT == 34|EMPSTAT == 36~"NILF"))


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

#Table showing SNAP participation by race and ethnicity 
prop.table(wtd.table(SNAP_mi$YEAR, SNAP_mi$raceethno, na.rm = TRUE, weights=SNAP_mi$FSSUPPWT), 1)

#For Indiana
#Table showing income of those on SNAP
prop.table(wtd.table(SNAP_in$YEAR, SNAP_in$FAMINC, na.rm = TRUE, weights=SNAP_in$FSSUPPWT), 1)

#Table showing employment status of those on SNAP
prop.table(wtd.table(SNAP_in$YEAR, SNAP_in$employment, na.rm = TRUE, weights=SNAP_in$FSSUPPWT), 1)

#Table showing gender by SNAP participation
prop.table(wtd.table(SNAP_in$YEAR, SNAP_in$SEX, na.rm = TRUE, weights=SNAP_in$FSSUPPWT), 1)

#Table showing SNAP participation by race and ethnicity 
prop.table(wtd.table(SNAP_in$YEAR, SNAP_in$raceethno, na.rm = TRUE, weights=SNAP_in$FSSUPPWT), 1)

Create dataframe showing food security status by food stamp recipiency for graph
table8 <- 
  dataFS %>% 
  drop_na(FSSTATUS) %>% 
  drop_na(FSFDSTMP) %>% 
  group_by(YEAR, FSSTATUS = as_factor(FSSTATUS), STATEFIP = as_factor(STATEFIP)) %>% 
  summarise(weight_value = 100*weighted.mean(FSFDSTMP == 2, FSSUPPWT, na.rm = TRUE)) 

#Graph showing food security status by food stamp recipiency
ggplot(table8, aes(YEAR, weight_value, fill = FSSTATUS))+
  geom_point(aes(shape = FSSTATUS, colour = FSSTATUS), size = 3)+
  labs(y="% of SNAP Recipients")+
  labs(x="Year")+
  ggtitle("Figure 4:% of SNAP Recipients by Food Security Status")+
  theme_classic()+
  theme(legend.title=element_blank())+
  facet_wrap(vars(STATEFIP), dir = "v")+
  geom_line(aes(linetype = FSSTATUS, colour = FSSTATUS), linewidth = 1)+
  My_Theme +
  labs(caption = "Source: Author calculations using Michigan and Indiana CPS Food Security Supplement data from 2010 to 2021") +
  theme(plot.caption = element_text(hjust=0, size = 10))

#Create table showing food stamp values (unadjusted)
table8 <- SNAP %>% 
  drop_na(FSSTMPVALC) %>%
  drop_na(FSSTATUS) %>% 
  group_by(YEAR, STATEFIP = as_factor(STATEFIP)) %>% 
  summarise(weight_value = weighted.mean(FSSTMPVALC, FSSUPPWT, na.rm = TRUE))

#Created new dataframe after manually adjusting for inflation with CPI Deflator
svf <- data.frame(year= rep(c('2019', '2021'), each=4),
                 raceethno=rep(c('White (Non-Hispanic)', 'Hispanic', 'Black (Non-Hispanic)', 'Other'), times=2),
                 Answer=c(333.6, 321.2, 350.3, 264.6, 430.3, 350.2, 478.6, 518.7))

svf$raceethno <- factor(svf$raceethno, levels=c("White (Non-Hispanic)", "Black (Non-Hispanic)",  "Hispanic", "Other"), labels=c("White (Non-Hispanic)", "Black (Non-Hispanic)",  "Hispanic", "Other")) 

ggplot(svf, aes(x=year, y= Answer, fill=raceethno)) + 
  geom_col(position = "dodge") +
  theme_minimal() + 
  theme(legend.title=element_blank())+
  scale_fill_manual(values = c("#C72027", "#E2D200", "#46ACC8", "#E58601")) +
  theme(legend.position="none")+
  theme(plot.caption = element_text(hjust=0, size = 10)) +
  labs(x='Year', y='Food Stamp Value (2019 USD)', title='Figure 8: Michigan Food Stamp Value by Race and Ethnicity') +
  #theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) +
  facet_grid(~raceethno) +
  My_Theme +
  labs(caption = "Source: Author calculations using Michigan CPS Household Food Security data from 2019 and 2021") +
  theme(strip.text.y = element_blank()) 

#Food stamp value by race and ethnicity for indiana adjusted for CPI
svfin <- data.frame(year= rep(c('2019', '2021'), each=4),
                 raceethno=rep(c('Black (Non-Hispanic)', 'White (Non-Hispanic)', 'Other', 'Hispanic'), times=2),
                 Answer=c(237.5, 263.08, 116.90, 509.7, 579.4, 488.7, 266.2, 575.1))

svfin$raceethno <- factor(svfin$raceethno, levels=c("White (Non-Hispanic)", "Black (Non-Hispanic)",  "Hispanic", "Other"), labels=c("White (Non-Hispanic)", "Black (Non-Hispanic)",  "Hispanic", "Other")) 

ggplot(svfin, aes(x=year, y= Answer, fill=raceethno)) + 
  geom_col(position = "dodge") +
  theme_minimal() + 
  theme(legend.title=element_blank())+
  scale_fill_manual(values = c("#C72027", "#E2D200", "#46ACC8", "#E58601")) +
  labs(x='Year', y='Food Stamp Value (2019 USD)', title='Figure 9: Indiana Food Stamp Value by Race and Ethnicity') +
  #theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) +
  labs(caption = "Source: Author calculations using Indiana CPS Household Food Security data from 2019 and 2021") +
  theme(legend.position="none")+
  theme(plot.caption = element_text(hjust=0, size = 10)) +
  My_Theme +
  facet_grid(~raceethno) +
  theme(strip.text.y = element_blank()) 

#dataframe for graph showing adult food secure population by race and ethnicity in Michigan
  table5 <- 
  dataFS_MI %>%
  group_by(YEAR = as.numeric(YEAR), raceethno = raceethno, STATEFIP = as_factor(STATEFIP)) %>%
  summarize(FOOD_STATUSA = 100*weighted.mean(FSSTATUSA==1, FSSUPPWT, na.rm = TRUE)) %>% 
  filter(YEAR == 2019|YEAR == 2021) 

table5$raceethno <- factor(table5$raceethno, levels=c("White (Non-Hispanic)", "Black (Non-Hispanic)",  "Hispanic", "Other"), labels=c("White (Non-Hispanic)", "Black (Non-Hispanic)",  "Hispanic", "Other"))

#graph
ggplot(table5, aes(x = factor(YEAR), FOOD_STATUSA, fill = raceethno)) +
  geom_bar(stat = "identity") +
  facet_grid(~ raceethno) +
  theme_minimal() +
  scale_fill_manual(values = c("#C72027", "#E2D200", "#46ACC8", "#E58601")) +
  theme(legend.title=element_blank())+
  scale_y_continuous(limits = c(0, 100)) +
  labs(x='Year', y='Food Secure (%)', title='Figure 6: Michigan Adult Food Security by Race and Ethnicity')+
  labs(caption = "Source: Author calculations using Michigan CPS Household Food Security data from 2019 and 2021") +
  theme(legend.position="none")+
  theme(plot.caption = element_text(hjust=0, size = 10)) +
  My_Theme +
  geom_col(position = "dodge")

#Create standard theme for graph
My_Theme <- theme(
  axis.title.x = element_text(size = 12),
  axis.text.x = element_text(size = 10),
  axis.text.y = element_text(size = 10),
  axis.title.y = element_text(size = 12),
  strip.text = element_text(size = 10), 
  legend.text = element_text(size= 12),
  plot.title = element_text(size=16)
)

#dataframe for graph to show adult food secure percentages from 2010 to 2021 by state
table1 <- 
dataFS %>%
  drop_na(FSSTATUSA) %>%
  group_by(YEAR = as.numeric(YEAR), STATEFIP = as_factor(STATEFIP)) %>%
  summarize(FOOD_STATUSA = 100*weighted.mean(FSSTATUSA==1, FSSUPPWT, na.rm = TRUE)) 

#Graph showing adult food secure percentages from 2010 to 2021 by state
ggplot(data=table1, aes(x=YEAR, y=FOOD_STATUSA, fill=STATEFIP, shape=STATEFIP))+
  labs(y="Percent Food Secure")+
  labs(x="Year")+
  ggtitle("Figure 2: Adult Food Security Levels 2010-2021")+
  theme_classic()+
  geom_point(aes(colour = STATEFIP, shape = STATEFIP), size = 3)+
  theme(legend.title=element_blank())+
  stat_smooth()+
  geom_line(aes(linetype=STATEFIP, color=STATEFIP), linewidth = 1)+
  scale_linetype_manual(values=c("twodash", "solid"))+
  labs(caption = "Source: Author calculations using CPS Adult Food Security data from 2010 to 2021") +
  My_Theme +
  theme(plot.caption = element_text(hjust=0, size = 10)) +
  theme(legend.position="bottom", legend.direction="horizontal")

#dataframe for graph to show child food secure percentages from 2010 to 2021 by state
table2 <- 
dataFS %>%
  drop_na(FSSTATUSC) %>%
  group_by(YEAR = as.numeric(YEAR), STATEFIP = as_factor(STATEFIP)) %>%
  summarize(FOOD_STATUSC = 100*weighted.mean(FSSTATUSC==1, FSSUPPWT, na.rm = TRUE)) 

#Graph showing child food secure percentages from 2010 to 2021 by state
ggplot(data=table2, aes(x=YEAR, y=FOOD_STATUSC, fill=STATEFIP, shape=STATEFIP))+
  labs(y="Percent Food Secure")+
  labs(x="Year")+
  ggtitle("Figure 3: Child Food Security Levels 2010-2021")+
  theme_classic()+
  geom_point(aes(colour = STATEFIP, shape = STATEFIP), size = 3)+
  theme(legend.title=element_blank())+
  geom_line(aes(linetype=STATEFIP, color=STATEFIP), linewidth = 1)+
  scale_linetype_manual(values=c("twodash", "solid"))+
  stat_smooth()+
  labs(caption = "Source: Author calculations using CPS Child Food Security data from 2010 to 2021") +
  theme(legend.position="bottom", legend.direction="horizontal") +
  theme(plot.caption = element_text(hjust=0, size = 10))

#dataframe for graph to show household food security levels
table3 <- 
dataFS %>%
  drop_na(FSSTATUS) %>%
  group_by(YEAR = as.numeric(YEAR), STATEFIP = as_factor(STATEFIP)) %>%
  summarize(FOOD_STATUS = 100*weighted.mean(FSSTATUS==1, FSSUPPWT, na.rm = TRUE)) 

#graph for household food security levels
ggplot(data=table3, aes(x=YEAR, y=FOOD_STATUS, fill=STATEFIP, shape=STATEFIP))+
  labs(y="Percent Food Secure")+
  labs(x="Year")+
  ggtitle("Figure 1: Household Food Security Levels 2010-2021")+
  theme_classic()+
  geom_point(aes(colour = STATEFIP, shape = STATEFIP), size = 3)+
  theme(legend.title=element_blank())+
  stat_smooth()+
  geom_line(aes(linetype=STATEFIP, color=STATEFIP), linewidth = 1)+
  scale_linetype_manual(values=c("twodash", "solid"))+
  labs(caption = "Source: Author calculations using Michigan and Indiana CPS Household Food Security data from 2010 to 2021") +
  theme(plot.caption = element_text(hjust=0, size = 10))

  table8 <- 
  dataFS %>% 
  drop_na(FSSTATUS) %>% 
  drop_na(FSFDSTMP) %>% 
  group_by(YEAR, FSSTATUS = as_factor(FSSTATUS), STATEFIP = as_factor(STATEFIP)) %>% 
  summarise(weight_value = 100*weighted.mean(FSFDSTMP == 2, FSSUPPWT, na.rm = TRUE)) 

#adding animation to exisiting graphs
library(gapminder)
library(ggplot2)
library(gganimate)
library(sf)
library(gifski)
library(maps)

p <- ggplot(table8, aes(YEAR, weight_value, fill = FSSTATUS))+
  geom_point(aes(shape = FSSTATUS, colour = FSSTATUS), size = 3)+
  labs(y="% of SNAP Recipients")+
  labs(x="Year")+
  ggtitle("Figure 4:% of SNAP Recipients by Food Security Status")+
  theme_classic()+
  My_Theme +
  theme(legend.title=element_blank())+
  theme(legend.position="none") +
  facet_wrap(vars(STATEFIP), dir = "v")+
  geom_line(aes(linetype = FSSTATUS, colour = FSSTATUS), linewidth = 1)+
  labs(caption = "Source: Author calculations using Michigan and Indiana CPS Food Security Supplement data from 2010 to 2021") +
  theme(plot.caption = element_text(hjust=0, size = 10))


animated_plot <- p +
  geom_point() +
  transition_reveal(YEAR)

anim_save("animated_graph1.gif", animation = animated_plot, renderer = gifski_renderer(loop = FALSE))

#Creating animated maps of Michigan and Indiana using food secure status from 2010 to 2021
library(usmap)
table1 <- 
dataFS %>%
  drop_na(FSSTATUS) %>%
  group_by(YEAR = as.numeric(YEAR), STATEFIP = as_factor(STATEFIP)) %>%
  summarize(FOOD_STATUS = 100*weighted.mean(FSSTATUS==1, FSSUPPWT, na.rm = TRUE)) 

#choosing michigan and indiana from map_data
state1 <- c("indiana", "michigan")
map <- map_data("state", region=state1)

#making the column lowercase to merge statefip and region
table1$STATEFIP <- tolower(table1$STATEFIP)

#renaming region column header to statefip so the columns can merge
states <- map %>% rename_at('region', ~'STATEFIP')

#merging columns
fs_map <- merge(states, table1, by = "STATEFIP")

#putting the order in the correct order so the states appear correctly
fs_map <- fs_map[order(fs_map$order), ]

#putting it into a ggplot
p <- ggplot(fs_map, aes(long, lat)) +
  geom_polygon(aes(group = group, fill=FOOD_STATUS)) +
  coord_map() +
  theme(panel.background = element_rect(fill = 'transparent'),
          plot.background = element_rect(fill = 'transparent', color = NA),
          panel.grid.major = element_blank()) 

animated_plot <- p +
  transition_states(YEAR, transition_length = 300,
                              state_length = 300) +
  labs(title = 'Year: {closest_state}')+

anim_save("animated_states.gif", animation = animated_plot, end_pause = 20, renderer = gifski_renderer())



