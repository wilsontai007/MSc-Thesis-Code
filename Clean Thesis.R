library(haven)
library(readxl)
library(dplyr)
library(tidyr)
library(stargazer)
library(lmtest)
library(car)
library(plm)
library(car)
library(writexl)
library(ggplot2)
library(corrplot)
library(ivreg)
library(tidyverse)
library(lubridate)
library(tsibble)
library(ggrepel)
library(sandwich)
library(gdata)
library(foreign)
View(patent_full)
keep(patent_full, result, sure = T)


patent_full <- read_dta("/Users/wilsontai/Downloads/fullpat_updated_version.dta", 
                        col_select = c(1,6,7, 25,27,  29, 34,35)) ##15 min to run
head(patent_full)
colnames(patent_full)[1] = "ID"
colnames(patent_full)[2] = "Year"
colnames(patent_full)[5] = "Country"
n_distinct(patent_full$ID)

##Years
patent_full <- patent_full[patent_full$Year >= 2000, ] 
patent_full <- patent_full[patent_full$Year <= 2017, ] ##now 56,101,135

##fractional
patent_full <- patent_full %>% add_count(ID,name = 'n') %>% 
  add_count(ID,Country, name = 'gn') %>% 
  mutate(Countryfrac=gn/n) %>% select(-c(n,gn))
patent_full <- patent_full  %>% 
  add_count(ID, invt_seq_nr, name = 'n') %>% 
  add_count(ID,invt_seq_nr, cpc4,  name = 'gn') %>% 
  mutate(techfrac=gn/n) %>% select(-c(n,gn))

patent_full$newfrac <- patent_full$Countryfrac*patent_full$techfrac ## 56,101,135


##EU countries
countries_to_keep = c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE","FI", "NL",
                      "FR", "DE", "GR", "HU", "IE","IT", "LV", "LT", "LU","MT",
                      "PL", "PT", "RO", "SK", "SI", "ES", "SE")
result = patent_full[patent_full$Country %in% countries_to_keep, ] ## 16,388,811
patent_full = result
sort(unique(patent_full$Country))
n_distinct(patent_full$ID)

##creating new variables
patent_full$quantity <- rep(1:1, each = nrow(patent_full))
patent_full$qualfrac  <- patent_full$appfwcit*patent_full$newfrac
patent_full$quantfrac  <- patent_full$quantity*patent_full$newfrac
patent_full$backwardfrac <- patent_full$appbwcit*patent_full$newfrac





##technology class
patent_full <- as.data.frame(patent_full)
patent_full <- patent_full[!grepl('NA', patent_full$cpc4),] ## 4,755,450
n_distinct(patent_full$ID) ##946825


n_distinct(patent_full$ID)


##Omitting duplicates
patent_full = patent_full[!(duplicated(patent_full[c('Country','ID', 'cpc4')]) | 
                              duplicated(patent_full[c('Country','ID', 'cpc4')]) | 
                              duplicated(patent_full[c('Country','ID', 'cpc4')])), ]  ## 2,037,835 obs


Malta <- patent_full[grepl('MT', patent_full$Country),]
n_distinct(Malta$ID)
patent_full <- patent_full[!grepl('MT', patent_full$Country),]
patent_full[is.na(patent_full)] <- 0


attach(patent_full)

##checking they are EU countries
patent_full <- as.data.frame(patent_full)

##cannot import to excel: max lim for excel is 1million rows
str(patent_full)
patent_full %>% 
  summarise(across(everything(), ~ sum(is.na(.))))

patent_full <- patent_full %>%
  drop_na()

##########################################
##putting corporate tax data into new data
##note: corporate data was from OECD, all exccept cyprus, which was from 'trading economics'
Corporate <- read_excel("/Users/wilsontai/Downloads/Corporate Tax (OECD DATA).xlsx")  
Corporate$Year <- as.numeric(Corporate$Year)

patent_full <- Corporate %>% 
  pivot_longer(cols = -Year, names_to = 'Country', values_to = 'CIT') %>%
  left_join(patent_full, .)
patent_full$CIT <- patent_full$CIT/100

GDPPerCap <- read_excel("/Users/wilsontai/Downloads/GDPperCapita.xlsx")
str(GDPPerCap)

patent_full <- GDPPerCap %>% 
  pivot_longer(cols = -Year, names_to = 'Country', values_to = 'GDPPerCap') %>%
  left_join(patent_full, .)

GDP <- read_excel("/Users/wilsontai/Downloads/GDP (WORLD BANK).xlsx")
str(GDP)
patent_full <- GDP %>% 
  pivot_longer(cols = -Year, names_to = 'Country', values_to = 'GDP') %>%
  left_join(patent_full, .)

##
RandD_to_GDP <- read_excel("/Users/wilsontai/Downloads/R&D to GDP.xlsx")
patent_full <- RandD_to_GDP %>% 
  pivot_longer(cols = -Year, names_to = 'Country', values_to = 'RandD') %>%
  left_join(patent_full, .)
patent_full$RandD = patent_full$RandD/100

##
Population_Growth <- read_excel("/Users/wilsontai/Downloads/Population Growth (WORLD BANK).xlsx")
patent_full <- Population_Growth %>% 
  pivot_longer(cols = -Year, names_to = 'Country', values_to = 'Population_Growth') %>%
  left_join(patent_full, .)

##
Num_research <- read_excel("/Users/wilsontai/Downloads/R&D researchers (per million people).xlsx")
patent_full <- Num_research %>% 
  pivot_longer(cols = -Year, names_to = 'Country', values_to = 'Researchers') %>%
  left_join(patent_full, .)

library(readxl)
ETR <- read_excel("/Users/wilsontai/Downloads/MTR.xlsx")
ETR$BG <- as.numeric(ETR$BG)
ETR$LV <- as.numeric(ETR$LV)
ETR$LU <- as.numeric(ETR$LU)
ETR$MT <- as.numeric(ETR$MT)
colnames(ETR)[28] = "SE"
patent_full <- ETR %>% 
  pivot_longer(cols = -Year, names_to = 'Country', values_to = 'ETR') %>%
  left_join(patent_full, .)

Trade_Imports <- read_excel("/Users/wilsontai/Downloads/Trade_Imports.xlsx")
patent_full <- Trade_Imports %>% 
  pivot_longer(cols = -Year, names_to = 'Country', values_to = 'Trade_Imports') %>%
  left_join(patent_full, .)
patent_full$Trade_Imports = patent_full$Trade_Imports/100


Trade_Exports <- read_excel("/Users/wilsontai/Downloads/Trade_Exports.xlsx")
patent_full <- Trade_Exports %>% 
  pivot_longer(cols = -Year, names_to = 'Country', values_to = 'Trade_Exports') %>%
  left_join(patent_full, .)
patent_full$Trade_Exports = patent_full$Trade_Exports/100


patent_full$Openess <- patent_full$Trade_Imports + patent_full$Trade_Exports 

College <- read_excel("/Users/wilsontai/Downloads/Tertiary_Enrollment.xlsx")
patent_full <- College %>% 
  pivot_longer(cols = -Year, names_to = 'Country', values_to = 'College') %>%
  left_join(patent_full, .)



########


EMTR <- read_excel("/Users/wilsontai/Downloads/B-Index1.xlsx")
patent_full <- EMTR %>% 
  pivot_longer(cols = -Year, names_to = 'Country', values_to = 'EMTR') %>%
  left_join(patent_full, .)
########

####
patent_full %>% 
  summarise(across(everything(), ~ sum(is.na(.))))

patent_full <- patent_full[!grepl('MT', patent_full$Country),] ##  1,360,948

#####check
None <- patent_full[is.na(patent_full$Openness), ]
View(None)
patent_full <- patent_full %>%
  drop_na()
View(patent_full)
#######

plot(country_patent$EMTR, country_patent$CIT)


##country level
attach(patent_full)
country_patent <- patent_full %>%
  group_by(Year, Country) %>%
  summarize(qualfrac = sum(qualfrac),
            quantfrac = sum(quantfrac),
            ETR = ETR,
            CIT = CIT,
            GDP = GDP,
            GDPPerCap = GDPPerCap,
            Population_Growth = Population_Growth,
            RandD = RandD,
            Researchers = Researchers,
            Openness = Openess,
            College = College)##missing malta
country_patent <- country_patent[!(duplicated(country_patent[c('qualfrac','quantfrac')]) | 
                                     duplicated(country_patent[c('qualfrac','quantfrac')])), ] 
country_patent <- as.data.frame(country_patent)
country_patent$lnETR = log(country_patent$ETR)
country_patent$lnPop_Growth = log(country_patent$Population_Growth)
country_patent$lnPop_Growth[is.nan(country_patent$lnPop_Growth)] <- 0
country_patent$lnGDPPerCap = log(country_patent$GDPPerCap)
country_patent$lnRandD = log(country_patent$RandD)
country_patent$lnResearchers = log(country_patent$Researchers)
country_patent$lnOpenness = log(country_patent$Openness)
country_patent$lnCollege = log(country_patent$College)

write_xlsx(country_patent ,"/Users/wilsontai/Downloads/Country Patent ETR.xlsx")
View(country_patent)


stargazer(country_patent[c("Year", "quantfrac","qualfrac",
                           "lnETR",
                           "lnGDPPerCap",
                           "lnPop_Growth", "lnRandD", "lnOpenness",
                           "lnResearchers", "lnCollege")],
          type = "latex",
          covariate.labels=c("Year", "Quantity", "Quality",
                             "ETR", "GDPPerCap","PopulationGrowth","R&D-GDP", "Openness",
                             "Researchers", "College"),
          summary.stat = c("n", "mean", "sd", "min", "max"), digits = 2)



##Plots
##truncation bias
x_quantiles <- quantile(patent_full$appfwcit, c(0.05, 0.95))  # Calculate 5th & 95th percentiles
x_quantiles
top5 <- patent_full[patent_full$appfwcit >= x_quantiles[2], ]

bottom95 <- patent_full[patent_full$appfwcit < x_quantiles[2], ]

All <- patent_full %>% group_by(Year) %>% summarise(sum = sum(appfwcit))
Bottom5 <- bottom95 %>% group_by(Year) %>% summarise(sumbottom = sum(appfwcit))
Top5 <- top5 %>% group_by(Year) %>% summarise(sumtop = sum(appfwcit))


All$bottom <- Bottom5$sumbottom
All <- All[-18,]
All$top <- Top5$sumtop


cols = c("Bottom 95%" = "blue", "Top 5%" = "blue", "All" = "blue")
linemethod = c("Bottom 95%" = "dashed", "Top 5%" = "dotted", "All" = "dashed")

ggplot(All, aes(x = Year)) + 
  geom_line(aes(y = log(sum), color = "All")) +
  geom_line(aes(y = log(top), color = "Top 5%", linetype = "twodash")) +
  geom_line(aes(y = log(bottom), color = "Bottom 95%", linetype = "dashed")) +
  scale_colour_manual(name="Legend",values=cols) +
  theme_bw() + ylim(6.3, 12.75) +
  labs(x="Year",
       y="log(Forward Citations)", title= "Mean Forward Citations: 2000 - 2017")


attach(All)

d = data.frame(sum = sum, top = top, bottom = bottom, Year = Year,
               Type=c(rep("All", length(sum)), rep("Top 5%", length(top)), 
                      rep("Bottom 95%", length(bottom)) ))

ggplot(d, aes(x = Year, color = "blue", linetype = Type)) +
  geom_line(aes(y = log(sum)), color = "blue") +
  geom_line(aes(y =log(top)), color = "blue", linetype = 2) +
  geom_line(aes(y = log(bottom)), color = "blue", linetype = 3) +
  scale_linetype_manual(values = c(1, 2,3),
                        labels = c("All", "Top 5%", "Bottom 95%")) +
  scale_color_manual(values = c("blue", "blue", "blue"),
                     labels = c("All", "Top 5%", "Bottom 95%")) +
  theme_bw() + ylim(6.3, 12.75) +
  labs(x="Year",
       y="log(Forward Citations)", title= "Mean Forward Citations: 2000 - 2017")





###Data Analysis
dataAnalysis = patent_full[!(duplicated(patent_full[c("ID")])), ]
View(dataAnalysis)
nrow(dataAnalysis[dataAnalysis$granted == 'N', ])
nrow(dataAnalysis[dataAnalysis$appfwcit >= 7, ])
nrow(dataAnalysis[dataAnalysis$appfwcit == 0, ])
nrow(dataAnalysis[dataAnalysis$appfwcit == 0 & dataAnalysis$Year < 2009, ])
nrow(dataAnalysis[dataAnalysis$appbwcit == 0, ])
nrow(dataAnalysis[dataAnalysis$appbwcit == 0 & dataAnalysis$granted == 'N', ])

