library(tidyverse)
library(janitor)

## Import the data

mfi_tidy <- read_csv('data/raw_data/MulligansFlatInfiltration.csv', skip = 5) %>% # Read the data and remove first five blank lines
  clean_names(case = 'lower_camel') %>% # clean column names to lower camel
  select(-c('x14','x15')) %>% # remove 2 columns that contain no data  
  drop_na() %>% # Drop last line that was just NA values
  select(-c('siteNo','elementNo','date', 'averageBulkDensityGCm3')) %>%  # remove date and columns that are unnessesary as the IDs will be factors
  mutate(avgBD.g_cm3 = ((bulkDensity1GCm3 + bulkDensity2GCm3)/2)) %>% #Create columns of avaerages for the soil bulk density
  mutate(avgMC.percent = (bd1PercentMoistureContent + bd2PercentMoistureContent)/2) %>% # & soil moisture content
  select(-c('bulkDensity1GCm3', 'bulkDensity2GCm3', 'bd1PercentMoistureContent', 'bd2PercentMoistureContent')) %>% 
  rename (minus4cm = atPotential4CmSteadyStateInfiltrationRateMlMin,
          minus1cm = atPotential1CmSteadyStateInfiltrationRateMlMin,
          plus1cm = atPotential1CmSteadyStateInfiltrationRateMlMin_2) %>%
  gather(key = 'discPotential', value = 'waterInfiltraion.ml_min', c('minus4cm','minus1cm','plus1cm'))

# Make the siteID and element columns factors (couldn't do it in pipe)
mfi_tidy$siteId <- as.factor(mfi_tidy$siteId)
mfi_tidy$element <- as.factor(mfi_tidy$element)
mfi_tidy$discPotential <- as.factor(mfi_tidy$discPotential) #Not sure about this!

str(mfi_tidy)

write_csv(mfi_tidy, path = 'data/munged_data/MulligansFlatInfiltration_tidy.csv')

ggplot(mfi_tidy, aes(x = siteId, y = g_cm3, colour = element))+
  geom_boxplot()
ggplot(mfi_tidy, aes(x = element, y = g_cm3, colour = siteId))+
  geom_boxplot()
