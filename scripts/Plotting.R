library(tidyverse)

mfi <- read_csv('data/munged_data/MulligansFlatInfiltration_tidy.csv')


# Make the siteID and element columns factors (couldn't do it in pipe)
mfi$siteId <- factor(mfi$siteId)
mfi$element <- factor(mfi$element)
mfi$discPotential <- factor(mfi$discPotential, levels = c('minus4cm','minus1cm','plus1cm')) #Not sure about this!


### Element vs. water infiltration vs. disc potential
ggplot(mfi, aes(x = element, y = waterInfiltration.ml_min, colour = discPotential)) + 
  geom_boxplot()

ggplot(mfi, aes(x = factor(discPotential, level = disc_order), y = waterInfiltration.ml_min, colour = element)) + 
  geom_boxplot()
# Test the log of the water infiltration
ggplot(mfi, aes(x = element, y = log(waterInfiltration.ml_min), colour = discPotential)) + 
  geom_boxplot()

ggplot(mfi, aes(x =  discPotential, y = log(waterInfiltration.ml_min), colour = element)) + 
  geom_boxplot()

ggplot(mfi, aes(x = element, y = log(waterInfiltration.ml_min))) + 
  geom_boxplot()


## site ID vs. water infiltraion vs. disc potential
ggplot(mfi, aes(x = siteId, y = waterInfiltration.ml_min, colour = discPotential)) + 
  geom_boxplot() 

ggplot(mfi, aes(x = discPotential, y = waterInfiltration.ml_min, colour = siteId)) + 
  geom_boxplot()
# Test the log of the water infiltration
ggplot(mfi, aes(x = siteId, y = log(waterInfiltration.ml_min), colour = discPotential)) + 
  geom_boxplot()

ggplot(mfi, aes(x =  discPotential, y = log(waterInfiltration.ml_min), colour = siteId)) + 
  geom_boxplot()

ggplot(mfi, aes(x = siteId, y = log(waterInfiltration.ml_min))) + 
  geom_boxplot()



## Test the 3 variables (BD, MC and water infiltration)
ggplot(mfi, aes(avgMC.percent,avgBD.g_cm3)) + 
  geom_point(size = 4) +
  geom_smooth()

ggplot(mfi, aes(avgBD.g_cm3, avgMC.percent, colour = waterInfiltration.ml_min))+
  geom_point(size = 4) +
  geom_smooth() +
  scale_color_gradient(low="blue", high="red")

ggplot(mfi, aes(avgMC.percent, avgBD.g_cm3, colour = waterInfiltration.ml_min))+
  geom_point(size = 4) +
  geom_smooth() +
  scale_color_gradient(low="blue", high="red")
# Test the log of the water infiltration
ggplot(mfi, aes(avgMC.percent, avgBD.g_cm3, colour = log(waterInfiltration.ml_min)))+
  geom_point(size = 4) +
  geom_smooth() +
  scale_color_gradientn(colours = rainbow(5))

ggplot(mfi, aes(avgBD.g_cm3, log(waterInfiltration.ml_min), colour = avgMC.percent))+
  geom_point(size = 4)+
  geom_smooth()+
  scale_color_gradientn(colours = rainbow(5))
  
ggplot(mfi, aes(avgMC.percent, log(waterInfiltration.ml_min), colour = avgBD.g_cm3))+
  geom_point(size = 4)+
  geom_smooth()+
  scale_color_gradient(low="blue", high="red")

