library(tidyverse)

mfi <- read_csv('data/munged_data/MulligansFlatInfiltration_tidy.csv')


# Make the siteID and element columns factors (couldn't do it in pipe)
mfi$siteId <- factor(mfi$siteId)
mfi$element <- factor(mfi$element)
mfi$discPotential <- factor(mfi$discPotential, levels = c('minus4cm','minus1cm','plus1cm'))


### Element vs. water infiltration vs. disc potential
#plot 1
# Shows the correlation of discPotential on water infiltration (expected)
ggplot(mfi, aes(x = element, y = waterInfiltration.ml_min, colour = discPotential)) + 
  geom_boxplot()

#plot 2
# Shows the possibility of some influence of element on water infiltration
ggplot(mfi, aes(x = factor(discPotential, level = disc_order), y = waterInfiltration.ml_min, colour = element)) + 
  geom_boxplot()

# Due to the difference in discPotential need to test the log of the water infiltration
#plot 3
# Still shows the influence of discPotential but may be some element factor, lower number of 
#  data points for some elements
ggplot(mfi, aes(x = element, y = log(waterInfiltration.ml_min), colour = discPotential)) + 
  geom_boxplot()+
  geom_point(position = position_dodge(width=0.75))

#plot 4
# Less obvious there are between element differences
ggplot(mfi, aes(x =  discPotential, y = log(waterInfiltration.ml_min), colour = element)) + 
  geom_boxplot() +
  geom_point(position = position_dodge(width=0.75))

#plot 5
# Showing no difference between the elements
ggplot(mfi, aes(x = element, y = log(waterInfiltration.ml_min))) + 
  geom_boxplot()

ggplot (mfi, aes(element, log(waterInfiltration.ml_min))) +
  geom_point() +
  facet_wrap(~siteId)

ggplot (mfi, aes(avgMC.percent, avgBD.g_cm3)) +
  geom_point() +
  facet_wrap(~siteId)

## site ID vs. water infiltration vs. disc potential

#plot 6
# Could be site factors, influence of discPotential obvious except in 2 sites
ggplot(mfi, aes(x = siteId, y = log(waterInfiltration.ml_min), colour = discPotential)) + 
  geom_boxplot() +
  geom_point(position = position_dodge(width=0.75))

#plot 7
# Shows site factors but crudely
ggplot(mfi, aes(x = siteId, y = log(waterInfiltration.ml_min))) + 
  geom_boxplot()

#plot 8
# Shows more clearly site factors
ggplot(mfi, aes(x =  discPotential, y = log(waterInfiltration.ml_min), colour = siteId)) + 
  geom_boxplot()+
  geom_point(position = position_dodge(width=0.75))

# plot 9
# Shows clear differences between the MC and BD between sites
ggplot (mfi, aes(avgMC.percent, avgBD.g_cm3)) +
  geom_point() +
  facet_wrap(~siteId)
  

## Test the 3 variables (BD, MC and water infiltration)

# plot 11
# No overall correlation between BD and MC
ggplot(mfi, aes(avgMC.percent,avgBD.g_cm3)) + 
  geom_point(size = 4) +
  geom_smooth()

# plot 12
# Appears that low MC, high BD leads to lower water infiltration
ggplot(mfi, aes(avgBD.g_cm3, avgMC.percent, colour = log(waterInfiltration.ml_min)))+
  geom_point(size = 4) +
  geom_smooth()  +
  scale_color_gradientn(colours = rainbow(5))

# plot 13
# Teases out previous plot, shows some loose increase in water Infil as MC increses but plots are all over the place
ggplot(mfi, aes(avgMC.percent, log(waterInfiltration.ml_min)))+
  geom_point(size = 4)+
  geom_smooth()

# plot 14
# Shows no real correlation between waterInfil increase and BD
ggplot (mfi, aes(avgBD.g_cm3, log(waterInfiltration.ml_min))) +
  geom_point() +
  geom_smooth()

# Test BC vs element vs infiltration
# plot 15
# Possible correlation between element and BD but....
ggplot(mfi, aes(element, avgBD.g_cm3, colour = log(waterInfiltration.ml_min))) + 
  geom_point(size=4)+
  scale_color_gradientn(colours = rainbow(5))
#  .. the second plots shows that the site is a confounding factor
ggplot(mfi, aes(element, avgBD.g_cm3, colour = siteId)) + 
  geom_point(size=4)

# Test MC vs element vs infiltration
# plot 16 & 17
# first plot initiall seems to show a mild correlation between the MC and the element but..
ggplot(mfi, aes(element, avgMC.percent, colour = log(waterInfiltration.ml_min))) + 
  geom_point(size=4)+
  scale_color_gradientn(colours = rainbow(5))
#  .. the second plots shows that the site is a confounding factor
ggplot(mfi, aes(element, avgMC.percent, colour = siteId)) + 
  geom_point(size=4)

