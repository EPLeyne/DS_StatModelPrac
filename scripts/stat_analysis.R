library(tidyverse)
library(emmeans)
library(lmerTest)

#linear model

## NULL HYPOTHESIS:
# There is no effect of element type on the speed of water infiltration into the soil
# Confounding factors is that there is an appearance that soil MC and soil BD have an influence on the water infiltration
#  rate and that there are differences between sites on those factors.

# Import the tidied data set

mfi <- read_csv('data/munged_data/MulligansFlatInfiltration_tidy.csv')

# Change columns to factors

mfi$siteId <- factor(mfi$siteId)
mfi$element <- factor(mfi$element)
mfi$discPotential <- factor(mfi$discPotential, levels = c('minus4cm','minus1cm','plus1cm'))


#These aren't woring for the logs
mfi_lm1 <- lmer(log(waterInfiltration.ml_min)~avgMC.percent, data = mfi)
anova(mfi_lm1)
mfi_lm2 <- lmer(log(waterInfiltration.ml_min)~element*siteId + (1|discPotential), mfi)
anova(mfi_lm2)
