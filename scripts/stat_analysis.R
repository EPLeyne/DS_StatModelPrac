library(tidyverse)
library(emmeans)
library(lmerTest)
library(IDPmisc)


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
mfi$log_wI <- log(mfi$waterInfiltration.ml_min)

# Model water Infiltration rate against MC% --- significant
mfi_lm1 <- lm(log_wI ~ avgMC.percent, data = mfi)
anova(mfi_lm1)
summary(mfi_lm1)
emmeans(mfi_lm1, pairwise~avgMC.percent)

# Model water Infiltration rate against BD --- not significant
mfi_lm2 <- lm(log_wI ~ avgBD.g_cm3, data = mfi)
anova(mfi_lm2)
summary(mfi_lm2)

mfi_lm1000 <- lm(log_wI ~ avgBD.g_cm3 * avgMC.percent, data = mfi)
anova(mfi_lm1000)


#Test if correlation between MC and BD --- significant
mfi_lm9 <- lm(avgMC.percent~avgBD.g_cm3 + siteId, mfi)
anova(mfi_lm9)
summary(mfi_lm9)

# Model water Infiltration rate against site ID as a proxy for the diffent soil conditions -- significant
mfi_lm3 <- lm(log_wI ~ siteId, data = mfi)
anova(mfi_lm3)
summary(mfi_lm3)

# Model water Infiltration rate against site ID & soil conditions (additive) --- site ID significant
mfi_lm4 <- lm(log_wI ~ siteId + avgMC.percent + avgBD.g_cm3, data = mfi)
anova(mfi_lm4)
summary(mfi_lm4)

# Model water Infiltration rate against element --- not significant
mfi_lm6 <- lm(log_wI ~ element, data = mfi)
anova(mfi_lm6)
summary(mfi_lm6)

# Model water Infiltration rate against site ID and element --- site ID significant
mfi_lm5 <- lm(log_wI ~ siteId * element, data = mfi)
anova(mfi_lm5)
summary(mfi_lm5)

# Model water Infiltration rate against site ID and element with the disc potential blocking --- site ID significant
mfi_lm7 <- lmer(log_wI ~ element * siteId + (1|discPotential), mfi)
anova(mfi_lm7)
summary(mfi_lm7)

# Model water infilatration against all variables --- site ID significant
mfi_lm8 <- lmer(log_wI~element*(siteId + avgBD.g_cm3 + avgMC.percent) + (1|discPotential), mfi)
anova(mfi_lm8)
summary(mfi_lm8)


