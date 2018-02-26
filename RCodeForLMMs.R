### R code for linear mixed-effect models for PeerJ manuscript submission entitled:
### Stream acidification and reduced aquatic prey availability are associated with dietary shifts in an obligate riparian Neotropical migratory songbird
### Brian K. Trevelline1*, Tim Nuttle2, Brady A. Porter1, Nathan L. Brouwer3, Brandon D. Hoenig1, Zachary D. Steffensmeier1, and Steven C. Latta3
### 1 Department of Biological Sciences, Duquesne University, 600 Forbes Avenue, Pittsburgh, PA 15282
### 2 Civil and Environmental Consultants, Inc., 333 Baldwin Road, Pittsburgh, PA 15205
### 3 Department of Conservation and Field Research, National Aviary, 700 Arch Street, Pittsburgh, PA 15212
### *Corresponding author: Brian K. Trevelline; E-mail: btrevelline@gmail.com
########################################################################################################################################

library(lme4) #LOAD LME4 PACKAGE

############################################################### DIET MOTU RICHNESS VS. PH AND PROPORTION EPT (LOWA NESTLINGS AND ADULTS)
all.lowa.lmm <- read.csv("ALL_LOWA_LMM.csv") #READ IN DIET DATA FOR ALL LOWA (NESTLINGS AND ADULTS)

# DIET MOTU RICHNESS VS. PH (LOWA NESTLINGS AND ADULTS)
lmer.motus.ph.all.lowa.null <- lmer(MOTU_RICHNESS ~ 1 + (1|STREAM) + (1|NEST) + (1|NEST:VISIT), data = all.lowa.lmm, REML = FALSE) #NULL MODEL
lmer.motus.ph.all.lowa.model <- lmer(MOTU_RICHNESS ~ AVG_PH + (1|STREAM) + (1|NEST) + (1|NEST:VISIT), data = all.lowa.lmm, REML = FALSE) #MOTU RICHNESS VS. PH MODEL
anova(lmer.motus.ph.all.lowa.null, lmer.motus.ph.all.lowa.model) #ANOVA BETWEEN NULL AND PH MODEL

# DIET MOTU RICHNESS VS. PROPORTION EPT (LOWA NESTLINGS AND ADULTS)
lmer.motus.ept.all.lowa.null <- lmer(MOTU_RICHNESS ~ 1 + (1|STREAM) + (1|NEST) + (1|NEST:VISIT), data = all.lowa.lmm, REML = FALSE) #NULL MODEL
lmer.motus.ept.all.lowa.model <- lmer(MOTU_RICHNESS ~ PROPORTION_EPT + (1|STREAM) + (1|NEST) + (1|NEST:VISIT), data = all.lowa.lmm, REML = FALSE) #MOTU RICHNESS VS. PROPORTION EPT MODEL
anova(lmer.motus.ept.all.lowa.null, lmer.motus.ept.all.lowa.model) #ANOVA BETWEEN NULL AND EPT MODEL

# TERRESTRIAL DIET MOTUS VS. PH (LOWA NESTLINGS AND ADULTS)
lmer.terr.motus.ph.all.lowa.null <- lmer(TERRESTRIAL_MOTU_RICHNESS ~ 1 + (1|STREAM) + (1|NEST) + (1|NEST:VISIT), data = all.lowa.lmm, REML = FALSE) #NULL MODEL
lmer.terr.motus.ph.all.lowa.model <- lmer(TERRESTRIAL_MOTU_RICHNESS ~ AVG_PH + (1|STREAM) + (1|NEST) + (1|NEST:VISIT), data = all.lowa.lmm, REML = FALSE) # TERRESTRIAL MOTU RICHNESS VS. PH MODEL
anova(lmer.terr.motus.ph.all.lowa.null, lmer.terr.motus.ph.all.lowa.model) #ANOVA BETWEEN NULL AND PH MODEL

# TERRESTRIAL DIET MOTUS VS. PROPORTION EPT (LOWA NESTLINGS AND ADULTS)
lmer.terr.motus.ept.all.lowa.null <- lmer(TERRESTRIAL_MOTU_RICHNESS ~ 1 + (1|STREAM) + (1|NEST) + (1|NEST:VISIT), data = all.lowa.lmm, REML = FALSE) #NULL MODEL
lmer.terr.motus.ept.all.lowa.model <- lmer(TERRESTRIAL_MOTU_RICHNESS ~ PROPORTION_EPT + (1|STREAM) + (1|NEST) + (1|NEST:VISIT), data = all.lowa.lmm, REML = FALSE) # TERRESTRIAL MOTU RICHNESS VS. PROPORTION EPT MODEL
anova(lmer.terr.motus.ept.all.lowa.null, lmer.terr.motus.ept.all.lowa.model) #ANOVA BETWEEN NULL AND EPT MODEL

############# DIET MOTU RICHNESS VS. PH AND PROPORTION EPT (LOWA ADULTS ONLY)
adult.lowa.lmm <- read.csv("ADULT_LOWA_LMM.csv") #READ IN DIET DATA FOR LOWA ADULTS ONLY

# DIET MOTU RICHNESS VS. PH (LOWA ADULTS ONLY)
lmer.motus.ph.adult.lowa.null <- lmer(MOTU_RICHNESS ~ 1 + (1|STREAM) + (1|NEST), data = adult.lowa.lmm, REML = FALSE) #NULL MODEL
lmer.motus.ph.adult.lowa.model <- lmer(MOTU_RICHNESS ~ AVG_PH + (1|STREAM) + (1|NEST), data = adult.lowa.lmm, REML = FALSE) #MOTU RICHNESS VS. PH MODEL
anova(lmer.motus.ph.adult.lowa.null, lmer.motus.ph.adult.lowa.model) #ANOVA BETWEEN NULL AND PH MODEL

#DIET MOTU RICHNESS VS. PROPORTION EPT (LOWA ADULTS ONLY)
lmer.motus.ept.adult.lowa.null <- lmer(MOTU_RICHNESS ~ 1 + (1|STREAM) + (1|NEST), data = adult.lowa.lmm, REML = FALSE) #NULL MODEL
lmer.motus.ept.adult.lowa.model <- lmer(MOTU_RICHNESS ~ PROPORTION_EPT + (1|STREAM) + (1|NEST), data = adult.lowa.lmm, REML = FALSE) #MOTU RICHNESS VS. PROPORTION EPT MODEL
anova(lmer.motus.ept.adult.lowa.null, lmer.motus.ept.adult.lowa.model) #ANOVA BETWEEN NULL AND EPT MODEL

############# DIET MOTU RICHNESS VS. PH AND PROPORTION EPT (LOWA NESTLINGS ONLY)

#DIET MOTU RICHNESS VS. PH (LOWA NESTLINGS ONLY)
nestling.lowa.lmm <- read.csv("NESTLING_LOWA_LMM.csv") #LOAD IN DIET DATA FOR LOWA NESTLINGS ONLY
lmer.motus.ph.nestling.lowa.null <- lmer(MOTU_RICHNESS ~ 1 + (1|STREAM) + (1|NEST) + (1|NEST:VISIT), data = nestling.lowa.lmm, REML = FALSE) #NULL MODEL
lmer.motus.ph.nestling.lowa.model <- lmer(MOTU_RICHNESS ~ AVG_PH + (1|STREAM) + (1|NEST) + (1|NEST:VISIT), data = nestling.lowa.lmm, REML = FALSE) #MOTU RICHNESS VS. PH MODEL
anova(lmer.motus.ph.nestling.lowa.null, lmer.motus.ph.nestling.lowa.model) #ANOVA BETWEEN NULL AND PH MODEL

#DIET MOTU RICHNESS VS. PROPORTION EPT (LOWA NESTLINGS ONLY)
lmer.motus.ept.nestling.lowa.null <- lmer(MOTU_RICHNESS ~ 1 + (1|STREAM) + (1|NEST) + (1|NEST:VISIT), data = nestling.lowa.lmm, REML = FALSE) #NULL MODEL
lmer.motus.ept.nestling.lowa.model <- lmer(MOTU_RICHNESS ~ PROPORTION_EPT + (1|STREAM) + (1|NEST) + (1|NEST:VISIT), data = nestling.lowa.lmm, REML = FALSE) #MOTU RICHNESS VS. PROPORTION EPT MODEL
anova(lmer.motus.ept.nestling.lowa.null, lmer.motus.ept.nestling.lowa.model) #ANOVA BETWEEN NULL AND EPT MODEL

############################################################### LEVINS NICHE BREADTH  VS. PH AND PROPORTION EPT (LOWA NESTLINGS AND ADULTS)
lowa.nests.lmm <- read.csv("LOWA_NESTS_LMM.csv") #READ IN DIET DATA FOR ALL LOWA (LOWA NESTLINGS AND ADULTS)

#LEVINS NICHE BREADTH VS. PH (LOWA NESTLINGS AND ADULTS)
lmer.levins.ph.lowa.nests.null <- lmer(LEVINS ~ 1 + (1|STREAM) + (1|INCLUDES_ADULTS), data = lowa.nests.lmm, REML = FALSE) # NULL MODEL WITH RANDOM EFFECT FOR STREAM AND IF NEST GROUP INCLUDED ADULTS
lmer.levins.ph.lowa.nests.model <- lmer(LEVINS ~ AVG_PH + (1|STREAM) + (1|INCLUDES_ADULTS), data = lowa.nests.lmm, REML = FALSE) # LEVINS NICHE BREADTH VS. PH MODEL
anova(lmer.levins.ph.lowa.nests.null, lmer.levins.ph.lowa.nests.model)  #ANOVA BETWEEN NULL AND PH MODEL

#LEVINS NICHE BREADTH VS. PROPORTION EPT (LOWA NESTLINGS AND ADULTS)
lmer.levins.ept.lowa.nests.null <- lmer(LEVINS ~ 1 + (1|STREAM) + (1|INCLUDES_ADULTS), data = lowa.nests.lmm, REML = FALSE) # NULL MODEL WITH RANDOM EFFECT FOR STREAM AND IF NEST GROUP INCLUDED ADULTS
lmer.levins.ept.lowa.nests.model <- lmer(LEVINS ~ PROPORTION_EPT + (1|STREAM) + (1|INCLUDES_ADULTS), data = lowa.nests.lmm, REML = FALSE) # LEVINS NICHE BREADTH VS. PROPORTION EPT MODEL
anova(lmer.levins.ept.lowa.nests.null, lmer.levins.ept.lowa.nests.model) #ANOVA BETWEEN NULL AND EPT MODEL

############################################################### DIET MOTU RICHNESS VS. PH AND PROPORTION EPT (ACFL NESTLINGS)
acfl.nestlings.lmm <- read.csv("ACFL_NESTLINGS_LMM.csv") #READ IN DIET DATA FOR ACFL NESTLINGS

########################################################################## ACFL DIET MOTU RICHNESS VS. PH 
lmer.acfl.motus.ph.null <- lmer(MOTU_RICHNESS ~ 1 + (1|STREAM) + (1|NEST) , data = acfl.nestlings.lmm, REML = FALSE) #NULL MODEL
lmer.acfl.motus.ph.model <- lmer(MOTU_RICHNESS ~ AVG_PH + (1|STREAM) + (1|NEST), data = acfl.nestlings.lmm, REML = FALSE) #MOTU RICHNESS VS. PH MODEL
anova(lmer.acfl.motus.ph.null, lmer.acfl.motus.ph.model) #ANOVA BETWEEN NULL AND PH MODEL

########################################################################## ACFL DIET MOTU RICHNESS VS. PROPORTION EPT

lmer.acfl.motus.ept.null <- lmer(MOTU_RICHNESS ~ 1 + (1|STREAM) + (1|NEST) , data = acfl.nestlings.lmm, REML = FALSE) #NULL MODEL
lmer.acfl.motus.ept.model <- lmer(MOTU_RICHNESS ~ PROPORTION_EPT + (1|STREAM) + (1|NEST), data = acfl.nestlings.lmm, REML = FALSE) #MOTU RICHNESS VS. PROPORTION EPT MODEL
anova(lmer.acfl.motus.ept.null, lmer.acfl.motus.ept.model) #ANOVA BETWEEN NULL AND EPT MODEL

############################################################### DIET MOTU RICHNESS VS. PH AND PROPORTION EPT (WOTH NESTLINGS)
woth.nestlings.lmm <- read.csv("WOTH_NESTLINGS_LMM.csv") #READ IN DIET DATA FOR WOTH NESTLINGS

########################################################################## WOTH DIET MOTU RICHNESS VS. PH 
lmer.woth.motus.ph.null <- lmer(MOTU_RICHNESS ~ 1 + (1|STREAM) + (1|NEST) , data = woth.nestlings.lmm, REML = FALSE) #NULL MODEL
lmer.woth.motus.ph.model <- lmer(MOTU_RICHNESS ~ AVG_PH + (1|STREAM) + (1|NEST), data = woth.nestlings.lmm, REML = FALSE) #MOTU RICHNESS VS. PH MODEL
anova(lmer.woth.motus.ph.null, lmer.woth.motus.ph.model) #ANOVA BETWEEN NULL AND PH MODEL

########################################################################## WOTH DIET MOTU RICHNESS VS. PROPORTION EPT
lmer.woth.motus.ept.null <- lmer(MOTU_RICHNESS ~ 1 + (1|STREAM) + (1|NEST) , data = woth.nestlings.lmm, REML = FALSE) #NULL MODEL
lmer.woth.motus.ept.model <- lmer(MOTU_RICHNESS ~ PROPORTION_EPT + (1|STREAM) + (1|NEST), data = woth.nestlings.lmm, REML = FALSE) #MOTU RICHNESS VS. PROPORTION EPT MODEL
anova(lmer.woth.motus.ept.null, lmer.woth.motus.ept.model) #ANOVA BETWEEN NULL AND EPT MODEL

### END
