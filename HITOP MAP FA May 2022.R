require(psych)
require(tidyverse)
require(lm.beta)
require(DescTools)
require(ggplot2)


# Check on missing data

map %>%
  select(PQ1:PQ94, PRIME1:PRIME12, PROD_1:PROD_21,
         DES_1:DES_14, CESD_1:CESD_14, 
         STAI_1:STAI_7, SPS_1:SPS_20, 
         GBI_1:GBI_10, 
         DUF11:DUF_22,
         -ends_with("a")) %>%
  CountCompCases()
#  PlotMiss()
  

# Parallel analysis
map %>%
  select(PQ1:PQ94, PRIME1:PRIME12, PROD_1:PROD_21,
         DES_1:DES_14, CESD_1:CESD_14, 
         STAI_1:STAI_7, SPS_1:SPS_20, 
         GBI_1:GBI_10, 
         DUF11:DUF_22,
         -ends_with("a")) %>%
fa.parallel(quant = .95)


# Run the bass-ackwards analysis

vfas <- list()
for(i in 1:15){
  fa(select(map, 
            PQ1:PQ94, PRIME1:PRIME12, PROD_1:PROD_21,
            DES_1:DES_14, CESD_1:CESD_14, 
            STAI_1:STAI_7, SPS_1:SPS_20, 
            GBI_1:GBI_10, 
            DUF11:DUF_22,
            -ends_with("a")), 
     nfactors = i, rotate = "varimax", missing = TRUE, impute = "median") -> vfas[[i]]
}



# Build a list of regression models predicting outcomes of interest 
# from factor scores at levels 1 - 15
# This loop also extracts Adj. R2 for plots

mr_list <- c("MR1", "MR2", "MR3", "MR4", "MR5", "MR6", 
             "MR7", "MR8", "MR9", "MR10", "MR11", "MR12",
             "MR13", "MR14", "MR15")
vlms <- list()
vr2s <- list()

for(v in c("SIPS_CHR", "sips_pos", "pins_total")){
  for(i in 1:12){
    vlms[[v]][[i]] <- map %>%
      cbind(vfas[[i]]$scores) %>%
      lm(paste(v, " ~ ", paste(mr_list[1:i], collapse = "+")), data = ., 
         na.action = na.exclude) 
    vr2s[[v]][[i]] <- summary(vlms[[v]][[i]])$adj.r.squared
  }
}


# Test improvements in model fit at various factor levels

do.call(anova, vlms$SIPS_CHR)
do.call(anova, vlms$sips_pos)
do.call(anova, vlms$pins_total)


# Follow-up test re: poential spurious improvement in fit

anova(vlms$sips_pos[[7]], vlms$sips_pos[[9]])



# Supplemental analysis: PINS negatively predicted by substance use
# Which PINS symptoms drove this effect?

map %>%
  cbind(vfas[[9]]$scores) %>%
  select(MR8, pins_1:pins_16) %>%
  corr.test()

# Significant negative correlations with Asociality (behaviour and experience),
# blunted facial affect, body gestures, and alogia (PINS 6, 7, 13, 15, and 16). 
# I.e., people who use more substances are more social, talkative, and animated.


