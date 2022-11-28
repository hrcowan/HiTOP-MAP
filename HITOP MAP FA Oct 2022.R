require(psych)
require(tidyverse)
require(lm.beta)
require(DescTools)
require(ggplot2)

# Read dataset
map <- readRDS("MAP HiTOP dataset.rds")

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
map_pa <- map %>%
  select(PQ1:PQ94, PRIME1:PRIME12, PROD_1:PROD_21,
         DES_1:DES_14, CESD_1:CESD_14, 
         STAI_1:STAI_7, SPS_1:SPS_20, 
         GBI_1:GBI_10, 
         DUF11:DUF_22,
         -ends_with("a")) %>%
  fa.parallel(quant = .95, n.obs = 3460, fa = "pc")


# Run the bass-ackwards analysis
bass_10 <- map %>%
  select(PQ1:PQ94, PRIME1:PRIME12, PROD_1:PROD_21,
         DES_1:DES_14, CESD_1:CESD_14, 
         STAI_1:STAI_7, SPS_1:SPS_20, 
         GBI_1:GBI_10, 
         DUF11:DUF_22,
         -ends_with("a")) %>%
  bassAckward(nfactors = 10, rotate = "varimax")
write_rds(bass_10, "bass_10.rds")

# Calculate separate EFAs to double check bassAckward findings
# and generate factor scores
vfas <- list()
for(i in 1:10){
  vfas[[i]] <- 
    map %>%
    select(PQ1:PQ94, PRIME1:PRIME12, PROD_1:PROD_21,
           DES_1:DES_14, CESD_1:CESD_14, 
           STAI_1:STAI_7, SPS_1:SPS_20, 
           GBI_1:GBI_10, 
           DUF11:DUF_22,
           -ends_with("a")) %>%
    fa(nfactors = i, rotate = "varimax")
}

write_rds(vfas, "vfas.rds")


# Build a list of regression models predicting outcomes of interest 
# from factor scores at levels 1 - 10
# This loop also extracts Adj. R2 for plots

mr_list <- c("MR1", "MR2", "MR3", "MR4", "MR5", "MR6", 
             "MR7", "MR8", "MR9", "MR10")
vlms_436 <- list()
vr2s_436 <- list()

for(v in c("SIPS_CHR", "sips_pos", "pins_total")){
  for(i in 1:10){
    vlms_436[[v]][[i]] <- map %>%
      cbind(vfas[[i]]$scores) %>%
      filter(pops == 0 | is.na(pops)) %>%
      lm(paste(v, " ~ ", paste(mr_list[1:i], collapse = "+")), data = ., 
        na.action = na.exclude) 
    vr2s_436[[v]][[i]] <- summary(vlms_436[[v]][[i]])$adj.r.squared
  }
}

write_rds(vlms_436, "vlms_436.rds")
write_rds(vr2s_436, "vr2s_436.rds")


# Test improvements in model fit at various factor levels

do.call(anova, vlms_436$SIPS_CHR)
do.call(anova, vlms_436$sips_pos)
do.call(anova, vlms_436$pins_total)


# Same loop with logistic regression for CHR status
vlms_436_log <- list()
vr2s_436_log <- list()

for(i in 1:10){
  vlms_436_log[[i]] <- map %>%
    cbind(vfas[[i]]$scores) %>%
    filter(pops == 0 | is.na(pops)) %>%
    glm(paste("SIPS_CHR ~ ", paste(mr_list[1:i], collapse = "+")), data = ., 
        na.action = na.exclude, family = binomial(link = "logit")) 
  vlms_436_log[[i]]$or <- exp(coef(vlms_436_log[[i]]))
  vr2s_436_log[[i]] <- pscl::pR2(vlms_436_log[[i]])[[4]]
}

write_rds(vlms_436_log, "vlms_436_log.rds")
write_rds(vr2s_436_log, "vr2s_436_log.rds")

invoke("anova", vlms_436_log, test = "LR")


# Follow-up test re: positive symptoms at level 7 vs 9

anova(vlms$sips_pos[[7]], vlms$sips_pos[[9]])