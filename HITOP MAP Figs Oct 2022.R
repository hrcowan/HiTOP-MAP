require(psych)
require(tidyverse)
require(lm.beta)
require(DescTools)
require(ggplot2)
require(gridExtra)

# Build a ggplot-friendly dataframe of Adj R2s w logistic R2

vr2_plot_log <- as.data.frame(matrix(unlist(vr2s_436), nrow = 10, ncol = 3))
vr2_plot_log <- cbind(unlist(vr2s_436_log), vr2_plot_log[,2:3])
vr2_plot_log$Level <- seq(1, nrow(vr2_plot_log))
colnames(vr2_plot_log)[1:3] <- c("CHR Status", "Positive Symptoms", "Negative Symptoms")
vr2_plot_log_long <- vr2_plot_log %>%
  filter(Level <= 10) %>%
  pivot_longer(c("CHR Status", "Positive Symptoms", "Negative Symptoms"), names_to = "Outcome", values_to = "R2") 

# Plot
vr2_plot_log_long %>%
  ggplot(aes(x = Level, y = R2, group = Outcome, color = Outcome, shape = Outcome)) +
  geom_line(size = 1) + 
  geom_point(size = 2.5, fill = "grey90") +
  geom_text(data = subset(filter(vr2_plot_log_long, Outcome != "Conversion Risk"), Level == 3), 
            aes(label = Outcome, colour = Outcome, x = 2, y = R2), 
            vjust = 1.78, hjust = -0.02, size = 4, fontface = "bold") +
  scale_shape_manual(values=c(21, 22, 23, 24)) +
  scale_color_manual(values = c("plum3", "palegreen4", "gold3")) +
  scale_x_discrete(name = "Factor Level", limits = factor(c(1:10))) +
  theme_bw() +
  ylim(0, 0.45) +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none") +
  labs(title = "a. Overall Model Fit",
       subtitle = "Predicted by 1 to 10 factors",
       y = "Adjusted R Squared") -> vr2_plot_noflip_3outcomes_log



# Plot CHR results with logistic regression 
chr_reg_plot_log <- 
  summary(vlms_436_log[[6]])$coefficients[2:7,] %>%
  cbind(vlms_436_log[[6]]$or[2:7]) %>%
  round(2) %>%
  as.data.frame
colnames(chr_reg_plot_log)[5] <- "OR"
chr_reg_plot_log$Factor <- c("Distress", "Reality distortion", "Fear", 
                             "Detachment/\nDisorganization", "Mania", "Dissociation")
chr_reg_plot_log$Sig[chr_reg_plot_log$`Pr(>|z|)`<=.05 & chr_reg_plot_log$Estimate>=0] <- "sig_pos"
chr_reg_plot_log$Sig[chr_reg_plot_log$`Pr(>|z|)`<=.05 & chr_reg_plot_log$Estimate<0] <- "sig_neg"
chr_reg_plot_log$Sig[chr_reg_plot_log$`Pr(>|z|)`>.05] <- "non_sig"
chr_reg_plot_log %>%
  arrange(Estimate) %>%
  mutate(Factor=factor(Factor, levels=Factor)) ->
  chr_reg_plot_log

ggplot(chr_reg_plot_log, aes(x = OR, y = Factor, label = OR,
                             color = Sig)) +
  geom_segment(aes(x = 1, y = Factor, xend = OR, yend = Factor)) +
  geom_vline(xintercept = 1, color = "grey50", lty = "dashed") +
  geom_point(size = 9) +
  geom_text(color = "white", size = 3) +
  theme_minimal() +
  scale_color_manual(values = c("grey75", "tomato", "skyblue3")) +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4), limits = c(-0.5, 4.0)) +
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(title = "c. CHR Status", 
       subtitle = "Predicted by 6 factors", 
       y = "", x = "Odds Ratio") -> chr_coef_log


# PINS with linear regression

summary(lm.beta(vlms_436$pins_total[[9]]))$coefficients[2:10,] %>%
  round(2) %>%
  as.data.frame -> pins_reg_plot
pins_reg_plot$Factor <- c("Distress", "Reality distortion", "Fear", 
                          "Detachment", "Mania", "Dissociation", 
                          "Disorganization", "Substance use", "Sub. perc. abn.")
pins_reg_plot$Sig[pins_reg_plot$`Pr(>|t|)`<=.05 & pins_reg_plot$Standardized>=0] <- "sig_pos"
pins_reg_plot$Sig[pins_reg_plot$`Pr(>|t|)`<=.05 & pins_reg_plot$Standardized<0] <- "sig_neg"
pins_reg_plot$Sig[pins_reg_plot$`Pr(>|t|)`>.05] <- "non_sig"
pins_reg_plot %>%
  arrange(Standardized) %>%
  mutate(Factor=factor(Factor, levels=Factor)) ->
  pins_reg_plot

ggplot(pins_reg_plot, aes(x = Standardized, y = Factor, label = (Standardized*100),
                          color = Sig)) +
  geom_segment(aes(x = 0, y = Factor, xend = Standardized, yend = Factor)) +
  geom_vline(xintercept = 0, color = "grey50", lty = "dashed") +
  geom_point(size = 7) +
  geom_text(color = "white", size = 3) +
  theme_minimal() +
  scale_color_manual(values = c("grey75", "skyblue3", "tomato")) +
  scale_x_continuous(breaks = c(-.6, -.4, -.2, 0, .2, .4, .6), limits = c(-.3, .6)) +
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(title = "d. Negative Symptoms", subtitle = "Predicted by 9 factors", 
       y = "", x = "Std. coefficient") -> pins_coef



# SIPS Positive with linear regression

summary(lm.beta(vlms_436$sips_pos[[7]]))$coefficients[2:8,] %>%
  round(2) %>%
  as.data.frame -> sips_p_reg_plot
sips_p_reg_plot$Factor <- c("Distress", "Reality distortion", "Fear",  
                            "Detachment", "Mania", "Dissociation",   
                            "Disorganization")
sips_p_reg_plot$Sig[sips_p_reg_plot$`Pr(>|t|)`<=.05 & sips_p_reg_plot$Standardized>=0] <- "sig_pos"
sips_p_reg_plot$Sig[sips_p_reg_plot$`Pr(>|t|)`<=.05 & sips_p_reg_plot$Standardized<0] <- "sig_neg"
sips_p_reg_plot$Sig[sips_p_reg_plot$`Pr(>|t|)`>.05] <- "non_sig"
sips_p_reg_plot %>%
  arrange(Standardized) %>%
  mutate(Factor=factor(Factor, levels=Factor)) ->
  sips_p_reg_plot


ggplot(sips_p_reg_plot, aes(x = Standardized, y = Factor, label = (Standardized*100),
                            color = Sig)) +
  geom_segment(aes(x = 0, y = Factor, xend = Standardized, yend = Factor)) +
  geom_vline(xintercept = 0, color = "grey50", lty = "dashed") +
  geom_point(size = 7) +
  geom_text(color = "white", size = 3) +
  theme_minimal() +
  scale_color_manual(values = c("grey75", "tomato", "skyblue3")) +
  scale_x_continuous(breaks = c(-.6, -.4, -.2, 0, .2, .4, .6), limits = c(-.3, .6)) +
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(title = "b. Positive Symptoms", subtitle = "Predicted by 7 factors", 
       y = "", x = "Std. coefficient") -> sips_p_coef


# Fig 3 with logistic
grid.arrange(vr2_plot_noflip_3outcomes_log, arrangeGrob(sips_p_coef, chr_coef_log, pins_coef), ncol = 2)





# SIPS plot with 9 factors for supplement

summary(lm.beta(vlms_436$sips_pos[[9]]))$coefficients[2:10,] %>%
  round(2) %>%
  as.data.frame -> sips_p_reg_plot_9
sips_p_reg_plot_9$Factor <- c("Distress", "Reality distortion", "Fear",  
                              "Detachment", "Mania", "Dissociation",   
                              "Disorganization", "Substance use", "Sub. percectual abn.")
sips_p_reg_plot_9$Sig[sips_p_reg_plot_9$`Pr(>|t|)`<=.05 & sips_p_reg_plot_9$Standardized>=0] <- "sig_pos"
sips_p_reg_plot_9$Sig[sips_p_reg_plot_9$`Pr(>|t|)`<=.05 & sips_p_reg_plot_9$Standardized<0] <- "sig_neg"
sips_p_reg_plot_9$Sig[sips_p_reg_plot_9$`Pr(>|t|)`>.05] <- "non_sig"
sips_p_reg_plot_9 %>%
  arrange(Standardized) %>%
  mutate(Factor=factor(Factor, levels=Factor)) ->
  sips_p_reg_plot_9


ggplot(sips_p_reg_plot_9, aes(x = Standardized, y = Factor, label = (Standardized*100),
                              color = Sig)) +
  geom_segment(aes(x = 0, y = Factor, xend = Standardized, yend = Factor)) +
  geom_vline(xintercept = 0, color = "grey50", lty = "dashed") +
  geom_point(size = 7) +
  geom_text(color = "white", size = 3) +
  theme_minimal() +
  scale_color_manual(values = c("grey75", "tomato", "skyblue3")) +
  scale_x_continuous(breaks = c(-.6, -.4, -.2, 0, .2, .4, .6), limits = c(-.3, .6)) +
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(title = "Positive Symptoms", subtitle = "Predicted by 9 factors", 
       y = "", x = "Std. coefficient") -> sips_p_coef_9
