require(psych)
require(tidyverse)
require(lm.beta)
require(DescTools)
require(ggplot2)
require(gridExtra)


# Plot changes in Adjusted R2 at different factor levels

# Build a ggplot-friendly dataframe of Adj R2s

vr2.plot <- as.data.frame(vr2s[c(1, 2, 4)])
vr2.plot.3$Level <- seq(1, nrow(vr2.plot.3))
colnames(vr2.plot.3)[1:3] <- c("CHR Status", "Positive Symptoms", "Negative Symptoms")
vr2.plot.3 %>%
  filter(Level <= 10) %>%
  pivot_longer(c("CHR Status", "Positive Symptoms", "Negative Symptoms"), names_to = "Outcome", values_to = "R2") ->
  vr2.plot.long

# Plot
vr2.plot.long %>%
  ggplot(aes(x = Level, y = R2, group = Outcome, color = Outcome, shape = Outcome)) +
  geom_line(size = 1) + 
  geom_point(size = 2.5, fill = "grey90") +
  geom_text(data = subset(filter(vr2.plot.long, Outcome != "Conversion Risk"), Level == 3), 
            aes(label = Outcome, colour = Outcome, x = 2, y = R2), 
            vjust = 1.75, hjust = -0, size = 4, fontface = "bold") +
  scale_shape_manual(values=c(21, 22, 23, 24)) +
  scale_color_manual(values = c("plum3", "palegreen4", "gold3")) +
  scale_x_discrete(name = "Factor Level", limits = factor(c(1:10))) +
  theme_bw() +
  ylim(0, 0.5) +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none") +
  labs(title = "a. Overall Model Fit",
       subtitle = "Predicted by 1 to 10 factors",
       y = "Adjusted R Squared") -> vr2_plot_noflip_3outcomes


# Plot regression coefficients for 3 outcomes of interest

# CHR status

summary(lm.beta(vlms$SIPS_CHR[[6]]))$coefficients[2:7,] %>%
  round(2) %>%
  as.data.frame -> chr_reg_plot
chr_reg_plot$Factor <- c("Distress", "Bizarre ideas", "Fear", 
                         "Detachment/\nDisorganization", "Mania", "Dissociation")
chr_reg_plot$Sig[chr_reg_plot$`Pr(>|t|)`<=.05 & chr_reg_plot$Standardized>=0] <- "sig_pos"
chr_reg_plot$Sig[chr_reg_plot$`Pr(>|t|)`<=.05 & chr_reg_plot$Standardized<0] <- "sig_neg"
chr_reg_plot$Sig[chr_reg_plot$`Pr(>|t|)`>.05] <- "non_sig"
chr_reg_plot %>%
  arrange(Standardized) %>%
  mutate(Factor=factor(Factor, levels=Factor)) ->
  chr_reg_plot

ggplot(chr_reg_plot, aes(x = Standardized, y = Factor, label = (Standardized*100),
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
  labs(title = "c. CHR Status", subtitle = "Predicted by 6 factors", 
       y = "", x = "Std. coefficient") -> chr_coef


# PINS

summary(lm.beta(vlms$pins_total[[9]]))$coefficients[2:10,] %>%
  round(2) %>%
  as.data.frame -> pins_reg_plot
pins_reg_plot$Factor <- c("Distress", "Bizarre ideas", "Fear", 
                            "Detachment", "Mania", "Dissociation", 
                            "Disorganization", "Substance use", "Perceptual abn.")
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



# SIPS Positive

summary(lm.beta(vlms$sips_pos[[9]]))$coefficients[2:10,] %>%
  round(2) %>%
  as.data.frame -> sips_p_reg_plot
sips_p_reg_plot$Factor <- c("Distress", "Bizarre ideas", "Fear",  
                             "Detachment", "Mania", "Dissociation",   
                             "Disorganization", "Substance use", "Perceptual abn.")
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
  labs(title = "b. Positive Symptoms", subtitle = "Predicted by 9 factors", 
       y = "", x = "Std. coefficient") -> sips_p_coef


# Arrange plots
grid.arrange(vr2_plot_noflip_3outcomes, arrangeGrob(sips_p_coef, chr_coef, pins_coef), ncol = 2)

