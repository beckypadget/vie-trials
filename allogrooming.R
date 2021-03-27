allo_comp <- bayesfactor_models(allo_m1$model, allo_m2$model, allo_m3$model, allo_m4$model, denominator = allo_m0$model)
allo_inc <- bayesfactor_inclusion(allo_comp, match_models = TRUE)
allo_comp # treatment and day
allo_inc # treatment + day
(allogrooming_box | allo_est_m3)
allo_est_m3 <- emmip(allo_m3$model, Treatment ~ Day, type="response", CIs = TRUE) + theme_classic()

allogrooming_box <- ggplot(all_data, aes(x=Day, y=Allogrooming, fill=Treatment)) + 
  geom_boxplot() + theme_classic() + ylim(0,0.5) + ylab("Proportion time spent")

allogrooming_box_nz <- ggplot(all_data[all_data$Allogrooming>0,], aes(x=Day, y=Allogrooming, fill=Treatment)) + 
  geom_boxplot() + theme_classic() + ylim(0,0.5)

(allogrooming_box | allogrooming_box_nz | allo_est)

mat_allo_m3 <- as.matrix(allo_m3$model)
post_allo_m3 <- mcmc_areas(mat_allo_m3,
                          pars = c(colnames(mat_allo_m3)[1:4]),
                          prob=c(0.89)) 
post_allo_m3

# Predictions
c2_sample <- plogis(mat_allo_m3[,"b_Intercept"])
c4_sample <- plogis(mat_allo_m3[,"b_Intercept"] + mat_allo_m3[,"b_Day4"])
c10_sample <- plogis(mat_allo_m3[,"b_Intercept"] + mat_allo_m3[,"b_Day10"])
t2_sample <- plogis(mat_allo_m3[,"b_Intercept"] + mat_allo_m3[,"b_TreatmentT"])
t4_sample <- plogis(mat_allo_m3[,"b_Intercept"] + mat_allo_m3[,"b_TreatmentT"] + mat_allo_m3[,"b_Day4"])
t10_sample <- plogis(mat_allo_m3[,"b_Intercept"] + mat_allo_m3[,"b_TreatmentT"] + mat_allo_m3[,"b_Day10"])
length(c2_sample)
allo_m3_df <- data.frame(c(rep("C", times=30000), rep("T", times=30000)),
                 factor(c(rep("Day 2", times=10000), rep("Day 4", times=10000), rep("Day 10", times=10000),
                   rep("Day 2", times=10000), rep("Day 4", times=10000), rep("Day 10", times=10000)), levels=c("Day 2", "Day 4", "Day 10")),
                 c(c2_sample, c4_sample, c10_sample,
                   t2_sample, t4_sample, t10_sample))
colnames(allo_m3_df) <- c("Treatment", "Day", "y")

allo_predicted <- ggplot(allo_m3_df, aes(x=Day, y=y, fill=Treatment)) + 
  geom_boxplot() + theme_classic() + ylim(0,0.5) + ylab("Predicted value")

allogrooming_box_nz <- ggplot(all_data[all_data$Allogrooming>0,], aes(x=Day, y=Allogrooming, fill=Treatment)) + 
  geom_boxplot() + theme_classic() + ylim(0,0.5)

allogrooming_box <- ggplot(all_data, aes(x=Day, y=Allogrooming, fill=Treatment)) + 
  geom_boxplot() + theme_classic()+ ylim(0,0.5)

(allogrooming_box | allogrooming_box_nz | allo_predicted)

# Back-transformed effect size esimates
allo_t_effect <- plogis(mat_allo_m3[,"b_TreatmentT"] + mat_allo_m3[,"b_Intercept"])-plogis(median(mat_allo_m3[,"b_Intercept"]))

# Find nice way to visualise HDI and ROPE
t_hdi_plot <- plot(hdi(t_effect, ci=c(0.89, 0.95))) + 
  scale_fill_manual(values=c("#f7fcf0", "#ccebc5", "#7bccc4", "#2b8cbe")) + 
  theme_classic() + 
  ggtitle("Treatment") 
t_hdi_plot

# Effect sizes
c4_effect <- plogis(mat_allo_m3[,"b_Intercept"] + mat_allo_m3[,"b_Day4"]) - plogis(mat_allo_m3[,"b_Intercept"])
c10_effect <- plogis(mat_allo_m3[,"b_Intercept"] + mat_allo_m3[,"b_Day10"]) - plogis(mat_allo_m3[,"b_Intercept"])
t_effect <- plogis(mat_allo_m3[,"b_Intercept"] + mat_allo_m3[,"b_TreatmentT"]) - plogis(mat_allo_m3[,"b_Intercept"])

allo_effects_df <- data.frame(c(rep("Day 4", times=10000), rep("Day 10", times=10000), 
                                  rep("Treatment", times=10000)),
                              c(c4_effect, c10_effect,
                                 t_effect))
colnames(allo_effects_df) <- c("Condition", "Effect")

allo_effects <- ggplot(allo_effects_df, aes(x=Condition, y=Effect, fill=Condition)) + 
  geom_violinhalf() + theme_classic() + ylab("Effect size (proportion change)") + 
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color="grey") +
  scale_fill_manual(values=c("#f7fcf0", "#ccebc5", "#7bccc4", "#2b8cbe"))
allo_effects

allo_predicted <- ggplot(allo_m3_df, aes(x=Day, y=y, fill=Treatment)) + 
  geom_line() + theme_classic() + ylim(0,0.5) + ylab("Predicted value") 
allo_predicted

allogrooming_box <- ggplot(all_data, aes(x=Day, y=Allogrooming, fill=Treatment)) + 
  geom_boxplot() + theme_classic()+ ylim(0,0.5)

(allogrooming_box | allo_predicted)

y_mean_allo <- c(median(allo_m3_df$y[which(allo_m3_df$Treatment=="C" & allo_m3_df$Day=="Day 2")]),
            median(allo_m3_df$y[which(allo_m3_df$Treatment=="C" & allo_m3_df$Day=="Day 4")]),
            median(allo_m3_df$y[which(allo_m3_df$Treatment=="C" & allo_m3_df$Day=="Day 10")]),
            median(allo_m3_df$y[which(allo_m3_df$Treatment=="T" & allo_m3_df$Day=="Day 2")]),
            median(allo_m3_df$y[which(allo_m3_df$Treatment=="T" & allo_m3_df$Day=="Day 4")]),
            median(allo_m3_df$y[which(allo_m3_df$Treatment=="T" & allo_m3_df$Day=="Day 10")]))

y_hdi_l_allo <- c(hdi(allo_m3_df$y[which(allo_m3_df$Treatment=="C" & allo_m3_df$Day=="Day 2")])[[2]],
          hdi(allo_m3_df$y[which(allo_m3_df$Treatment=="C" & allo_m3_df$Day=="Day 4")])[[2]],
          hdi(allo_m3_df$y[which(allo_m3_df$Treatment=="C" & allo_m3_df$Day=="Day 10")])[[2]],
          hdi(allo_m3_df$y[which(allo_m3_df$Treatment=="T" & allo_m3_df$Day=="Day 2")])[[2]],
          hdi(allo_m3_df$y[which(allo_m3_df$Treatment=="T" & allo_m3_df$Day=="Day 4")])[[2]],
          hdi(allo_m3_df$y[which(allo_m3_df$Treatment=="T" & allo_m3_df$Day=="Day 10")])[[2]])

y_hdi_h_allo <- c(hdi(allo_m3_df$y[which(allo_m3_df$Treatment=="C" & allo_m3_df$Day=="Day 2")])[[3]],
             hdi(allo_m3_df$y[which(allo_m3_df$Treatment=="C" & allo_m3_df$Day=="Day 4")])[[3]],
             hdi(allo_m3_df$y[which(allo_m3_df$Treatment=="C" & allo_m3_df$Day=="Day 10")])[[3]],
             hdi(allo_m3_df$y[which(allo_m3_df$Treatment=="T" & allo_m3_df$Day=="Day 2")])[[3]],
             hdi(allo_m3_df$y[which(allo_m3_df$Treatment=="T" & allo_m3_df$Day=="Day 4")])[[3]],
             hdi(allo_m3_df$y[which(allo_m3_df$Treatment=="T" & allo_m3_df$Day=="Day 10")])[[3]])

new_df_allo <- data.frame(y_mean_allo, y_hdi_l_allo, y_hdi_h_allo, c(rep("C", times=3), rep("T", times=3)), factor(rep(c("2", "4", "10"), times=2), levels=c("2", "4", "10")))
colnames(new_df_allo) <- c("Median", "lower", "upper", "Treatment", "Day")
new_df_allo

p_allo<- ggplot(new_df_allo, aes(x=Day, y=Median, group=Treatment, color=Treatment)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=y_hdi_l, ymax=y_hdi_h), width=.2) +
  theme_classic() +
  ylim(c(0,0.5)) +
  ylab("Predicted proportion time spent")
p_allo

(allogrooming_box | p_allo)
