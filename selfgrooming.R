sg_est <- emmip(sg_m4, Treatment ~ Day, type="response", CIs = TRUE) + theme_classic()
sg_contrasts <- emmeans(sg_m4, pairwise ~ Treatment, type = "response")
sg_contrasts

(selfgrooming_m4_box | sg_est)

# Model 1 predictions
mat_sg_m1 <- as.matrix(sg_m1)
sg1_int_sample <- exp(mat_sg_m1[,"(Intercept)"])
sg1_treat_sample <- exp(mat_sg_m1[,"(Intercept)"] + mat_sg_m1[,"TreatmentT"])

sg1_df <- data.frame(c(rep("T", times=4000), rep("C", times=4000)), 
                     c(sg1_treat_sample, sg1_int_sample))
colnames(sg1_df) <- c("Treatment", "y")

sg_m1_predicted <- ggplot(sg1_df, aes(x=Treatment, y=y, fill=Treatment)) + 
  geom_boxplot() + theme_classic() + ylim(0,26)

# Model 4 predictions
mat_sg_m4 <- as.matrix(sg_m4)
sg4_c2_sample <- exp(mat_sg_m4[,"(Intercept)"])
sg4_c4_sample <- exp(mat_sg_m4[,"(Intercept)"] + mat_sg_m4[,"Day4"])
sg4_c10_sample <- exp(mat_sg_m4[,"(Intercept)"] + mat_sg_m4[,"Day10"])
sg4_t2_sample <- exp(mat_sg_m4[,"(Intercept)"] + mat_sg_m4[,"TreatmentT"])
sg4_t4_sample <- exp(mat_sg_m4[,"(Intercept)"] + mat_sg_m4[,"TreatmentT"] + mat_sg_m4[,"Day4"] + mat_sg_m4[,"TreatmentT:Day4"])
sg4_t10_sample <- exp(mat_sg_m4[,"(Intercept)"] + mat_sg_m4[,"TreatmentT"] + mat_sg_m4[,"Day10"] + mat_sg_m4[,"TreatmentT:Day10"])

sg4_df <- data.frame(c(rep("T", times=12000), rep("C", times=12000)),
                     factor(c(rep("Day 2", times=4000), rep("Day 4", times=4000), rep("Day 10", times=4000), 
                       rep("Day 2", times=4000), rep("Day 4", times=4000), rep("Day 10", times=4000)), levels=c("Day 2", "Day 4", "Day 10")),
                     c(sg4_t2_sample, sg4_t4_sample, sg4_t10_sample,
                       sg4_c2_sample, sg4_c4_sample, sg4_c10_sample))
colnames(sg4_df) <- c("Treatment", "Day", "y")

sg_m4_predicted <- ggplot(sg4_df, aes(x=Day, y=y, fill=Treatment)) + 
  geom_boxplot() + theme_classic() + ylim(0,26)

# Model 2 predictions
mat_sg_m2 <- as.matrix(sg_m2)
sg2_int_sample <- exp(mat_sg_m2[,"(Intercept)"])
sg2_d4_sample <- exp(mat_sg_m2[,"(Intercept)"] + mat_sg_m2[,"Day4"])
sg2_d10_sample <- exp(mat_sg_m2[,"(Intercept)"] + mat_sg_m2[,"Day10"])
length(mat_sg_m2[,"(Intercept)"])
sg2_df <- data.frame(c(rep("Day 2", times=4000), rep("Day 4", times=4000),rep("Day 10", times=4000)), c(sg2_int_sample, sg2_d4_sample, sg2_d10_sample))
colnames(sg2_df) <- c("Day", "y")

sg_m2_predicted <- ggplot(sg2_df, aes(x=Day, y=y, fill=Day)) + 
  geom_boxplot() + theme_classic() + ylim(0,26)

# Plot data
selfgrooming_m4_box <- ggplot(all_data, aes(x=Day, y=Self.grooming,fill=Treatment)) + 
  geom_boxplot() + theme_classic() + ylim(0,26)

selfgrooming_m2_box <- ggplot(all_data, aes(x=Day, y=Self.grooming, fill=Day)) + 
  geom_boxplot() + theme_classic() + ylim(0,26)

# We want to compare models 1 (treatment only), 3 (day only) and 4 (full model)
((selfgrooming_m1_box | sg_m1_predicted) /
    (selfgrooming_m2_box | sg_m2_predicted) / 
    (selfgrooming_m4_box | sg_m4_predicted))

sg_comp <- bayesfactor_models(sg_m1$model, sg_m2$model, sg_m3$model, sg_m4$model, denominator = sg_m0$model)
sg_inc <- bayesfactor_inclusion(sg_comp, match_models = TRUE)
sg_comp 
sg_inc 

# Have a look at coefficients
posterior_summary(sg_m3$model, robust = TRUE)[1:5,]
mat_sg_m1 <- as.matrix(sg_m1)
post_sg_m1 <- mcmc_areas(mat_sg_m1,
                          pars = colnames(mat_sg_m1)[1:2],
                          prob=c(0.89)) 
post_sg_m1

# Back-transformed effect size esimates
sg_c_effect <- exp(mat_sg_m1[,"(Intercept)"])
sg_t_effect <- exp(mat_sg_m1[,"TreatmentT"] + mat_sg_m1[,"(Intercept)"])-plogis(mat_sg_m1[,"(Intercept)"])
median(sg_t_effect)
sg_effects_df <- data.frame(c(rep("Treatment", times=4000)),
                             c(sg_t_effect))
colnames(sg_effects_df) <- c("Condition", "Effect")

sg_effects <- ggplot(sg_effects_df, aes(x=Condition, y=Effect, fill=Condition)) + 
  geom_violinhalf() + theme_classic() + ylab("Effect size (proportion change)") + 
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color="grey") +
  scale_fill_manual(values=c("#f7fcf0", "#ccebc5", "#7bccc4", "#2b8cbe"))
sg_effects


# Find nice way to visualise HDI and ROPE
t_hdi_plot <- plot(hdi(t_effect, ci=c(0.89, 0.95))) + scale_fill_manual(values=c("cyan4", "cyan3", "white")) + theme_classic() + ggtitle("Treatment") + xlim(-0.05, 0.1)
t_hdi_plot

# Plot predictions
sg_int_pred <- exp(mat_sg_m1[,"(Intercept)"])
sg_treatment_pred <- exp(mat_sg_m1[,"(Intercept)"] + mat_sg_m1[,"TreatmentT"])

sg_m1_df <- data.frame(c(rep("C", times=4000), rep("T", times=4000)),
                        c(sg_int_pred, sg_treatment_pred))
colnames(sg_m1_df) <- c("Treatment", "y")
head(sg_m1_df)

y_mean_sg <- c(median(sg_m1_df$y[which(sg_m1_df$Treatment=="C")]),
                median(sg_m1_df$y[which(sg_m1_df$Treatment=="T")]))

y_hdi_l_sg <- c(hdi(sg_m1_df$y[which(sg_m1_df$Treatment=="C")])[[2]],
                 hdi(sg_m1_df$y[which(sg_m1_df$Treatment=="T")])[[2]])

y_hdi_h_sg <- c(hdi(sg_m1_df$y[which(sg_m1_df$Treatment=="C")])[[3]],
                 hdi(sg_m1_df$y[which(sg_m1_df$Treatment=="T")])[[3]])

new_df_sg <- data.frame(y_mean_sg, y_hdi_l_sg, y_hdi_h_sg, c("C", "T"))
colnames(new_df_sg) <- c("Median", "lower", "upper", "Treatment")
new_df_sg

p_sg<- ggplot(new_df_sg, aes(x=Treatment, y=Median, color=Treatment)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=y_hdi_l_sg, ymax=y_hdi_h_sg), width=.2) +
  theme_classic() + 
  ylim(c(0,26))
p_sg

selfgrooming_box <- selfgrooming_box + ylim(c(0,26))
selfgrooming_box
