tro_est <- emmip(tro_m4$model, Treatment ~ Day, type="response", CIs = TRUE) + theme_classic()
tro_contrasts <- emmeans(tro_m4$model, pairwise ~ Treatment + Day, type = "simple")
tro_contrasts

trophallaxis_box <- ggplot(all_data, aes(x=Day, y=Trophallaxis, fill=Treatment)) + 
  geom_boxplot() + theme_classic() + ylim(c(0,0.1))


(trophallaxis_box | tro_est)

tro_comp <- bayesfactor_models(tro_m1$model, tro_m2$model, tro_m3$model, tro_m4$model, denominator = tro_m0$model)
tro_inc <- bayesfactor_inclusion(tro_comp, match_models = TRUE)
tro_comp # 
tro_inc # 

# Have a look at coefficients
posterior_summary(tro_m4$model, robust = TRUE)[1:5,]
mat_tro_m4 <- as.matrix(tro_m4$model)
post_tro_m4 <- mcmc_areas(mat_tro_m4,
                          pars = colnames(mat_tro_m4)[1:6],
                          prob=c(0.89)) 
post_tro_m4

# Back-transformed effect size esimates
tro_c2_effect <- plogis(mat_tro_m4[,"b_Intercept"])
tro_c4_effect <- plogis(mat_tro_m4[,"b_Intercept"] + mat_tro_m4[,"b_Day4"]) - plogis(mat_tro_m4[,"b_Intercept"])
tro_c10_effect <- plogis(mat_tro_m4[,"b_Intercept"] + mat_tro_m4[,"b_Day10"]) - plogis(mat_tro_m4[,"b_Intercept"])
tro_t2_effect <- plogis(mat_tro_m4[,"b_Intercept"] + mat_tro_m4[,"b_TreatmentT"]) - plogis(mat_tro_m4[,"b_Intercept"])
tro_t4_effect <- plogis(mat_tro_m4[,"b_Intercept"] + mat_tro_m4[,"b_TreatmentT"] + mat_tro_m4[,"b_Day4"] + mat_tro_m4[,"b_TreatmentT:Day4"]) - plogis(mat_tro_m4[,"b_Intercept"] + mat_tro_m4[,"b_TreatmentT"] + mat_tro_m4[,"b_Day4"])
tro_t10_effect <- plogis(mat_tro_m4[,"b_Intercept"] + mat_tro_m4[,"b_TreatmentT"] + mat_tro_m4[,"b_Day10"] + mat_tro_m4[,"b_TreatmentT:Day10"]) - plogis(mat_tro_m4[,"b_Intercept"] + mat_tro_m4[,"b_TreatmentT"] + mat_tro_m4[,"b_Day10"])
median(tro_t10_effect)
tro_effects_df <- data.frame(factor(c(rep("C4", times=10000),
                               rep("C10", times=10000),
                               rep("T2", times=10000),
                               rep("T4", times=10000),
                               rep("T10", times=10000)), 
                               levels=c("T10", "T4", "T2", "C10", "C4")),
                             c(tro_c4_effect,
                               tro_c10_effect, tro_t2_effect,
                               tro_t4_effect, tro_t10_effect))
colnames(tro_effects_df) <- c("Condition", "Effect")

tro_effects <- ggplot(tro_effects_df, aes(x=Condition, y=Effect, fill=Condition)) + 
  geom_violinhalf() + theme_classic() + ylab("Effect size (proportion change)") + 
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color="grey") +
  scale_fill_manual(values=c("#f7fcf0", "#e0f3db","#ccebc5", "#7bccc4", "#2b8cbe", "#08589e"))
tro_effects

equivalence_test(tro_effects_df[,"Effect"][tro_effects_df[,"Condition"]=="T10"], range=c(-0.1, 0.1))

# Find nice way to visualise HDI and ROPE
t_hdi_plot <- plot(hdi(t_effect, ci=c(0.89, 0.95))) + scale_fill_manual(values=c("cyan4", "cyan3", "white")) + theme_classic() + ggtitle("Treatment") + xlim(-0.05, 0.1)
t_hdi_plot

# Plot predictions
tro_c2_pred <- plogis(mat_tro_m4[,"b_Intercept"])
tro_c4_pred <- plogis(mat_tro_m4[,"b_Intercept"] + mat_tro_m4[,"b_Day4"])
tro_c10_pred <- plogis(mat_tro_m4[,"b_Intercept"] + mat_tro_m4[,"b_Day10"])
tro_t2_pred <- plogis(mat_tro_m4[,"b_Intercept"] + mat_tro_m4[,"b_TreatmentT"])
tro_t4_pred <- plogis(mat_tro_m4[,"b_Intercept"] + mat_tro_m4[,"b_TreatmentT"] + mat_tro_m4[,"b_Day4"] + mat_tro_m4[,"b_TreatmentT:Day4"])
tro_t10_pred <- plogis(mat_tro_m4[,"b_Intercept"] + mat_tro_m4[,"b_TreatmentT"] + mat_tro_m4[,"b_Day10"] + mat_tro_m4[,"b_TreatmentT:Day10"])
median(tro_t10_pred)
tro_m4_df <- data.frame(c(rep("C", times=30000),
                          rep("T", times=30000)),
                        c(rep("Day 2", times=10000),
                          rep("Day 4", times=10000),
                          rep("Day 10", times=10000),
                          rep("Day 2", times=10000),
                          rep("Day 4", times=10000),
                          rep("Day 10", times=10000)),
                        c(tro_c2_pred, tro_c4_pred,
                         tro_c10_pred, tro_t2_pred,
                         tro_t4_pred, tro_t10_pred))
colnames(tro_m4_df) <- c("Treatment", "Day", "y")


y_mean_tro <- c(median(tro_m4_df$y[which(tro_m4_df$Treatment=="C" & tro_m4_df$Day=="Day 2")]),
                 median(tro_m4_df$y[which(tro_m4_df$Treatment=="C" & tro_m4_df$Day=="Day 4")]),
                 median(tro_m4_df$y[which(tro_m4_df$Treatment=="C" & tro_m4_df$Day=="Day 10")]),
                 median(tro_m4_df$y[which(tro_m4_df$Treatment=="T" & tro_m4_df$Day=="Day 2")]),
                 median(tro_m4_df$y[which(tro_m4_df$Treatment=="T" & tro_m4_df$Day=="Day 4")]),
                 median(tro_m4_df$y[which(tro_m4_df$Treatment=="T" & tro_m4_df$Day=="Day 10")]))

y_hdi_l_tro <- c(hdi(tro_m4_df$y[which(tro_m4_df$Treatment=="C" & tro_m4_df$Day=="Day 2")])[[2]],
                  hdi(tro_m4_df$y[which(tro_m4_df$Treatment=="C" & tro_m4_df$Day=="Day 4")])[[2]],
                  hdi(tro_m4_df$y[which(tro_m4_df$Treatment=="C" & tro_m4_df$Day=="Day 10")])[[2]],
                  hdi(tro_m4_df$y[which(tro_m4_df$Treatment=="T" & tro_m4_df$Day=="Day 2")])[[2]],
                  hdi(tro_m4_df$y[which(tro_m4_df$Treatment=="T" & tro_m4_df$Day=="Day 4")])[[2]],
                  hdi(tro_m4_df$y[which(tro_m4_df$Treatment=="T" & tro_m4_df$Day=="Day 10")])[[2]])

y_hdi_h_tro <- c(hdi(tro_m4_df$y[which(tro_m4_df$Treatment=="C" & tro_m4_df$Day=="Day 2")])[[3]],
                  hdi(tro_m4_df$y[which(tro_m4_df$Treatment=="C" & tro_m4_df$Day=="Day 4")])[[3]],
                  hdi(tro_m4_df$y[which(tro_m4_df$Treatment=="C" & tro_m4_df$Day=="Day 10")])[[3]],
                  hdi(tro_m4_df$y[which(tro_m4_df$Treatment=="T" & tro_m4_df$Day=="Day 2")])[[3]],
                  hdi(tro_m4_df$y[which(tro_m4_df$Treatment=="T" & tro_m4_df$Day=="Day 4")])[[3]],
                  hdi(tro_m4_df$y[which(tro_m4_df$Treatment=="T" & tro_m4_df$Day=="Day 10")])[[3]])

new_df_tro <- data.frame(y_mean_tro, y_hdi_l_tro, y_hdi_h_tro, c(rep("C", times=3), rep("T", times=3)), factor(rep(c("2", "4", "10"), times=2), levels=c("2", "4", "10")))
colnames(new_df_tro) <- c("Median", "lower", "upper", "Treatment", "Day")
new_df_tro

p_tro<- ggplot(new_df_tro, aes(x=Day, y=Median, group=Treatment, color=Treatment)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=y_hdi_l_tro, ymax=y_hdi_h_tro), width=.2) +
  theme_classic() +
  ylim(c(0,0.1))
p_tro
