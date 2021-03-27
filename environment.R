env_est <- emmip(env_m3$model, Treatment ~ Day, type="response", CIs = TRUE) + theme_classic()
env_contrasts <- emmeans(env_m3$model, pairwise ~ Treatment, type = "response")
env_contrasts

environment_box <- ggplot(all_data, aes(x=Day, y=Environment,fill=Treatment)) + 
  geom_boxplot() + theme_classic() + ylim(c(0,0.8))

(environment_box | env_est)

env_comp <- bayesfactor_models(env_m1$model, env_m2$model, env_m3$model, env_m4$model, denominator = env_m0$model)
env_inc <- bayesfactor_inclusion(env_comp, match_models = TRUE)
env_comp # day only
env_inc # day

# Have a look at coefficients
posterior_summary(env_m2$model, robust = TRUE)[1:5,]

mat_env_m2 <- as.matrix(env_m2$model)
post_env_m2 <- mcmc_areas(mat_env_m2,
                           pars = colnames(mat_env_m2)[1:3],
                           prob=c(0.89,1)) 
post_env_m2

# Back-transformed effect size esimates
env_int_effect <- plogis(mat_env_m2[,"b_Intercept"])
env_d4_effect <- plogis(mat_env_m2[,"b_Intercept"] + mat_env_m2[,"b_Day4"]) - plogis(mat_env_m2[,"b_Intercept"])
env_d10_effect <- plogis(mat_env_m2[,"b_Intercept"] + mat_env_m2[,"b_Day10"]) - plogis(mat_env_m2[,"b_Intercept"])

env_effects_df <- data.frame(c(rep("Day 4", times=10000), rep("Day 10", times=10000)),
                              c(env_d4_effect, env_d10_effect))
colnames(env_effects_df) <- c("Condition", "Effect")

env_effects <- ggplot(env_effects_df, aes(x=Condition, y=Effect, fill=Condition)) + 
  geom_violinhalf() + theme_classic() + ylab("Effect size (proportion change)") + 
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color="grey") +
  scale_fill_manual(values=c("#f7fcf0", "#ccebc5", "#7bccc4", "#2b8cbe"))
env_effects

# Find nice way to visualise HDI and ROPE
t_hdi_plot <- plot(hdi(t_effect, ci=c(0.89, 0.95))) + scale_fill_manual(values=c("cyan4", "cyan3", "white")) + theme_classic() + ggtitle("Treatment") + xlim(-0.05, 0.1)
t_hdi_plot

# Plot predictions
env_int_pred <- plogis(mat_env_m2[,"b_Intercept"])
env_d4_pred <- plogis(mat_env_m2[,"b_Intercept"] + mat_env_m2[,"b_Day4"])
env_d10_pred <- plogis(mat_env_m2[,"b_Intercept"] + mat_env_m2[,"b_Day10"])
env_m2_df <- data.frame(c(rep("Day 2", times=10000), rep("Day 4", times=10000), rep("Day 10", times=10000)),
                             c(env_int_pred, env_d4_pred, env_d10_pred))
colnames(env_m2_df) <- c("Day", "y")
head(env_m2_df)
y_mean_env <- c(median(env_m2_df$y[which(env_m2_df$Day=="Day 2")]),
                 median(env_m2_df$y[which(env_m2_df$Day=="Day 4")]),
                 median(env_m2_df$y[which(env_m2_df$Day=="Day 10")]))

y_hdi_l_env <- c(hdi(env_m2_df$y[which(env_m2_df$Day=="Day 2")])[[2]],
                  hdi(env_m2_df$y[which(env_m2_df$Day=="Day 4")])[[2]],
                  hdi(env_m2_df$y[which(env_m2_df$Day=="Day 10")])[[2]])

y_hdi_h_env <- c(hdi(env_m2_df$y[which(env_m2_df$Day=="Day 2")])[[3]],
                  hdi(env_m2_df$y[which(env_m2_df$Day=="Day 4")])[[3]],
                  hdi(env_m2_df$y[which(env_m2_df$Day=="Day 10")])[[3]])

new_df_env <- data.frame(y_mean_env, y_hdi_l_env, y_hdi_h_env, factor(rep(c("2", "4", "10"), times=2), levels=c("2", "4", "10")))
colnames(new_df_env) <- c("Median", "lower", "upper", "Day")
new_df_env

p_env<- ggplot(new_df_env, aes(x=Day, y=Median)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
  theme_classic() +
  ylim(c(0,0.8))
p_env
