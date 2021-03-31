but_est <- emmip(but_m4$model, Treatment ~ Day, type="response", CIs = TRUE) + theme_classic()
but_contrasts <- emmeans(but_m3$model, pairwise ~ Treatment, type = "response")
but_contrasts

butting_box <- ggplot(all_data, aes(x=Day, y=Butting,fill=Treatment)) + 
  geom_boxplot() + theme_classic() + ylim(c(0,17))

(butting_box | but_est)

but_comp <- bayesfactor_models(but_m1$model, but_m2$model, but_m3$model, but_m4$model, denominator = but_m0$model)
but_inc <- bayesfactor_inclusion(but_comp, match_models = TRUE)
but_comp 
but_inc 

# Have a look at coefficients
#posterior_summary(but_m3$model, robust = TRUE)[1:5,]
mat_but_m2 <- as.matrix(but_m2)
post_but_m2 <- mcmc_areas(mat_but_m2,
                          pars = colnames(mat_but_m2)[1:3],
                          prob=c(0.89)) 
post_but_m2


# Find nice way to visualise HDI and ROPE
t_hdi_plot <- plot(hdi(t_effect, ci=c(0.89, 0.95))) + scale_fill_manual(values=c("cyan4", "cyan3", "white")) + theme_classic() + ggtitle("Treatment") + xlim(-0.05, 0.1)
t_hdi_plot


# Plot predictions
but_int_pred <- exp(mat_but_m2[,"(Intercept)"])
but_d4_pred <- exp(mat_but_m2[,"(Intercept)"] + mat_but_m2[,"Day4"])
but_d10_pred <- exp(mat_but_m2[,"(Intercept)"] + mat_but_m2[,"Day10"])
but_m2_df <- data.frame(c(rep("Day 2", times=4000), rep("Day 4", times=4000), rep("Day 10", times=4000)),
                        c(but_int_pred, but_d4_pred, but_d10_pred))
colnames(but_m2_df) <- c("Day", "y")

y_mean_but <- c(median(but_m2_df$y[which(but_m2_df$Day=="Day 2")]),
                median(but_m2_df$y[which(but_m2_df$Day=="Day 4")]),
                median(but_m2_df$y[which(but_m2_df$Day=="Day 10")]))

y_hdi_l_but <- c(hdi(but_m2_df$y[which(but_m2_df$Day=="Day 2")])[[2]],
                 hdi(but_m2_df$y[which(but_m2_df$Day=="Day 4")])[[2]],
                 hdi(but_m2_df$y[which(but_m2_df$Day=="Day 10")])[[2]])

y_hdi_h_but <- c(hdi(but_m2_df$y[which(but_m2_df$Day=="Day 2")])[[3]],
                 hdi(but_m2_df$y[which(but_m2_df$Day=="Day 4")])[[3]],
                 hdi(but_m2_df$y[which(but_m2_df$Day=="Day 10")])[[3]])

new_df_but <- data.frame(y_mean_but, y_hdi_l_but, y_hdi_h_but, factor(rep(c("2", "4", "10"), times=2), levels=c("2", "4", "10")))
colnames(new_df_but) <- c("Median", "lower", "upper", "Day")
new_df_but

p_but<- ggplot(new_df_but, aes(x=Day, y=Median)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
  theme_classic() +
  ylim(c(0,17))
p_but
