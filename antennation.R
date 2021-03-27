# Antennation // best support for model including only treatment
ant_est <- emmip(ant_m3$model, Treatment ~ Day, type="response", CIs = TRUE) + theme_classic()
ant_contrasts <- emmeans(ant_m1$model, pairwise ~ Treatment, type = "response")
ant_contrasts
ant_m1$model
antennation_box <- ggplot(all_data, aes(x=Treatment, y=Antennation, fill=Treatment)) + 
  geom_boxplot() + theme_classic()

(antennation_box | ant_est)

ant_comp <- bayesfactor_models(ant_m1$model, ant_m2$model, ant_m3$model, ant_m4$model, denominator = ant_m0$model)
ant_inc <- bayesfactor_inclusion(ant_comp, match_models = TRUE)
ant_comp # treatment only
ant_inc # treatment

mat_ant_m1 <- as.matrix(ant_m1$model)
post_ant_m1 <- mcmc_areas(mat_ant_m1,
                           pars = colnames(mat_ant_m1)[1:2],
                           prob=c(0.89)) 
post_ant_m1
colnames(mat_ant_m1)
## Get predictions
ant1_int_sample <- plogis(mat_ant_m1[,"b_Intercept"])
ant1_treat_sample <- plogis(mat_ant_m1[,"b_Intercept"] + mat_ant_m1[,"b_TreatmentT"])

ant_m1_df <- data.frame(c(rep("C", times=10000), rep("T", times=10000)),
                         c(ant1_int_sample, ant1_treat_sample))
colnames(ant_m1_df) <- c("Treatment", "y")

ant_predicted <- ggplot(ant_m1_df, aes(x=Treatment, y=y, fill=Treatment)) + 
  geom_boxplot() + theme_classic() + ylim(0,0.2) + ylab("Predicted value")
mean(all_data$Antennation>0)
antennation_box_nz <- ggplot(all_data[all_data$Antennation>0,], aes(x=Treatment, y=Antennation, fill=Treatment)) + 
  geom_boxplot() + theme_classic() + ylim(0,0.5)

antennation_box <- ggplot(all_data, aes(x=Treatment, y=Antennation, fill=Treatment)) + 
  geom_boxplot() + theme_classic()+ ylim(0,0.2)

(antennation_box | antennation_box_nz | ant_predicted)

# Back-transformed effect size esimates
ant_t_effect <- plogis(mat_ant_m1[,"b_TreatmentT"] + mat_ant_m1[,"b_Intercept"])-plogis(mat_ant_m1[,"b_Intercept"])
median(ant_t_effect)
ant_effects_df <- data.frame(c(rep("Treatment", times=10000)),
                              c(ant_t_effect))
colnames(ant_effects_df) <- c("Condition", "Effect")

ant_effects <- ggplot(ant_effects_df, aes(x=Condition, y=Effect, fill=Condition)) + 
  geom_violinhalf() + theme_classic() + ylab("Effect size (proportion change)") + 
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color="grey") +
  scale_fill_manual(values=c("#f7fcf0", "#ccebc5", "#7bccc4", "#2b8cbe"))
ant_effects

# Find nice way to visualise HDI and ROPE
t_hdi_plot <- plot(hdi(ant_t_effect, ci=c(0.89, 0.95))) + scale_fill_manual(values=c("cyan4", "cyan3", "white")) + theme_classic() + ggtitle("Treatment") + xlim(-0.05, 0.1)
t_hdi_plot

ppc_stat_grouped(y = all_data$Antennation,
                 yrep = posterior_predict(ant_m4$model, draws = 10),
                 group=all_data$Treatment,
                 stat=mean)


post_pred <- posterior_predict(ant_m3$model, draws = 10)
dim(post_pred)
all_data$Antennation==0
post_pred

ant_predicted <- ggplot(ant_m1_df, aes(x=Treatment, y=y, fill=Treatment)) + 
  geom_boxplot() + theme_classic() + ylim(0,0.2) + ylab("Predicted value")

antennation_box <- ggplot(all_data, aes(x=Treatment, y=Antennation, fill=Treatment)) + 
  geom_boxplot() + theme_classic()+ ylim(0,0.2)

(antennation_box | ant_predicted)


y_mean_ant <- c(median(ant_m1_df$y[which(ant_m1_df$Treatment=="C")]),
            median(ant_m1_df$y[which(ant_m1_df$Treatment=="T")]))

y_hdi_l_ant <- c(hdi(ant_m1_df$y[which(ant_m1_df$Treatment=="C")])[[2]],
             hdi(ant_m1_df$y[which(ant_m1_df$Treatment=="T")])[[2]])

y_hdi_h_ant <- c(hdi(ant_m1_df$y[which(ant_m1_df$Treatment=="C")])[[3]],
             hdi(ant_m1_df$y[which(ant_m1_df$Treatment=="T")])[[3]])

new_df_ant <- data.frame(y_mean_ant, y_hdi_l_ant, y_hdi_h_ant, c("C", "T"))
colnames(new_df_ant) <- c("Median", "lower", "upper", "Treatment")
new_df_ant

p_ant<- ggplot(new_df_ant, aes(x=Treatment, y=Median, color=Treatment)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=y_hdi_l_ant, ymax=y_hdi_h_ant), width=.2) +
  theme_classic() +
  ylim(c(0,0.1))
p_ant

(antennation_box | p_ant)
