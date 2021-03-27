allogrooming_box <- ggplot(all_data, aes(x=Day, y=Allogrooming, fill=Treatment)) + 
  geom_boxplot() + theme_classic() + ylim(0,0.5) + ylab("Proportion time spent") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14))
p_allo<- ggplot(new_df_allo, aes(x=Day, y=Median, group=Treatment, color=Treatment)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=y_hdi_l_allo, ymax=y_hdi_h_allo), width=.2) +
  theme_classic() +
  ylim(c(0,0.5)) +
  ylab("Predicted proportion time spent") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14))
(allogrooming_box | p_allo)


antennation_box <- ggplot(all_data, aes(x=Treatment, y=Antennation, fill=Treatment)) + 
  geom_boxplot() + theme_classic() + ylab("Proportion time spent") + ylim(c(0,0.2)) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14))
p_ant<- ggplot(new_df_ant, aes(x=Treatment, y=Median, color=Treatment)) + 
  #geom_segment(x=1, y=new_df_ant$Median[which(new_df_ant$Treatment=="C")], xend=2, yend=new_df_ant$Median[which(new_df_ant$Treatment=="T")], color="grey") +
  geom_point()+
  geom_errorbar(aes(ymin=y_hdi_l_ant, ymax=y_hdi_h_ant), width=.2) +
  theme_classic() +
  ylim(c(0,0.2)) +
  ylab("Predicted proportion time spent") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14))
(antennation_box | p_ant)

environment_box <- ggplot(all_data, aes(x=Day, y=Environment)) + 
  geom_boxplot(fill="grey") + theme_classic() + ylim(c(0,0.8)) + ylab("Proportion time spent") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14))
p_env<- ggplot(new_df_env, aes(x=Day, y=Median)) + 
  geom_segment(x=1, y=new_df_env$Median[which(new_df_env$Day=="2")][1], xend=2, yend=new_df_env$Median[which(new_df_env$Day=="4")][1], color="grey") +
  geom_segment(x=2, y=new_df_env$Median[which(new_df_env$Day=="4")][1], xend=3, yend=new_df_env$Median[which(new_df_env$Day=="10")][1], color="grey") +
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
  theme_classic() +
  ylim(c(0,0.8)) +
  ylab("Predicted proportion time spent") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14))
(environment_box | p_env)

trophallaxis_box <- ggplot(all_data, aes(x=Day, y=Trophallaxis, fill=Treatment)) + 
  geom_boxplot() + theme_classic() + ylim(c(0,0.1)) + ylab("Proportion time spent") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14))
p_tro<- ggplot(new_df_tro, aes(x=Day, y=Median, group=Treatment, color=Treatment)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=y_hdi_l_tro, ymax=y_hdi_h_tro), width=.2) +
  theme_classic() +
  ylim(c(0,0.1)) +
  ylab("Predicted proportion time spent") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14))
(trophallaxis_box | p_tro)

butting_box <- ggplot(all_data, aes(x=Day, y=Butting,fill=Treatment)) + 
  geom_boxplot() + theme_classic() + ylim(c(0,17)) + ylab("Number of events") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14))
p_but<- ggplot(new_df_but, aes(x=Day, y=Median)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
  theme_classic() +
  ylim(c(0,17)) + 
  ylab("Predicted number of events") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14))
(butting_box | p_but)

selfgrooming_box <- ggplot(all_data, aes(x=Treatment, y=Self.grooming,fill=Treatment)) + 
  geom_boxplot() + theme_classic() + ylim(c(0,26)) + ylab("Number of events") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14))
p_sg<- ggplot(new_df_sg, aes(x=Treatment, y=Median, color=Treatment)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=y_hdi_l_sg, ymax=y_hdi_h_sg), width=.2) +
  theme_classic() + 
  ylim(c(0,26)) +
  ylab("Predicted number of events") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14))
(selfgrooming_box | p_sg)
