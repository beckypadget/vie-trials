allo_effects <- ggplot(allo_effects_df, aes(x=Condition, y=Effect, fill=Condition)) + 
  geom_violinhalf() + theme_classic() + ylab("Effect size (additional proportion time)") + 
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color="grey") +
  scale_fill_manual(values=c("#f7fcf0", "#ccebc5", "#7bccc4", "#2b8cbe"))
allo_effects

ant_effects <- ggplot(ant_effects_df, aes(x=Condition, y=Effect, fill=Condition)) + 
  geom_violinhalf() + theme_classic() + ylab("Effect size (additional proportion time)") + 
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color="grey") +
  scale_fill_manual(values=c("#f7fcf0", "#ccebc5", "#7bccc4", "#2b8cbe"))
ant_effects

env_effects <- ggplot(env_effects_df, aes(x=Condition, y=Effect, fill=Condition)) + 
  geom_violinhalf() + theme_classic() + ylab("Effect size (additional proportion time)") + 
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color="grey") +
  scale_fill_manual(values=c("#f7fcf0", "#ccebc5", "#7bccc4", "#2b8cbe"))
env_effects

tro_effects <- ggplot(tro_effects_df, aes(x=Condition, y=Effect, fill=Condition)) + 
  geom_violinhalf() + theme_classic() + ylab("Effect size (additional proportion time)") + 
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color="grey") +
  scale_fill_manual(values=c("#f7fcf0", "#e0f3db","#ccebc5", "#7bccc4", "#2b8cbe", "#08589e"))
tro_effects

but_effects <- ggplot(but_effects_df, aes(x=Condition, y=Effect, fill=Condition)) + 
  geom_violinhalf() + theme_classic() + ylab("Effect size (additional proportion time)") + 
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color="grey") +
  scale_fill_manual(values=c("#f7fcf0", "#e0f3db","#ccebc5", "#7bccc4", "#2b8cbe", "#08589e"))
but_effects

sg_effects <- ggplot(sg_effects_df, aes(x=Condition, y=Effect, fill=Condition)) + 
  geom_violinhalf() + theme_classic() + ylab("Effect size (additional proportion time)") + 
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color="grey") +
  scale_fill_manual(values=c("#f7fcf0", "#ccebc5", "#7bccc4", "#2b8cbe"))
sg_effects

