## Allogrooming HDI plots
allo_t_hdi <- plot(hdi(mat_allo_m3[,'b_TreatmentT'], ci=c(0.89, 0.9, 0.95))) + 
  scale_fill_manual(values=c("#f7fcf0", "#ccebc5", "#7bccc4", "white")) + 
  theme_classic() + 
  ggtitle("Treatment") 
#allo_t_hdi

allo_d4_hdi <- plot(hdi(mat_allo_m3[,'b_Day4'], ci=c(0.89, 0.9, 0.95))) + 
  scale_fill_manual(values=c("#f7fcf0", "#ccebc5", "#7bccc4", "white")) + 
  theme_classic() + 
  ggtitle("Day 4") 
#allo_d4_hdi

allo_d10_hdi <- plot(hdi(mat_allo_m3[,'b_Day10'], ci=c(0.89, 0.9, 0.95))) + 
  scale_fill_manual(values=c("#f7fcf0", "#ccebc5", "#7bccc4", "white")) + 
  theme_classic() + 
  ggtitle("Day 10") 
#allo_d10_hdi

## Antennation HDI plot
ant_t_hdi <- plot(hdi(mat_ant_m1[,'b_TreatmentT'], ci=c(0.89, 0.9, 0.95))) + 
  scale_fill_manual(values=c("#f7fcf0", "#ccebc5", "#7bccc4", "white")) + 
  theme_classic() + 
  ggtitle("Treatment")
#ant_t_hdi

## Environment HDI plots
env_d4_hdi <- plot(hdi(mat_env_m2[,'b_Day4'], ci=c(0.89, 0.9, 0.95))) + 
  scale_fill_manual(values=c("#f7fcf0", "#ccebc5", "#7bccc4", "white")) + 
  theme_classic() + 
  ggtitle("Day 4") 
#env_d4_hdi

env_d10_hdi <- plot(hdi(mat_env_m2[,'b_Day10'], ci=c(0.89, 0.9, 0.95))) + 
  scale_fill_manual(values=c("#f7fcf0", "#ccebc5", "#7bccc4", "white")) + 
  theme_classic() + 
  ggtitle("Day 10") 
#env_d10_hdi

## Trophallaxis HDI plots
tro_c4_hdi <- plot(hdi(mat_tro_m4[,'b_Day4'], ci=c(0.89, 0.9, 0.95))) + 
  scale_fill_manual(values=c("#f7fcf0", "#ccebc5", "#7bccc4", "white")) + 
  theme_classic() + 
  ggtitle("Control/Day 4") 
#tro_c4_hdi

tro_c10_hdi <- plot(hdi(mat_tro_m4[,'b_Day10'], ci=c(0.89, 0.9, 0.95))) + 
  scale_fill_manual(values=c("#f7fcf0", "#ccebc5", "#7bccc4", "white")) + 
  theme_classic() + 
  ggtitle("Control/Day 10")
#tro_c10_hdi

tro_t2_hdi <- plot(hdi(mat_tro_m4[,'b_TreatmentT'], ci=c(0.89, 0.9, 0.95))) + 
  scale_fill_manual(values=c("#f7fcf0", "#ccebc5", "#7bccc4", "white")) + 
  theme_classic() + 
  ggtitle("Treatment/Day 2") 
#tro_t2_hdi

tro_t4_hdi <- plot(hdi(mat_tro_m4[,'b_TreatmentT:Day4'], ci=c(0.89, 0.9, 0.95))) + 
  scale_fill_manual(values=c("#f7fcf0", "#ccebc5", "#7bccc4", "white")) + 
  theme_classic() + 
  ggtitle("Treatment/Day 4")
#tro_t4_hdi

tro_t10_hdi <- plot(hdi(mat_tro_m4[,'b_Treatment:Day10'], ci=c(0.89, 0.9, 0.95))) + 
  scale_fill_manual(values=c("#f7fcf0", "#ccebc5", "#7bccc4", "white")) + 
  theme_classic() + 
  ggtitle("Treatment/Day 10") 
#tro_t10_hdi

## Butting HDI plots
but_d4_hdi <- plot(hdi(mat_but_m2[,'Day4'], ci=c(0.89, 0.9, 0.95))) + 
  scale_fill_manual(values=c("#f7fcf0", "#ccebc5", "#7bccc4", "white")) + 
  theme_classic() + 
  ggtitle("Day 4") 
#but_d4_hdi

but_d10_hdi <- plot(hdi(mat_but_m2[,'Day10'], ci=c(0.89, 0.9, 0.95))) + 
  scale_fill_manual(values=c("#f7fcf0", "#ccebc5", "#7bccc4", "white")) + 
  theme_classic() + 
  ggtitle("Day 10") 
#but_d10_hdi

## Self-grooming HDI plot
sg_t_hdi <- plot(hdi(mat_sg_m1[,'TreatmentT'], ci=c(0.89, 0.9, 0.95))) + 
  scale_fill_manual(values=c("#f7fcf0", "#ccebc5", "#7bccc4", "white")) + 
  theme_classic() + 
  ggtitle("Treatment") 
#sg_t_hdi

(allo_t_hdi | allo_d4_hdi | allo_d10_hdi)
(ant_t_hdi)
(env_d4_hdi | env_d10_hdi)
((tro_c4_hdi | tro_c10_hdi | tro_t2_hdi )/(tro_t4_hdi | tro_t10_hdi))
(but_d4_hdi | but_d10_hdi)
(sg_t_hdi)
